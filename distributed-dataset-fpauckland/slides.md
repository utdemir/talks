---
title: distributed-dataset
author: Utku Demir
date: March 2019, FP Auckland Meetup

---

## Why do we distribute data?

* Lots of data
* Arbitrarily complex queries
* Fault-tolerancy

\vspace{5em}

### Why we do **not** distribute data?

* Network latency
* Less flexible
* Faults

---

## How?

* Store a **Dataset** as **Partition**s.
* Lazily transform this Dataset with functional operators
  * map, filter, reduce eg.
* 

\small
```haskell
mkPartition :: Closure (ConduitT () a (ResourceT IO) ()) -> Partition a
dExternal :: [Partition a] -> Dataset a
dMap    :: Closure (a -> b) -> Dataset a -> Dataset b
dFilter :: Closure (a -> Bool) -> Dataset a -> Dataset a
```


* **Executor**s read/write to partitions
* **Narrow** operations only operate over one partition
  * map, filter
* **Wide** operations require data from multiple partitions
  * aggregations, joins
  * expensive, requires coordination

---

# Composable Aggregations

```haskell
data Aggr a b =
  forall t. StaticSerialise t =>
  Aggr (Closure (a -> t))                     -- ^ Map
       (Closure (Dict (CommutativeMonoid t))) -- ^ Reduce
       (Closure (t -> b))                     -- ^ Extract
      
instance StaticApply (Aggr a)
instance StaticProfunctor Aggr 

dAggr :: Aggr a b -> Dataset a -> IO b
dGroupedAggr :: (a -> k) -> Aggr a b 
             -> Dataset a -> Dataset (k, b)
```

# Thanks!

Questions?

--- 

# Extras

---

# Example

\small
```haskell
 ghArchive (fromGregorian 2018 1 1, fromGregorian 2018 12 31)
   & dConcatMap (static (\e ->
       let author = e ^. gheActor . ghaLogin
           commits = e ^.. gheType . _GHPushEvent 
                       . ghpepCommits . traverse . ghcMessage
       in  map (author, ) commits
     ))
   & dFilter (static (\(_, commit) ->
       T.pack "cabal" `T.isInfixOf` T.toLower commit
     ))
   & dMap (static fst)
   & dGroupedAggr 50 (static id) dCount
   & dToList
```

---

# An external data source

\small
```haskell
import Network.HTTP.Simple
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.JSON.NewlineDelimited as NDJ

data GHEvent = ... deriving FromJSON

urlToPartition :: String -> Partition GHEvent
urlToPartition url' = mkPartition $ (\url -> do
  req <- parseRequest url
  httpSource req getResponseBody 
    .| ungzip
    .| NDJ.eitherParser @_ @GHEvent
    .| C.mapM (either fail return)
  ) `cap` cpure (static Dict) url'
```

---

# Aggregation Example

```haskell
input & groupedAggr 3 (static getColor) dSum
```

* Input
  * Partition 1: [$\textcolor{red}{3}$, $\textcolor{green}{5}$, $\textcolor{red}{2}$, $\textcolor{black}{1}$]
  * Partition 2: [$\textcolor{blue}{3}$, $\textcolor{red}{7}$, $\textcolor{blue}{2}$]
  * Partition 3: [$\textcolor{green}{1}$, $\textcolor{red}{2}$, $\textcolor{blue}{8}$]
* Aggregation Step 1:
  * Partition 1: [$\textcolor{red}{5}$, $\textcolor{green}{5}$, $\textcolor{black}{1}$]
  * Partition 2: [$\textcolor{blue}{5}$, $\textcolor{red}{7}$]
  * Partition 3: [$\textcolor{green}{1}$, $\textcolor{red}{2}$, $\textcolor{blue}{8}$]
* Shuffle!
  * Partition 1: [$\textcolor{red}{5}$, $\textcolor{red}{7}$, $\textcolor{red}{2}$]
  * Partition 2: [$\textcolor{green}{5}$, $\textcolor{green}{1}$, $\textcolor{black}{1}$]
  * Partition 3: [$\textcolor{blue}{5}$, $\textcolor{blue}{8}$]
* Aggregation Step 2:
  * Partition 1: [$\textcolor{red}{14}$]
  * Partition 2: [$\textcolor{green}{6}$, $\textcolor{black}{1}$]
  * Partition 3: [$\textcolor{blue}{13}$]
```
