---
title: \large Experimenting with Distributed Data Processing in Haskell
author: Utku Demir
date: YOW! Lambda Jam 2019
---

# Me

* 2014 - 2016: **Picus Security**
  * A distributed Haskell application to simulate cyber attacks
* 2016 - now: **Movio**
  * A SaaS to analyze moviegoer data
  * Billions of transactions, millions of people
  * Large-scale data analytics

\note{
Hello everyone. I am going to talk about a distributed data processing
library I was working on over the last 6 months.

First, I want to tell you about how I decided to work on this.
3 years ago, I was working at a startup called Picus Security, where we 
developed a distributed Haskell application.

After that, I moved to New Zealand to work on a data company called Movio,
where analyse moviegoer data. We have a lot of data, we have billions of
movie tickets from millions of people. 

Turns out, techniques we used to process large amounts of data really fits
into functional programming paradigm. We used laziness, pure functions, higher 
order functions. And I thought that Haskell would be a great fit for data
analytics.
}

---

# Distributed Data Processing

- "Big Data"
  - Terabytes?
- Unknown set of complex queries or transformations

\note{
Before we start, lets quickly talk about when do we want to do distributed
data processing. 

It becomes useful when the size of the data vastly exceeds your main memory,
and it takes a long time to read from disk.

Also, I would prefer to use it when my queries are really complex or I have
many different queries.
}

---

# Resilient Distributed Datasets (Apache Spark)

- **Dataset** is a multiset of rows.
- A Dataset is stored as many **Partition**'s.
- Partitions can be processed in parallel on many **executor**'s
- **Driver** coordinates executors.
- High-level combinators for transforming a Dataset
    - `map`, `filter`, `groupBy`, `reduce` eg.

\note{
There are a lot of ways to do distributed data processing, but I'm going to talk 
about a single specific one; it's the technique used by Apache Spark and the core
data structure is called a "Resilient Distributed Dataset".

On this technique, we call our collection of data "Dataset". It contains rows 
without any specific ordering, so it is like a set. However it can contain duplicate
rows, hence it is a multiset.

A really important trick is partitioning. When we are storing our Dataset, we do not
store it as a single entity, but we store them as separate partitions. The point about
partitions is that they can be processed in parallel on many "executors", preferably 
on different machines. And in order to coordinate those executors, we have a "driver"
process which actually runs your code and sends commands to executors to transform
a partition.

In another words, if you have a terabyte of data, instead of having a single thing
which processes terabytes, you can have ten thousand things processing 100 megabytes
each in parallel, so they finish quicker.

As you can imagine, those things are fast, but also more complex. You do not want to
deal with all those partitions and executors and parallelism. So, we hide all those
complexity behind easy-to-use high-level combinators like maps and filters and groupBy's,
so we can express our transformation in a nice looking DSL and all the complexity is
handled by the library.
}

---

# distributed-dataset

- Written in Haskell.
- Borrows ideas from Apache Spark.
- Composes nicely with the existing Haskell ecosystem.
- Can be used with "Function as a Service" / "Serverless" offerings.

\note{
So, "distributed-dataset" is a library, which implements the Dataset idea.

It is written in Haskell, borrows the ideas I mentioned before from Apache Spark.

The two major difference from Apache Spark is that,

distributed-dataset tries to compose with the existing Haskell ecosystem. It doesn't 
come with built-in support for different data formats or compression algorithms, instead
it tries make it easy to use existing Haskell libraries. 

Another difference is that, distributed-dataset is designed to have small, short-lived 
executors. Because of that, it can work with "Function as a Service" or "Serverless" 
services like Apache Spark or Google Cloud Functions. We're going to talk about this
later.
}

---

# -XStaticPointers

* -XStaticPointers

```haskell
    static (\x -> x + 1) :: StaticPtr (Int -> Int)
```

* distributed-closure

```haskell
    data Closure a
    
    closure :: StaticPtr a -> Closure a
    cpure   :: Serializable a => a -> Closure a
    cap     :: Closure (a -> b) -> Closure a -> Closure b
```

\note{
When I started to work on the library, my first issue was to distribute a Haskell function
to the remote machines. If we were using an interpreted language like Lisp or Python, or a 
language with a portable bytecode, distributing a function is easier, you can just send the
function code or the bytecode, and eval it on the remote machine. 

However in Haskell, it's not that easy. If I have a function, I can not serialise it to a 
ByteString, send it over the wire and run it somewhere else. 

However turns out this problem is solved by CloudHaskell, where they, instead of sending the
function itself, they assign functions a unique id at compile time, and send that id instead.
And, you can convert that id back to your function if you are using the same executable on
the remote machine. CloudHaskell used to use Template Haskell to create those id to function
mapping, however now GHC natively supports it with the `StaticPointers` language extensions.

You can see an example here; we have a function which takes a number and adds one to it. The
`static` keyword there takes that function, and returns you a `StaticPtr` and you can serialize
that pointer and send it over the network.

StaticPointers is very useful and really easy to use. But the main issue is that once you have
a Static Pointer, there is nothing much you can do with it. They don't compose with themselves
or with anything else.

In order to overcome this limitation, there's this library called "distributed-closure", which
introduces an algebraic data type called "Closure" and it makes these Static pointers composable.

It provides you the combinators you can see on the slides an much more, and basically means that
you can combine static pointers to each other to build bigger computations, and your result will
still be serialisable.
}

---

# In detail

```haskell
mkPartition :: Closure (ConduitT () a (ResourceT IO) ()) 
            -> Partition a

data Dataset a where
  DExternal  :: StaticSerialise a => [Partition a] -> Dataset a
  
  DPipe      :: (StaticSerialise a, StaticSerialise b)
             => Closure (ConduitT a b (ResourceT IO) ())
             -> Dataset a -> Dataset b
             
  DPartition :: (StaticHashable k, StaticSerialise a)
             => Int
             -> Closure (a -> k)
             -> Dataset a -> Dataset a
```

\note{
Turns out, once you have serialisable Closures, it is really easy to create a distributed data
processing framework. Actually these types you see on the slides are the only things you need.

The first function, `mkPartition` is the definition of a `Partition`. It basically says that,
if you have a `Conduit` which produces values of type `a`, you can create a `Partition` of a.

If you're not familiar with the Haskell ecosystem, you can think of `Conduit` as a "stream 
processor". So whenever you see something like `Conduit a b`, it is a stream processor which 
takes `a`'s and yields `b`'s. And they have a lot of nice properties and they compose with 
each other.

So, once we have our `Partition`'s, then we can define our `Dataset` type. There're multiple
constructors for a dataset, first of them is this `DExternal` constructor. Simply, if you have
a list of partitions, you can create a dataset. This is the only way to create a dataset from
scratch.

The second constructor, `DPipe`, takes a conduit from a to b, and using that converts a Dataset of a
to a Dataset of b. It basically streams every partition through your conduit separately and
upload the result. As you can imagine, using only this `DPipe` we can implement combinators 
like `map` and `filter` easily.

However turns out it is not enough for some operations we want to have, basically we can not
implement operations which require information from multiple partitions. For example, let's say we want
to remove the duplicate rows from a Dataset. Using the `DPipe` constructor, I can remove the 
duplicates within a partition, however it is not enough since there can be duplicates across
different partitions too. In order to solve this problem, I need this `DPartition` combinator.
This combinator takes a key function, takes a `Dataset` and returns the same `Dataset` back at us.
The difference is that, on the returned dataset, every row with the same key is on the same 
partition. Now, with using this DPartition combinators, I can move the equal rows to the same 
partition, and then deduplicate them.

As I said before, turns out these 2 combinators is enough to implement most of the Apache Spark.
And the only thing we do is to implement our functions like `map`, `filter` and `groupBy` in terms
of this DPipe and DPartition combinators.
}

---

# Aggregations

```haskell
-- Aggregates many a's to a single b.
data Aggr a b = ...
      
instance StaticApply (Aggr a)
instance StaticProfunctor Aggr 

aggrFromMonoid :: StaticSerialise a
               => Closure (Dict (Monoid a))
               -> Aggr a a

dAggr :: Aggr a b -> Dataset a -> IO b
dGroupedAggr :: StaticHashable k
             => (a -> k) -> Aggr a b 
             -> Dataset (k, b)
```

\note{
And turns out, aggregating multiple rows together is a very common operation. When you have a 
Dataset with billions of rows, you probably want a summary. For example, you can have a Dataset
containing all your sales figures, and you might want to see your total yearly income, where you
sum up the sale amounts per year.

To do that, we have a type called `Aggr`, which takes two type parameters, input and the output.
How it works is that it aggregates many inputs to a single output value. However, it does it in 
a distributed fashion, where it aggregates each partition within itself, and then repartition the
dataset and aggregates it again.

Turns out, this idea can be represented using a `Monoid`. Since monoids are associative, we can 
monoidally append inputs on each partition, and then monoidally append those results, you
are going to get the same result as doing everything sequentially. This function tells you that,
if you have a monoid instance of `a`, then you can have an aggregation.

Another nice side-effect is that, this Aggr data type has Profunctor and Applicative instances, 
so using the Profunctor instance you can apply the aggregation on a specific field on your input,
and using the Applicative instance you can compose aggregations with each other to create more
complex aggregations.
}

---

# Backend

* LocalProcessBackend

* distributed-dataset-aws

  * Uses AWS Lambda to run executors and S3 to exchange information.
  * Scales well, cost-effective
  * No infrastructure necessary

\note{
Up until now, we talked about expressing our transforms, and now we have to actually execute them.
In order to run the executors, "distributed-dataset" has a concept called a "Backend". 

"Backend"'s are provided as separate libraries, so if you are not happy with the existing backends
you can implement your own. A backend is simply a way to run a Haskell process somewhere, and it 
only requires you to implement a single function. So it is relatively easy to write a new one.

Currently there is two backends implemented, the first one is "LocalProcessBackend", which runs local
processes on the same machine and uses the filesystem to exchange information. As you can imagine,
it is not really useful on production; however it is really useful if you are developing or for the
unit tests.

The more exciting backend is "distributed-dataset-aws", where it uses AWS Lambda to run executors.
The advantage of AWS Lambda is that, 
it scales really good, so in a few seconds we can have thousands of processes running. It is also
cost effective, since as soon as a process finishes, the executor shuts down and AWS stops billing you.
And to me the biggest advantage is that you don't have to have any infrastructure installed on your 
AWS account; if you just have AWS credentials on your machine distributed-dataset will work.
}

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
      "cabal" `T.isInfixOf` T.toLower commit
    ))
  & dGroupedAggr 50 (static fst) dCount
  & dAggr (dTopK (static Dict) 20 (static snd))
  >>= mapM_ (liftIO . print)
```

* 126 GB compressed, 909 GB uncompressed JSON
* 2190 executors
* ~ 2 minutes, including the time to deploy infrastructure

---

# Alternatives in Haskell \small (as far as I know)

## Sparkle

> A library for writing resilient analytics applications in Haskell that
scale to thousands of nodes, using Spark and the rest of the Apache ecosystem
under the hood.

## HSpark

> A port of Apache Spark to Haskell using distributed process

---

# Thanks!

* \url{https://github.com/utdemir/distributed-dataset}
* me@utdemir.com

Questions?

---

# Extras

# An external data source

\small
```haskell
import Network.HTTP.Simple
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.JSON.NewlineDelimited as NDJ

data GHEvent = ... deriving FromJSON

urlToPartition :: String -> Partition GHEvent
urlToPartition url' = mkPartition $ static (\url -> do
  req <- parseRequest url
  httpSource req getResponseBody 
    .| ungzip
    .| NDJ.eitherParser @_ @GHEvent
    .| C.mapM (either fail return)
  ) `cap` cpure (static Dict) url'
```

---

# How to aggregate

```haskell
input & groupedAggr 3 (static getColor) (dSum (static Dict))
```

- Input
    - Partition 1: [$\textcolor{red}{3}$, $\textcolor{green}{5}$, $\textcolor{red}{2}$, $\textcolor{black}{1}$]
    - Partition 2: [$\textcolor{blue}{3}$, $\textcolor{red}{7}$, $\textcolor{blue}{2}$]
    - Partition 3: [$\textcolor{green}{1}$, $\textcolor{red}{2}$, $\textcolor{blue}{8}$]
- Aggregation Step 1:
    - Partition 1: [$\textcolor{red}{5}$, $\textcolor{green}{5}$, $\textcolor{black}{1}$]
    - Partition 2: [$\textcolor{blue}{5}$, $\textcolor{red}{7}$]
    - Partition 3: [$\textcolor{green}{1}$, $\textcolor{red}{2}$, $\textcolor{blue}{8}$]
- Shuffle!
    - Partition 1: [$\textcolor{red}{5}$, $\textcolor{red}{7}$, $\textcolor{red}{2}$]
    - Partition 2: [$\textcolor{green}{5}$, $\textcolor{green}{1}$, $\textcolor{black}{1}$]
    - Partition 3: [$\textcolor{blue}{5}$, $\textcolor{blue}{8}$]
- Aggregation Step 2:
    - Partition 1: [$\textcolor{red}{14}$]
    - Partition 2: [$\textcolor{green}{6}$, $\textcolor{black}{1}$]
    - Partition 3: [$\textcolor{blue}{13}$]

---

# Composing Aggr's

```haskell
dConstAggr :: (Typeable a, Typeable t)
           => Closure a -> Aggr t a

dSum :: StaticSerialise a 
     => Closure (Dict (Num a)) -> Aggr a a

dCount :: Typeable a => Aggr a Integer
dCount = static (const 1) `staticLmap` dSum (static Dict)

dAvg :: Aggr Double Double
dAvg = dConstAggr (static (/))
         `staticApply` dSum (static Dict)
         `staticApply` staticMap (static realToFrac) dCount
```

---

# StaticFunctor

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Typeable f => StaticFunctor f where
  staticMap :: (Typeable a, Typeable b) 
            => Closure (a -> b) -> f a -> f b
```

---

# Results

```haskell
("peti",899)
("haskell-pushbot",535)
("bgamari",418)
("phadej",307)
("23Skidoo",208)
("alanz",174)
("edolstra",141)
("quasicomputational",136)
("jneira",135)
("hvr",133)
("Ericson2314",133)
("felixonmars-bot",130)
("DanielG",129)
("philderbeast",120)
("coreyoconnor",115)
("rcaballeromx",109)
("snoyberg",108)
("aarongable",105)
("e1528532",101)
("RyanGlScott",98)
```
