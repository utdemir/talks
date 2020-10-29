{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTSyntax #-}

import Prelude.Linear
import qualified Data.Array.Mutable.Linear as Array
import qualified Data.HashMap.Mutable.Linear as HashMap
import Data.Vector (Vector)

-----------------------------------------------------------

reverse' :: [a] %1-> [a]
reverse' = foldl (flip (:)) []

-----------------------------------------------------------

data Currency where
  Currency :: Int -> Currency

create :: Int -> (Currency %1-> Ur a) %1-> Ur a
create total f = f (Currency total)

amount :: Currency %1-> (Ur Int, Currency)
amount (Currency v) = (Ur v, Currency v)

add :: Currency %1-> Currency %1-> Currency
add (Currency m1) (Currency m2) = Currency (m1 + m2)

split :: Int -> Currency %1-> (Currency, Currency)
split v (Currency total) =
  let v' = clamp 0 total v
  in  (Currency v', Currency (total - v'))
 where
  clamp lo hi = max lo . min hi

-----------------------------------------------------------

doArrayStuff :: Vector Bool
doArrayStuff =
  unur . Array.alloc 100 False $ \arr ->
    arr
      & Array.set 1 True
      & Array.set 5 False
      & Array.get 6
      & \(Ur val, arr) -> Array.set 12 (not val) arr
      & Array.freeze

-----------------------------------------------------------

doHashMapStuff :: [(Int, Bool)]
doHashMapStuff =
  unur . HashMap.empty 5 $ \hm ->
    hm
      & HashMap.insert 1 True
      & HashMap.insert 5 False
      & HashMap.lookup 6
      & \(Ur val, hm) -> HashMap.insert 12 (maybe False not val) hm
      & HashMap.toList

-----------------------------------------------------------

newtype DList a = DL { unDL :: [a] %1-> [a] }

-- | Converting a normal list to a dlist
fromList :: [a] %1-> DList a
fromList = DL . (++)
{-# INLINE fromList #-}

-- | Converting a dlist back to a normal list
toList :: DList a %1-> [a]
toList (DL f) = f []
{-# INLINE toList #-}

-- | Create a difference list containing no elements
empty :: DList a
empty = DL id
{-# INLINE empty #-}

-- | Create difference list with given single element
singleton :: a %1-> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

-- | /O(1)/, Prepend a single element to a difference list
infixr `cons`
cons :: a %1-> DList a %1-> DList a
cons x (DL xs) = DL ((x:) . xs)
{-# INLINE cons #-}

-- | /O(1)/, Append a single element at a difference list
infixl `snoc`
snoc :: DList a %1-> a %1-> DList a
snoc (DL xs) x = DL (xs . (x:))
{-# INLINE snoc #-}

-- | /O(1)/, Appending difference lists
append :: DList a %1-> DList a %1-> DList a
append (DL xs) (DL ys) = DL (xs . ys)

-----------------------------------------------------------

main :: IO ()
main = putStrLn "Done."
