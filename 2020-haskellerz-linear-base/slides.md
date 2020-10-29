Experience report on -XLinearTypes and linear-base
============

Utku Demir (utdemir)

me@utdemir.com
utku.demir@tweag.io

---

Intro
=====

- I used the _LinearTypes_ extension for a month or so, while working
on _linear-base_.

- I am not the right person to talk to about its implementation or about
linear logic.

- But I will still talk about my experience of using it, as someone
without much of a background.

---

-XLinearTypes
=============

- A way to declare _how many times_ a function uses its parameter.

```{.haskell}
f :: Foo %Many -> Bar
g :: Foo %One -> Bar

g :: Foo %1-> Bar
g :: Foo ⊸ Bar
```

- Old syntaxes:

```{.haskell}
g :: Foo #-> Bar
g :: Foo ->. Bar
```

**Note**: Using a composite type "exactly once" is using each of its fields
exactly once. One can use GADTSyntax to fine tune this.

---

-XLinearTypes
=============

- Enforcing "conservation" and "consumption" of values

```{.haskell}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude.Linear

reverse :: [a] %1-> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
```

Given:

```{.haskell}
(Prelude.Linear.++) :: [a] %1-> [a] %1-> [a]
```

- Implementing a safer sort with linear types
    - https://www.tweag.io/blog/2018-03-08-linear-sort/

---

-XLinearTypes
=============

- Enforcing "conservation" and "consumption" of values
- Enforcing uniqueness

```{.haskell}
data Currency where
  -- Hide this constructor
  Currency :: Int -> Currency

with :: Int -> (Currency %1-> Ur a) %1-> Ur a
with total f = f (Currency total)

amount :: Currency %1-> (Ur Int, Currency)
amount (Currency v) = (Ur v, Currency v)

add :: Currency %1-> Currency %1-> Currency
add (Currency m1) (Currency m2) = Currency (m1 + m2)

split :: Int -> Currency %1-> (Currency, Currency)
split v (Currency total) =
  let v' = clamp 0 total v
  in  (Currency v', Currency (total - v'))
```

---

-XLinearTypes
=============

- Encode state transitions in types using linear types
    - https://www.tweag.io/blog/2017-08-03-linear-typestates/
- Making two garbage collectors be good neighbours
    - https://www.tweag.io/blog/2017-11-29-linear-jvm/

---

-XLinearTypes
=============

- Enforcing uniqueness

```{.haskell}
HashMap.empty
  :: (Hashable k, Eq, k) => (HashMap k v %1-> Ur b) %1-> Ur b

HashMap.insert
  :: ... => k -> v -> HashMap k v %1-> HashMap k v

HashMap.lookup
  :: ... => k -> HashMap k v %1-> (Ur (Maybe v), HashMap k v)

f :: [(Int, Bool)]
f =
  unur . HashMap.empty $ \hm ->
    hm
      & HashMap.insert 1 True
      & HashMap.insert 5 False
      & HashMap.lookup 6
      & \(Ur val, hm) -> HashMap.insert 12 (maybe False not val) hm
      & HashMap.toList
```
---

linear-base
===========

- _base_ with linearity information
    - Typeclasses with linear signatures

- Utilities for working with linear functions
    - Unrestricted, Consumable, Dupable

- Pure collections with in-place updates
    - Arrays, vectors, maps and sets

- Allocation free array pipelines (polarized arrays)

- A safe resource abstraction

- A fork of _streaming_ using linear monads

- (very experimental) Tools to use non GC'd memory safely

---

Limitations
===========

(from worst to not-so-worst)

* Missing ecosystem.

* Unhelpful error messages.

* Having to switch between unrestricted and linear functions.

* Some Haskell structures can not be used with linear values:
    * let, case bindings
    * ViewPatterns

* Type inference only infers unrestricted arrows.

---

The last words
==============

* It is simple and intuitive to use.

* It will become much better as more people use it and it is further
developed.

* I am looking forward to see what people will come up with.

---

How to use it
=============

- Landed in GHC HEAD, coming soon in: GHC 9.0.1

- _haskell.compiler.{ghc901,ghcHEAD}_ on nixpkgs.

- linear-base's README has links to resources:
    - https://github.com/tweag/linear-base

---

Questions?
=========

---

Extra: Unrestricted
===================

```{.haskell}
data Ur a where
  Ur :: a -> Ur a

f :: Ur Int %1-> Ur (Int, Bool)
f (Ur i) = Ur (i * 2, i > 0)
```

---

-XLinearTypes
=============

- Enforcing "conservation" and "consumption" of values

```{.haskell}
mkDough :: Egg -> Flour -> Dough
mkFrosting :: Egg -> Sugar -> Frosting
mkCake :: Dough -> Frosting -> Cake

bake :: Egg -> Flour -> Sugar -> Cake
bake egg flour sugar =
  mkDough
    (mkDough egg flour)
    (mkFrosting egg sugar)
```

---

-XLinearTypes
=============

- Enforcing "conservation" and "consumption" of values

```{.haskell}
mkDough :: Egg %1-> Flour %1-> Dough
mkFrosting :: Egg %1-> Sugar %1-> Frosting
mkCake :: Dough %1-> Frosting %1-> Cake

bake :: Egg %1-> Egg %1-> Flour %1-> Sugar %1-> Cake
bake egg1 egg2 flour sugar =
  mkDough
    (mkDough egg1 flour)
    (mkFrosting egg2 sugar)
```

---


