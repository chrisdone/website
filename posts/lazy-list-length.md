---
date: 2018-08-23
title: Lazy list length
description: Lazy list length
author: Chris Done
tags: haskell
---

To get the length of a list like

```haskell
length [1..10] == 10
```

You have to walk the whole list to check that it's indeed of length
`10`.

Another way is to define a natural number in peano arithmetic style:

``` haskell
data Nat = Zero | Add1 Nat
  deriivng (Ord, Eq)
```

So

``` haskell
-- 0 = Zero
-- 1 = Add1 Zero
-- 2 = Add1 (Add1 Zero)
-- 3 = Add1 (Add1 (Add1 Zero))
```

We define a trivial instance of `Num` for it:

``` haskell
instance Num Nat where
  Zero + y = y
  Add1 x + y = x + (Add1 y)
  fromInteger 0 = Zero
  fromInteger n = Add1 (fromInteger (n - 1))
```

Now we can use `genericLength` on a list like this. Equal-sized lists:

``` haskell
> genericLength [1..3] == (genericLength [1..3] :: Nat)
True
```

With infinite lists:

``` haskell
> genericLength [1..3] < (genericLength [1..] :: Nat)
True
> genericLength [1..3] == (genericLength [1..] :: Nat)
False
> genericLength [1..3] > (genericLength [1..] :: Nat)
False
```

We didn't have to walk the whole list to compare its length with a
finite number. That's pretty cool.
