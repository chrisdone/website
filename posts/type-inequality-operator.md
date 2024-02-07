---
date: 2018-06-19
title: Type inequality operator
description: Type inequality operator
author: Chris Done
tags: haskell
---

Here's a fun trick. If you want to define a function that accepts
everything but one type, you can make a type family like this. If the
two types unify, we produce the type `'False` of kind `Bool`.

``` haskell
type family a /~ b where
  a /~ a = 'False
  _ /~ _ = 'True
```

You can use it like this:

``` haskell
foo :: (Integral i, i /~ Word8 ~ 'True) => i -> ()
foo = undefined
```

The argument can be any type `i` that doesn't unify with `Word8`. So
this type-checks:

``` haskell
bar = foo (undefined ::  Int)
```

But this doesn't:

``` haskell
bad = foo (undefined :: Word8)
```

``` haskell
Couldn't match type ‘'False’ with ‘'True’.
```

Not that I have a use for this. It was just discussed at work.
