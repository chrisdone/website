---
date: 2021-04-10
title: The Movement Principle
description: The Movement Principle
author: Chris Done
tags: haskell, plt
---

A language design principle I wanted to highlight in a page of its own
is what I'm calling the movement principle.

In particular, for statically-typed languages, type-classes are what I
have in mind for this post.

## Type-classes

We can write a piece of code like this:

``` haskell
thing = 'a'
example = length (show thing) * 2 + 7
```

There's a type for this `example` expression: `Int`

The language compiler infers that. Some observations:

* The operators `*` and `+` are generic functions that will work on any
  numeric type.
* The `length` function returns the length of a sequence as a specific
  `Int` type.
* The `show` function is a generic function that will show a value as
  a string.

Now I can perform a step to move this logic out of this function into
its own:

``` haskell
thing = 'a'
example = work thing
work x = length (show x) * 2 + 7
```

There's a type for `work`, too: `Show a => a -> Int`

The language compiler infers that too. That means: the input is `a`
and, by the way, it has a constraint that there's an instance of the
class `Show` for that `a`, and the output is `Int`.

We can do it again. Let's move the arithmetic to its own function:

``` haskell
thing = 'a'
example = work thing
work x = arith (length (show x))
arith n = n * 2 + 7
```

There's a type for `arith`, too: `Num a => a -> a`

The language compiler infers that too. As above, this means: input and
output is a type `a` that is an instance of `Num` (numbers).

What's going on here? The type system is saying: I don't know which
instance to use for this function, so I'm going to make it someone
else's problem and throw a constraint on the type. Job done.

This is what I'm calling "movement". I can move my code freely and
there is still a type for the code and the compiler will accept it.

## Type-directed name resolution

If we suppose that instead of type-classes we had type-directed name
resolution, such as seen in Idris, or C++ (function overloading), we
run into a problem at the first step. Consider an imaginary Haskell
where operator overloading is achieved via this mechanism instead of
type-classes:

``` haskell
n :: Int
n = 1
example = n + 5
```

If we imagine that `+` is one of many functions called `+`: one for
`Int`, one for `Float`, etc. and then the compiler picks which one you
intended based on the type, then it can see `n` is an `Int` and then
pick the right `+` which has type `Int -> Int -> Int`.

However, if we try to abstract this:

``` haskell
n :: Int
n = 1
example = oops n
oops x = x + 5
```

What is the type for `oops`? We can't give one, because we don't know
which `+` is intended. We don't know what the type of `x` will be. So
this is a compile error. At this point we've interrupted the
programmer with an awkward problem.

This is a very leaky abstraction. I can't even move code.

## Exercise

Here's an exercise. Try this in your statically-typed language of
choice. If you can use overloading of some kind in an expression, and
then abstract the generic part of it into its own function, then your
language's overloading satisfies the movement principle.

## Summary

There are many factors that go into making a language let you move
code easily. One is making everything an expression, rather than
distinguishing between statements and expressions, for
example. Another is to avoid things like move semantics and ownership
as seen in Rust, which puts a lot of friction on moving code.[^1]

I think type-classes are overlooked in contributing to this quality in
a language. They enable generic programming, but in a way that
preserves the movement principle.

[^1]: In Rust, the designers wanted this property.
