---
date: 2019-12-07
title: Ghosts of Departed Proofs Convenience
description: Ghosts of Departed Proofs Convenience
author: Chris Done
tags: haskell
---

Following on from
[_Ghosts of Departed Proofs_](https://kataskeue.com/gdp.pdf) with a
`Named` type as defined in
[Olliver Charles's post on the same topic](https://ocharles.org.uk/blog/posts/2019-08-09-who-authorized-these-ghosts.html), we can add a usability improvement by defining a `SomeNamed`
existential in order to wrap up the new type introduced for the `n` in
`Name n a`, and then a pattern synonym to conveniently unwrap that
constructor.

``` haskell
-- | Our main engine for naming a value, then we can prove properties
--   about a named value.

{-# LANGUAGE ExistentialQuantification #-} -- Used for SomeNamed.
{-# LANGUAGE PatternSynonyms #-} -- Used for the Name pattern.
{-# LANGUAGE ViewPatterns #-} -- Used for the Name pattern.
{-# LANGUAGE RankNTypes #-} -- Used for withName.

module Named ( Named, pattern Name, forgetName
             , withName, someNamed, SomeNamed(..) ) where

-- | Give a generated type-level name to any value.
newtype Named n a = Named_ { forgetName :: a }

withName :: a -> (forall name. Named name a -> r) -> r
withName x f = f (Named_ x)

-- | A convenient way to name something and access the name later.
data SomeNamed a = forall n. SomeNamed (Named n a)

-- | Wrap a value up with a non-exposed name.
someNamed :: a -> SomeNamed a
someNamed x = SomeNamed (Named_ x)

-- | A convenient way to quickly name a value as a pattern.
pattern Name t <- (someNamed -> SomeNamed t)
```

With this, we can write `case x of Name x' -> ...` and
now we have a named version of `x`. This scales to any number of
tuples or pattern matches.

## Some proof generators

A couple trivial modules for checking non-zeroness of a number, and a
function that makes use of such proof to perform division.

``` haskell
-- | A trivial proof of nonzero for a given named thing.
--
-- Note that only this module can produce an IsNonzero value. Hence
-- you can only get a proof of nonzero via checkNonzero.

module Nonzero ( IsNonzero, checkNonzero ) where

import Named

data IsNonzero name = IsNonzero

checkNonzero :: (Num i, Eq i) => Named name i -> Maybe (IsNonzero name)
checkNonzero named
  | forgetName named /= 0 = Just IsNonzero
  | otherwise = Nothing
```

``` haskell
-- | A simple API that requires proof of nonzero.
--
-- An obvious example: division requires a nonzero denominator.

module Div (divide) where

import Named
import Nonzero

divide :: Fractional i => IsNonzero y -> Named x i -> Named y i -> i
divide _ x y = (forgetName x / forgetName y)
```

## Example use

Here's a simple program that parses two numbers, checks that the
latter is non-zero, and then does a division by that non-zero
number. Trivial, but it helps demonstrate the syntax without
untidiness.

``` haskell
import Div
import Nonzero
import Text.Read
import Named

main = do
  numeratorString <- getLine
  denominatorString <- getLine
  case (,) <$> readMaybe numeratorString
           <*> readMaybe denominatorString :: Maybe (Double, Double) of
    Nothing -> error "Both must be valid numbers."
    Just (Name numeratorNum, Name denominatorNum) ->
      case checkNonzero denominatorNum of
        Nothing -> error "Denominator must be non-zero."
        Just denominatorNonZero ->
          let result = divide denominatorNonZero numeratorNum denominatorNum
          in putStrLn ("Result: " ++ show result)
```

One nice addition is that we can use monad syntax to unwrap them
conveniently:

``` haskell
main2 = do
  numeratorString <- getLine
  denominatorString <- getLine
  let maybeResult = do
        Name numeratorNum   <- readMaybe numeratorString
        Name denominatorNum <- readMaybe denominatorString
        denominatorNonZero  <- checkNonzero denominatorNum
        pure (divide denominatorNonZero numeratorNum denominatorNum)
  maybe
    (error "Something wasn't right and we don't care why.")
    print
    maybeResult
```

With an `Either E` return type, we could return or throw an exception,
if we wanted to.

## Proof is in the pudding: Examples that do not compile

Here are some example variations of `Main` which fail to compile,
demonstrating that this technique is helping the program be more
correct:

This version of main fails to compile simply because I haven't named
the `numeratorInt`.

``` haskell
import Div
import Nonzero
import Text.Read
import Named

main = do
  numeratorString <- getLine
  denominatorString <- getLine
  case (,) <$> readMaybe numeratorString
           <*> readMaybe denominatorString :: Maybe (Double, Double) of
    Nothing -> error "Both must be valid numbers."
    Just (numeratorInt, Name denominatorInt) ->
      case checkNonzero denominatorInt of
        Nothing -> error "Denominator must be non-zero."
        Just denominatorNonZero ->
          let result = divide denominatorNonZero numeratorInt denominatorInt
          in putStrLn ("Result: " ++ show result)
```

Yields the error:

```
Couldn't match expected type ‘Named x0 Double’ with actual type ‘Double’
```

Here is a version where I got the arguments to divide the wrong way round:

``` haskell
import Div
import Nonzero
import Text.Read
import Named

main = do
  numeratorString <- getLine
  denominatorString <- getLine
  case (,) <$> readMaybe numeratorString
           <*> readMaybe denominatorString :: Maybe (Double, Double) of
    Nothing -> error "Both must be valid numbers."
    Just (Name numeratorInt, Name denominatorInt) ->
      case checkNonzero denominatorInt of
        Nothing -> error "Denominator must be non-zero."
        Just denominatorNonZero ->
          let result = divide denominatorNonZero  denominatorInt numeratorInt
          in putStrLn ("Result: " ++ show result)
```

Yields the error:

```
• Couldn't match type ‘n’ with ‘n1’
```

The `denominatorNonZero` proof refers to `denominatorInt` by a
generate name type (`n1`), and `numeratorInt`'s name (`n`) is
different.
