---
date: 2020-12-21
title: Try.do for recoverable errors in Haskell
description: Try.do for recoverable errors in Haskell
author: Chris Done
tags: haskell
---

**UPDATE 2021-01-02**: I have since written [a GHC compiler plugin to
  implement an alternative `?`-based syntax for early return.](https://github.com/inflex-io/early)
  I prefer that one than use of `Try.do`, because it doesn't require
  any type magic or special instances, and the `?` is more readable.

---

**UPDATE**: I've added a follow-up post to this
  [here](/posts/is-try-do-dangerous/), where I address some criticisms
  of this post.

---

The first half of this post is
[here](/posts/recoverable-errors-in-haskell). Please read that for context.

## Qualified do

One thing that struck me was that our earlier `Make the IO action
return Either` approach produced code that was still perfectly
satisfying the unliftio laws. Perhaps, like in Rust, we need a
syntactic solution to the problem.

Enter
[QualifiedDo](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html),
which will be available on the 9.0.1 version of GHC. What this would
allow us to do is rebind `(>>=)` to mean what we'd like:

``` haskell
module Try ((Try.>>=)) where
(>>=) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
(>>=) m f = do
  result <- m
  case result of
    Left e -> pure (Left e)
    Right a -> f a
```

We put this in a module called `Try` and import it with `QualifiedDo`
enabled.

Now our code becomes:

```haskell
Try.do constraints <- constrainRenamed renamed
       solved <- solveConstraints constraints
       generalised <- generaliseSolved solved
       resolveGeneralised generalised
```

where each action's type is `SomeThing -> IO (Either ErrorType OtherThing)`.

Full working example:

```haskell
{-# LANGUAGE QualifiedDo #-}
import Try
data ErrorType = OhNo deriving (Show)
action1 :: IO (Either ErrorType Int)
action1 = pure (Right 10)
action2 :: Int -> IO (Either ErrorType Int)
action2 x = pure (Left OhNo)
action3 :: Int -> IO (Either ErrorType Int)
action3 x = pure (Right (x+30))
main = do
  result <-
    Try.do
      output <- action1
      output2 <- action2 output
      output3 <- action3 output2
  print result
```

If you want a final return, you need to wrap it up in `Either`, as:

``` haskell
main = do
  result <-
    Try.do
      output <- action1
      output2 <- action2 output
      output3 <- action3 output2
      pure (Right output3)
  print result
```

Otherwise it won't match our type:

``` haskell
Main.hs:16:7: error:
    • Couldn't match type ‘Int’ with ‘Either ErrorType b0’
      Expected: IO (Either ErrorType b0)
        Actual: IO Int

```

People who know a bit of Rust will see this as a familiar pattern;
putting `Ok(output3)` at the end of your function.

What did we gain? We can have our cake and eat it too. We get a
trivial, syntactically-lightweight, way to string possibly-failing
actions together, while retaining all the benefits of being an
unliftio-able monad.

**Verdict**: Best of all worlds.

## Good things come to those who wait

Unfortunately, it'll be a while before I'll be upgrading to this
version of GHC, but I look forward to being able to use this time
saver.
