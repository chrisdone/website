---
date: 2020-12-21
title: Recoverable errors in Haskell
description: Recoverable errors in Haskell
author: Chris Done
tags: haskell
---

In Haskell, I sometimes have IO-based actions that may produce
failures. The IO aspect is usually incidental; because I need logging
or metrics generation.

When not using a free monad or a fancy effect-system--just plain IO--I
like to follow a mental model similar to
[Rust's definition of error handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html),
which splits them into _recoverable_ vs _unrecoverable_ errors.

In Rust, when a function can fail with a recoverable error, it returns
a `Result`, like:

```rust
Result<OutputType, ErrorType>
```

which the caller can pattern-match on. Let's compare recoverable
errors in Haskell.

## Have the IO action throw a runtime exception

Our code might look like:

```haskell
do constraints <- constrainRenamed renamed
   solved <- solveConstraints constraints
   generalised <- generaliseSolved solved
   resolveGeneralised generalised
```

And each of these steps may throw an exception. We leave it up to code
above in the call chain to remember to catch them.

The trouble with exceptions is that they're not mentioned in the type
system. They're also handled not at the call site, when for
recoverable errors, that is usually the most straight-forward place to
handle them.

It's simply very easy--and it happens all the time--that you either
throw the wrong exception type in the wrong place, or you forget to
catch exceptions at the right place.

**Verdict**: Too dangerous.

## Make the IO action return Either

A more direct approach, which is a bit like Rust, is to simply have
the function return `Either ErrorType OutputType` and then pattern
match on the result to find out whether everything went fine.

Now our code looks like this:

```haskell
do constraints <- constrainRenamed renamed
   case constraints of
     Left err -> return (Left err)
     Right constraints' -> do
       solved <- solveConstraints constraints
       case solved of
         Left err -> return (Left err)
         Right solved' -> do
           generalised <- generaliseSolved solved'
           case generalised of
             Left err -> return (Left err)
             Right generalised' -> do
              resolveGeneralised generalised'
```

This is tedious to write, there's all that repetition on the `Left`
case. Reading it, you also can't immediately tell whether there's any
extra logic going on here.

**Verdict**: Too much code.

## Wrap your IO code up in `ExceptT`

One way to solve this issue is to wrap up these actions in the
`ExceptT` monad transformer. We have to make each of the actions now
live in the `ExceptT` monad, so

```haskell
IO (Either ErrorType OutputType)
```

becomes

```haskell
ExceptT ErrorType IO OutputType
```

And our code goal is clean again:

```haskell
runExceptT
  (do constraints <- constrainRenamed renamed
      solved <- solveConstraints constraints
      generalised <- generaliseSolved solved
      resolveGeneralised generalised)
```

The `runExceptT` produces an `IO (Either ErrorType OutputType)`.

However, `ExceptT` [cannot be an instance of
`MonadUnliftIO`](https://github.com/fpco/unliftio/tree/master/unliftio#limitations)
-- because it necessarily requires multiple exit points. See
[this discussion](https://github.com/fpco/unliftio/issues/68) which
should give you an idea of how hairy and unpredictable this can be.

**This is a big deal.**

Essentially, if a monad is unliftio-able, then:

1. The regular exception system will work as normal. This is
   important, because you do want regular exceptions to be thrown
   upwards.
2. Concurrent programming works as usual: you can use `async` with
   [`UnliftIO.Async`](https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Async.html)
   and get predictable results.
3. Resource clean-up is also straight-forward.

So, `ExceptT` has to be thrown out too, sadly.

**Verdict**: Not compatible.

## Solutions

I previously explored in
[another post](https://chrisdone.com/posts/try-do/) about `Try.do`,
which I've decided against in favor of...

I have since written
[a GHC compiler plugin to implement an alternative `?`-based syntax for early return.](https://github.com/inflex-io/early)
I prefer that one than use of `Try.do`, because it doesn't require any
type magic or special instances, and the `?` is more readable.
