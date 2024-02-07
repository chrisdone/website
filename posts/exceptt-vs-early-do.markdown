---
date: 2021-01-05
title: How exactly ExceptT differs to EarlyDo
description: How exactly ExceptT differs to EarlyDo
author: Chris Done
tags: haskell
---

After publishing my [early](https://github.com/inflex-io/early) GHC
plugin, I saw a lot of response from people saying "Why not
`ExceptT`?", despite having outlined it in the README of the
project. After reading some sincere responses failing to see why I
bothered with this at all, it seems I haven't explained myself well.

I'm happy for people to just disagree because they have different
values, but I want to make sure we're talking about the same
trade-offs.

I think people need to see real code to see why I think this is a
substantial improvement. You'll have to use your imagination
generously to raise the contrived to the "real world".

(I'm calling this plugin `EarlyDo` so I can talk specifically about the
syntax extension.)

# Setup

Here is a module of three functions (but use your imagination to make
a couple dozen), where the IO actions return `Either failure a`. The
proposal I have been repeatedly given is to use `ExceptT`. I'm
typically using RIO, so I define a fake one, and then use a real one
later.

# Basic ExceptT

To do concurrency or exception handling or any with-like thing, you
need the equivalent of `MonadUnliftIO` to capture the current context,
and then re-run the monad further in the new thread. Here we do it
manually to demonstrate what's happening.

```haskell
import Control.Concurrent.Async
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans

type RIO r a = ReaderT r IO a

foo :: Int -> RIO () (Either () ())
foo i = runExceptT $ do
  _ <- ExceptT $ bar (i + 1)
  ExceptT $ zot (i + 2)

bar :: Int -> RIO () (Either () ((), ()))
bar i =
  runExceptT $ do
    ExceptT $ zot 0
    r <- ExceptT $ fmap Right ask
    (x, y) <-
      ExceptT $ lift $
      fmap
        (\(x, y) -> (,) <$> x <*> y)
        (concurrently
         (flip runReaderT r $ foo (i + 1))
         (flip runReaderT r $ zot (i + 2)))
    pure (x, y)

zot :: Int -> RIO () (Either () ())
zot i = runExceptT $ do
  _ <- ExceptT $ bar (i + 1)
  ExceptT $ zot (i + 2)
```

(Why am I not using `ExceptT () (RIO ()) ..` in my type signatures?
Continue reading to the end.)

# Using MonadUnliftIO

Here's a version using `MonadUnliftIO`. Not that the work of unlifting
has been done for me.

``` haskell
import           Control.Monad.Trans.Except
import           RIO (RIO)
import           UnliftIO

foo :: Int -> RIO () (Either () ())
foo i = runExceptT $ do
  _ <- ExceptT $ bar (i + 1)
  ExceptT $ zot (i + 2)

bar :: Int -> RIO () (Either () ((), ()))
bar i =
  runExceptT $ do
    ExceptT $ zot 0
    (x, y) <-
      ExceptT $
      fmap
        (\(x, y) -> (,) <$> x <*> y)
        (concurrently
         (foo (i + 1))
         (zot (i + 2)))
    pure (x, y)

zot :: Int -> RIO () (Either () ())
zot i = runExceptT $ do
  _ <- ExceptT $ bar (i + 1)
  ExceptT $ zot (i + 2)
```

# Evaluating cost and ergonomics

What remains is the `runExceptT` and `ExceptT` calls which cannot be
eliminated. Here we are unsatisfied, because all the functions in our
module are using this "pattern". A pattern is what we call repeating
yourself because your language can't abstract it for you. We pay an
`O(1)` cost per definition, and an `O(n)` cost for calls per
definition.

Additionally, any `traverse` call that is just `MonadIO m => ..` in
the middle of the function, or logging function, will also run in
`ExceptT`, causing a performance penalty that we didn't want. If we
want to avoid that penalty, we must use `lift`. But I haven't
benchmarked this. You are welcome to do so.

You pay to short-circuit and you pay to do normal actions in the base
monad, and if you're not careful you _may_ accidentally pay extra.

Given that pretty much all of my apps have monads that look something
like this, this seemed worth improving.[^1]

# EarlyDo

With `EarlyDo`, we have:

```haskell
{-# OPTIONS -F -pgmF=early #-}
import Control.Early
import RIO (RIO)
import UnliftIO

foo :: Int -> RIO () (Either () ())
foo i = do
 _ <- bar (i + 1)?
 zot (i + 2)

bar :: Int -> RIO () (Either () ((), ()))
bar i = do
  zot 0?
  (x, y) <-
    fmap (\(x, y) -> (,) <$> x <*> y) (concurrently (foo (i + 1)) (zot (i + 2)))?
  pure (Right (x, y))

zot :: Int -> RIO () (Either () ())
zot i = do
  _ <- bar (i + 1)?
  zot (i + 2)
```

A single character that appears in a not-normally-valid syntax
position signals short-circuiting. Any base monad action like `mapRIO`
(runs in `RIO r`) can be used without lifting or unlifting. I have to
explicitly produce `Right` in `bar`, which is a cost, but not in
`zot`/`foo`.

# Using our own monad type

Finally, we can use our own wrapper monad:

``` haskell
{-# OPTIONS -F -pgmF=early #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Early
import RIO (RIO)
import UnliftIO

data AppEnv
newtype App a = App { runApp :: RIO AppEnv a} deriving (Functor, Applicative, Monad, MonadIO)
instance MonadUnliftIO App where withRunInIO = wrappedWithRunInIO App runApp

foo :: Int -> App (Either () ())
foo i = do
  _ <- bar (i + 1)?
  zot (i + 2)

bar :: Int -> App (Either () ((), ()))
bar i = do
  zot 0?
  (x, y) <-
    fmap (\(x, y) -> (,) <$> x <*> y) (concurrently (foo (i + 1)) (zot (i + 2)))?
  pure (Right (x, y))

zot :: Int -> App (Either () ())
zot i = do
  _ <- bar (i + 1)?
  zot (i + 2)
```

The `App` monad has the full power of all the previously mentioned
things.

But not with `ExceptT`. I **cannot put `ExceptT` in this monad stack
and retain the `MonadUnliftIO` instance.** It has no valid instance!

# But you should still use ExceptT because I like it

Could the mechanics of this syntax use `ExceptT` underneath? Sure, but
then the whole do block would be under `ExceptT`, and I'd have to lift
the rest of the do notation too (even if it's not necessary, incurring
extra cost for no reason). Even if I could tell which statements
should be `ExceptT`'d or `lift`'d, it would be overkill.

At this level, it doesn't matter much. All you need
the syntax to produce is a case expression. The use of an `Early`
class is unneccessary, it just makes it easy to use either `Maybe` or
`Either` with the same syntax.

[^1]: If your answer is "just define really short names for
     `runExceptT` and `ExceptT` and `lift`", perhaps you could go back
     to using `>>=` and `>>` instead of `do`, and
     [`ApplicativeDo`](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/desugaring-haskell-haskell16.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Flist-comp%2Fapplicativedo.pdf),
     and
     [`RecursiveDo`](https://ocharles.org.uk/posts/2014-12-09-recursive-do.html),
     and
     [`Arrows`](https://ocharles.org.uk/guest-posts/2014-12-21-arrows.html),
     and report your experience.

     I've never once used `Arrow` and I've
     used `RecursiveDo` as a novelty, but `mfix` was sufficient in the
     end. `ApplicativeDo` is a brittle extension, but that doesn't
     stop me from using it.
