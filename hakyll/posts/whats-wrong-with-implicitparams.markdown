---
date: 2021-03-20
title: What's wrong with ImplicitParams
description: What's wrong with ImplicitParams
author: Chris Done
tags: haskell
---

Implicit parameters, enabled by the `ImplicitParams` extension in GHC,
often come up in Haskell discussions,[^2] with the statement that
they're so convenient, yet nobody[^1] seems to use them. Why is that?

The problem is incoherence. Type class resolution works in a very
binary way: either it finds one, singular instance with one behavior
and we compile, or it finds none at all and we don't compile. It's
never[^3] the case that changing our code will _change_ a resolved
instance. I can change my import list, or update my
type-signatures.

Incoherence breaks this guarantee. Implicit parameters rely on
incoherence to work, because they piggyback on the type-class
machinery with magical instances that appear as needed.

Let's see an example of how this leads to surprising results. The two
declarations below produce different results.

``` haskell
> let ?myparam = 123 in terror
(123,123)
> let ?myparam = 123 in horror
(123,456)
```

Check out the code and find the reason why.

``` haskell
{-# LANGUAGE ImplicitParams #-}
horror :: (?myparam :: Int) => (Int, Int)
horror =
  let result :: (?myparam :: Int) => Int
      result = ?myparam
   in ( result
      , let ?myparam = 456
         in result)

terror :: (?myparam :: Int) => (Int, Int)
terror =
  let result :: Int
      result = ?myparam
   in ( result
      , let ?myparam = 456
         in result)
```

You found it; it's that we added a type signature to `result`. That
changed the behavior! If you find this surprising, it's because you're
used to coherence. In normal Haskell, an inferred type and an explicit
type don't produce different code.

Here's another, more insidious, way to evoke the problem:

``` haskell
{-# LANGUAGE ImplicitParams, NoMonomorphismRestriction #-}
terror :: (?myparam :: Int) => (Int, Int)
terror =
  let result = ?myparam
   in ( result
      , let ?myparam = 456
         in result)
```

Try removing `NoMonomorphismRestriction`. The output is different
depending on whether this extension is on or not. **Be afraid, be very
afraid.**

If this constraint was `Show`, I wouldn't be able to reproduce this
difference, because there's only one `Show Int` instance.

Implicit parameters break fundamental expectations about one of
Haskell's most basic features: its type inference. For me, this is
reason enough to avoid it.

---

Thanks Andrew Martin for insightful discussion on this topic a few
years ago.

[^1]: People do use them, see
e.g. [Ed Kmett](https://old.reddit.com/r/haskell/comments/m8o88a/who_still_uses_readert/grkh17q/),
with caveats.

[^2]: Such as this blog post
[Who still uses ReaderT](https://hugopeters.me/posts/10/) by Hugo Peters.

[^3]: Orphan instances change this guarantee, which is why they're
maligned.
