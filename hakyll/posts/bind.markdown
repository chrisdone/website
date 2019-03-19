---
date: 2014-12-09
title: "Proposal: bind"
description: "Proposal: bind"
author: Chris Done
tags: haskell
---

I often find myself writing:

``` haskell
fmap (mu bar)
     (foo zot)
```

Then I decide to change the type of `mu`, so instead I want to just
write:

``` haskell
bind (mu bar)
     (foo zot)
```

Which is just like `fmap` but the function can run in the
monad. Similar to traverse:

``` haskell
(Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
```

As someone who isn't a fan of operators, I generally am appreciative
of alternative regular plain English word versions of functions, which
I find easier to type, read and edit. Currently without defining such
a handy name, I have to transform the code to this:

``` haskell
mu bar =<<
foo zot
```

The name for this function is a no-brainer (`(>>=)` is now pronnounced
"bind"):

``` haskell
bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)
```

For comparison, the not-very-pleasant `<$>` and `<*>` each have word
alternatives, `fmap` and `ap`.

----

I submitted this to the haskell libraries mailing list, but include it
here for future reference.
