---
date: 2008-08-01
title: GHCi on Acid
description: GHCi on Acid
author: Chris Done
tags: haskell
---

GHCi on Acid is an extension to GHCi (Interactive GHC) for adding useful lambdabot features. This post details how to use it.

I use GHCi inside Emacs, so having GOA additionally.. increases my productivity.

## What does it do?

Here are some examples of the commands that can be used.

The :instances command shows all the instances of a class:

``` haskell
GOA> :instances Monad
 ((->) r), ArrowMonad a, Cont r, ContT r m, Either e, ErrorT e m, IO,
 Maybe, RWS r w s, RWST r w s m, Reader r, ReaderT r m, ST s, State s,
 StateT s m, Writer w, WriterT w m, []
GOA> :instances Arrow
 (->), Kleisli m
GOA> :instances Num
 Double, Float, Int, Integer
```

Here we have the :hoogle command, for querying the Hoogle database. Great for looking for functions of a specific type:

``` haskell
GOA> :hoogle Arrow
 Control.Arrow :: module
Control.Arrow.Arrow :: class Arrow a
Control.Arrow.ArrowZero :: class Arrow a => ArrowZero a
GOA> :hoogle b -> (a -> b) -> Maybe a -> b
 Prelude.maybe :: b -> (a -> b) -> Maybe a -> b
Data.Maybe.maybe :: b -> (a -> b) -> Maybe a -> b
```

The :source command gives a link to the source code of a module (sometimes you are curious):

GOA> :source Data.Maybe
 http://darcs.haskell.org/packages/base/Data/Maybe.hs
Similarly, :docs gives a link to the documentation of a module.

``` haskell
GOA> :docs Data.Maybe
 http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Maybe.html
```

:index is a nice way to search modules.

``` haskell
GOA> :index Monad
 Control.Monad, Prelude, Control.Monad.Reader,
 Control.Monad.Writer, Control.Monad.State, Control.Monad.RWS,
 Control.Monad.Identity, Control.Monad.Cont, Control.Monad.Error,
 Control.Monad.List
```

Then we have :pl, which shows the pointless (or: point-free) way of writing a function, which is very useful for learning and sometimes for fun:

``` haskell
GOA> :pl (\x -> x * x)
 join (*)
GOA> :pl (\x y -> (x * 5) + (y * 5))
 (. (5 *)) . (+) . (5 *)
```

And finally, the best of them all; :bs:

``` haskell
GOA> :bs
 :)
```

Botsnack.

## How to install it

You need to download and install lamdabot. This is easier than it sounds.

### Install happy

First, we need “happy”, which is a parser:

```
sudo cabal install happy
```

Right, that’s installed to ~/.cabal/bin, so you need to make sure ~/.cabal/bin is in your PATH. In Bash, you can do this by adding

```
export PATH="~/.cabal/bin:$PATH"
```

to your .bashrc.

### Try it out:

``` haskell
chris@chrisdesktop:~$ happy --version
Happy Version 1.17 Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow

Happy is a Yacc for Haskell, and comes with ABSOLUTELY NO WARRANTY.
This program is free software; you can redistribute it and/or modify
it under the terms given in the file 'LICENSE' distributed with
the Happy sources.
If yours isn’t found, you need to restart bash, or type export PATH="~/.cabal/bin:$PATH".
```

### Grab Hoogle

We need Hoogle for lambdabot’s Hoogle functionality.

```
sudo cabal install hoogle
```

### Download and build lambdabot

You need darcs. We’re using darcs. It should be on your package manager, otherwise install it from the web site. I don’t know why it’s not on Hackage.

Now, we grab lambdabot from code.haskell.org:

```
darcs get http://code.haskell.org/lambdabot/

cd lambdabot && sudo ./build
```

./build downloads and installs everything we need, and builds lambdabot for us.

Once that’s built, we can install GOA.

### Download, build and install GOA

Just install with your usual cabal install goa method. :)

Finally, copy the dot-ghci from goa/ to your ~/.ghci file, and edit the lambdabot home directory to where your lambdabot directory is. Mine looks like this:

``` haskell
:m - Prelude
:m + GOA
setLambdabotHome "/var/www/chrisdone/lambdabot-personal"
:def bs        lambdabot "botsnack"
:def pl        lambdabot "pl"
:def index     lambdabot "index"
:def docs      lambdabot "docs"
:def instances lambdabot "instances"
:def hoogle    lambdabot "hoogle"
:def source    lambdabot "fptools"
:def where     lambdabot "where"
:def version   lambdabot "version"
```

Now when you run ghci, you will have GOA! Tip: if you accidentally unload the GOA module, use :m + GOA to load it.
