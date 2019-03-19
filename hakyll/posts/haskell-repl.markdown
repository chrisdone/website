---
date: 2015-07-15
title: Use the REPL, Luke
description: Use the REPL, Luke
author: Chris Done
tags: haskell
---

There was an online discussion about iteration times in Haskell and
whether and why they are slow. For me, it's not slow. I do all my
Haskell development using a REPL. Here are some tips I wrote up in
that discussion.

## Prepare for GHCi use

The first thing you want to do before writing anything for your
project is make sure you can load your code in the REPL;
GHCi. Sometimes you have special configuration options or whatnot
(`cabal repl` and `stack ghci` make this much easier than in the
past). The sooner you start the better. It can be a PITA to load some
projects that expect to just be a "start, run and die" process, they
often launch threads without any clean-up procedure; in this way the
REPL makes you think about cleaner architecture.

## Make sure it scales

Learn [how to make GHCi](http://chrisdone.com/posts/making-ghci-fast)
fast for your project so that you don't hit a wall as your project
scales. Loading code with byte-code is much faster than object code,
but loading with object code has a cache so that in a 100 module
project if you only need to reload one, it'll just load one. Make sure
this is happening for you, when you need it. Dabble with the settings.

## Write small, parametrized functions

Code that is good for unit tests is code that is good for the
REPL. Write small functions that take state as arguments
([dependency injection](https://en.wikipedia.org/wiki/Dependency_injection))
rather than loading their own state, then they can be ran in the REPL
and used in a test suite easily. Regard functions that you can't just
call directly with suspicion.

## Test work-in-progress implementations in the REPL

While writing, test your function in the REPL with typical arguments
it will expect, rather than implementing a function and then
immediately using it in the place you want to ultimately use it. You
can skip this for trivial "glue" functions, but it's helpful for
non-trivial functions.

## Setup/teardown helpers

Write helpful setup/teardown code for your tests and REPL code. For
example, if you have a function that needs a database and application
configuration to do anything, write a function that automatically and
conveniently gets you a basic development config and database
connection for running some action.

## Make data inspectable

Make sure to include `Show` instances for your data types, so that you
can inspect them in the REPL. Treat `Show` as your development
instance, it's for you, don't use it for "real" serialization or for
"user-friendly" messages. Develop a distaste for data structures that
are hard to inspect.

## Figure out the fastest iteration for you

Use techniques like `:reload` to help you out. For example, if I'm
working on [hindent](http://hackage.haskell.org/package/hindent), then
I will test a style with `HIndent.test chrisDone "x = 1"`, for
example, in the REPL, and I'll see the output pretty printed as
Haskell in my Emacs REPL. But I work on module
`HIndent.Style.ChrisDone`. So I first `:load HIndent` and then for
future work I use `:reload` to reload my `.ChrisDone` changes and give
me the `HIndent` environment again.

## Configuration

Make sure you know about the `.ghci` file which you can put in your
`~/` and also in the project directory where GHCi is run from. You can
use `:set` to set regular GHC options including packages (`-package
foo`) and extensions (`-XFoo`), and any special include directories
(`-ifoo`).

## More advanced tricks

Consider tricks like
[live reloading](http://chrisdone.com/posts/ghci-reload); if
you can support it. I wrote an IRC server and I can run it in the
REPL, reload the code, and update the handler function without losing
any state. If you use foreign-store you can make things available,
like the program's state, in an IORef or MVar.

This trick is a trick, so don't use it in production. But it's about
as close as we can get to Lisp-style image development.

## In summary

Haskell's lucky to have a small REPL culture, but you have to work
with a Lisp or Smalltalk to really know what's possible when you fully
"buy in". Many Haskellers come from C++ and "stop program, edit file,
re-run compiler, re-run whole program" cycles and don't have much
awareness or interest in it. If you are such a person, the above
probably won't come naturally, but try it out.
