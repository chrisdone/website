---
date: 2023-12-26
title: "Hell: Shell scripting Haskell dialect"
description: Shell scripting based on Haskell
author: Chris Done
tags: haskell
---

[Hell](https://github.com/chrisdone/hell) is a shell scripting
language that is a tiny dialect of Haskell.

Note: I'm using it to [generate this
blog](https://github.com/chrisdone/hell/blob/main/examples/19-blog-generator.hell),
instead of Hakyll.[^1]

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Bash downsides](#bash-downsides)
- [Bash upsides](#bash-upsides)
- [Anatomy of a shell scripting language](#anatomy-of-a-shell-scripting-language)
- [Shell scripting threshold](#shell-scripting-threshold)
- [Why a Haskell dialect?](#why-a-haskell-dialect)
- [Decisions](#decisions)
- [References](#references)

<!-- markdown-toc end -->

As of 2024, my New Year's Resolution is to write more shell scripts in
the name of automation. I've always avoided this because of the
downsides of bash. And [other problems.]([The growth of command line options, 1979-Present](https://danluu.com/cli-complexity/#maven))

## Bash downsides

Bash, zsh, fish, etc. have problems:

* They're incomprehensible gobbledegook.
* They use quotation (`x=$(ls -1) ..`) which makes it easy to make
  mistakes.
* They lean far too heavily on sub processes to do basic things.
  * Therefore things like equality, arithmeti, ordering, etc. are
    completely unprincipled.

## Bash upsides

But, bash is:

* Stable
* Simple
* Works the same on every machine.

You can write a bash script and keep it running for years while never
having to change any code. The code you wrote last year will be the
same next year.

## Anatomy of a shell scripting language

It's helpful to define the anatomy of a shell scripting language to
know what we need.

* It should be very basic.
* It should run immediately (no visible compilation steps)
* No module system
* No package system
* No abstraction capabilities (classes, data types, polymorphic
  funcions, etc.)
* Does not change in backwards-incompatible ways
  * See also: [Escaping the Hamster Wheel of Backwards Incompatibility](https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/#s4-escaping-the-hamster-wheel-of-backwards-incompatibility)

Why no module or package system? They make it harder for a system to
be "done." There always some other integration that you can do; some
other feature. I'd prefer Hell to be [cold-blooded
software](https://dubroy.com/blog/cold-blooded-software/), there's
[beauty in finished
software](https://josem.co/the-beauty-of-finished-software/).

## Shell scripting threshold

The Scripting Threshold is that,

* when you reach for a module system or a package system, or abstraction capabilities.
* when you want more than what’s in the standard library.

… you probably want a general purpose programming language instead.

## Why a Haskell dialect?

The reasons for making a Haskell dialect (and not using some other
alt. shell scripting language or [using
Elixir](https://arathunku.com/b/2024/shell-scripting-with-elixir/))
are:

* I know Haskell. It’s my go-to.
* It has a good story about equality, ordering, etc.
* It has a good runtime capable of trivially doing concurrency.
* Garbage collected, no funny business.
* Distinguishes bytes and text properly.
* Can be compiled to a static Linux x86 binary.
* Performs well.
* Types!

## Decisions

I made the following decisions when designing the language:

* Use a faithful Haskell syntax parser.
  *  It’s better that way; you get re-use.
* No imports/modules/packages.
  *  That’s code reuse and leads to madness.
* No recursion (simpler to implement).
* Type-classes (Eq, Ord, Show, Monad).
  *  Needed for e.g. List.lookup and familiar equality things.
* No polytypes.
  *  That’s a kind of abstraction.
* Use all the same names for things (List.lookup, Monad.forM, Async.race, etc.)
  *  Re-use intuitions.

## Further reading

* [Why Create a New Unix Shell?](http://www.oilshell.org/blog/2018/01/28.html)
* On the architecture of Hell, see [Tour of
  Hell](/pdfs/tour-of-hell.pdf) - slides I made for presenting Hell at work.

[^1]: Tired of issues like [this](https://discourse.haskell.org/t/hakyll-error-watching-and-building/8834).
