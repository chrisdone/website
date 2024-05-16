---
date: 2023-12-26
title: "Hell: Shell scripting Haskell dialect"
description: Shell scripting based on Haskell
author: Chris Done
tags: haskell
---

[Hell](https://github.com/chrisdone/hell) is a shell scripting
language that is a tiny dialect of Haskell that I wrote for my own
shell scripting purposes.
As of February, I'm using Hell to [generate this
blog](https://github.com/chrisdone/hell/blob/main/examples/19-blog-generator.hell),
instead of Hakyll.[^1]

My 2024 New Year's Resolution is to write more
shell scripts in the name of automation.
I've always avoided this
because of the downsides of bash.
And [other problems.](https://danluu.com/cli-complexity/#maven)

Bash, zsh, fish, etc. have problems:

* They're incomprehensible gobbledegook.
* They use quotation (`x=$(ls -1) ..`) which makes it easy to make mistakes.
* They lean far too heavily on sub processes to do basic things.
* Therefore things like equality, arithmetic, ordering,
  etc. are completely unprincipled. Absolutely full of pitfalls.[^4]

But, bash does have some upsides: It's stable, it's simple, and it
works the same on every machine.
You can write a bash script and keep it running for years while never
having to change any code.
The code you wrote last year will be the same next year, which is not
true of most popular programming languages.

So in the interest of defining a language that I *would* like to use,
let's discuss the anatomy of a shell scripting language:

* It should be very basic.
* It should run immediately (no visible compilation steps).
* It should have no module system.
* It should have no package system.[^5]
* It should have no abstraction capabilities (classes, data types, polymorphic
* functions, etc.).
* And it does not change in backwards-incompatible ways.[^2]

Why no module or package system? They make it harder for a system to
be "done." There always some other integration that you can do; some
other feature.
I'd prefer Hell to be [cold-blooded
software](https://dubroy.com/blog/cold-blooded-software/), there's
[beauty in finished
software](https://josem.co/the-beauty-of-finished-software/).

Based on the above I can define a _scripting threshold_, meaning, when
you reach for a module system or a package system, or abstraction
capabilities, or when you want more than what’s in the standard
library, then you probably want a general purpose programming language
instead.

Taking this into consideration, I opted for making a Haskell dialect[^3]
because of the following reasons:

* I know Haskell.
* It’s my go-to.
* It has a good story about equality, ordering, etc.
* It has a good runtime capable of trivially doing concurrency.
* It's garbage collected.
* It distinguishes bytes and text properly.
* It can be compiled to a static Linux x86 binary.
* It performs well.
* It has static types!

I made the following decisions when designing the language:

* Use a faithful Haskell syntax parser.
* It’s better that way; you get re-use.
* It has no imports/modules/packages.
* It doesn't support recursive definitions, but can use `fix` to do so.
* It supports basic type-classes (Eq, Ord, Show, Monad), which are needed for e.g. List.lookup and familiar equality things.
* It does not support polytypes. That’s a kind of abstraction and not needed.
* It use all the same names for things (List.lookup, Monad.forM,
Async.race, etc.) that are already used in Haskell, which lets me re-use intuitions.

You can download statically-linked Linux binaries from [the
releases page.](https://github.com/chrisdone/hell/releases)
To read about the implementation internals, see [Tour of
Hell](/pdfs/tour-of-hell.pdf) which is a set of slides I made for
presenting Hell at work.

[^1]: Tired of issues like [this](https://discourse.haskell.org/t/hakyll-error-watching-and-building/8834).

[^2]: See also: [Escaping the Hamster Wheel of Backwards Incompatibility](https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/#s4-escaping-the-hamster-wheel-of-backwards-incompatibility)

[^3]: And not using some other alt. shell scripting language or [using
    Elixir](https://arathunku.com/b/2024/shell-scripting-with-elixir/),
    or [Oil](http://www.oilshell.org/blog/2018/01/28.html).

[^4]: Just check out the huge list of
  [linting issues in ShellCheck.](https://github.com/koalaman/shellcheck)

[^5]: This excludes scripting languages like
    [zx](https://github.com/google/zx), which sits, unbelievably, on
    the nodejs ecosystem.
