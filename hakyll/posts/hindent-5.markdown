---
date: 2016-08-29
title: "hindent 5: One style to rule them all"
description: "hindent 5: One style to rule them all"
author: Chris Done
tags: haskell
---

## Reminder of the past

In 2014, in [my last post](http://chrisdone.com/posts/hindent) about
[hindent](https://github.com/chrisdone/hindent), I wrote these points:

1. [Automatic formatting is important](http://chrisdone.com/posts/hindent#whats-the-deal-with-code-style):
   * [External tooling works better](http://chrisdone.com/posts/hindent#okay-but-is-this-a-problem-tooling-even-needs-to-solve)
   * Reading code is easier
   * Writing code is faster
2. [Other people also care about this](http://chrisdone.com/posts/hindent#is-this-really-an-issue-though)
3. [The Haskell community is not immune to code formatting debates](http://www.reddit.com/r/haskell/comments/15gz8q/a_nondirty_shot_at_tabs_vs_spaces/)

I
[proposed](http://chrisdone.com/posts/hindent#a-practical-way-forward)
my hindent tool, which:

1. Would format your code.
2. Supported multiple styles.
3. Supported further extension/addition of more styles trivially.

## Things learned

I made some statements in that post that I'm going to re-evaluate in
this post:

1. Let’s have a code style discussion. I propose to solve it with
   tooling.
2. It’s not practical to force everyone into one single style.

## Code formatting is solved with tooling

I've used hindent for two years, it solves the problem. There are a
couple exceptions[^2]. On the whole, though, it's a completely different working
experience:

* Code always looks the same.
* I don't make _any_ style decisions. I just think about the tree I
  need for my program.
* I don't do any manual line-breaking.
* I've come to exploit it by writing lazy code like `do
  x<-getLine;when(x>5)(print 5)` and then hitting a keybinding to
  reformat it.

## Switching style is realistic

I've been writing Haskell in my own style for years. For me, my style
is better for structured editing, more consistent, and visually easier
to read, than most code I've seen. It's like Lisp. Using hindent, with
my ChrisDone style, I had it automatically formatted for me. I used
2-space indents.

The most popular style in the community[^1] is JohanTibell: The
alignment, line-breaking, and spacing (4 spaces instead of 2) differs
significantly to my own style.

At FP Complete I've done a lot of projects, private FP Complete
projects, client projects, and public FP Complete projects (like
Stack). For the first year or so I generally stuck to my guns when
working on code only I was going to touch and used my superior
style.

But once the JohanTibell style in hindent was quite stable, I found
that **I didn't mind** using it while collaborating with people who
prefer that style. The tooling made it so automatic, that I didn't
have to understand the style or make any style decisions, I just wrote
code and got on with it. It doesn't work great with
structured-haskell-mode, but that's ok. Eventually I got used to it,
and eventually switched to using it for my own personal projects.

I completely did a U-turn. So I'm hoping that much of the community
can do so too and put aside their stylistic preferences and embrace a
standard.

## Going forward

hindent-5.* now supports _one style_, based on
[the Johan Tibell style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md). My
[own style guide](https://github.com/chrisdone/haskell-style-guide) is
now deprecated in favor of that. The style flag `--style foo` is now
silently ignored.

There is a [demonstration web site](http://chrisdone.com/hindent/)
in which you can try examples, and also get a link for the example to
show other people the output (for debugging).

HIndent now has a "literate" test suite here:
[TESTS.md](https://github.com/chrisdone/hindent/blob/master/TESTS.md).
You can read through it as a document, a bit like Johan's style
guide. But running the test suite parses this file and checks that
each code fence is printed as written.

There's also a
[BENCHMARKS.md](https://github.com/chrisdone/hindent/blob/master/BENCHMARKS.md),
since I rewrote comment handling, switched to a bytestring-builder,
improved the quadratic line-breaking algorithm to short-circuit, among
other improvements, hindent now formats things in 1.5ms instead of 1s.

For those who still want to stick with their old hindent, Andrew
Gibiansky is
[keeping a fork of hindent 4](https://github.com/gibiansky/hindent)
for his personal use, and has said he'll accept PR's for that.

HIndent is not perfect, there's always room for improvement ([issue
tracker](https://github.com/chrisdone/hindent/issues) welcomes issues), but over
time that problem space gets smaller and smaller. There is support for
Emacs, Vim and Atom. I would appreciate support for SublimeText too.

Give it a try!

[^1]: From a survey of the top downloaded 1000 packages on Hackage,
660 are 4-spaced and 343 are 2-spaced. All else being equal, 4
spaces wins.

[^2]: Such as CPP `#if` directives--they are tricky to
handle. Comments are also tricky, but I've re-implemented comment
handling from scratch and it works pretty well now. See the
[pretty extensive tests](https://github.com/chrisdone/hindent/blob/master/TESTS.md#comments).
