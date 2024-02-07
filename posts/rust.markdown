---
date: 2021-12-22
title: My negative views on Rust
description: My negative views on Rust
author: Chris Done
tags: rust
---

**2023-10-13** Edited based on feedback, but preserves the same content.

This is a little summary of my current thoughts on Rust. I wonder
whether I'll look back in five years and see if my views have changed.

## The Good

So that Gentle Reader knows that I'm not entirely biased 
against the language, there are obviously positive things
to appreciate about it:

* Rust's macros are very good. They act like Lisp's macros, unlike Haskell's.
* The fact that Rust has type-classes ("traits") and sum types ("enums")
  and pattern matching is very attractive. Making orphans
  package-wide rather than module-wide is a very good decision.
* I like its treatment of records.
* Its standard library does a few things well, like handling strings as
  UTF-8.
* Distinguishing mutability has its advantages, it's easy to see that a
  function is "morally" pure--if not actually pure--and that's good for
  reading.

## Unsafe and panic

The use of unsafe is a little disturbing, because many libraries
feature it, and people are tempted to use it on private work projects
to quickly get around the language's limitations. But it's not much 
different to using an FFI. I don't see this is a big downside. 

`panic` is a little more bothersome, because 
Rust libraries go to great pains (with many syntactic tricks like `?`
and auto-conversions from smaller error types to larger ones) to handle 
errors explicitly, but then panics unwind the stack to the top of the 
process, and panics inside a panic don't run destructors, etc. The overall 
effect is that, like [my three questions of language design](https://chrisdone.com/posts/three-questions-of-lang-design/),
the answer to "how you handle errors" is "at least two, incompatible ways."

## Take Sugar?

Rust's use of magical sugar constructs, where the compiler will
automatically insert dereferences and copies and drops for
you has an initial appealing "it's all simple underneath" quality to
it, but in practice this leads to bad compile errors: The worst kind
of compile error is the one where the compiler is complaining about
something that it generated for you, rather than something you
explicitly wrote.

This can be compared with Haskell's use of monads, that provide
syntactic sugar. The more magic you introduce, the harder it is for
newbies to learn and to talk about.

## Fetishization of Efficient Memory Representation

I've watched people on calls that are a couple years into Rust spend
20 minutes attempting to understand why their perfectly reasonable
code doesn't fit into Rust's tight memory restrictions.

I've also been Rust-splained, by people with white in their hair, with an air
of misty-eyed revelation, that once you "get" Rust's memory model of
the stack and the heap,[^1] that things just all fit together
wonderfully. There's nothing wrong with that, but it's a theme.

This touches on another topic I'd like to write about elsewhere: the
difference between practice and theory and how users of languages like
Rust and Haskell that make big promises and require big sacrifices
don't seem to distinguish the difference. [It's not the technology 
that's working poorly, it's that you're using it wrongly.](https://chrisdone.com/posts/reasoning-violently/)

In practice, people just want to be able to write a tree-like type
without having to play Chess against the compiler. I predict that
tracing garbage collectors will become popular in Rust eventually.

This is both Rust's main goal--be like C, no garbage collection,[^2] 
but safe--and also its main downside. People waste time 
on trivialities that will never make a difference.

## The Rewrite Fallacy

I see a lot of "we rewrote X in Rust and it got faster" posts. I think
that if you rewrite anything from scratch with performance in mind,
you'll see a significant performance improvement. I'm suspicious of
how much Rust itself is needed versus the developers having some
performance discipline.

## Complexity

Rust has arrived at the complexity of Haskell and C++, each year 
requiring more knowledge to keep up with the latest and greatest. Go was
designed as the antidote to this kind of endlessly increasing language 
surface area. Endless libraries re-treading existing patterns (web 
services, parsers, etc.) in Rust. As a long-term Haskeller, I've done 
more than 15 years of riding a hamster wheel like that. It is fun for
a while, but at some point I grew 
tired of it. This aspect of Rust puts me off. I don't need another 
[tamagotchi](https://chrisdone.com/posts/tamagotchi-tooling/).

## The "Friendly" Community

All new language communities are nice. When things don't matter much,
people have no reason to get mad.

As soon as people have a stake in something, that's when things heat
up and tempers come out. You get a stake in a programming language by
writing a lot of code in it, or by building a business on it. When you
have a stake in how a language works, you're highly sensitive to
changes that will make you do more work than needed, or will limit
your goals.

I've seen this play out for Haskell. Around 2007, when I started with
Haskell, the community was friendly as anything, evangelic,
open. People praised it for this. Everyone just felt blessed to be
able to use this exciting language. Today, it's more like any other
community. What happened? People started using Haskell for real,
that's all.

Rust is going through the same thing, much more rapidly. Is it a
reason to avoid Rust? No. But a "nice" community isn't a reason to
join an upcoming language, either.

Since I wrote this, people are already trying to [fork Rust](https://news.ycombinator.com/item?id=36122270) 
because they're not happy with the governance of it. This doesn't mean
anything deep other than that people are using Rust now, as stated 
above, and the "friendly, welcoming" starts to become mixed with more diverse 
moods and motivations.

## Async is highly problematic

Rust's choice to exclude a runtime/scheduler blessed and built-in to
the language means they had to develop alternative strategies in the
language itself. This is not turning out well.

[Coloured Functions](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/)
is a powerful metaphor for the incompatibility between async and
synchronous functions and the awkward situations that mixing them
introduces. Some blog posts have attempted to downplay the situation
by focusing on technical aspects of specific syntactic forms. That
doesn't really matter, though, because the reality is much simpler:
Async code is easier to use when dependencies are in an async form.

People will choose libraries that are async over libraries that are
not. However, maintainers that have written good, maintained code, are
also resistant to adopt async.

* One conversation I've overhead:

  > Person 1: Another difference between diesel and sqlx is that
  > diesel is not async yet and from looking at the issues it doesn't
  > seem to be priority yet.
  >
  > Person 2: That sounds like a major issue

As the saying goes, the proof of the pudding is in the eating of the
pudding. Async introduces
[long, heated discussions.](https://github.com/diesel-rs/diesel/issues/399)

The problem for Rust is that its users want a runtime, but want the
option of not having one. The result is a mess. When combined with 
iterators, I think understanding such code is quite
difficult.

Generally, I think Go, Erlang and Haskell are the better choice
here for general purpose use. A tracing garbage collector and green 
threads make programmers more productive for general purpose 
programming (not systems programming). 
Which brings me to the next section.

## As a general purpose language

I feel like Rust is self-defined as a "systems" language, but it's
being used to write web apps and command-line tools and all sorts of
things.

This is a little disappointing, but also predictable: the more
successful your language, the more people will use your language for
things it wasn't intended for.

This post still offends [many who have tied Rust to their identity](http://www.paulgraham.com/identity.html),
but that's their problem, not mine.

## Conclusions, if any

I won't be using Rust for any of my own personal projects for the above
stated reasons. But it was used at my job at the time of writing, so I 
felt the need to express myself about it.

But I wouldn't mind using it as a replacement for single-threaded C if
I just use the standard library, that might be fun, although I don't do
any embedded work, so I wouldn't hold my breath.

I think that the excellent tooling and dev team for Rust, [subsidized by 
Big Tech](https://www.youtube.com/watch?v=XZ3w_jec1v8), pulls the wool 
over people's eyes and convinces them that this is a good language that is
simple and worth investing in. There's danger in that type of thinking.

[^1]: Having done my fair share of C code, there's nothing new here for me.

[^2]: Of course, `Rc` and `Arc` are reference counters, which is a form 
      of garbage collection. See _The Garbage Collection Handbook: The 
      Art of Automatic Memory Management_ published in 1996. But most
      developers have a superficial understanding of garbage collection
      as a technical subject, and therefore "garbage collection" for them
      means "tracing garbage collector." Hence bizarre discussions that 
      pit "garbage collecion versus reference counting." It's like saying 
      fruit versus apples.
