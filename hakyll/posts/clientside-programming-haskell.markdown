---
date: 2019-07-25
title: Client-side web programming in Haskell
description: Client-side web programming in Haskell
author: Chris Done
tags: haskell
---

At the beginning of this decade, a few of us Haskellers were exploring
how best to do client-side web programming. We didn't want to write
JavaScript. There's a surprising number of techniques we tried to
avoid doing so. Here's a history of my own experience in this problem
space.

In August 2011, I was
[experimenting with GHCJS](https://chrisdone.com/posts/ghcjs/), notes
which later I copied over to the Haskell Wiki under the name
[The JavaScript Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem). At
the time, I encountered bugs and runtime difficulties with some simple
GHCJS experiments. From there I mostly abandoned it as a choice.

In December 2011, I came up with
[ji](https://chrisdone.com/posts/ji-haskell-web/), a means of
controlling a web page from a Haskell web service, which later was
renamed to
[threepenny-gui](http://hackage.haskell.org/package/threepenny-gui)
and is now maintained by Heinrich Apfelmus to this day. It turned out
to be extremely powerful; I wrote an IRC-like app in which people
could chat on a web page in a page of code. However, in the end it
wasn't to be for mainstream web dev; a good point that Michael Snoyman
made was that it had a high server-side cost, and did not scale to
multiple servers. In the end, threepenny-gui is a great library to
consider for cross-platform desktop programs (such as with Electron).

In January 2012, I
[experimented with UHC](https://web.archive.org/web/20120623212312/https://chrisdone.com/posts/2012-01-06-uhc-javascript.html). My
experience was similar to GHCJS. I tried some basic experiments, but
the heavy runtime that came along with it wasn't attractive to me.

In March 2012,
[Evan Czaplicki announced that his thesis on Elm](https://www.reddit.com/r/haskell/comments/rkyoa/my_thesis_is_finally_complete_elm_concurrent_frp/)
was complete. The emphasis on FRP drew me (and probably others, at the
time) away from it, as FRP was seen as green, experimental and
impractical. Indeed, in 2016, Evan wrote
[A Farewell to FRP](https://elm-lang.org/news/farewell-to-frp). Elm
used to be all about FRP, and then it wasn't. "I might argue that Elm
was _never_ about FRP," writes Evan. I imagine this contributed to
Elm's growing success.

--------
TODO:

Note about Michael Snoyman's very interesting explorations in 2012:

https://www.yesodweb.com/blog/2012/04/client-side
https://www.yesodweb.com/blog/2012/04/yesod-js-todo

--------

In September 2012, I
[authored the Fay compiler](https://chrisdone.com/posts/fay/), which
was a Haskell subset compiler. I was inspired by Elm and Roy. The idea
was to re-use the GHC compiler for type-checking, and separately do
codegen with haskell-src-exts without type information. As a result,
it didn't support type-classes, and didn't compile basically any of
Hackage. But it was useful enough and simple enough to get use in the
community and at FP Complete, where we used it for the web-based IDE.

To add some additional context, at the time, I wrote:

> My approach to the problem, as with everyone else, has long been:
> well, we can’t do anything about it, let’s just wait for Google to
> complete their native client project and hope that it breaks the
> market.
