---
date: 2019-07-25
title: "Client-side web programming in Haskell: A retrospective"
description: "Client-side web programming in Haskell: A retrospective"
author: Chris Done
tags: haskell
---

At the beginning of this decade (2010), a few of us Haskellers were exploring
how best to do client-side web programming. We didn't want to write
JavaScript. There's a surprising number of techniques we tried to
avoid doing so. There was work coming from academia and
industry. Here's a history of my own experience in this problem space.

In 2008, Joel Bjornson and Niklas Broberg published
[HJScript](http://hackage.haskell.org/package/HJScript-0.4), which was
a Haskell EDSL for writing JavaScript. It had the ability to express
typed JS via a GADT. I used it in 2010 on a project to make limited
DOM manipulations. I wrote
[a wrapper around jquery](https://github.com/benarmston/hpaste/blob/master/src/HJScript/Objects/JQuery/Extra.hs),
for example. It was nice to write in Haskell, but it was also mental
overhead to write in two languages at once (it still had JavaScript's
semantics). In the end I went back to using plain JavaScript.

Around 2010, Adam Chlipala announces
[Ur](http://impredicative.com/ur/), a radical web dev language with
row types, which compiles to both native object code and JavaScript,
fairly transparently, embedding both HTML and SQL syntax into the
language. I am both impressed by the simplicity of the code and
correctness, and horrified by some of the code involving
metaprogramming. The row types documentation frankly scares me
away. After trying out some examples, I don't return to it.[^1] To
this day I am still interested in this architecture.

Some time in 2011, [Opa](http://opalang.org/) appears, but apparently
nobody wants to learn yet another server-side language. I don't know
anyone who has used this in production.

In August 2011, I was
[experimenting with GHCJS](https://chrisdone.com/posts/ghcjs/), notes
which later I copied over to the Haskell Wiki under the name
[The JavaScript Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem). At
the time, I encountered bugs and runtime difficulties with some simple
GHCJS experiments. From there I mostly abandoned it as a choice,
underwhelmed.

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
the heavy runtime that came along with it wasn't attractive to me. It
felt like I would spend half my time maintaining _it_ aside from the
applications I might've written with it.

In March 2012,
[Evan Czaplicki announced that his thesis on Elm](https://www.reddit.com/r/haskell/comments/rkyoa/my_thesis_is_finally_complete_elm_concurrent_frp/)
was complete. The emphasis on FRP drew me (and probably others, at the
time) away from it, as FRP was seen as green, experimental and
impractical. Indeed, in 2016, Evan wrote
[A Farewell to FRP](https://elm-lang.org/news/farewell-to-frp). Elm
used to be all about FRP, and then it wasn't. "I might argue that Elm
was _never_ about FRP," writes Evan. I imagine this contributed to
Elm's growing success. Today, Elm is not my choice because it lacks
type-classes.

In April 2012, Michael Snoyman was doing some very interesting
explorations into
[writing high-level Haskell code that would produce limited JavaScript](https://www.yesodweb.com/blog/2012/04/client-side) transparently to produce dynamic page changes
(and [here](https://www.yesodweb.com/blog/2012/04/yesod-js-todo)). To
this day I am still interested in exploring this path.

In May 2012, Anton Ekblad announces his
[Haste compiler](https://www.reddit.com/r/haskell/comments/tw997/haste_haskell_to_javascript_compiler/),
a Haskell-to-JavaScript compiler like GHCJS which also goes via GHC
Core, but doesn't try as hard to implement the GHC runtime in the
browser. It's the driver for
[his PhD thesis](https://ekblad.cc/pubs/thesis.pdf) _Functional EDSLs
for Web Applications_. We never really give it a try on a real project.

In September 2012, I
[authored the Fay compiler](https://chrisdone.com/posts/fay/), which
was a Haskell subset compiler. I was inspired by Elm and Roy. The idea
was to re-use the GHC compiler for type-checking, and separately do
codegen with haskell-src-exts without type information. As a result,
it didn't support type-classes, and didn't compile basically any of
Hackage. But it was useful enough and simple enough to get use in the
community and at FP Complete, where we used it for the web-based IDE,
which had about 15k lines of Fay code in it.

To add some additional context, at the time, I wrote:

> My approach to the problem, as with everyone else, has long been:
> well, we can’t do anything about it, let’s just wait for Google to
> complete their native client project and hope that it breaks the
> market.

In March 2013, Andrew Gill and Jan Bracker published their
[sunroof](https://github.com/ku-fpg/sunroof-compiler)
project, which was a HJScript on steroids, and which actually seemed
compelling to some people at the time. I'm not sure anybody ever used
it in production. I think this was the last ember of effort in
JavaScript DSLs in Haskell, and it flickered out.

In October 2013, Phil Freeman
[announced PureScript](https://www.reddit.com/r/haskell/comments/1pkzd0/show_reddit_my_weekend_project_purescript/),
a strict, pure, Haskell-like language with row-polymorphism that
compiled to JS with no runtime. I think it was seen as an interesting
concept, but it wasn't until a few years later that PureScript came
back on my radar as a practical language.

Around late 2014, at FP Complete, we were ready to try GHCJS
again. Luite Stegeman had been working hard on it and bringing it to
life. In August of 2014, Simon Meier released
[blaze-react](https://github.com/meiersi/blaze-react), which was a
GHCJS-based React.js binding with an Elm-style of processing events as
a sum type. This re-ignited interest, at least from my
perspective. Although we were doubtful that the Elm architecture
scaled. Fay is officially out of the picture.

In April 2015, we whipped up a similar package called
[ghcjs-react](https://github.com/fpco/ghcjs-react), which implemented
React without the Om-style handling of state via a lens/cursor (Redux
appeared around the same time, doing a similar thing). I wrote the
experimental
[stackage-view](https://github.com/fpco/stackage-view#introduction)
tool based on this library; which we thought would be a good litmus
test both of GHCJS and our react bindings. It was awkward, but when it
worked, it worked well.

I will note that, we're slow on the up-take. React.js was released in
May 2013, and we twigged onto it as a good way to write UIs around
2015. Partly in thanks to the Clojure library [Om](https://github.com/omcljs/om).

Also in April 2015, Ryan Trinkle
[presented his new library Reflex](https://www.reddit.com/r/haskell/comments/31rat9/reflex_practical_functional_reactive_programming/),
with a very compelling talk. I wrote at the time:

> The video is quite comprehensible, it may be the first FRP demo I've
> seen that isn't mired in nomenclature I don't understand, builds up
> from small components but doesn't bore me with unrealistic counters
> or sliders, and has some real world products written in it. 10/10
> talk, count me interested.

In Jan 2016, some of the FP Complete team started learning Reflex and
using it on a large client pilot project. Experience reports can be
summed up as: mind-bending but eye-opening in the same way learning
Haskell was. Awkward to express some things, but also unquestionably
composable and re-usable. Groans and grumbles about GHCJS being heavy,
slow and brittle.

In Jan 2015, Slam Data start their
[Halogen library](https://github.com/slamdata/purescript-halogen) for
PureScript.

During 2015 and 2016, Tim Dysinger is a growing advocate for PureScript at FP
Complete internal discussions.

In October 2016, I'm using PureScript with React.js bindings to make
internal demo UIs for FP Complete. I'm enjoying it a lot. We have an
engineering meeting about PureScript vs other technologies.

December 2016, Michael Snoyman comments on Slack:

> PureScript seems like it's doing everything right.

April 2017, Michael Snoyman
[writes a PureScript + Halogen version](https://github.com/snoyberg/purescript-halogen-onetimepad)
of his one-time-pad toy app. At the time, I'm not interested in
Halogen. It looks complicated, and I'm still enjoying React.

August 2018, it has been decided that we will use PureScript to
build a UI for a cryptocurrency client at FP Complete. I'm happy about
that. I decide to give Halogen a try. After initial pain, I end up
loving it.

November 2018, we briefly discuss whether Rust+WASM is a new possible
contender. We'll have to wait for WASM to become supported on all
browsers, especially mobile.

December 2018, we have a dev meeting on the state of frontend
development. Our meeting notes are long and detailed, the brief
summary: we don't feel comfortable with GHCJS, Reflex is great but
also costly (double novelty budget), Halogen is a better architecture
than Elm. PureScript and Halogen come out as the best possible
choice. Other things considered: ClojureScript, TypeScript, Rust,
[ReasonML](https://reasonml.github.io/), Elm,
[OCaml](https://ocsigen.org/js_of_ocaml/3.1.0/manual/overview).

2019: As of today, I am personally still using PureScript with Halogen.

**2024 update:** In 2023 have pushed for our team at Artificial Labs
to start trying [Htmx](https://htmx.org/) combined with Haskell. So
far, the experience is good: we like writing Haskell. There are some
downsides to Htmx, however: inheritence is a bad idea, request queues
have bad defaults and are hard to manage, and htmx intuitions (such as
related to request queues) don't play well with morphdom.

[^1]: In retrospect, if it was marketed in the same way Elm was, and
less "academic", it may have taken off. But it could just be that it
arrived before anyone was ready to try alternative languages. Or
perhaps its syntax could have matched Haskell. Comparatively,
PureScript has row types too.
