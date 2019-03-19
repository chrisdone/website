---
date: 2015-10-26
title: "Idle thoughts: More open, more free software"
description: "Idle thoughts: More open, more free software"
author: Chris Done
tags: haskell, programming, open source
---

I'm a bit busy, these are just some idle thoughts.

I just upgraded my Android OS to some other kind of dessert name and a
bunch of stuff changed in a way I had no desire for.

It made me think about the virtues of open source software. I can just
go and change it! Free software means benefiting from the work of
others without being shackled by them at the same time.

And then about the problems of open source software, which is that
only developers-skilled developers-with specific knowledge, are able
to approach the codebase of an app they use, update it, and then use
that new software in a continuous and smooth way. Everyone else's
hands are effectively tied behind their backs.

So that got me thinking about how software could be more “open” than
simply “open source”, if it was inherently more configurable. And also
about better migration information from one piece of software to the
next.

So I imagined a world in which when I get an update for a piece of
software I could see a smart diff, as a regular human, of what the new
UI and behaviour looks like, how it changed. This button moved there,
changed color. Pressing this button used to exhibit X behaviour, now
that behaviour is more complicated, or more limited, to trigger this
action, and so on.

I believe that a properly declarative UI library with explicit state
modeling, such as
[in Elm](http://elm-lang.org/guide/model-the-problem) or whatnot,
could actually handle a thing like that, but that it would have to be
designed from the bottom up like that. And every component would need
to have some “mock” meta-data about it, so that the migration tool
could say “here’s what the old UI looks like with lorem ipsum data in
it and here’s what that same data, migrated, looks like in the new UI”
and you could interact with this fake UI on fake data, with no
consequences. Or interact with the user's data in a read-only "fake"
way.

You could say: actually, no, I want to configure that this button will
stay where it is, that the theme will stay my current dark theme, etc.

You could visualize state changes in the UI such as with the time
traveling thing in Elm or React and make new decision trees, or
perhaps pick between built-in behaviours.

But one key idea could be that when you update software in a new way,
unless you’re removing the ability to do a feature completely
(e.g. the server won’t even respond to that RPC call), then you should
indicate that, in the intelligent “software diff”: then the user can
say, no I still want to use that and now they have a “patched” or
“forked” version of the software locally but that the maintainers of
the software don’t have to worry about.

Normally configuring software is a thing developers manually hard code
into the product. It seems obviously better to make software
inherently configurable, from a free software perspective at least
(not from a proprietary locked-in perspective).

Of course, you could write code at any time; drop down to that. But if
most of the code can be self-describing at least in a high-level “do
the thing or that thing” way, this would be far more accessible to
general users than code itself which at the moment is magic and
certainly beyond my interest to go and patch for the most part.
