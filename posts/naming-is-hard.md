---
date: 2024-07-09
title: Naming sum types is often hard
description: A small noodling thought about naming cases
---

After doing quite a lot of software design in systems which encourage
use of enumeration types, I've noticed that there's a common challenge
in naming both the type and the cases of the type. It usually exposes
a painful inadequacy in the English language and my grasp of it,
assuming any kind of aesthetic appreciation.

To take a trivial example, it’s only thanks to George Boole that we
have an elegant and concise name for the type of terms that consist of
either true or false. If we went for “truth“ or “truthiness“ we’d be
left unsatisfied because this name is biased towards one of the two
choices.

A more realistic example is the type of terms that consist of either
“local” or “remote.” If we call this “locality” then we are both
biased towards the local and overlap with a common, political
understanding of the word “locality.” “Remoteness“ presents similar
problems. The word “location“ doesn’t capture the modality. I’ll stop
here because I’m just upsetting myself.

Another example is the challenge of expressing whether something is
qualified or not qualified, in the sense of prefixed or suffixed with
some extra term. It’s really grating to have to express the thing in
terms of not-something. We prefer to say static versus dynamic, and
not static versus non-static or, above, local versus
non-local. Ignoring this, what do we call the type of these terms?
“Qualified” is biased. “Qualification” overlaps with a recognised
achievement. We frown and move on with something rubbish.

I probed an LLM to search for more obvious, satisfying answers to
these, in vain. But these are mere examples of a common experience. I
know that one of the two hard problems of computer science is naming
things, but this feels like a particularly bothersome flavour of it.
