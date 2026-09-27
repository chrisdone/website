---
date: 2026-09-27
title: "LLM-Complete: The next 700 programming languages"
---

I was reading
[microgpt](https://karpathy.github.io/2026/02/12/microgpt/)[^1] and
found myself brushing up on linear algebra and tensors, and then
separately was reading articles du jour about how prompting will
replace coding and so on,[^4] and thought: why might that be? That is, if
we are to take all the bold claims of vibecoders at face value. Bear
with me.

Dijkstra comments, in various places, that language is important. LLMs
by definition provide compelling evidence that language is profoundly
important (or that we think it is[^2]). Dijkstra argues quite
convincingly that the good thing about abstraction is to let us think
clearer, say what's needed unambiguously, and _no more than
that_. Formal languages can't say _a lot_ by design, but what they do
say is absolutely precise. I'll add my own observation that this
is a humble acknowledgement of the human mind's limitations.

There are examples of formal languages (notations, DSLs, etc.) that
I'd prefer to think in than English (or substitute your own natural
language) for certain problems:

* Type systems: `f (a -> b) -> f b -> f a` (by people who grok them)
  tells me quite a lot about what can and cannot happen.
* Mathematical notation (any: pick one) is preferred over spoken
  language (e.g. English) by pretty much anyone. In the above post,
  Karpathy tried his best to write in plain, "approachable" English,
  and seemingly found it impossible: often formal notation is better
  for thinking.
* Regexes (by people who grok them) are a simple example;
  `[a-z-]{2,3}$` is both shorter to digest and more precise "a suffix
  consisting of alphabetical letters or a hyphen of length between 2
  and 3."
* Some would argue TLA+, but I've only read the book, I haven't really
  gone deep on it yet.

That makes me think, for any piece of code in a "high-level" language,
if for whatever reason we'd prefer to express that in plain English to
an LLM and have the LLM be the "compiler" into some other
"lower-level" language (C, Rust, TypeScript), then might we consider
that to be a genuine failing of the language, or our abstractions
within the language. At least as far as the declarative, "high-level"
aspiration and conventional thinking goes.

I've been using the term "LLM-Complete"[^3] for this property: If a
language, or certain task within a language, is LLM complete, then
it's easier, more convenient and preferable to simply _use the formal
language_ than to ask an LLM to work with it. In this sense, it's
**more powerful as a knowledge tool.** If a language (this includes
notations, DSLs) isn't LLM complete, it will inevitably be (and is
probably already) thought of as "low-level" by the LLM user. Fit only
for generating and ingesting, a glorified boilerplate.

Given that [we really don't know how to
compute](https://www.youtube.com/watch?v=HB5TrK7A4pI&t=1507s), giants
of the computer science field often acknowledge that the field is
maturing (sort of[^6]), but still we have a lot of work left to
do. There aren't vocab or algorithms to describe a lot of types of
problems, and behaviours we see in nature.
 
Are the The Next 700 Programming Languages[^5] going to be about being
LLM-Complete?

[^1]: Quite a good article on building a GPT from scratch in 200 lines
    of plain Python. Sort of like Norvig's [Lisp in
    Python](https://www.norvig.com/lispy.html) in its brevity and
    implications.
    
[^2]: Specifically, the absolute enraptured fascination with LLMs proves
    that people (informally) equate language capability with
    intelligence. Sorry,
    [Portia,](https://en.wikipedia.org/wiki/Portia_(spider)) it's bad news.

[^3]: As a stylistic (not technical) nod to Turing-Complete, this has
    also been applied in a more jocular way to eso-language capabilities,
    e.g. Tetris-Complete or Pac-Man-Complete.

[^4]: I believe that regardless of my oscillating mood around LLMs in
    the current moment, learning how these things work and how
    services and tools that drive them work, on a technical level, is
    worth doing. For other thoughts on LLMs, see [my diary on
    LLMs](/posts/llms).

[^5]: As a reminder, Peter Landin's paper [The next 700 programming
    languages](https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf)
    predicted a move towards a more compositional, denotational set of
    programming languages.

[^6]: As Alan Kay derides, in so many words, software culture is a pop
    culture; we don't know our own history. Myself included. See also
    [Turing
    Oversold](https://people.idsia.ch/~juergen/turing-oversold.html).
