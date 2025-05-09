---
date: 2022-01-31
title: A plain English description of monads without Haskell code
description: A plain English description of monads without Haskell code
author: Chris Done
tags: haskell
---

> "That's actually the first time I read an explanation about monads that I understand." -- Commenter

> "I just read your explanation, and suddenly I think I understand monads." -- Reviewer

Monads are notorious in the programming world for their use in the Haskell
programming language and for being hard to grasp. There's even a joke that
writing a "monad tutorial" is a rite of passage for new Haskellers, and been 
described as pointless.[^2] I've been
using Haskell for over a decade and have refrained from writing 
yet another monad tutorial. 

A friend asked for an easy explanation of them
that doesn't involve Haskell code.[^3] That's easier.

You can re-use your intuition from existing common place chaining of things 
in other popular languages: 

* Async chains ([JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise))
* Parser combinator chains ([Rust](https://github.com/Geal/nom), JS)
* Optional or erroneous value chains (TypeScript, [Rust](https://doc.rust-lang.org/std/result/))
* Continuation passing style (you can do this in Lisp and JS)
* Cartesian products/SQL ([C#’s LINQ](https://en.wikipedia.org/wiki/Language_Integrated_Query))

One of the Haskell designers[^6] in the 90s just came up with a **class/interface 
that worked for all of these**. As he was into category theory, he related it 
to the idea of a "monad". The types also sort of match the theory if you 
squint hard enough.

Then they came up with a syntax (actually, two[^1]) that was syntactic sugar for 
calling the class methods to make it feel more ergonomic.

Parsers, CPS, async, optional chaining, all look more or less like this in most modern
popular programming languages:

```javascript
getThing.and_then(x => putThingElsewhere(x+4)).and_then(..)
```

For a parser you track the position in the string and plumb that through. For 
an "optional" it just short circuits if the left hand side of `and_then` is 
nil/null/undefined. For async it would do something async, like make
a web request to get the thing, and then call the callback, etc.

**Monad is the name of the class** for “and_then”,[^4] defined in a sensible way, 
with some laws for how it should behave predictably, and then a bunch of 
library code works on anything that implements “and_then”. 

Apart from F# or Haskell (or descendants), no other language[^5] embraces 
the abstraction with syntax so it’s hard to find a good explanation without 
them. It’s like explaining Lisp macros without using a Lisp, the 
explanation tends to be awkward and unconvincing.

If you have mutation, you might not bother plumbing stuff for a parser, 
you might just update the position in place, and throw an exception 
on a parse error. If your language supports returning early from 
functions then you can just use that to short circuit (Rust does this). 
If your language puts in a special case for async with “await” 
syntax (JS, Rust, C#), you’ll just use that. Other ways of doing 
things just make the idea of a monad abstraction kind of unnecessary.

Haskellers don’t like to throw exceptions, or use mutation, and 
functions can’t return early, etc. Suddenly Monad and syntactic sugar for it 
looks pretty attractive to them. 

[^1]: Either do-notation or list comprehensions (yes, like in Python), which can be generalised to monad comprehensions. You can look that up if interested.

[^2]: [Abstraction, intuition, and the “monad tutorial fallacy”](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/)

[^3]: I understand, almost any explanation of them uses Haskell types to aid their explanation. [Wikipedia's article is garbage.](https://en.wikipedia.org/wiki/Monad_(functional_programming))

[^4]: There happen to be a plethora of other
things that implement the Monad class--some really weird stuff--but linking to them would just give
you a big list of Haskell type signatures, which you need to know Haskell to understand. It's not called "and_then", either
but that's an implementation detail. 

[^5]: Well, OCaml and other ML-descendents do dabble with this these days.

[^6]: [Monads for functional programming](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)
