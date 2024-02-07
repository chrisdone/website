---
date: 2015-01-24
title: My Haskell tooling wishlist
description: My Haskell tooling wishlist
author: Chris Done
tags: haskell
---

I spend a lot of my time on Haskell tooling, both for my hobbies and
my job. Almost every project I work on sparks a desire for another
piece of tooling. Much of the time, I'll follow that wish and take a
detour to implement that thing (Fay, structured-haskell-mode, hindent,
are some Haskell-specific examples). But in the end it means less time
working on the actual domain problem I'm interested in, so a while ago
I intentionally placed a quota on the amount of time I can spend on
this.

So this page will contain a list of things I'd work on if I had
infinite spare time, and that I wish someone else would make. I'll
update it from time to time as ideas come to the fore.

These projects are non-trivial but are do-able by one person who has
enough free time and motivation. There is a common theme among the
projects listed, which is that they are things that Haskell among most
other well known languages is particularly well suited for and yet we
don't have such tooling as standard tools in the Haskell tool
box. They should be!

## An equational reasoning assistant

Equational reasoning lets you prove properties about your functions by
following a simple substitution model to state that one term is equal
to another. The approach I typically take is to expand and reduce
until both sides of the equation are the same.

Here is an example. I have a data type, `Consumer`. Here is an
instance of `Functor`:

``` haskell
instance Functor (Consumer s d) where
  fmap f (Consumer d p) =
    Consumer d
             (\s ->
                case p s of
                  (Failed e,s') -> (Failed e,s')
                  (Continued e,s') -> (Continued e,s')
                  (Succeeded a,s') ->
                    (Succeeded (f a),s'))
```

I want to prove that it is a law-abiding instance of Functor, which
means proving that `fmap id ≡ id`. You don't need to know anything
about the `Consumer` type itself, just this implementation. Here are
some very mechanical steps one can take to prove this:

``` haskell
id ≡ fmap id
   ≡ \(Consumer d p) ->
        Consumer d
         (\s ->
            case p s of
              (Failed e,s') -> (Failed e,s')
              (Continued e,s') -> (Continued e,s')
              (Succeeded a,s') -> (Succeeded (id a),s'))
   ≡ \(Consumer d p) ->
        Consumer d
         (\s ->
            case p s of
              (Failed e,s') -> (Failed e,s')
              (Continued e,s') -> (Continued e,s')
              (Succeeded a,s') -> (Succeeded a,s'))
   ≡ \(Consumer d p) ->
        Consumer d
         (\s -> p s)
   ≡ \(Consumer d p) ->
        Consumer d p
   ≡ id
```

So that's:

* Expand the `fmap id` into the instance's implementation.
* Reduce by applying the property that `id x ≡ x`.
* Reason that if every branch of a case returns the original value of
  the case, then that whole case is an identity and can be dropped.
* Eta-reduce.
* Again, pattern-matching lambdas are just syntactic sugar for cases,
  so by the same rule this can be considered identity.
* End up with what we wanted to prove: `fmap id ≡ id`

These are pretty mechanical steps. They're also pretty laborious and
error-prone. Of course, if you look at the first step, it's pretty
obvious the whole thing is an identity, but writing the steps out
provides transformations that can be statically checked by a
program. So it's a good example, because it's easily understandable
and you can imagine proving something more complex would require a lot
more steps and a lot more substitutions. Proof of
[identity for Applicative has substantially more steps](http://lpaste.net/5232874250913710080),
but is equally mechanical.

Wouldn't it be nice if there was a tool which given some expression
would do the following?

* Suggest a list of in-place expansions.
* Suggest a list of reductions based on a set of pre-defined rules (or
  axioms).

Then I could easily provide an interactive interface for this from
Emacs.

In order to do expansion, you need the original source of the function
name you want to expand. So in the case of `id`, that's why I
suggested stating an axiom (id a ≡ a) for this. Similarly, I could state the
identity law for Monoids by saying `mappend mempty a ≡ a`, `mappend a
mempty ≡ a`. I don't necessarily need to expand the source of all
functions. Usually just the ones I'm interested in.

Given such a system, for my example above, the program could actually
perform all those steps automatically and spit out the steps so that I
can read them if I choose, or otherwise accept that the proof was
derived sensibly.

In fact, suppose I have my implementation again, and I state what must
be satisfied by the equational process (and, perhaps, some axioms that
might be helpful for doing it, but in this case our axioms are pretty
standard), I might write it like this:

``` haskell
instance Functor (Consumer s d) where
  fmap f (Consumer d p) = ...
proof [|fmap id ≡ id :: Consumer s d a|]
```

This template-haskell macro `proof` would run the steps above and if
the equivalence is satisfied, the program compiles. If not, it
generates a compile error, showing the steps it performed and where it
got stuck. TH has limitations, so it might require writing it another
way.

Such a helpful tool would also encourage people (even newbies) to do
more equational reasoning, which Haskell is often claimed to be good
at but you don't often see it in evidence in codebases. In practice
isn't a standard thing.


Promising work in this area:

* [Introducing the Haskell Equational Reasoning Assistant](http://ittc.ku.edu/~andygill/papers/IntroHERA06.pdf)
  -- works pretty much how I described above. I don't know where the
  source is, I've emailed the author about it. Will update with any results.

**Update 2014-01-25**: Andrew Gill got back to me that
[HERMIT](http://ku-fpg.github.io/software/hermit/) is the continuation
of HERA. It seems that you can get
[inlining](http://hackage.haskell.org/package/hermit-0.6.0.0/docs/HERMIT-Dictionary-Inline.html),
[general reduction](http://hackage.haskell.org/package/hermit-0.6.0.0/docs/HERMIT-Dictionary-Composite.html)
and
[a whole bunch of case rewrites](http://hackage.haskell.org/package/hermit-0.6.0.0/docs/HERMIT-Dictionary-Local-Case.html)
from this project. Check
[the KURE paper](http://ku-fpg.github.io/papers/Sculthorpe-14-KURE/)
for the DSL used to do these rewrites, it looks pretty aweeome. So if
anyone's thinking of working on this, I'd probably start with reading
`HERMIT.Shell` or `HERMIT.Plugin` and see how to get it up and
running. It's a pity it has to work on Core, that's a little sad, but
as far as trade-offs go it's not too bad. Doing proofs on things more
complicated than core might be hard anyway. It does mean you'll
probably want to make a rewrite that does a global variable
replacement: `x` and `y` is a little easier to read than `x0_6` and
the like that you get in Core.

## Catch for GHC

Ideally, we would never have inexhaustive patterns in Haskell. But a
combination of an insufficient type system and people's insistence on
using partial functions leads to a library ecosystem full of potential
landmines. [Catch](http://community.haskell.org/~ndm/catch/) is a
project by Neil Mitchell which considers how a function is called when
determining whether its patterns are exhaustive or not. This lets us
use things like `head` and actually have a formal proof that our use
is correct, or a formal proof that our use, or someone else's use,
will possibly crash.

``` haskell
map head . group
```

This is an example which is always correct, because `group` returns a
list of non-empty lists.

Unfortunately, it currently works for a defunct Haskell compiler, but
apparently it can be ported to GHC Core with some work. I would very
much like for someone to do that. This is yet another project which is
the kind of thing people claim is possible thanks to Haskell's unique
properties, but in practice it isn't a standard thing, in the way that
QuickCheck is.

## A substitution stepper

This is semi-related, but different, to the proof assistant. I would
like a program which can accept a Haskell module of source code and an
expression to evaluate in the context of that module and output the
same expression, as valid source code, with a single evaluation step
performed. This would be fantastic for writing new algorithms, for
understanding existing functions and algorithms, writing proofs, and
learning Haskell. There was something like this demonstrated in
Inventing on Principle. The opportunities for education and general
development practice are worth such a project.

Note: A debugger stepper is not the same thing.

Example:

``` haskell
foldr (+) 0 [1, 2, 3, 4]

foldr (+) 0 (1 : [2, 3, 4])

1 + foldr (+) 0 [2, 3, 4]

1 + foldr (+) 0 (2 : [3, 4])

1 + (2 + foldr (+) 0 [3, 4])

1 + (2 + foldr (+) 0 (3 : [4]))

1 + (2 + (3 + foldr (+) 0 [4]))

1 + (2 + (3 + foldr (+) 0 (4 : [])))

1 + (2 + (3 + (4 + foldr (+) 0 [])))

1 + (2 + (3 + (4 + 0)))

1 + (2 + (3 + 4))

1 + (2 + 7)

1 + 9

10
```

Comparing this with foldl immediately shows the viewer how they differ
in structure:

``` haskell
foldl (+) 0 [1, 2, 3, 4]

foldl (+) 0 (1 : [2, 3, 4])

foldl (+) ((+) 0 1) [2, 3, 4]

foldl (+) ((+) 0 1) (2 : [3, 4])

foldl (+) ((+) ((+) 0 1) 2) [3, 4]

foldl (+) ((+) ((+) 0 1) 2) (3 : [4])

foldl (+) ((+) ((+) ((+) 0 1) 2) 3) [4]

foldl (+) ((+) ((+) ((+) 0 1) 2) 3) (4 : [])

foldl (+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) []

(+) ((+) ((+) ((+) 0 1) 2) 3) 4

1 + 2 + 3 + 4

3 + 3 + 4

6 + 4

10
```

Each step in this is a valid Haskell program, and it's just simple
substitution.

If the source for a function isn't available, there are a couple
options for what to do:

* Have special-cases for things like `(+)`, as above.
* Just perform no substitution for that function, it will still be a
  legitimate program.

It's another project I could easily provide see-as-you-type support
for in Emacs, given an engine to query.

Again, this is just one more project which should just be a standard
thing Haskell can do. It's a pure language. It's used to teach
equational reasoning and following a simple lambda calculus
substitution model. But there is no such tool. Haskell is
practically waving in our faces with this opportunity.

Existing work in this area:

* [stepeval](https://github.com/bmillwood/stepeval) - a prototype
  which nicely demonstrates the idea. It's based on HSE and only
  supports a tiny subset. There aren't any plans to move this forward
  at the moment. I'll update the page if this changes.
