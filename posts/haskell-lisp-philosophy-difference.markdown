---
date: 2015-12-19
title: A philosophical difference between Haskell and Lisp
description: A philosophical difference between Haskell and Lisp
author: Chris Done
tags: haskell, lisp
---

**UPDATE 2020-08-03**: I no longer stand by the content in this
post. I think the overall sentiment is marginally accurate; however,
the details in the post are incorrect (as many have pointed out over
the years).

As has been pointed out, `remove-if-not`'s start/count parameters
behave differently and cannot easily be separated out of the function,
a design trade-off that I appreciate.

As has been noted and pointed out, Clojure permits compositional style
and laziness. I would also point out that laziness isn't exactly
necessary for stream fusion (just purity will do), but it helps with
ergonomics.

I don't feel like pulling the article completely. It has made the
rounds on HN/reddit for years. I feel good about just admitting that
it's not very good. Feel free to read on, but take it with a grain of
salt.

---

One difference in philosophy of Lisp (e.g. Common Lisp, Emacs Lisp)
and Haskell is that the latter makes liberal use of many tiny
functions that do one single task. This is known as _composability_,
or the UNIX philosophy. In Lisp a procedure tends to accept many
options which configure its behaviour. This is known as _monolithism_,
or to make procedures like a kitchen-sink, or a Swiss-army
knife.

Which one is better can be discussed in another post. I just want to
make the simple case that there is indeed a difference in philosophy
and practice. Having written my fair share of non-trivial Emacs Lisp
(and a small share of Common Lisp; I've maintained Common Lisp
systems) and my fair share of non-trivial Haskell I think I'm in a
position to judge.

Full disclosure: We'll just look at some trivial examples anyone can
understand, with the (unproven but asserted) implication that these
examples are representative of the general way software is written in
these languages.

An example which should be readily familiar to any programmer of any
background is working on lists. For example, CL has the
`remove-if-not` procedure. Its documentation signature is like this:

``` lisp
(REMOVE-IF-NOT predicate seq :key :count :start :end :from-end)
```

It packs a number of ideas into one procedure.

By comparison, Haskell has the `filter` function:

``` haskell
filter :: (a -> Bool) -> [a] -> [a]
```

Given a problem statement "take all elements from the list--except the
first three--that satisfy predicate `p`, and take only the first five
of those", in Common Lisp you'd express it quite concisely as this:

``` lisp
(remove-if-not #'p xs :count 5 :start 3)
```

The same in Haskell would be expressed as this:

``` haskell
take 5 . filter p . drop 3
```

The difference which should be obvious whether you know Haskell or
Lisp is that in the Lisp code the function does a few behaviours and
accepts arguments to configure them. In the Haskell code, we use three
different functions which do one task:

``` haskell
take ∷ Int -> [a] -> [a]
filter ∷ (a -> Bool) -> [a] -> [a]
drop ∷ Int -> [a] -> [a]
```

The `.` operator composes functions together, just like pipes in
UNIX. We might express this in UNIX something like:

``` bash
bash-3.2$ cat | tail -n '+4' | grep -v '^p' | head -n 5
1
2
3
4
5
6
7
8
9
10
```

Press Ctrl-d here we get:

``` bash
4
5
6
7
8
```

Like pipes in UNIX, the functions are clever enough to be performant
when composed together--we don't traverse the whole list and generate
a new list each time, each item is generated on demand. In fact, due
to [stream fusion](http://chrisdone.com/posts/stream-composability),
the code will be compiled into one fast loop.

If we want things that don't satisfy the predicate, we just compose
again with `not`:

``` haskell
take 5 . filter (not . p) . drop 3
```

In Common Lisp composition is a bit wordier because it's rarely if
ever used, so instead there is another function for that:

``` haskell
(remove-if #'p xs :count 5 :start 3)
```

(Probably a more Lispy approach would've been to have a `:not` keyword
argument to the `remove-if` function.)

The most pathological example of such a kitchen sink in Lisp is the
well known LOOP macro.

Problem: get all elements less than 5, then just the even ones of
that set.

With the LOOP macro this can be expressed quite readily:

``` lisp
> (loop for i in '(1 2 3 4)
        when (evenp i)
        collect i
        when (> i 5) do (return))
(2 4)
```

In Haskell this is expressed with two separate functions:

``` haskell
λ> (filter even . takeWhile (< 5)) [1..4]
[2,4]
```

In Haskell the same applies to vector libraries and text libraries and
bytes libraries, which can be fused. Fusion is chiefly an advantage of
purity -- you can fuse n loops together into one loop if you know that
they don't do side-effects. Such an advantage can also be applied to
other pure languages like Idris or PureScript or Elm.
