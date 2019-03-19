---
date: 2015-06-21
title: Existentials and the heterogenous list fallacy
description: Existentials and the heterogenous list fallacy
author: Chris Done
tags: haskell
---

An oft-stated argument against static typing is that heterogenous
lists are unreasonably difficult to model. Why is static typing being
so difficult? Why can't it just be like dynamic typing? This is a
specious argument.

For example, in one article I read, I saw:

> In fact you can program heterogeneous lists in dependently typed
> languages, but it’s unreasonably complicated. Python makes no
> complaints:
>
>     >>> g = lambda x: x**2
>     >>> [1, 'a', "hello", g]
>     [1, 'a', 'hello', <function <lambda> at 0x103e4aed8>]
>
> To me this is one methodological weakness of type theory […]

(I'm not sure what “methodological weakness” is supposed to mean, but
let's ignore that.)

There are two problems with this argument and demonstration:

1. It's contrived. I've written about as much Emacs Lisp and
   JavaScript as I have written Haskell and C#, and I cannot with all
   intellectual honesty remember wanting a heterogenous list.[^1]
2. It's ill-defined. What is this data structure? What can I assume
   about the elements so that I can write operations generically?
   Which, I presume, is the only reason I would be using a list in the
   first place (otherwise a record would be the correct thing to use);
   to write algorithms that apply generally to any index.

Even cutting the author some slack and assuming they might want to
just temporarily put some things together as a tuple, static languages
have tuples, which are heterogenous.

When you look at it beyond the superficial, it's rather odd.

Regardless, I am sporting. Some people will say, yes, okay, it's
contrived, and never really arises, but *if I really wanted this*, how
could I do it in a statically typed language? So here is the above in
Haskell.

Let's look at the example:

    >>> g = lambda x: x**2
    >>> [1, 'a', "hello", g]
    [1, 'a', 'hello', <function <lambda> at 0x103e4aed8>]

So the list contains a bunch of disparate things and the implicit
invariant here is that we can print each of them out. So we can model
that with an existential data type `Py` (for “Python”) that holds some
type `a` that is showable.

    data Py = forall a. Show a => Py a
    instance Show Py where show (Py s) = show s

(Oh, Haskell doesn't define an instance for printing functions, so
let's use `instance Show (a -> b) where show _ = "<function>"` to
vaguely match Python.)

I may not know, or care, *what* the type is, but I at least need to
know *something about it*, in a duck-typing kind of way. If it walks
like a duck, quacks like a duck, etc. then it's a good enough duck for
my purposes. In this case, `Py` says, is it at least showable?

Now we can wrap up any type which is an instance of `Show` with that:

    λ> [Py 1,Py 'a',Py "hello",Py (\x -> x ** 2)]
    [1,'a',"hello",<function>]

And we can apply the `show` method (from the `Show` class) to
each element generically:

    λ> map (\(Py p) -> show p) [Py 1,Py 'a',Py "hello",Py (\x -> x ** 2)]
    ["1","'a'","\"hello\"","<function>"]

So that's how to do it.

Continuing the discussion, if I extend the type to support also `Num`
(numbers),

    data Py = forall a. (Show a,Num a) => Py a

then I can use `*`, but only for actual numbers. If I try something
else I get a type error:

    λ> map (\(Py p) -> Py (p*2)) [Py 1,Py 'c']
    <interactive>:634:33:
        No instance for (Num Char) arising from a use of ‘Py’
        In the expression: Py 'c'
    λ> map (\(Py p) -> Py (p*2)) [Py 1,Py 2.0]
    [2,4.0]

Doing the same in Python, we have more or less the same error:

    >>> def pow2(x): return x ** 2
    ...
    >>> list(map(pow2,[1,5,'a']))
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "<stdin>", line 1, in pow2
    TypeError: unsupported operand type(s) for ** or pow(): 'str' and 'int'

The difference being the Haskell error was signaled statically at
compile-time and the Python error was signalled dynamically at
run-time.

[^1]: To demonstrate my good faith, I have constructed heterogenous
      lists in Emacs Lisp to model a record, but this is because Emacs
      Lisp lacks native records. JavaScript, on the other hand, has
      records.
