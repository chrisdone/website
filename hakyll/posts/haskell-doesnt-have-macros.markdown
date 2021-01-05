---
date: 2020-12-29
title: Haskell doesn't have macros
description: Haskell doesn't have macros
author: Chris Done
tags: haskell
---

I wanted to underscore a specific point about Haskell's
metaprogramming capabilities. It doesn't have a macro system, the kind
that Lispers love. It has metaprogramming, yes. But not the kind
that's useful in the same way.

## What macros are

In Lisp, macros are:

1. Syntactically light-weight.
2. Don't require writing a parser to deal with the language's native
syntax.
3. They work on trees of lexical tokens of the language; _not_
abstract syntax trees. At best, they are incomplete ASTs.

Macros are functions that take in this tree of tokens, either atomic
lexical tokens, or lists denoted by balanced parentheses, and
do some transforms, and return the same as output.

```lisp
foo
"bar"
123
(foo "bar" (123) nil (blob x))
```

## Quoting in Lisp vs Haskell

In Haskell, we have GHC's Template Haskell. What that provides is:

1. Splices: `$(foo)` -- this syntax means: run the code `foo`, in the
`Q` monad, at compile time, and it should produce a valid Haskell
abstract syntax tree.
2. Quotation: `[|bar|]` -- this means: produce an action in the `Q`
monad which will produce the abstract syntax tree for the code
`bar`.
3. Quasiquotation: `[zot|anything|]` -- this means: the `zot` is a
parser that will parse the string contents `anything` and produce
an abstract syntax tree in the `Q` monad.

This code is ran at compile-time. That's different to Lisp.

In Lisp, e.g. Common Lisp, we have:

1. Splices: `,foo` -- run the code to produce a tree.
2. Quotation: `'bar` or `'(bar abc)` -- quote the code, that's all.
3. Quasiquotation: `` `bar`` or `` `(bar ,foo)``  --
this is like a regular quotation, but I can place `,` anywhere to
mean "run this code".

The code runs at **runtime** at this point, not compile-time yet.[^1]

## Macros

To have macros run at compile-time, Common Lisp specifically has
`defmacro` to define a macro. Here's an example of the famous
_anaphoric if_. It's a short-hand for let-binding followed by an
immediate if, testing that the value is "true-ish" (non-nil), and
makes whatever it is available as `it` in the if:

```lisp
(defmacro aif (test-form then-form else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))
```

You use it like this:

```lisp
(aif (calculate-something) (use it) (do-something-else))
```

Here's how we implement the same thing in Template Haskell:

``` haskell
aif testForm thenForm elseForm =
  [| let it = $testForm
     in if it
     then $thenForm
     else $elseForm
  |]
```

So far so good, apart from the fact no one would write this macro in
Haskell.[^2] Here's how we use it:

``` haskell
$(aif [|calculate something|] [|use it|] [|something other|])
```

Oh no! I have to manually quote every argument myself! The quotation
syntax isn't even short, I have to write `[| .. |]` around
everything. No one will use your macros if they are like that.

## Quasiquotes don't solve the problem

Haskell has what it calls "quasiquotes" which are like Lisp's reader
macros: You can parse an arbitrary string and produce an AST for the
compiler. They're actually great for embedding other languages into
Haskell like JSON, YAML, HTML, etc.

Maybe that could make writing this easy? Perhaps something like:

``` haskell
[aif|calculate something; use it; something other|]
```

On the surface this looks like we're there. But we're not, because
remember that Haskell's metaprogramming requires you to produce an
AST. The type `Exp` forms the abstract syntax tree and looks like this:

``` haskell
data Exp = VarE Name | ConE Name | LitE Lit | AppE Exp Exp | AppTypeE
Exp Type | ...
```

So you can't go from the string above into the compiler without
converting to an AST. **You need a parse step**.

Fine, let's just import GHC's own parser to produce the AST. Surely
that's possible? Sadly, not. GHC's own API parser has a completely
different tree type to the one in Template Haskell.[^3] This
is where we get stuck.

Implementing such a translation might be the way to salvage
quasi-quote syntax into a macro system. It might be a little slow,
though. It'd need long-term maintenance.

Quasiquotes in GHC optimise a different use-case to macros: they let
you deal with _foreign_ languages. Macros let you add slight
enhancements to your _existing_ language.

## A quick fix

A quick fix to at least let us get at the parse tree would be to have
some special syntax, e.g. to make something up completely[^4]:

```haskell
aif!(if calculate something then use it else something other)
```

Could produce:

```haskell
$(aif [| if calculate something then use it else something other |])
```

The limitation here is that the input to the macro has to parse
correctly into an AST. You can't have the branches of the `if`
separately from the `if` itself.

The surface syntax of Lisp (token trees), used by the macro system, is
a superset of the core syntax of Lisp (let/setf/cond/progn,
etc.). Unfortunately, the surface syntax of Haskell (used by the
metaprogramming) is exactly the core syntax:

``` haskell
> :t $(lamE [varP (mkName "x")] (varE (mkName "x")))
<interactive>:1:3-46: Splicing expression lamE [varP (mkName "x")] (varE (mkName "x"))
======>
\ x -> x
```

This is how you have to construct the AST in Haskell: it has to be of
the right schema immediately, there's no way to produce a "loose tree
of tokens" that TH will consume for you.

Additionally, quotation demands correct syntax too: `[| let |]` is a
syntax error. What if I wanted to define my own variation on let? Too
bad.

This would encourage strange warping and re-using of existing syntax
to achieve what you want. I don't think this is a good idea on the
whole.

Therefore, we either have to fit our macros into existing syntax (as
above), or parse strings (quasiquotes). We need the middle ground that
Lisp offers.

## An ideal design

Going back to the original definition of macros, we had:

1. Syntactically light-weight.
2. Don't require writing a parser to deal with the language's native
syntax.
3. They work on trees of lexical tokens of the language; _not_
abstract syntax trees.

Let's explore this in Haskell. If we had this, it might look like:

``` haskell
aif!(calculate something; use it; something other)
```

This is light-weight. It doesn't require a parser. We could receive
the lexical tokens as:

``` haskell
calculate  something  ;  use  it  ;  something  other
```

The only special addendum would be that bracketing syntax be balanced
into a tree, so: `() {} []`.[^5] Just like in Lisp. Finally, the
Template Haskell API would provide a trivial way to go from a token
tree to an abstract syntax tree: `[if,x,then,y,else,z]` into `CondE
...`

## Motivating examples

So the following constructs would be fine:

```haskell
nat!1      -- natural number.
ne![1,2,3] -- non-empty list.
sha1!cf23df2207d99a74fbe169e3eba035e633b65d94 -- compile-time validated sha1
email!"chrisdone@gmail.com" -- compile-time validated email
set!{1 2 3 c 4}
vec![ 1, 2, 3 ] -- vector type
map!{ x: 1, y: 2, z: 3 } -- easier-to-read compile-time constructed map
ado!(x <- y
     y <- z
     x)  -- applicative or qualified do could have been macros
```

(If you don't find those examples enticing there's no hope for you.)

As it happens, Rust's macros work just like this. Lisp's do. Haskell
could potentially have this. It depends whether the community wants
it, and/or whether someone is willing to implement a patch for GHC and
lobby for it. But it seemed worth pointing out.

---

Thanks [Jānis ǅeriņš](https://www.jonis.lv/) and
[Mihai Bazon](http://lisperator.net/) for reviewing this post.

[^1]: They're also syntactic sugar for `QUOTE`, `UNQUOTE`, and
`QUASIQUOTE`. The latter can be implemented with macros.

[^2]: No one would write this because Haskell doesn't have
"true-ish-ness" or implicit null, so this would simply produce a type
error. But we're going with a very typical example.

[^3]: Feel like writing that translation?  Haskell's surface syntax is
      big; be my guest. See Nikita Volkov's famous
      [records library post](http://nikita-volkov.github.io/record/)
      for an example of where this bites:

      > Unfortunately since the introduced syntax is not supported by
      > “haskell-src-exts” and since neither does that library expose a
      > parser API, Haskell syntax parsing needs to be reimplemented in the
      > “record” library. This is not a trivial task, so currently the
      > quasi-quoters do not support all of the Haskell syntax. All the
      > basic stuff is supported however: variables, literals, tuple and
      > list syntax. Don’t worry though, if you’ll try to use something
      > unsupported, you’ll get notified during compilation.

[^4]: Actually, this is very similar to Rust syntax.

[^5]: An open question would be whether you would include
whitespace-aligned lines as "these are a list of trees", to support
Haskell's indentation-sensitive syntax. Personally, I never liked that
part of Haskell. But it's a real consideration, it's how people code
in reality. Infix operators are another consideration: you may want to
resolve their precedence, or not. If you want `*` in prefix/postfix
position, then balancing shouldn't get in your way.
