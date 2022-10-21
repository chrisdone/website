---
date: 2022-10-21
title: Inflex
description: Inflex
author: Chris Done
tags: haskell, designs
---

Inflex® is a new creative digital workspace: powerful functional language (Haskell-inspired), rich data structures (no grid!), browser-based.

That's the tagline.

The frank explanation is that Inflex is a project that I've 
been working on in my spare time, along with my wife,
Giulia Costa, for 2 years. Progress is slow.

You can (for now) checkout our Twitter [InflexHQ](https://twitter.com/InflexHQ)
feed for screenshots, videos and updates.

There used to be a homepage on inflex.io on a beefy Hetzner 
server, but that's shut down now to cut expenses. Besides,
I am moving away from the cloud towards a more user-empowering
desktop experience.

I'll expand on this in future updates. There will be a desktop
program to download, more explanations, screenshots, etc.

Below is a sample of blog posts from the original homepage.

# What's wrong with the grid?

The grid system in spreadsheets is very simple. It’s also very
powerful, and it’s powerful because it’s simple. As a user you can
immediately start entering data without even thinking about it. And
then, in an ad hoc fashion, you can add logic and conditions as you
think of them.

## Lack of boundaries cause bugs

This fast and loose paradigm is also the downfall of spreadsheets,
because the user is encouraged to do a form of programming, without
encouraging them to do any kind of discipline. Spreadsheets quickly
get out of hand. The boundaries are arbitrary, and usually are only
visual, which means it’s very difficult to write good code based on
this.

For example, there is the famous case of a large spreadsheet used in
an economics paper, called
[Growth in a time of Debt](https://en.wikipedia.org/wiki/Growth_in_a_Time_of_Debt#Methodological_flaws),
and there was a miscalculation because a few rows at the end of a list
of items were not taken into account, as, per usual in a spreadsheet,
the user specified them in a range. It has been criticised anywhere
from a gross oversight to disastrous.

## Proper data structures are simpler

However, in normal programming, we don't have just one data
structure. The grid is just one data structure, a matrix of
practically infinite width and height. You can access its elements by
coordinates. But in programming––normal programming––we have, for
example, lists. If a proper list data structure were were used for the
list of elements in the above example, there wouldn’t have been this
bug, because you don’t have to specify how many elements of a list you
want to work on. We just say "apply this function to all elements of
the list."

Data structures like lists and matrices can all be approximated in a
spreadsheet by a grid, badly. However, anything more than this becomes
awkward very quickly.

For example, a simple record consisting of a name and an age in a
spreadsheet must be modelled by using a cell for the label of the name
and label of the age, and then the value for each thing. This is a
hack, using superficial visual embellishments to reflect what is
actually data.

In normal programming languages, this is called a record. There’s no
need for any kind of strange and brittle hacking and tricks to make
this work as you have to do in a spreadsheet. The basic spreadsheet,
as-is, is very accessible to the normal person who just wants to do
every day calculations about their business. But the ideas of a
record, list, tree or graph are also intuitive for normal people
because they actually model the kind of data that their domain is
dealing with. Sadly, knowledge workers using spreadsheets are deprived
of these tools.

## Data modelling is poor

Therefore we find spreadsheets very lacking, in fact, when we try to
model even every day pedestrian problems.  Consider for example trying
to model a family tree, or the hierarchical structure of the US
government, or a taxonomy of a species.

Trees are three data structures, perfectly normal, well understood,
pliable, things that you can express in a programming language and
manipulate using code in a very logical manner. You can count how many
items are in the tree, you can restrict the tree to a certain branch,
perform a transformation on each node in the tree, or transpose the
tree and flip it upside down!

Another example is a graph. For example, the Coronavirus can be
modelled by its spread using a graph the graph consists of a set of
people in the connections between those people. You can easily write
code to count how many connections a given node has to it; that's a
normal graph operation. Or count how many transitive node are
connected to it in one direction, in other words; you can measure the
influences of one person within a community.

Aside from using code to manipulate said data structures, your user
interface should allow you to click a node and edit the text, maybe
write some code in there, like any other type of cell.

Both of those things are impossible, practically, to describe, talk
about, express, manipulate, or visualise, in a traditional grid-based
spreadsheet!

So, we’ve established two things so far. The first is that the grid
system lacks boundaries between different data structures, which can
lead to bugs. Second is that there are actually very normal, every
day, useful data structures which we should, in a reasonable system,
be able to express, but which the grid system is unable to express.

Additionally, we have seen that these data structures actually have
very well understood and rich and useful operations which work only if
you have that kind of data structure.

But a spreadsheet simply has one single data structure: the grid. And,
the only elements that you can put in that data structure are numbers,
strings and dates. In other words, atomic values. But that’s not real
life. There’s no reason that I should not be able to have a list of
lists. Or a tree of records. Or a list of records (aka a "table"). I
should, hypothetically, be able to arbitrarily nest data structures as
it fits my problem.

## Correctness checks

Turning to a different topic, when you have different data structures,
you can start to add checks that help you avoid making other types of
mistakes. If your system knows the shape of data, it can have
expectations about how you use it.

You can add a type system to the system. The purpose of a type system
is to make sure that you are combining your different operations
together in a way that makes sense. Not putting a square peg in around
hole. For example, you can calculate the sum of an list, of a graph,
of a tree. However, you cannot concatenate a graph to an list. What
would that even mean?

## Conclusion

In conclusion, we’ve looked at reasons why a traditional spreadsheet
grid system is lacking in several key areas of expression,
correctness, and even convenience, in a way that is actually important
for normal knowledge work and not just for programmers.

If you look through various competitors to the established spreadsheet
vendors, you'll see a lot of "reinvent spreadsheets" language. But
it's always the same thing: we have a grid system with coordinates and
an underwhelming untyped expression language. But they added
JavaScript or Python to it, or added a "low-code" app generator on top
of the spreadsheet, or some special views.

We're working on [a system called Inflex](https://inflex.io/) that
really does rethink the spreadsheet fundamentally from the ground
up. And the first thing to be thrown out is the grid, the coordinate
system and replace them with real data structures and a type system.

We keep what's *good* about spreadsheets: the reactivity and
"edit-in-place" user experience.

In our next post we'll be discussing the fundamental problems with the
expression/formula languages used in spreadsheet software, and how we
are fixing it.

# The language of spreadsheets is bad

The user interface in a spreadsheet is modal. There are two modes:

* The first mode is **code** (also known as: formula): `=SUM(A1:A5)`
* The second mode is the **result** of running that code: `234`

You type in some equation, some mathematics, some conditions, and then
you hit return, and then you see the results.

## Declarative programming is a good fit

The programming language that you write your code in is a declarative
programming language. We call programming languages declarative when
you say what you want (declarative) more than instructing the computer
how to go about achieving it (imperative). When languages are
declarative, it gives the system more freedom to make choices about
how to go about computing results.

The code in spreadsheets is this kind of language. It has to be. When
you change code in one cell, all the other cells are updated. If the
language was imperative, that would mean it could change
things. That's bad when cells are re-run all the time during your
course of work. It'd be chaos to keep track of what you changed.

That's why the language must be declarative, to give the system
freedom to run formulae whenever it needs to without worrying about
side effects. On VisiCalc, the first spreadsheet software from 1979,
Ted Nelson said:

"VISICALC represented a new idea of a way to use a computer and a new
way of thinking about the world. Where conventional programming was
thought of as a sequence of steps, this new thing was no longer
sequential in effect: When you made a change in one place, all other
things changed instantly and automatically."

## That's where it ends

However, it’s quite a limited programming language. It was initially
designed to handle a small subset of problems that you might encounter
in finance. It has been extended with plenty of functions like
trigonometric functions and things like that, but without any
particular rigour or academic insight or critical eye.

It’s very convenient for simple problems, dealing with simple numbers,
text and dates. However, any programmer can tell you immediately that
there are limitations to this language. And indeed any Excel or Google
Sheets user can tell you that they have hit the limits of this
language often.

## The solutions aren't solutions

When you hit the limits of the spreadsheets expression language, there
are two approaches:

* The first is to try to remodel your problem to avoid the
  limitation, this is really just a hack/kludge.
* The other is to simply abandon this language and use either Visual
  Basic or JavaScript, or Python, or some other general purpose
  programming language that has the power to express what you really
  wanted to express in the first place. This is typically called
  "scripting".

The first approach is not a solution.

For the second, there are two solid problems with this approach:

* The first problem is that you are no longer using the original
  language and therefore you have to keep two languages in sync and in
  your head.
* You have to work with two completely different programming
  paradigms, because all of the mainstream scripting languages are
  imperative.

You have lost the declarative nature of what makes spreadsheets
great. You also have to learn a new language.

If you were a normal spreadsheet user, with a full plate of work, the
chances of actually learning Visual Basic or Python on your own time
are very slim.

So, probably, you’ll have to ask a "programmer" to solve your problem
for you, which is really annoying. A wise person once said: in
computing, there is nothing worse than a computer telling you that you
cannot express a thought.

## Wizard politics

This creates a funny class system of "muggles and wizards", to borrow
a Harry Potter term, wherein the millions of users of spreadsheets are
the muggles that make do with rudimentary tools, and the wizards are a
privileged class with all the power. Modern offerings like AirTable
continue this narrative: in the community forums, I have read this
comment by a community leader:

> I don’t recommend attempting this using a formula field. Look into a
> scripting solution—either in the Scripting app, or in a “Run script”
> action in an automation—where you can tap into the built-in sorting
> features of the JavaScript language.

It also makes bad sense from a business perspective; I paid for a tool
for all my employees, and now my employees are asking for developers
to do something that the tool should be able to do already. My
employees are wasted even though they are perfectly good at their
domain and are willing.

We are also faced with a secondary problem, which is the problem of
choice. You have to choose where to put your logic, either in the
spreadsheet or in the scripting language. Now you have two problems.

## Let's call a spade a spade

The elephant in the room is simply that the expression language in
spreadsheets is insufficient, not up to the task, not up to snuff,
including for people who aren’t programmers or
engineers. Hybridisation does not work well to paper over this issue
either.

This is also omitting other criticisms, like a lack of first-class
functions, which would make awkward abominations like `VLOOKUP` and
friends unnecessary.

We also know that this language simply does not scale. People write
god-awful messes of `IF(IF(..))` expressions that fill a whole screen
in one cell. Formulae are duplicated across ranges and then
accidentally modified only in some of them. It's the wild
west.

## There is already a better language

The good news is that for 35 years there has been in development
so-called _pure functional programming languages_, which are
declarative languages which have the power of general purpose
languages like Visual Basic, yet retain the declarative purity that we
enjoy in spreadsheets.

The most popular incarnation of this is called Haskell. It has a
static type system which prevents some issues. It has a
well-developed, comprehensive set of functions for expressing common
problems like loops, filters, reductions, etc.

Unlike your Visual Basics, your Pythons, your JavaScripts, Haskell
knows how to express normal every day programming problems in a
functional declarative way, and that is what makes it a perfect
candidate replacement language for spreadsheets.

Haskell also has something to say about dealing with time (think:
`=NOW()`), streams (think reading data from external systems), events
(think button clicks) and the rest in a pure language. Spreadsheet
systems side-step the whole issue (and miss a huge opportunity),
opting to simply call these "volatile" cells that may change in a
variety of ad hoc cases, but we'll see more about that in another
future article.

## Conclusion

In conclusion, there’s no point trying to maintain a hybrid approach
of using a very restricted language combined with an imperative
language, when you could go straight to the obvious solution and use a
real, functional, powerful language from the beginning, which has been
tried and tested for 35 years, and is easily up to the task of
expressing spreadsheet problems. There is also an optimising compiler
that can compile it to machine code to run very quickly.

You can educate people in a tool to do simple arithmetic and filters
very easily, I've done it. But the key addition is that you can level
up in the same language to do more complex things. There's a
progression path.

Our Inflex language is built from the ground up based on Haskell (with
lessons from PureScript, Unison and OCaml). It's designed to be run in
a reactive document, to deal with numbers, records, lists, tables,
etc. and in the future, streams. At the time of writing, we're in an
invite-only beta, but we'll be documenting more of that language over
time.


# Inflex language

Here's a brief introduction to Inflex syntax. We'll be writing a much
more detailed set of documentation soon, including interactive
examples in the page. But right now, hopefully the below is enough to
get you started.

**Table of Contents**

- [Arithmetic](#arithmetic)
- [Ordering and comparison](#ordering-and-comparison)
- [Records](#records)
- [Variants](#variants)
- [Text](#text)
- [Functions](#functions)
- [Function calls](#function-calls)
- [Available functions](#available-functions)
- [If](#if)
- [Cases](#cases)
- [Tables](#tables)

<!-- markdown-toc end -->


## Arithmetic

`*`, `/`, `+`, `-` are supported. Try `2 * 3 + 5.0` in the app.

You can use `* 1.00000` to get e.g. 5 decimal places of precision.

## Ordering and comparison

`<`, `<=`, `>=` for ordering. Try `4 = 5` or `42 * 23 < 53 * 24` in the app.

`=` and `/=` for comparison.

Currently these work on numbers and text, but not lists or records (yet).

## Records

You can put records together as:

`{foo: 1, "bar": 123}`

access:

`myrecord.name`

Try calling one cell `person` with `{name: "Charlie"}` in it. Then in
another cell, you can write `person.name`.

## Variants

You can refer to choices with hashtag syntax, like:

`#true`/`#false`

`#red`/`#black`/whatever

`#ok(123)`/`#none` - this is used by CSV import for missing fields.

It's fine to make a list like `[#red, #black, #red, #blue]`.

See `if` section.

## Text

Put your text in speech marks:

`"foo"`

But the Inflex interface will display a text editor, so you can just
edit it like that after hitting enter.

## Functions

Simply write,

```haskell
x: x * x
```

And this means "a function of input `x` with output `x * x`". Done!

See below for a good example.

## Function calls

Familiar like in spreadsheets:

```haskell
map(x:x*x,[1,2,3,4])
```

A nicer way to call functions is using dot syntax:

```haskell
[1,2,3].filter(x:x>2).sum()
```

Any built in function or cell with a function in it can be called in this way.

## Available functions

There is a limited list so far:

<table><tr><th>Name</th><th>Example</th></tr>
<tr><td>map</td><td><code>map(x: x * 2, [1,2,3])</code></td></tr>
<tr><td>filter</td><td><code>filter(x: x > 2, [1,2,3])</code></td></tr>
<tr><td>sum</td><td><code>sum([1,2,3,4])</code></td></tr>
<tr><td>average</td><td><code>average([1,2,3,4])</code></td></tr>
<tr><td>vega</td><td><code>vega({...})</code> (see <a href="https://vega.github.io/vega-lite/">vega</a>)</td></tr>
<tr><td>null</td><td><code>null([])</code></td></tr>
<tr><td>length</td><td><code>length([1,2,3])</code></td></tr>
<tr><td>distinct</td><td><code>distinct([2,3,3,4,3,2])</code></td></tr>
<tr><td>minimum</td><td><code>minimum([1,2,3])</code></td></tr>
<tr><td>maximum</td><td><code>maximum([1,2,3])</code></td></tr>
<tr><td>sort</td><td><code>sort([4,3,2,4,2,1])</code></td></tr>
<tr><td>find</td><td><code>find(x:x>5,[2,5,8,2,1])</code></td></tr>
<tr><td>all</td><td><code>all(x:x /= 0,[1,2,3,4])</code></td></tr>
<tr><td>any</td><td><code>any(x:x=0,[1,2,3,0,4])</code></td></tr>
<tr><td>from_ok</td><td><code>from_ok(0,#ok(1))</code></td></tr>
</table>

## If/case

Cases currently only work on variants (`#foo`).

```haskell
if #true { #true: "ok", _: "boo!" }

if 3 > 2 { #true: "Yep", _: "Maths is broken!" }
```

```haskell
if #true { #true: "true", #false: "false" }
```

Wildcards (`x` below):

```haskell
if #true { #true: "true", x: "false" }
```

Arguments to variants:

```haskell
if sum([1,2,3,4]) { #ok(n): n, x: 0 }
```

## Tables

Tables are just lists of records. In the UI, you see:

<div class="cell-wrapper"><div class="cell"><div class="cell-header"><div class="cell-name " title="Click to edit cell's name">(unnamed)</div><button class="delete-cell" title="Delete this cell">×</button></div><div class="cell-body"><div class="editor-boundary-wrap"><div class="ellipsis-button" title="Edit this as code"></div><table class="table"><thead class="table-header"><th class="table-column" title=""></th><th class="table-column" title="Click to edit"><div class="table-column-content"><div class="cell-name " title="Click to edit column name">name</div><button class="remove-column-button">×</button></div></th><th class="table-column" title="Click to edit"><div class="table-column-content"><div class="cell-name " title="Click to edit column name">age</div><button class="remove-column-button">×</button></div></th><th class="add-column"><button class="add-column-button" title="Add column to this table">+</button></th></thead><tbody class="table-body"><tr><td class="row-number"><div class="row-number-div"><div class="row-number-text">1</div><button class="remove-row-button">×</button></div></td><td class="table-datum-value"><div class="editor-boundary-wrap" title=""><div class="ellipsis-button" title="Edit this as code"></div><div class="text"><div class="cell-name " title="Click to edit text">Terry</div></div></div></td><td class="table-datum-value"><div class="editor-boundary-wrap clickable-to-edit" title="Click to edit"><div class="misc">52</div></div></td><td class="add-column-blank"></td></tr><tr><td class="row-number"><div class="row-number-div"><div class="row-number-text">2</div><button class="remove-row-button">×</button></div></td><td class="table-datum-value" colspan="1"><div class="editor-boundary-wrap" title=""><div class="ellipsis-button" title="Edit this as code"></div><div class="text"><div class="cell-name " title="Click to edit text">Billie</div></div></div></td><td class="table-datum-value"><div class="editor-boundary-wrap clickable-to-edit" title="Click to edit"><div class="misc">42</div></div></td><td class="add-column-blank"></td></tr><tr><td class="add-row"><button class="add-row-button " title="Add row">+</button></td><td class="bottom-blank" colspan="3"></td></tr></tbody></table></div></div></div></div>

Underneath, this is just:

```haskell
[{name:"Terry",age:52},{name:"Billie",age:42}]
```

So you can use `map`, `filter`, `sum`, etc. on them.

# Bibliography

## Direct influences

* [Why do we need modules at all?](http://erlang.org/pipermail/erlang-questions/2011-May/058768.html)
* [Polymorphic variants](https://caml.inria.fr/pub/papers/garrigue-polymorphic_variants-ml98.pdf)
* [Behaviors of Reflex](https://qfpl.io/posts/reflex/basics/behaviors/)
* [Ray Panko’s work on spreadsheets and bugs](https://www.researchgate.net/profile/Ray-Panko)
* [Pain We Forgot](http://lighttable.com/2014/05/16/pain-we-forgot/)
* [Haskell](https://www.haskell.org/) (classes, pure)
* [Duet](https://chrisdone.com/toys/duet-delta/) (stepper)
* [PureScript](https://www.purescript.org/) (row types)
* [Unison](https://www.unisonweb.org/) (CAS code)
* [Clojure](https://clojure.org/)
* [Type classes vs the world](https://www.youtube.com/watch?v=hIZxTQP1ifo) (coherence)
* [Simple made easy](https://www.youtube.com/watch?v=oytL881p-nQ)
  (names over positioning)
* [Spec-ulation](https://m.youtube.com/watch?v=oyLBGkS5ICk) (specifically, dependencies)

## Reading list

* [Haxcel](https://www.semanticscholar.org/paper/Haxcel-A-spreadsheet-interface-to-Haskell-written-Malmström/739998d4b2cd3b389f1593f20ca326f0025b3a32?p2df)
* [Streaming spreadsheets](http://hirzels.com/martin/papers/ecoop14-activesheets.pdf)
* [Smalltalk spreadsheet](http://www.bitsavers.org/pdf/xerox/xsis/XSIS_Smalltalk_Products_Apr87.pdf)
* [Implementing VisiCalc](http://rmf.vc/implementingvisicalc) -
  some things in Inflex coincidentally match VisiCalc's original
  design: @func for functions, fixed precision numbers
* [Excel will never die](https://www.notboring.co/p/excel-never-dies)
* [The trouble with typeclasses](https://pchiusano.github.io/2018-02-13/typeclasses.html)

## Consulted

* [Stepping OCaml](https://arxiv.org/abs/1906.11422)
* [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)
* [Type Inference with Polymorphic Recursion](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.42.3091&rep=rep1&type=pdf)
* [A reckless introduction to Hindley-Milner type inference](http://reasonableapproximation.net/2019/05/05/hindley-milner.html)
* [Type classes: confluence, coherence and global uniqueness](http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/)
* [Haskell with only one typeclass](http://okmij.org/ftp/Haskell/TypeClass.html#Haskell1)
* [Algorithmically Scrapping Your Typeclasses](https://reasonablypolymorphic.com/blog/algorithmic-sytc/)

## Similar space research projects

* [Mesh](http://mesh-spreadsheet.com/)
* [Gneiss](https://www.cs.cmu.edu/~shihpinc/gneiss.html)
* [Plutojl](https://plutojl.org/)
* [Apogee](https://www.apogeejs.com/)
* [Bubble Pop!](https://chrisuehlinger.com/LambdaBubblePop/)
* [Ravi Chugh's work](http://people.cs.uchicago.edu/~rchugh/)
  (Sketch-n-Sketch, Ivy)

## Vaguely similar market products

* [Mito](https://trymito.io/launch)
* [Bamboo](https://bamboolib.8080labs.com/)
* [Blockpad](https://blockpad.net/)
* [CubeWeaver](https://cubeweaver.com/)
* [Livebook](https://github.com/livebook-dev/livebook)
* [Coda](https://coda.io/welcome)
* [Rows](https://blog.rows.com/p/rows-beta) (previously dashdash)
* [Grid](https://grid.is/)
* [Ellx](https://ellx.io/)
* [Baserow](https://baserow.io/)
* [CloudTables](https://cloudtables.com/)
* [BaseDash](https://www.basedash.com/)
* [SeaTable](https://seatable.io/en/)
* [Casual](https://www.causal.app/)
* [Grid Studio](https://gridstudio.io/)
* [Patera](https://patera.io/)
* [Visor](https://www.visor.us/)
* [AirTable](https://airtable.com/)
* [Lido](https://www.lido.app/)

## Dead products

The irony of some of these link names is not lost on me.

* [https://ethersheet.org/](https://ethersheet.org/)
* [http://getpermanent.com/](http://getpermanent.com/)
* [https://stenci.la/stencila/blog/introducing-sheets/spreadsheets-are-dead-long-live-reactive-programming-environments-](https://stenci.la/stencila/blog/introducing-sheets/spreadsheets-are-dead-long-live-reactive-programming-environments-)
* [http://dtab.io/](http://dtab.io/)
* [https://www.herculus.io/](https://www.herculus.io/) (Haskell spreadsheet)
* [https://www.resolversystems.com/](https://www.resolversystems.com/)
