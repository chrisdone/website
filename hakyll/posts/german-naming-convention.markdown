---
date: 2019-06-07
title: German Naming Convention
description: German Naming Convention
author: Chris Done
tags: programming
---

## Introduction

As a software consultant, mostly contributing code to other company's
codebases, and doing code review, I've seen about thirty separate
codebases over the past half dozen years, and there's one thing that
could make my life much easier: better naming convention.

Jump to [German Naming Convention](#german-naming-convention) to skip
straight to it.

## Expect the Violent Psychopath

You're trying to tell a story with your code. Your code should tell
that story clearly, not cryptically, for an audience besides
yourself. A good yardstick for deciding what kind of audience you are
writing for is to imagine someone who has a familiarity with your
domain but not your program's take on the domain. I think programmers
forget that, as they are authors, they have readers.

A famous and regularly quoted piece of advice, from the mailing list
`comp.lang.c` in 1991, John F. Woods wrote:

> Always code as if the guy who ends up maintaining your code will be a
> violent psychopath who knows where you live.  Code for readability.

It's hard to put it better than that.

## Naming Tropes

There are some common naming conventions which are departures from
plain English, usually in the interest of brevity:

1. Abbreviations: when words are abbreviated such as `fct` for
   "function", `dfn` for "definition", `ctx` for "context."
2. It's All Greek To Me: using simply `a`, `x`, etc. as in mathematics.
3. "Hungarian" notation: any prefix or suffix notation in which a
   single letter is used to refer to a _type_ or _property_ of the
   variable, as in sigils like `$foo` ("scalar foo"), `lpszFoo` ("long
   pointer string zero-terminated"), or `fooL` (list of foo).
4. Acronyms: using initial letters to refer to concepts: `throwVE`
   ("throw validation error").

Most of these are unnecessary[^typedness] and/or harmful.

## It's All Greek To Me

A word on this convention. Single letter naming comes from
mathematical tradition; it means "there isn't a good noun for this
because it's general". A person of X height. In some cases, this is
actually reasonable. Consider:

```haskell
identity x = x
```

The `identity` function isn't enhanced by calling its parameter
`thing`; it literally doesn't matter what it is, especially in some
typed languages[^parametricity]. In fact, one could argue that it's
harmful to try to using a meaningful English name.

However, anywhere that your variables have some meaning, by using
"Greek convention", you're throwing away information that could help
someone to digest your code better. You're not trying to fit your code
on a napkin.

## German Naming Convention

This is what I consider good naming convention. I discovered this
convention while working with a German colleague, who, I'd always
joked, uses long variable names, and almost never abbreviates
anything. However, the more I read his code, the more I realised I was
able to read the story he was trying to tell, and appreciated it a
lot: Using as many words as necessary to clearly name
something. _Everything_.

I called this "German" naming convention as a reference to the fact
that the German language is known for its compound words, which can
become comically long and specific at times. Some examples include,
_Betäubungsmittelverschreibungsverordnung_ ("regulation requiring a
prescription for an anaesthetic"),
_Rechtsschutzversicherungsgesellschaften_ (""legal protection
insurance companies"), and the 1999 German "Word of the Year":
_Rindfleischetikettierungsüberwachungsaufgabenübertragungsgesetz_
("beef labelling regulation and delegation of supervision law").

Don't write `fopen` when you can write `openFile`. Write
`throwValidationError` and not `throwVE`. Call that name `function`
and not `fct`. That's German naming convention. Do this and your
readers will appreciate it.

## Isomorphic Naming

This convention complements German naming convention completely.

_Isomorphic naming_ is to say that the name of the variable is the
same form[^iso] of the name of the type. A simple
heuristic, in other words: **just use the name of the type.**

Here's a real sample where better naming convention would make this
easier to read without being a cryptographer:

``` haskell
updateColExp
  :: QualifiedTable -> RenameField -> ColExp -> IO ColExp
updateColExp qt rf (ColExp fld val) =
  ColExp updatedFld <$> updatedVal
  ...
```

Look at this naming convention. This may be appropriate if you're in
some kind of code golfing competition, but I can't even pronounce
these names. Applying the type-based naming heuristic, we get:

``` haskell
updateColumnExpression
  :: QualifiedTable -> RenameField -> ColumnExpression -> IO ColumnExpression
updateColumnExpression qualifiedTable renameField (ColumnExpression field value) =
  ColumnExpression updatedField <$> updatedValue
  ...
```

Look, it's readable, plain English! Isn't this a huge improvement? Any
maintainer reading this code can read each variable and know what it
is. I can even pronounce the names out loud.

Note that this convention only works well when your _types_ are well-named too,
by German naming convention.

[^iso]: From iso- "equal, identical" + -morphic, from Greek morphē
"form, shape," See the Etymology Online entry for
[isomorphic](https://www.etymonline.com/word/isomorphic).

[^typedness]: Especially for e.g. Haskell, C#, Rust, TypeScript or
Java. I give more legroom to unityped languages like Python, Perl,
JavaScript, etc. because your editor typically can't tell you the type
of a name, and there typically isn't a lexical indication of the type
anywhere nearby, like a type signature.

[^parametricity]:
[Parametricity](https://www.well-typed.com/blog/2015/05/parametricity/)
ensures that this really does not matter in a formal theory sense.
