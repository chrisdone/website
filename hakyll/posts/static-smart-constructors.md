---
date: 2019-08-22
title: "Static Smart Constructors with Double Splices"
description: "Static Smart Constructors with Double Splices"
author: Chris Done
tags: haskell
---

I plan on using this technique in at least one library, and so thought
it would be worth giving a name and a page dedicated to it, so it can
be linked to, instead of explained all over again.

---

Sometimes you're making a DSL and you need a way to make a smart
constructor that is static. You want to validate a value at
compile-time (possibly because the validation logic is harder to do in
the type-system or not ergonomic enough), and also return the run-time
value, without having to `lift` the whole structure from TH-land to
real-land. Perhaps your data type is a funky GADT with functions in
it, and therefore has no `Lift` instance anyway!

One pattern to achieve the desired result is using a double splice.

Here's a really trivial example with strings:

```haskell
> :t $$($$(notNull [|| thename ||]))
$$($$(notNull [|| thename ||])) :: NonEmptyString
> $$($$(notNull [|| thename ||]))
"Hello!"

> :t $$($$(notNull [|| thename_empty ||]))

<interactive>:1:4: error:
    • Exception when trying to run compile-time code:
        String is not empty.
```

Note that the `notNull` validator is well-typed (which is commonly
lacking in Template-Haskell, but here works well):

``` haskell
> :t notNull
notNull :: Q (TExp String) -> Q (TExp (Q (TExp NonEmptyString)))
```

The above uses typed splices `$$( ... )` and typed quotes
`[|| ... ||]`. However, you can implement the same technique with
regular `$(..)` and `[| .. |]` if typed splices are too long for you.[^1]

Here is the implementation:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift ()

-- Here is a simple validator.

valueFine :: String -> Bool
valueFine = not . null

-- The static smart constructor with the nested expressions returned.

notNull :: Q (TExp String) -> Q (TExp (Q (TExp NonEmptyString)))
notNull q = do
  TExp expr <- q
  let name = 'NonEmptyString
  [|| if valueFine $$(q)
      then TExp <$> (appE (conE name) (pure expr))
      else error "String is not empty." ||]

-- A smart constructor.

data NonEmptyString = NonEmptyString String
 deriving (Show)

-- Some example values.

thename :: [Char]
thename = "Hello!"

thename_empty :: [Char]
thename_empty = ""
```

Thanks to [Michael Sloan](https://mgsloan.com/) for the idea to have
an expression return another expression.

[^1]: In my case, the length doesn't matter too much, just the correctness
properties, because it'll be used once or twice per codebase to
validate a much larger DSL AST.
