---
date: 2024-11-23
title: Adding sum types (variants) to Hell
---

[Hell](https://chrisdone.github.io/hell/) is my little shell scripting
language that's a thin layer over Haskell's own standard library and
runtime. This post is for people that like language implementation
details. It has a simply typed system, without polytypes (no
`forall`).

It has classic Haskell 98-ish records, this is a valid Hell program:

```haskell
data Person = Person { age :: Int, name :: Text }

main = do
  Text.putStrLn $ Record.get @"name" Main.person
  Text.putStrLn $ Record.get @"name" $ Record.set @"name" "Mary" Main.person
  Text.putStrLn $ Record.get @"name" $ Record.modify @"name" Text.reverse Main.person

person =
 Main.Person { name = "Chris", age = 23 }
```

It's built upon the following "classic" anonymous records definition
in Haskell:

```haskell
data List = NilL | ConsL Symbol Type List

data Record (xs :: List) where
  NilR  :: Record 'NilL
  ConsR :: forall k a xs.
    a -> Record xs -> Record (ConsL k a xs)
```

The record is a linked list type that is indexed by a type which
itself is also a linked list, and in both cases we keep both a key `k`
and a value `a`.

This exact representation is used in Hell. It's defined in the meta
language (Haskell) and used in the object language (Hell) directly and
is manipulated in a type-safe way; so accessor functions are all total
(they don't have an "otherwise throw an error" case). I cover this in
a bit more detail in [Tour of
Hell](https://chrisdone.com/posts/tour-of-hell/).

This week I added a very rudimentary implementation of sum types, so
the following is a valid Hell program:

```
data Value = Text Text | Number Int

main = do
  let printIt = \x ->
        Text.putStrLn case x of
          Number i -> Show.show i
          Text t -> t
  printIt $ Main.Number 123
  printIt $ Main.Text "abc"
  Monad.mapM_ printIt [Main.Number 123,Main.Text "abc"]
```

The runtime representation of a constructor is shaped like a
combination of `Either` and `Record`:

```haskell
data Variant (xs :: List) where
  LeftV :: forall k a xs. a -> Variant (ConsL k a xs)
  RightV :: forall k a xs k'' a''.
    Variant (ConsL k'' a'' xs) ->
    Variant (ConsL k a (ConsL k'' a'' xs))
```

The trick here is to note that the index of kind `List` is only ever
`ConsL ..` meaning it cannot be ever `NilL` (empty).

The challenging question in defining a case analysis on this type is
how to make it total, and not require an "or else throw" case. I went
with a simple accessor data structure that looks like a case:

```haskell
data Accessor (xs :: List) r where
  NilA  :: Accessor 'NilL r
  ConsA :: forall k a r xs. (a -> r) ->
    Accessor xs r ->
    Accessor (ConsL k a xs) r
```

It's basically the same as `Record` except that every field is `(a ->
r)` instead of just `a`, meaning we have a record of functions.

```haskell
runAccessor :: Variant xs -> Accessor xs r -> r
runAccessor (LeftV a) (ConsA f _) = f a
runAccessor (RightV xs) (ConsA _ ys) =
  runAccessor xs ys
```

This function is total and GHC is happy that it is exhaustive.

From here, Hell's runtime didn't need to be touched. The desugarer
simply replaces

```haskell
data Value = Number Int | Text Text
```
with something like:

```haskell
_Number = Variant.left @"Number" x)
_Text = Variant.right (Variant.left @"Text" x))
```

(I sort the constructors alphabetically for a canonical ordering.)

And replaces

```haskell
case .. x of Number -> Show.show i; Text t -> t
```

with

```haskell
Variant.run
  x
  $ Variant.cons @"Main.Number" (\i -> Show.show i) $
     Variant.cons @"Main.Text" (\t -> t) $
       Variant.nil
```

I thought this was pretty neat, because it's very simple and very
clean, and total! As a reminder this is still Hell's entire
interpreter:

```haskell
-- This is the entire evaluator. Type-safe and total.
eval :: env -> Term env t -> t
eval env (Var v) = lookp v env
eval env (Lam e) = \x -> eval (env, x) e
eval env (App e1 e2) = (eval env e1) (eval env e2)
eval _env (Lit a) = a

-- Type-safe, total lookup. The final @slot@ determines which slot of
-- a given tuple to pick out.
lookp :: Var env t -> env -> t
lookp (ZVar slot) (_, x) = slot x
lookp (SVar v) (env, _) = lookp v env
```

(To be entirely precise, Hell wraps both records and variants defined
with `data Foo` with a `Tagged "Foo"` to keep some semblance of
nominal typing. In case someone reads the code and thinks the tagged
part is important.)
