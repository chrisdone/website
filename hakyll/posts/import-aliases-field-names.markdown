---
date: 2020-12-15
title: Use import aliases to easily access fields
description: Use import aliases to easily access fields
author: Chris Done
tags: haskell
---

This post demonstrates using imports as a quick and easy way to
disambiguate record field names. You can still put all your types in
one module.

No fancy type classes or machinery needed.

Define all your types in one module:

```haskell
{-# LANGUAGE DuplicateRecordFields  #-}
module Types where
import Prelude
data Person = Person { name :: String, age :: Int } deriving Show
data Company = Company { name :: String, age :: Int } deriving Show
```

Now in another module, use import aliases to get at specific types
(`Person` and `Company`):

``` haskell
{-# LANGUAGE DuplicateRecordFields  #-}
module Use where
import Prelude
import Types as Person (Person(..))
import Types as Company (Company(..))
import Types

main :: IO ()
main =
  let -- Freely use Person/Company constructors, and field names unqualified:
      mary = Person {name = "Mary", age = 23}
      james = Person {name = "James", age = 22}
      company = Company {name="Fish Limited",age=150}
   in do -- Pattern matching convenience:
         case mary of
           Person{name} -> print name
         case company of
           Company{name} -> print name
         -- Easily transfer data between record types because the field names are the same.
         let transferred = Company {..} where Person {..} = mary
         -- Accessing:
         print (Person.name mary)
         print (Person.name james)
         print (map Person.name [mary,james])
         print (Company.name company)
         print (Company.name transferred)
         -- Updating:
         print (mary { Person.name = "Maria"})
```

Daniel Díaz Carrete commented:

> While it doesn't free you from having to prepend the qualified name
> when using the selectors, it feels conceptually "better" to define
> it once at the import list, instead of doing it at each field
> declaration.

Which I agree with!
