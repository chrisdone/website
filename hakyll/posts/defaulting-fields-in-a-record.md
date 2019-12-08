---
date: 2019-12-07
title: Defaulting fields in a record
description: Defaulting fields in a record
author: Chris Done
tags: haskell
---

Do you have 20+ fields of configuration for your kitchen sink API? This approach might be for you.

An approach to specifying many (required) fields, where some are defaulted. What you get:

1. The ability to specify what the defaults are in a single place.
2. That set of defaults is decoupled from the function that uses the record (separating concerns).
2. The ability to choose different sets of defaults easily (e.g. "dev" mode vs "production" mode).
3. Type-safety; you can't specify defaults for required fields as an API provider.
4. Overriding: you can provide values instead of using the default trivially.
5. Light-weight syntax.
6. Failing to provide a required field as an API end-user results in a type error.

A field which has no sensible default value (such as one enabling a
new feature like `TLSSettings`) can still be `Maybe a`.

Your consumer of this API:

``` haskell
-- | Usage of API.

module Main where

import DBAPI

-- Omitting either username or password triggers a type error.
main = connect connSpecDefaults {username = "test", password = "mypw"}

-- To override defaults, just specify the field e.g. port:
main2 = connect connSpecDefaults {username = "test", password = "mypw", port = 1234}

-- Thanks Aleksey Khudyakov (@pineapple_zombi) for pointing out that plain record
-- update has the same typing rules as RecordWildCards.
--
-- Old version was: ConnSpec{username="..",password="..",..} where
-- ConnSpec{..} = connSpecDefaults
```

Definition of an API, in this case the example is a database:

``` haskell
{-# LANGUAGE DataKinds #-}

-- | My database API.

module DBAPI where

import Data.Defaults

data ConnSpec p = ConnSpec
  { username :: !(Required p String)
  , password :: !(Required p String)
  , port :: !Int -- Optional and therefore requires a default.
  , host :: !String -- Optional and therefore requires a default.
  }

connSpecDefaults :: ConnSpec Defaults
connSpecDefaults = ConnSpec {
  -- Required fields are ()
  username = (), password = (),
  -- Defaulted fields need defaults specified
  port = 5432, host = "localhost"
  }

-- Example func.
connect :: ConnSpec Complete -> IO ()
connect _ = pure ()
```

Basic defaults module to support this approach:

``` haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Providing defaults for fields in a record.

module Data.Defaults where

-- | Purpose of a data type.
data Purpose
  = Defaults -- For specifying defaults.
  | Complete -- For making a complete record.

-- | Required fields are not usable from a defaults spec.
type family Required (p :: Purpose) a where
  Required 'Defaults a  = () -- When we're defining defaults, required fields are ().
  Required 'Complete a = a
```
