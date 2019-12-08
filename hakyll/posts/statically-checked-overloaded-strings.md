---
date: 2019-11-13
title: Statically Checked Overloaded Strings
description: Statically Checked Overloaded Strings
author: Chris Done
tags: haskell
---

This page demonstrates a trick I came up with which is defining
`IsString` for `Q (TExp a)`, where `a` is `lift`-able. It allows you
to write `$$("...")` and have the string parsed at compile-time.

This offers a light-weight way to enforce compile-time constraints. It's
basically `OverloadedStrings` with static checks.

This trick works already in old GHC versions.

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Network.URI
import Path
import PATHTH
import SHA256TH
import URITH

uri :: URI
uri = $$("https://releases.hashicorp.com/vault/1.2.2/vault_1.2.2_linux_amd64.zip")

sha256 :: SHA256
sha256 = $$("7725b35d9ca8be3668abe63481f0731ca4730509419b4eb29fa0b0baa4798458")

home :: Path Abs Dir
home = $$("/home/chris")
```

Scroll down to see the modules `PATHTH`, `SHA256TH` and `URITH`.

## Versus QuasiQuotes

This is more light-weight and overloaded than, e.g.

```haskell
[quasiquote|...|]
```

which requires stating the name of the quoter you want (sometimes you'd rather not), requires the
`QuasiQuotes` extension, and leaves syntax highlighters not sure how
to highlight your content properly.

## A step to make this syntax even lighter

It'd be nice to relax GHC's parser a little to support `$$"..."` to
mean the same thing. This wouldn't conflict with any existing syntax
that I am aware of, or of any existing plans or proposals.

## PATHTH

```haskell
{-# LANGUAGE FlexibleInstances #-}
module PATHTH where
import Data.String (IsString(..))
import Language.Haskell.TH.Syntax (Q, TExp(..), lift)
import Path
instance IsString (Q (TExp (Path Rel Dir))) where
  fromString = fmap TExp . mkRelDir
instance IsString (Q (TExp (Path Abs Dir))) where
  fromString = fmap TExp . mkAbsDir
instance IsString (Q (TExp (Path Rel File))) where
  fromString = fmap TExp . mkRelFile
instance IsString (Q (TExp (Path Abs File))) where
  fromString = fmap TExp . mkAbsFile
```

## SHA256TH

```haskell
{-# LANGUAGE DeriveLift, FlexibleInstances, TemplateHaskell #-}
module SHA256TH where
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as Hex
import           Data.String
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Syntax (Q, TExp(..), Lift(..))
newtype SHA256 = SHA256 ByteString deriving (Eq, Ord, Lift)
instance IsString (Q (TExp SHA256)) where
  fromString i =
    if length i == 64
      then case Hex.decode (fromString i) of
             (result, wrong)
               | S.null wrong -> fmap TExp (lift (SHA256 result))
             _ -> fail "Invalid SHA256 format."
      else fail "Incorrect length for SHA256."
```

## URITH

```haskell
{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TemplateHaskell #-}
module URITH where
import Data.String (IsString(..))
import Language.Haskell.TH.Syntax (Q, TExp(..), lift)
import Network.URI (URI(..), parseURI, URIAuth(..))
instance IsString (Q (TExp URI)) where
  fromString i =
    case parseURI i of
      Nothing -> fail ("Invalid URI: " ++ show i)
      Just uri -> liftURI uri
liftURI :: URI -> Q (TExp URI)
liftURI URI {uriScheme, uriAuthority, uriPath, uriQuery, uriFragment} =
  fmap TExp [|URI {uriScheme, uriAuthority = $(mauthority), uriPath, uriQuery, uriFragment}|]
  where
    mauthority = maybe [|Nothing|] liftAuthority uriAuthority
    liftAuthority URIAuth {uriUserInfo, uriRegName, uriPort} =
      [|Just (URIAuth {uriUserInfo, uriRegName, uriPort})|]
```
