---
date: 2023-12-26
title: Hell: Shell scripting based on Haskell
description: Shell scripting based on Haskell
author: Chris Done
tags: haskell
---

Hell is a shell scripting 
language 
and implementation
based on Haskell.

**Links**

* Source code: [Hell.hs](https://github.com/chrisdone/hell/blob/main/Hell.hs)
* Examples: [examples/](https://github.com/chrisdone/hell/tree/main/examples)
* Downloads: [Releases](https://github.com/chrisdone/hell/releases)
* Documentation: [README](https://github.com/chrisdone/hell/blob/main/readme.md)

**Quick example**

This script demonstrates 
using `curl`
to download things 
concurrently.

```haskell
main = do

  -- Run two things concurrently and return both results
  (left, right) :: (Text, Text) <-
    Async.concurrently @Text @Text
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/London")
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/Rome")
  Text.putStrLn left
  Text.putStrLn right

  -- Run two things concurrently and return the one that completes first
  result :: Either Text Text <-
    Async.race @Text @Text
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/London")
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/Rome")
  Either.either @Text @Text @(IO ()) Text.putStrLn Text.putStrLn result

curl = \(url :: Text) -> do
  (out, err) :: (Text, Text) <- Text.readProcess_ (Process.proc "curl" [url])
  IO.pure @Text out
```
