---
date: 2008-12-11
title: Kibro on Shared Hosting
description: Kibro on Shared Hosting
author: Chris Done
tags: haskell
---

Right now I am doing work with PHP on a shared host with MySQL. Wasn't my choice, of course. Maybe I can use Haskell and MySQL for this application and future ones. I've already tested compiling a Haskell web application on this specific hosting server, and it works fine.

This host is pretty limited. I can only use provided configuration interfaces. I had to use PHP to launch the Haskell process. Provided I can keep a process open on the server (a cron job should do it), I can probably use PHP as a kind of proxy to the FastCGI process.

The idea of using Haskell for my work sounds much more pleasant and exciting than PHP. I really wish this PHP version (5.2.6) had closures and namespaces. Yeah, it has those features now1 2, in newer versions that I can't use, but only tacked on, when they should've been there from the start. Oh well.

Will post more about my Haskell-on-horrid-but-necessary-shared-hosting adventures.

### HSQL MySQL

On a fairly related note, MySQL with Database.HSQL.MySQL, is fairly easy. Here is a basic example, which I tried:

``` haskell
out <- liftIO $ handleSql (return . Left . show) $
                      do conn <- connect "host" "db" "username" "password"
                         statement <- query conn "SELECT * FROM blah"
                         rows <- collectRows getId statement
                         closeStatement statement
                         disconnect conn
                         return $ Right rows
output $ show out
    where getId :: Statement -> IO String
          getId = flip getFieldValue "somefield"
```

This will return either (1) all the fields ‘somefield’ from table ‘blah’ or (2) an error message.

Obviously one can stick this connection in a Reader monad. There's not really much else to check out for HSQL. I suppose that's the point. All the work should be done in the query, anyway. Check out the rest of the documentation for more information.