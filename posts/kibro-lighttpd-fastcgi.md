---
date: 2008-11-14
title: "Kibro: Haskell, Lighttpd and FastCGI"
description: "Kibro: Haskell, Lighttpd and FastCGI"
author: Chris Done
tags: haskell
---

This is a follow-up from the previous post about Haskell, lighttpd and fastcgi. Hence forth I will call this kind of project an fcgi site.

I have come up with a simple framework with consists of a library called “Kibro” and an executable called “kibro”. Both come in one package.

I intended for it to make getting an fcgi site up and running very easily. I found myself previously copying over configuration files and also copying library functions. Now I have started work on putting that into one package, Kibro. The point of Kibro is that a web site is a directory containing configuration, source and static web files. It is contained and isolated.

Here is an example of what works:

I create the project:

``` haskell
chris@chrisdesktop:~$ kibro new example
Creating project example ...
Creating directory structure ...
Creating example/app/lighttpd ...
Creating example/app/fastcgi ...
Creating example/app/memcached ...
Creating example/db ...
Creating example/public ...
Creating example/src ...
Writing example/app/lighttpd/lighttpd.conf ...
Writing example/src/Main.hs ...
Writing example/example.kibro ...
```

Done. I go into the directory and start it.

``` haskell
chris@chrisdesktop:~$ cd example/
chris@chrisdesktop:~/example$ kibro start
Not built. Building ...
```

The project hasn’t been built yet, so it builds it.

``` haskell
ghc --make src/Main.hs -o public/example.fcgi -threaded ...
[1 of 1] Compiling Main             ( src/Main.hs, src/Main.o )
Linking public/example.fcgi ...
```

Once built, Lighttpd is started under your username, with access to this directory, with document root “public” and configured to work with fastcgi.

``` haskell
Starting lighttpd ...
```

Fastcgi follows, executing it in the background, as your username, saving the processID to a file so that we can stop it later.

``` haskell
Starting fastcgi process ...
spawn-fcgi -f public/example.fcgi -s /home/chris/example/app/fastcgi/example.sock -P app/fastcgi/fastcgi.pid ...
spawn-fcgi.c.197: child spawned successfully: PID: 25688
```

That’s it, I’m afraid! Now go to your browser and go to http://127.0.0.1:3000. Port 3000 is the default port, and I am going to add a kibro configure command with which one can set a different port (or you can just edit lighttpd manually).

BTW, Main.hs looks like this at the start of a new project:

``` haskell
module Main where
import Kibro
import Kibro.DB.Sqlite3

main = kibro (db "") pages

pages = [(".", example)]

example = output "Change me! :-)"
```

This file is to be edited by you. The kibro function taking a function which returns an SQL Connection, and a list of pages, and it starts the fastcgi process. pages is an assoc list of regular expressions to match on URLs and page functions. example is a page function.

Something else I’ve got is kibro refresh, which rebuilds your project and restarts the fastcgi process. No need to restart lighttpd.

``` haskell
chris@chrisdesktop:~/example$ kibro refresh
Rebuilding ...
Building example ...
ghc --make src/Main.hs -o public/example.fcgi -threaded ...
[1 of 1] Compiling Main             ( src/Main.hs, src/Main.o )
Linking public/example.fcgi ...
Restarting fastcgi ...
Stopping fastcgi ...
Starting fastcgi process ...
spawn-fcgi -f public/example.fcgi -s /home/chris/example/app/fastcgi/example.sock -P app/fastcgi/fastcgi.pid ...
spawn-fcgi.c.197: child spawned successfully: PID: 25737
chris@chrisdesktop:~/example$
```

This will be very useful for updating your web pages’ code.

Let me show an example with a bit of session state.

``` haskell
module Main where

import Kibro
import Kibro.DB.Sqlite3

import Control.Monad
import Data.Maybe

main = kibro (db "") pages

pages = [("^/$",index),("/inc", inc),("/adder",adder)]

index = outputHtml <<$
  p << hotlink "/inc" << "incrementer"
  +++
  p << hotlink "/adder" << "adder"

inc = do
  n <- readSessDef "n" 0
  writeSess "n" (n+1)
  outputHtml $ p << (show n)

adder = do n <- readInputDef "a" 0
           v <- readSessDef "a" 0
           writeSess "a" $ v+n
           outputHtml $ p << (show (v+n)) +++ theform
    where
      theform = form ! [method "post"] <<$
                p << "Enter a number to add:" +++ input ! [name "a"]
```

That’s great. Now let’s refresh:

``` haskell
chris@chrisdesktop:~/example$ kibro refresh
Rebuilding ...
Building example ...
ghc --make src/Main.hs -o public/example.fcgi -threaded ...
[1 of 1] Compiling Main             ( src/Main.hs, src/Main.o )
Linking public/example.fcgi ...
Restarting fastcgi ...
Stopping fastcgi ...
Starting fastcgi process ...
spawn-fcgi -f public/example.fcgi -s /home/chris/example/app/fastcgi/example.sock -P app/fastcgi/fastcgi.pid ...
spawn-fcgi.c.197: child spawned successfully: PID: 30907
chris@chrisdesktop:~/example$
```

You can probably guess what this does.

I think this is a nice-ish way to write web pages. Another is using stringtemplate, which I will be incorporating in the future. Finally, there is XSLT; equally doable. I can’t really think of other ways in which to write web page content other than XSLT, templates and code-as-data higher order functions or macros. I’d be interested in hearing about other ways.

At the moment, session state is stored in MVars as simple Map values, but it won’t take much changes to restructure the session code to use memcached. Why memcached? Because storing the session state in a separate process means fastcgi can restart, but your session state isn’t lost. Granted, alternatively one could simply write to a database and then session state would be safe! Well, it would be pointless. A session state should time out after a while of no activity. There is no point writing it to a file. I’d wage memcached is faster, but I haven’t tested it.

So with this setup of external session state storage, it means updating web page code is easy, similar to the “dynamic”ness of interpreted web languages.

I would like to add a selenium interface at somepoint, e.g. for the kibro refresh command.

Anyway, I’m trailing off into details.

Needs way more work before I’ll be showing off source code. It’s on Hackage, so you can look at it, or the darcs repo, but I wouldn’t bother trying it yet. I’m just giving a progress report. It’s therapeutic.
