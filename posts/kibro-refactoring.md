---
date: 2008-10-12
title: Kibro refactoring
description: Kibro refactoring
author: Chris Done
tags: haskell
---

I am refactoring Kibro, my web development "framework". The release on Hackage does bare bones things, and Jinjing Wang, what a legend, created this Panda software out of it. In doing so he has helped me notice what Kibro needs. I myself have found things that Kibro does not support in my projects that I will be addressing. I am writing this post so that anyone who cares will know that this project is being worked on, and because for those actually using Kibro it will help to know the reasoning behind the changes. The code needed refactoring anyway, it had !OMGPROTOTYPE! written all over it.

## Problems and solutions

### Lighttpd.conf configuration

Panda comes with a script to modify the lighttpd.conf which adds mime-types and things like that. It's a problem that you have to do this.

I have discovered that Lighttpd config files support an #include directive and it is therefore my solution that all settings that are not dependant on the path of the project[^1] be moved to a custom.conf in the lighttpd directory.

### Database not used

The Kibro library requires you to specify a database, but your web site might not always want a database (like Panda). I am removing the Sqlite interface from Kibro for now for this reason, and others[^2]. You ought to feel free to use the MonadIO instance of the Kibro monad to open connections to whichever database library you like.

### Adding persistent state

Furthmore addressing the problem of the Kibro library, perhaps adding custom state may be useful. For example, your main web site code might be:

``` haskell
import Kibro
import MyWebSiteStuff

main :: IO ()
main = do
  someResource <- getMyResource
  startKibro someResource pages

pages :: [PageAssign]
pages = ...
```

This lets us connect to a database or generate some resources that are too expensive to create for every page load. Typically it can be a pure value or an MVar or TVar or whatnot.

### Kibro command line program is rubbish

It is generally rubbish (e.g. doesn't even support a help function). I've changed this, and lots of other things. The code before was awful. It now uses a Command monad (of course). I guarentee it.

### Settings

There needs to be a way to configure Kibro for the project. Another thing I noticed was that Panda changed where to output the .fcgi file. This should be configurable. Thus, Kibro now reads settings from the .kibro file in the directory, the same as Cabal. Here is a real sample config[^3] file:

``` haskell
KibroProject { projName = "test"
             , projDirs = [("app","app")
                          ,("lighttpd","app/lighttpd")
                          ,("fastcgi","app/fastcgi")
                          ,("public","public")
                          ,("src","src")]
             , projOutDir = "public"
             , projMainIs = "src/Main.hs"
             }
```

The project name (projName) is important, that decides what .fcgi file name is outputted, what .sock file name is outputted, and etc.

projDirs tells Kibro where everything is. It is a list of (place,relative path) tuples. app refers to where the Kibro stuff like Lighttpd and FastCGI should go, among other things that may be added in future. lighttpd refers to where lighttpd.conf is, and where lighttpd.pid and custom.conf should be written. fastcgi is where fastcgi.pid should be written, and $projName.sock will be put. public is the public httpd directory (aka www-root or static), and $projName.fcgi will be put here so that Lighttpd can access it. (You can make it not directly accessible from the outside, read below). And finally, src is where your web site's source code is (i.e. where to cd to when performing the build).

projOutDir tells Kibro where to output $projName.fcgi. projMainIs tells Kibro where the main web site file is.

All these paths are accessed relative to the project directory.

### Integrating your FastCGI process with an existing web site

This is one old Kibro does not address at all. It's great to work in a confined directory on a special port like 3000, but at some point you need to deploy it on a real existing web server. Panda has this problem. It expects to be in the root directory. So that when I wanted to setup this blog as /blog, I had issues. All of the links on the blog software refer to a root directory (/), so I can't simply change the Kibro page regexes. However, Panda's issue is not solvable by Kibro, because it is just a problem of hard coding paths. Kibro's issue is integrating nicely, which is really the question of how to deal with the Lighttpd configuration.

My solution is that seeing as Kibro can output your .fcgi program anywhere, you can therefore put it in your web server's root directory (actually, I don't think you have to do this for Lighttpd to be able to access it[^4]). After reading about Lighttpd's configuration syntax a little more, I discovered a very nice way to get your FastCGI application onto an existing server under a directory.

The obvious way is to simply directly access foo.fcgi.

``` haskell
fastcgi.server = ("blog.fcgi" => (("socket" => "/var/www/chrisdone/blog/app/fastcgi/blog.sock"))
```

But then how can you have multiple pages on your FastCGI application? You need the 404-handler.

``` haskell
server.error-handler-404 = "/blog/public/blog.fcgi"
```

But if we set a 404-handler to a particular FastCGI application, what about the others? What if I want this blog, and another FastCGI application which can handle any path under some directory? Then I can use the $HTTP["url"] syntax:

``` haskell
fastcgi.server = ("blog.fcgi" => (("socket" => "/var/www/chrisdone/blog/app/fastcgi/blog.sock"))
                 ,"test.fcgi" => (("socket" => "/var/www/chrisdone/test.sock")))

server.error-handler-404 = "/blog/public/blog.fcgi"
$HTTP["url"] =~ "^/peanuts" {
   server.error-handler-404 = "/test.fcgi"
}
```

This means that any URL that starts with ^/peanuts will be handled by test.fcgi, as seen in the link just above! This will work with any regex. And, of course, if Panda wasn't so greedy, it could be nudged into a path like ^/blog.

For those curious my full lighttpd.conf entry for chrisdone.com is:

``` haskell
" => "/1") # Page 1 of blog
   server.error-handler-404 = "/blog/public/blog.fcgi"
   $HTTP["url"] =~ "^/peanuts" {
      server.error-handler-404 = "/test.fcgi"
   }
}
```

What about kibro start and kibro stop etc? Well, I am going to add a setting of whether or not to touch Lighttpd. This means that both of these commands will only deal with building, starting and stopping only the FastCGI process and you can work happily away on your application!

##New Kibro

It's pretty much the same interface. All the commands are the same, and the directory structure is the same for the most part.

``` haskell
chris@chrisdesktop:~$ kibro new fooble
Creating directory structure ...
  /home/chris/fooble/app
  /home/chris/fooble/app/lighttpd
  /home/chris/fooble/app/fastcgi
  /home/chris/fooble/public
  /home/chris/fooble/src
Finished creating directory structure.
Writing Main.hs ... done.
Writing lighttpd.conf ... done.
Writing fooble.kibro ... done.
```

Starting and stopping is exactly the same.

``` haskell
chris@chrisdesktop:~$ cd fooble/
chris@chrisdesktop:~/fooble$ kibro start
Reading config file fooble.kibro ... done.
Not yet built, building ...
[1 of 1] Compiling Main             ( /home/chris/fooble/src/Main.hs, /home/chris/fooble/src/Main.o )
Linking public/fooble.fcgi ...
Finished building.
Spawning FastCGI daemon ...
spawn-fcgi.c.197: child spawned successfully: PID: 26287
Done.
Spawning Lighttpd daemon ...
Lighttpd daemon started.
chris@chrisdesktop:~/fooble$ kibro stop
Reading config file fooble.kibro ... done.
Stopping Lighttpd ...
Stopping FastCGI ...
chris@chrisdesktop:~/fooble$
```

Examples

Once I have finished refactoring the library and the executable, I am going to make a couple examples.

* An interface to all the Lojban library goodies I've been working on. This will require a couple interesting kinds of state 1) Maintaining a database that takes a while to generate, 2) maintaining a pipe to a process.
* Maybe I can patch Panda to support a path other than /.

I'll blog about those later and provide all source code etc so that you can Try It At Home[^5] and comment on the blog about how it doesn't work on your Macs. :-P

[^1]: The paths that you provide in Lighttpd's configuration need to be absolute paths or else it fails. For this reason I have made Kibro be the maintainer of those values.

[^2]: I had some locking problems with the HDBC Sqlite3 library and generally I don't feel very happy with it. I'll probably try Postgres next.

[^3]: Yeah, I know. It's just the output of Show. Cheeky, huh? ;D ... I may write a different one later, if called for, but it's not a priority.

[^4]: document-root combined with $HTTP["url"]? I'll have to test this.

[^5]: No Windows support. (Does it matter? Who runs web servers on Windows anyway?)
