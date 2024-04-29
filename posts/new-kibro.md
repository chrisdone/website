---
date: 2008-10-14
title: New Kibro
description: New Kibro
author: Chris Done
tags: haskell
---

The latest Kibro release is 0.3. The version is 0.3, but it's really still unstable/Alpha. If it works on your computer it's generally a miracle. Anyway.

## What's new

Quoting directly from the CHANGELOG in README:

* DONE: Add persistent state value to Kibro monad (with default as ())

* DONE: Add perl-like regexps

* DONE: Option for whether or not to launch Lighttpd

* DONE: Strip Sqlite3

* DONE: custom.conf Lighttpd include

I'm quite sure these satisfy all the solutions that were mentioned in my previous Kibro blog post.

## How to get it

Make sure you have cabal-install.

### Install from Hackage

```
$ cabal install kibro
```

### Install from git repository

```
cd kibro && cabal install
```

That's it.

## Example

I have started a bit of work on a Lojban interface. I made a little "screencast"-type video about using Kibro (but without a microphone, so you will have to read me typing little messages to you). Check out the example page.

At some point Firefox started sending the PC speaker beep constantly and so I had to end the video. Still, it was a bit of fun and maybe it could be helpful.

Get the source code:

```
$ git clone git://github.com/chrisdone/jboborei.git
```

# What's next

For me; sleep.

For Kibro; I need to document it a little more, e.g. how to use it with an existing server etc.

I was thinking that implementing hooks might be mildly cool. That is, if Kibro were to run specific shell scripts before and after events like restarting, rebuilding, etc.

I also would like to add cabal support at some point. That way, you can write a web site that works as a package that is cabal-installable. Good for sharing with friends and such.

If you have questions/comments/ideas, feel free to e-mail me at chrisdone@gmail.com or comment on this blog post.
