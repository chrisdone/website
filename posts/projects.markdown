---
date: 2024-05-12
title: Projects
description: A hopefully complete list of all the software projects I worked on
---

# Introduction

It might be a "getting old[-er]" thing, but I've recently started feeling like documenting the complete set of software projects I've worked on that I remember.
Not as a CV (an advertisement of paid work), but a comprehensive list of *everything*.

I'll start with the old stuff and continuously make updates a little bit per day.
After a few weeks I should be done.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Introduction](#introduction)
- [2012](#2012)
    - [Fay](#fay)
- [2011](#2011)
    - [Hulk](#hulk)
    - [Ji](#ji)
- [2010](#2010)
    - [Lisk](#lisk)
- [2008](#2008)
    - [Kibro](#kibro)
    - [lojbot](#lojbot)
- [2006](#2006)
    - [SLSK/Soulski](#slsksoulski)
    - [WDN](#wdn)

<!-- markdown-toc end -->

# 2012

## Fay

I wrote a language called Fay, which was a Haskell without
type-classes that compiled to JavaScript, and used GHC's type system
for the type-checking.

It ended up being quite popular, it garnered its own
[organization](https://github.com/faylang/), and I had a comaintainer
called Adam Bergmark, who was very nice and smart.[^1]

I was invited to and went to a conference in Lisbon to give a talk on
it.

FP Complete used it for their IDE which was a web based Haskell IDE,
which is pretty much how I got the job at FP Complete in 2013. A
couple years or so later, after the IDE project petered out, we
stopped using it. There were better options by then (PureScript,
etc.).[^2]

* [Blog post](/posts/fay)

# 2011

## Hulk

I wrote an IRC server for use at work for us to chat on. We used it
for some years after we all left, until we eventually migrated to
an app called Wire (also written in Haskell).

This was my first proper experience using threading in Haskell and
working on a sockets server in Haskell. I would note that architecting
an IRC server in a nice way remains an unsolved problem in my head. In
any language.

* [Github archive](https://github.com/chrisdone-archive/hulk)
* [Blog post](/posts/hulk-haskell-irc-server)

## Ji

I wrote a library called [Ji](/posts/ji-haskell-web/), which is a
simple concept to write a web app in Haskell that sends commands to a
browser of what DOM changes to make and what events to listen to.

It was taken over by Heinrich Apfelmus and renamed to
[threepenny-gui](https://github.com/HeinrichApfelmus/threepenny-gui),
after which he added many FRP-based things and has likely rewritten
all of it by this point.

# 2010

## Lisk

I had enough hubris to think I would be able to write a Lisp syntax on
top of Haskell, and called it [Lisk](/posts/lisk-lisp-haskell).

I was happy with Haskell, but bummed out by its syntax compared to the
beauty of Lisp. That hasn't changed today in 2024.

* [Github archive](https://github.com/chrisdone-archive/lisk)

# 2008

## Kibro

In 2008 I was working for a small sum on a hair salon's new web site,
in Haskell. And I made a very trivial web library called "Kibro" (from
the Lojban "cyberspace"), which sat upon CGI and could be ran from
PHP, which is how I was running it.

* Source code archive: [kibro-0.5.0.tar.gz](/archives/2008/kibro-0.5.0.tar.gz)
* Other links: [Kibro refactoring](https://chrisdone.com/posts/kibro-refactoring/)

## lojbot

Around this time I was an enthusiastic Lojban hobbyist, so I wrote an IRC bot with various capabilitis.

* [Github archive](https://github.com/chrisdone-archive/lojbot)
* [Blog post](https://chrisdone.com/posts/lojbot/)

# 2006

## SLSK/Soulski

A project for the Soulseek music sharing network.
Web site, which may not remain long-term: [slsk.sourceforge.net](https://slsk.sourceforge.net/).
Amusingly, it still looks great. [Here's a screenshot.](/images/blog/slsk-screenshot-2007.png)
[Another](/images/blog/slsk.jpeg).
I started writing a C library for Soulseek: [soulseek-library-0.01.zip](/archives/2006/soulski/soulseek-library-0.01.zip)
I started writing a GUI library in C for it: [exgui-library-0.1.zip](/archives/2006/soulski/exgui-library-0.1.zip)
I started writing a CSS parser in C for it: [css-library-prototype-0.01.zip](/archives/2006/soulski/css-library-prototype-0.01.zip)

I originally had big ambitions for this, but later lost interest. It
looks like I planned to rewrite it in Common Lisp, but got bored of
the project before I did.

## WDN

I had a project called 'windows desktop notes' (WDN), that I
originally [wrote in C](/archives/2006/wdm/slsk-code-r110-wdm.zip). It
would overlay some plain text notes on your desktop and you could
click it and get an edit box and then hit Enter to save again.

Then I ported it, or tried to, [to Lisp](/archives/2006/wdn/wdn.lisp).

[^1]: In retrospect I mishandled a situation where Fay was being used
    in production and we were breaking its APIs too often, and I
    blocked Adam's `master` push access to the repo to give me some
    breathing room to slow things down, but this was obviously the
    wrong move, and not how you should treat early collaborators. That
    was my first experience managing a project, so I cut my younger
    self some slack. However, he was very gracious about it in the
    moment.

[^2]: See a complete history [here.](https://chrisdone.com/posts/clientside-programming-haskell/)
