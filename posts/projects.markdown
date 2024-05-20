---
date: 2024-05-12
title: Projects
description: A hopefully complete list of all the software projects I worked on
---

It might be a "getting old[-er]" thing, but I've recently started feeling like documenting the complete set of software projects I've worked on that I remember.
Not as a CV (an advertisement of paid work), but a comprehensive list of *everything*.

I'll start with the old stuff and continuously make updates a little bit per day.
After a few weeks I should be done.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [2014](#2014)
    - [ace](#ace)
    - [hl](#hl)
    - [formatting](#formatting)
    - [hindent](#hindent)
    - [foreign-store](#foreign-store)
    - [shell-conduit](#shell-conduit)
    - [lucid](#lucid)
- [2013](#2013)
    - [pure-io](#pure-io)
    - [god-mode](#god-mode)
    - [structured-haskell-mode](#structured-haskell-mode)
    - [haskellnews](#haskellnews)
    - [ircbrowse](#ircbrowse)
- [2012](#2012)
    - [fay](#fay)
- [2011](#2011)
    - [hulk](#hulk)
    - [ji](#ji)
- [2010](#2010)
    - [tryhaskell](#tryhaskell)
    - [jquery-console](#jquery-console)
    - [lisk](#lisk)
- [2008](#2008)
    - [kibro](#kibro)
    - [lojbot](#lojbot)
- [2006](#2006)
    - [slsk/soulski](#slsksoulski)
    - [wdn](#wdn)

<!-- markdown-toc end -->

<!-- # 2023 -->

<!-- ## hell -->

<!-- # 2020 -->

<!-- ## early -->

<!-- ## inflex -->

<!-- # 2019 -->

<!-- ## vado -->

<!-- # 2017 -->

<!-- ## xeno -->

<!-- # 2015 -->

<!-- ## path -->

# 2014

## ace

In the afterglow of growing bored of Lojban, but still enamoured by
formal human languages, I got into Attempto (Latin for 'I dare')
Controlled English. I ended up writing a parser package for it in
Haskell [here](https://github.com/chrisdone-archive/ace). It's
surprisingly comprehensive, I wrote [a blog post on
it](https://chrisdone.com/posts/attempto-controlled-english/). It even
permits passing a record of parsers, permitting the user to define
their own vocabulary and syntax for terms.

When I wrote it, use of such things was questionable. Combine it with
a logic database and *poof*, magic understanding! In reality, not
quite. Arguably, the use for such a thing in today's brave new world
of LLMs is even more questionable.

But I suppose there still lies a small place in my heart for a formal
language that is also readable by anyone who speaks English.

## hl

The Haskell web site has already been a bit crap. Today, it's
okay. It's been much worse. During that period of much-worseness, I
wrote [hl](https://github.com/chrisdone-archive/hl), an alternative
Haskell homepage, complete with
[reasoning](https://chrisdone.com/posts/haskell-lang/), which I
launched at haskell-lang.org as an alternative competing page, due to
making no progress at all for years in trying to get access to
haskell.org itself. Eventually, it did become the Haskell homepage due
to some diplomatic heroics. In that sense, I suppose the project was a
success. There was, as expected, lots of politics surrounding it, but
that's always the case with home pages of community projects.

## formatting

I thought that the
[HoleyMonoid](https://hackage.haskell.org/package/HoleyMonoid) package
was very neat, and decided it would make a perfect type-safe
printf-like package. I called it
[formatting](https://github.com/chrisdone-archive/formatting), after
Common Lisp's FORMAT.

Actually, it turned out that despite the neat trick, I really doing
like the position-dependent style of printf/FORMAT at all. I prefer
just writing `x <> " and " <> y` instead, or `concat [x, "and",
y]`. So the whole thing became an experiment in novelty, but
ultimately I lost interest and passed the maintainership onto someone
else.

## hindent

[hindent](https://github.com/chrisdone-archive/hindent) was my bold
attempt to make a complete pretty printer for Haskell, so that manual
code formatting would be a thing of the past. This was quite a
substantial project and took a lot of work. [The blog
post](https://chrisdone.com/posts/hindent/) explains the motivations,
but I just wanted to type less and get autoformatting. I later went
one step further and limited it to a single style (which is what gofmt
famously did), which I documented
[here](https://chrisdone.com/posts/hindent-5/). I even chose a style
that I didn't like, but that was popular. The decision to enforce just
one style made a lot of people mad.

Years later, there was a remake project called Ormolu[^4], which also
enforced one style. There was subsequently a fork called Fourmolu,
which permitted more style configuration.[^5]

## foreign-store

The
[foreign-store](https://github.com/chrisdone-archive/foreign-store)
package is a simple bit of C code wrapped in a Haskell package that
lets you keep a reference to a Haskell object between GHCi reloads,
which I wrote [for the purpose of hot
reloading.](https://chrisdone.com/posts/ghci-reload/) It's actually
still used to this day and maintained by someone else. There are 12
direct dependencies of it on Hackage, mostly various different ways of
doing code reloading.[^3] They're all fancy wrappers around this.

10 years after it had been published and not been touched for just as
long, someone found and fixed a segfaulting bug in the C code. Can you
believe it?

## shell-conduit

I've been wanted to write shell scripts since 2014. This attempt,
[shell-conduit](https://github.com/chrisdone-archive/shell-conduit),
was to get all available names in the `PATH`, available as real
Haskell identifiers to run. And then "scripts" would be regular
Haskell programs.

I've since changed my mind that this is a good idea, and see above for
the [Hell](#hell) project.

## lucid

[Lucid](https://github.com/chrisdone/lucid) is still actively
developed, and was motivated by have a uniformity of combinators, like
HTML handling in Common Lisp.

It's also a proper monad transformer, which has a small theoretical
satisfaction to it.

It's one of my projects that is using the [Immutable Publishing
Policy](https://chrisdone.com/posts/ipp/).

# 2013

## pure-io

A small project called
[pure-io](https://github.com/chrisdone-archive/pure-io) that provided
a little IO monad that would run as a pure function. I used this for
tryhaskell.org.

## god-mode

After suffering from RSI for a while, I had decided with Emacs I'd
look at my typing with statistics. After a week or so of full day
typing every day, I found that I made a significant proportion of key
chords in succession. I evaluated vim modes and realised that they
depart from Emacs quite a bit or lacked a story for many modes. In the
end I had the thought to make the Ctrl implicitly on all the time, and
this turned out to be undeniably productive and practical in the form
of [god-mode](https://github.com/chrisdone-archive/god-mode).

I'm still using it to this day, although it has been folded into my
Emacs config.

## structured-haskell-mode

In similar themes to god-mode, I made a structured editor for Emacs's
haskell-mode that would essentially let one edit Haskell code like
paredit. That used a Haskell parser via haskell-src-exts, and produced
a set of s-expressions for Emacs to convert to a set of markers. The
minor mode was called
[structured-haskell-mode](https://github.com/chrisdone-archive/structured-haskell-mode).

## haskellnews

[haskellnews](https://github.com/chrisdone-archive/haskellnews) was a
simple web app to list all the sources of blogs, posts, tweets,
etc. about Haskell. I sort of lost interest in social Mead and
therefore stopped running it. But for a while I enjoyed using it.

## ircbrowse

I wrote [ircbrowse](https://github.com/chrisdone-archive/ircbrowse) at
ircbrowse.net to be a replacement to the much older
ircbrowse.com.

ircbrowse.com was a great web site for browsing chat logs of various
IRC channels, it was also infamously slow as hell. So a fundamental
motivator was to make something that would be instant, and I succeeded
on that, much to my personal pride.

It could handle about 30 million lines of chat across a few channels
for about 10 years of logs. There were something in the order of 30k
lines of chat from the #haskell channel per month.

I stopped running it after I lost interest in IRC, but it was a very
satisfying project and I learned a lot about PostgreSQL's performance
characteristics in the process.

# 2012

## fay

I wrote a language called Fay, which was a Haskell without
type-classes that compiled to JavaScript, and used GHC's type system
for the type-checking.

It ended up being quite popular, it garnered its own
[organisation](https://github.com/faylang/), and I had a co-maintainer
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

## hulk

I wrote an IRC server for use at work for us to chat on. We used it
for some years after we all left, until we eventually migrated to
an app called Wire (also written in Haskell).

This was my first proper experience using threading in Haskell and
working on a sockets server in Haskell. I would note that architecting
an IRC server in a nice way remains an unsolved problem in my head. In
any language.

* [GitHub archive](https://github.com/chrisdone-archive/hulk)
* [Blog post](/posts/hulk-haskell-irc-server)

## ji

I wrote a library called [Ji](/posts/ji-haskell-web/), which is a
simple concept to write a web app in Haskell that sends commands to a
browser of what DOM changes to make and what events to listen to.

It was taken over by Heinrich Apfelmus and renamed to
[threepenny-gui](https://github.com/HeinrichApfelmus/threepenny-gui),
after which he added many FRP-based things and has likely rewritten
all of it by this point.

# 2010

## tryhaskell

This was a big splash when I made it, it hit the top of some big
subreddits and Hacker News. It was inspired by tryruby.org, which was
made by _whytheluckystiff, the mysterious Rubyist who disappeared
from the community. It runs you through an interactive tutorial about
Haskell basics and checks your answers.

I've been meaning to rewrite this one day, but so far haven't been
sufficiently inspired to do so.

It's been running for 14 years, which is quite a long time.

## jquery-console

Written for Try Haskell, this is
[was](https://github.com/chrisdone-archive/jquery-console) dumb
terminal/console-like experience in the browser using JQuery. It still
works correctly to this day, on tablets, phones and computers. I
stopped maintaining it as a separate package years and years ago, but
tryhaskell still works fine.

## lisk

I had enough hubris to think I would be able to write a Lisp syntax on
top of Haskell, and called it [Lisk](/posts/lisk-lisp-haskell).

I was happy with Haskell, but bummed out by its syntax compared to the
beauty of Lisp. That hasn't changed today in 2024.

* [GitHub archive](https://github.com/chrisdone-archive/lisk)

# 2008

## kibro

In 2008 I was working for a small sum on a hair salon's new web site,
in Haskell. And I made a very trivial web library called "Kibro" (from
the Lojban "cyberspace"), which sat upon CGI and could be ran from
PHP, which is how I was running it.

* Source code archive: [kibro-0.5.0.tar.gz](/archives/2008/kibro-0.5.0.tar.gz)
* Other links: [Kibro refactoring](https://chrisdone.com/posts/kibro-refactoring/)

## lojbot

Around this time I was an enthusiastic Lojban hobbyist, so I wrote an IRC bot with various capabilities.

* [GitHub archive](https://github.com/chrisdone-archive/lojbot)
* [Blog post](https://chrisdone.com/posts/lojbot/)

# 2006

## slsk/soulski

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

## wdn

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

[^3]: componentm-devel, espial, essence-of-live-coding, essence-of-live-coding-gloss, essence-of-live-coding-pulse, ghci-websockets, halive, jsaddle-warp, monomer, nvim-hs, rapid

[^4]:  It just had a small technological advantage of being based on GHC's parser, rather than haskell-src-exts, which is always playing catch-up.

[^5]: Which I consider a bit of a setback towards the dream of making style choices a thing of the past. But I think Ormolu will win eventually. Maintaining forks is hard work.
