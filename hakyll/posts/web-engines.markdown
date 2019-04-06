---
date: 2019-04-05
title: Web engines in Haskell
description: Web engines in Haskell
author: Chris Done
tags: haskell
---

## Vado

[Vado](https://github.com/chrisdone/vado) is a Haskell web browser I
wrote in two evenings that is the culmination of:

* Wanting to demonstrate some good Haskell libraries.
* That a cross-platform sdl2 package in Haskell is quite usable and
  works on Linux, OS X and Windows.
* That a simple web browser like something from the 90s was pretty
  straight-forward to implement.

In that I think the project is a success. The code isn't anything
special, and it's not beautiful, or efficient.

Quoting the page:

> This is a demonstration program that is able to load a web page up
> and render it like in the early 90's. It supports laying out text,
> different font sizes for headings, inline and block elements,
> hyperlinks, bold and italics. It supports mouse-wheel scrolling, too.

> I wrote this in a couple evenings, because it seemed
> straight-forward to do so given the libraries available
> today. That's a good sign for Haskell. Also, there's an inarticulate
> gut feeling I have that tells me maybe it's worth celebrating these
> days in which the web is still viewable in its simplest, earliest
> form.

## Fudgets

Meanwhile, I found that in 1994 there was [a Haskell web browser by the
Fudgets folk.](http://www.cse.chalmers.se/~hallgren/wwwbrowser.html)

Quoting from the page in case it goes offline one day:

> WWWBrowser is a prototype WWW Browser implemented in the functional
> programming language Haskell using the Fudget Library. WWWBrowser
> was mostly implemented in 1994, when NCSA Mosaic was the dominant
> web browser, and some updates have been made in 1997 and 1998.
>
> WWWBrowser is also described in the chapter WWWBrowser -- a WWW client in the Fudgets Thesis.
>
> * It supports forms.
> * It supports inlined images, like  [:-)]. The GIF, PNG, JPEG, PNM and XBM formats are recognized. PNM and GIF images are processed with Haskell code. For the other formats, conversions are done with external programs (from the PBMPLUS package). Dithering (or just color remapping) is done by the WWWBrowser itself. (Unfortunately this gets a bit slow for large images.) (See The Graphics File Format Page (2D specs) for info on these and other image file formats.)
> * Inlined images are fetched in parallel. This means that pages containing many small inlined images, such as IconBAZAAR, load faster in WWWBrowser (in spite of the slow image processing) than in browsers, like NCSA Mosaic (the dominating browser at the time when WWWBrowser was developed), that fetch one image at a time.
> * It understands most of the protocols used for information retrieval in WWW: http, ftp, nntp (news), gopher and telnet. It can also read files and directories in the local file system. When talking to nntp and ftp servers, it uses the same connection for several transfers rather than connecting/disconnecting for every document retrieved.
> * It can connect to the Internet directly or through a proxy.

Hard to believe there was a pretty viable looking browser back then,
and in Haskell! It's very impressive! The Fudgets work really is still
interesting to this day. I've started and will continue to read
through their documentation and ideas.

## Reflecting on browsers

You might think (or not think) that a cross-platform browser in 600
lines is pretty good. But this exercise also helps illuminate to me
the gulf that exists between what we had in the 90s, and what people
expect today. Vado doesn't do anything that you would expect from a
modern web browser. Here are just some things that I can think of:

* Support cookies
* Loading images
* Caching of assets
* History forward/back navigation
* Handle the plethora of custom form inputs (dates, integers, sliders, dropdowns, multiselects, basic text, textarea, buttons, etc.)
* Parse and handle even old CSS 2, nevermind modern CSS 3 (and custom fonts)
* Parse and handle plain old JavaScript, not to mention more modern JS
* Provide the large surface area of API for JavaScript, including but not limited to
   * The basic DOM API with all its events (keyboards, timers, mouse, form inputs)
    * Drawing APIs (canvas)
    * Network APIs (AJAX, websockets)
    * Cookies and web storage
    * Window APIs (navigation, refreshing, etc.), alerts, prompts
    * Audio, video elements, web workers, ...
* The whole UI around managing your history, addresses, bookmarks, cookies, etc. that they're expected to have.

Also to do all of these things efficiently, securely, and in a way
that matches (quirks and all) with the way other engines are expected
to render. And then if you want anyone to continue using it, you have
to keep on keeping up with the web, aside from the high maintenance
costs of a basic web browser.

## Approaching a mono culture

At present we have an unfortunate state of affairs, we used to have
these rendering engines:

* Blink (Chromium)
* KHTML (Konqueror)
* WebKit (Safari)
* Trident (IE)
* EdgeHTML (Edge)
* Presto (Opera)
* Gecko (Firefox)

Unfortunately, they've been collapsed down to:

* Blink (Chromium, Opera, Edge) -- controlled by Google
* KHTML (Konqueror) -- controlled by KDE
* WebKit (Safari) -- controlled by Apple
* Gecko (Firefox) -- controlled by Mozilla

I hope this trend doesn't continue. Google are already attempting to
destroy email as we know it, using their power afforded by
GMail.  Mozilla are pretty bent on keeping the web diverse and
competitive. I'm not sure whether Apple will follow Microsoft's and
Opera's lead and move to Blink for Safari.

We'll see.
