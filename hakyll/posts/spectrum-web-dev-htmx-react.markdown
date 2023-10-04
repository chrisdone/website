---
date: 2023-10-04
title: A spectrum of web dev abstraction
description: A spectrum of web dev abstraction
author: Chris Done
tags: web
---

There's a spectrum of web dev with two sides:

1. **Abstraction**: blur the lines between client and server, pretend you're writing an offline 
desktop app with no server, no latency, no RPC calls, etc. as much as possible. Poster child: **React**.

2. **Non-abstraction**: write code like it's the 90s. Admit there is a server, use GET/PUT/DELETE/POST 
end-points, that serve HTML, use `<form>`, etc. Poster child: **Htmx**.

Over 10 years,[^1] the more I have done (1), the more I've regretted it and wished I'd done (2).

[^1]: See also [this retrospective](https://chrisdone.com/posts/clientside-programming-haskell/).
