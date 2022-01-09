---
date: 2022-01-09
title: Thoughts on type class implementation
description: Thoughts on type class implementation
author: Chris Done
tags: haskell, plt
---

While designing and implementing Inflex, I discovered that there’s a lot of space in design decisions around type classes. Haskell’s implementation of them has a number of pre-made decisions built into Haskell 98. Many extensions (multiple type params, functional dependencies, associated types) have been added to support type level programming since then. Inflex’s needs are more modest.

Oleg has shown that in practice you only really need one type class, with clever use of instances. So you could imagine that a language with just a single type class and open instances would be called ClosedClassOpenInstances.

You might not permit new classes and instances, only built in ones. So an alternative language might be called ClosedClassesClosedInstances. A language that would benefit from this would be Elm.

I decided not to support multiple methods for a single class. This makes the feature more like “generic functions.” For any multi method class in Haskell, I can often find a type that would be an instance if it weren’t for one method.

Another consideration is whether to permit superclasses. Haskellers are very tempted to make class hierarchies. People like Don Syme would argue that this urge leads nowhere good. Haskell’s own Prelude has a very questionable hierarchy. I decided against superclasses too.

Elsewhere there are other surprises. A really simple compiler like Inflex goes through stages like this:

* Lex, parse, rename
* Generate constraints (elaboration)
* Solve constraints
* Generalize (insert poly types)
* Resolve (resolve class method calls to instances)
* Defaulter

This is reasonable enough. Very easy to understand each stage.

But there’s a non-obvious limitation in this approach which is that if you want to support instance contexts, then, after resolving an instance, you need to add the constraints from the context, and then go back to solving constraints again. Inflex doesn’t support instance contexts, but if it did, it would need a different pipeline. 

There’s a bunch of papers about type classes, especially getting inference to work well for them. Other things like instance chains as implemented in PureScript are also a departure from Haskell. 

I’ll write more another time with more code examples.
