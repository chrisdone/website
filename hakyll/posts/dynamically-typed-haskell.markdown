---
date: 2019-04-06
title: Dynamically typed Haskell
description: Dynamically typed Haskell
author: Chris Done
tags: haskell
---


For a while I've been intending to make a package that provides a
dynamic type and a trivial means to read/write that type to/from
CSV/JSON and web requests. I thought that this would be enough to
provide a convenient package for doing exploratory work against
unknown data.

In Haskell, typically we would model all the types of the data that we
want to work with ahead of time. That's inconvenient if you want to
just quickly look through a JSON or CSV file and do some quick
calculation: that's where you would reach for Python. Even type
providers[^1] don't provide that level of convenience.

I thought, well, it's pretty easy to define a `Dynamic` type which is
an instance of a bunch of classes like `Num`, `FromCSV` and
`FromJSON`, etc. with which one could program like they do in a
dynamically typed language.[^2] But I
had other things to work on.

April 1st came around the corner, so I decided that would be a good
time to publish such a package then, that way any detractors could be
dismissed by "April fools!"
[Here is the package](https://github.com/chrisdone/dynamic) in all its
glory. I have actually already used it several times to play with some
public APIs. It's nice to be able to use regular Haskell once I want
to actually write some logic. I can see myself using this fairly
often.

[^1]: Such as
    [those in F#](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/):
    An F# type provider is a component that provides types, properties,
    and methods for use in your program.

[^2]: Oddly, no one seems to have done
    this. There are plenty of "dynamic type" packages in Haskell, but
    their authors couldn't resist making it well-typed in some way.
