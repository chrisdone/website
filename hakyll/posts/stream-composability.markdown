---
date: 2015-06-11
title: Stream fusion and composability (Java 8 and Haskell) for newbies
description: Stream fusion and composability (Java 8 and Haskell) for newbies
author: Chris Done
tags: haskell, java
---

In an online discussion, when Java 8 released their stream API,
written about
[here](http://www.deadcoderising.com/java-8-no-more-loops/), you can
write e.g.


``` java
public List<Article> getAllJavaArticles() {
    return articles.stream()
        .filter(article -> article.getTags().contains("Java"))
        .collect(Collectors.toList());
}
```

Someone asked, "But my question: would the streams be faster than
loops? Or is the only benefit better readability?" Someone answered
that the benefit is that streams compose and loops don't. What does
composable mean here? Below is my answer, using two languages I know,
JavaScript and Haskell.

Composable in this context means: To be able to compose two things
into one without redundancy or overhead. For example, consider you
want to map a function `f` over an array `arr` to produce a new array,
you might do this:

``` javascript
var arr2 = [];
for (var i = 0; i < arr.length; i++)
    arr2.push(f(arr[i]));
```

If you want to filter the array based on a predicate `p`, you might do
this:

``` javascript
var arr3 = [];
for (var i = 0; i < arr2.length; i++)
    if (p(arr2[i]))
        arr3.push(arr2[i]);
```

Or maybe you want to take all elements until a a predicate `p2` is not
satisfied:

``` javascript
var arr4 = [];
for (var i = 0; i < arr3.length; i++)
    if (p2(arr3[i]))
        arr4.push(arr3[i]);
    else
        break;
```


Now, if you want to do that all in one process you have a few options:

* Put them all one after the other verbatim as I've written
  above. Redundant, a maintenance issue and inefficient.
* Merge them all into one clever loop. Also redundant (re-implementing
  the same concept of mapping, filtering and taking), error prone
  (it's easy to get manual loops wrong, especially merging several
  concepts together), and a maintenance burden.
* Put them each into a method on your language's Array type as
  `map()`, `filter()`, and `takeWhile()` and then write
  `arr.map(f).filter(p).takeWhile(p2)`. Good abstraction, very low
  maintenance because the functions are black boxes. But inefficient.

An ideal stream API will give you the last point, but be able to
understand concepts like mapping and filtering and know how to merge
them together into an efficient loop. This is called stream fusion,
which you can google if you want to know more.

I don't know Java but I can give a Haskell example:

``` haskell
map f . filter p . takeWhile p2
```

(Note: In Haskell the operations separated by `.` are run right to
left, like `map f (filter p (takeWhile p2 …))`.)

If I compile this with GHC, e.g.

``` haskell
main = print ((map f . filter p . takeWhile p2) [1..10])
  where p2 = (<5)
        p = even
        f = (+2)
```

and look at the reduced output called Core, a language the compiler
generates code for before generating assembly or byte code, the `map f
. filter p` are both compiled into a single loop (Core output is
verbose, so I collapsed it into this more readable form). This just
walks over the list, checks whether the item is even, if so, keeps it
and adds 2 to it, otherwise skips that item:

``` haskell
mainzugo xs =
  case xs of
    [] -> []
    (x:ys) ->
      case even x of
        False -> mainzugo ys
        True -> x + 2 : mainzugo ys
```

Which is pretty nifty. Furthermore, if you fold (also called reducing) e.g.

``` haskell
foldr (+) 0 . map f . filter p
```

Then that whole thing is also compiled into one loop:

``` haskell
mainzugo xs =
  case xs of
    [] -> 0
    (x:ys) ->
      case even x of
        False -> mainzugo ys
        True -> (x + 2) + mainzugo ys
```

There're limits to what can compose with what, though.
