---
date: 2014-11-20
title: "Lucid 2.0: clearer than before"
description: "Lucid 2.0: clearer than before"
author: Chris Done
tags: haskell
---

Since [my last post](http://chrisdone.com/posts/lucid) about Lucid,
I've
[updated Lucid to major version 2.0](http://hackage.haskell.org/package/lucid)
in a way that removes the need for the `with` combinator. Now, you can
just write:

``` haskell
term_ <children>
term_ [<props>] <children>
```

Example:

``` haskell
page :: Html ()
page =
  html_
    (do head_
          (do title_ "Introduction page."
              link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
              style_ "body{background:red}")
        body_
          (do div_ [id_ "header",style_ "color:white"] "Syntax"
              p_ (span_ (strong_ "This is an example of Lucid syntax."))
              hr_ []
              ul_ (mapM_ (li_ . toHtml . show)
                         [1,2,3])
              table_ (tr_ (do td_ "Hello!"
                              td_ [class_ "alt"] "World!"
                              td_ "Sup?"))))
```

Here's the (pretty printed) output:

``` html
λ> page
<!DOCTYPE html>
<html>
<head>
    <title>Introduction page.</title>
    <link href="screen.css" rel="stylesheet" type="text/css">
    <style>body{background:red}</style>
</head>
<body>
    <div id="header" style="color:white">Syntax</div>
    <p><span><strong>This is an example of Lucid syntax.</strong></span></p>
    <hr>
    <ul>
        <li>1</li>
        <li>2</li>
        <li>3</li>
    </ul>
    <table>
        <tr>
            <td>Hello!</td>
            <td class="alt">World!</td>
            <td>Sup?</td>
        </tr>
    </table>
</body>
</html>
```

## Overloaded

Element terms are now typed like this:

``` haskell
p_ :: Term arg result => arg -> result
```

Giving a couple overloaded instances:

``` haskell
p_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
p_ :: Monad m => HtmlT m () -> HtmlT m ()
```

This is similar to the variadic `printf` from `Text.Printf`, but
limited to one level of variance.

# Retaining old invariants

In [my last post I listed](http://chrisdone.com/posts/lucid) a bunch
of factors that Lucid should solve, I worked hard to make sure these
were met in this change.

## Preserving liberal term use

You can still use `style_` or `title_` as an element or an attribute:

``` haskell
λ> style_ [style_ "inception"] "Go deeper." :: Html ()
<style style="inception">Go deeper.</style>
```

## Preserving encoding properties

The `script_` and `style_` elements still output unencoded:

``` haskell
λ> script_ "alert('Hello!' > 12)" :: Html ()
<script>alert('Hello!' > 12)</script>
```

## With is still available

You can still add attributes to elements using `with`:

``` haskell
λ> p_ [id_ "foo"] "" :: Html ()
<p id="foo"></p>
λ> with (p_ [id_ "foo"]) [class_ "red"] "yo" :: Html ()
<p id="foo" class="red">yo</p>
```

## Convenient construction of custom elements

You can construct custom elements if needed:

``` haskell
λ> with (term "potato" [id_ "foo"]) [class_ "red"] "yo" :: Html ()
<potato id="foo" class="red">yo</potato>
```

But you can also construct normal elements with a custom class, so
that you don't have to use `with` for extending elements like our old
`container_` example, you can construct an element with some given
attributes:

``` haskell
λ> let container_ = termWith "div" [class_ " container "]
```

And then use it later like a normal element:

``` haskell
λ> container_ [class_ "main"] "OK, go!" :: Html ()
<div class=" container main">OK, go!</div>
```

Some basic Bootstrap terms are available in
[Lucid.Bootstrap](http://hackage.haskell.org/package/lucid-2.1/docs/Lucid-Bootstrap.html).

## Still a monad transformer

I didn't change anything about the monad itself. Just the
combinators. So you can still use it as a transformer:

``` haskell
λ> runReader (renderTextT (html_ (body_ (do name <- lift ask
                                            p_ [class_ "name"] (toHtml name)))))
             ("Chris" :: String)
"<html><body><p class=\"name\">Chris</p></body></html>"
```

## Small trade-off

One small difference is that elements that take no children always
take arguments:

``` haskell
-- | @input@ element
input_ :: Monad m => [Attribute] -> HtmlT m ()
input_ = with (makeElementNoEnd "input")
```

So you will always write:

``` haskell
input_ [<something>]
```

But in practice it seems that elements with no children almost always
take a number of attributes. Exceptions to that rule are `br_` and
`hr_`, but those are quite rare. So this is a very happy trade-off, I
feel. (See the ‘real examples’ at the end of this post.)

Extending elements like this is straight-forward using our usual
`with` combinator. Example, suppose you're sick of writing the classic
`input type="text"`, you can define a combinator like this:

``` haskell
text_ :: Monad m => [Attribute] -> HtmlT m ()
text_ = with (with (makeElementNoEnd "input") [type_ "text"])
```

And now you can write:

``` haskell
λ> text_ []
<input type="text">
λ> text_ [class_ "foo"]
<input type="text" class="foo">
```

## Larger trade-off

Due to the overloadedness, similar to the overloaded strings example:

``` haskell
λ> "foo > bar" :: Html ()
foo &gt; bar
```

You have to use a type annotation in GHCi:

``` haskell
λ> p_ "foo" :: Html ()
<p>foo</p>
```

Otherwise you get

> No instance for `(Term arg0 a0)` arising from a use of `it`

Most Haskellers won't care about this case, but for GHCi users it's a
slight regression. Also, in some local where declarations, you might
need a type signature. For example, the following is OK:

``` haskell
people :: Html ()
people = ul_ (mapM_ person ["Mary Smith","Dave Jones"])
  where person name = li_ name
```

Whereas in this case:

``` haskell
bigTable :: [[Int]] -> Html ()
bigTable t = table_ (mapM_ row t)
  where row r = tr_ (mapM_ (td_ . toHtml . show) r)
```

It's a little harder for GHC to infer this, so you add a
type-signature:

``` haskell
bigTable :: [[Int]] -> Html ()
bigTable t = table_ (mapM_ row t)
  where row :: [Int] -> Html ()
        row r = tr_ (mapM_ (td_ . toHtml . show) r)
```

Not a big deal given the benefits, but something to be aware of.

## Summary

In total, I've made this library almost perfect for my own
tastes. It's concise, easy to read and edit (and auto-format), it
lacks namespace issues, it's easy to make re-usable terms, and it's
fast enough. The need for the `with` combinator was the only wart that
naggled me over the past week, I knew I'd end up making some
change. I've also covered the trade-offs that come with this design
decision.

As far as I'm concerned, Lucid can rest at major version `2.*` for a
long time now. I added some newfangled HTML5 elements (who knew `main`
was now an element?) and
[a test suite](https://github.com/chrisdone/lucid/blob/master/test/Main.hs). You
can expect the only minor version bumps henceforth to be bugfixes,
regression tests, and more documentation.

For some real examples:

* [Try Haskell](https://github.com/chrisdone/tryhaskell/blob/d8b59e71d46cb890935f5c0c6c1d723cc9f78d99/src/TryHaskell.hs#L326-L419) is now using Lucid.
* As is the upcoming [Haskell homepage](https://github.com/haskell-infra/hl/blob/master/src/HL/V/Template.hs#L37-171).
* And [isysuclosed.com](https://github.com/relrod/isysuclosed.com/blob/haskell/Main.hs#L34).
