---
date: 2020-12-24
title: Is it Try.do that is dangerous?
description: Is it Try.do that is dangerous?
author: Chris Done
tags: haskell
---

I wrote the post
[Try.do for recoverable errors in Haskell](https://chrisdone.com/posts/try-do/)
and someone has written
[a nice response post](https://github.com/effectfully/sketches/blob/d45c3ec127e8789ab1af657f911c1a2a070d3780/try-do-is-dangerous/README.md)
proposing that this pattern is actually dangerous. While reading it I
wrote up some of my own notes to share in reply.

I'd like to preface with the fact that I'm grateful for discussion and
criticism of this approach. I'd like to use this approach, but if
someone finds genuine issues with it, then I'll naturally have to
look elsewhere.

## Loss of code re-use

The first point made, and I think this is a solid point, is that you
can't re-use handy monadic functions like `traverse` or `forever` with
this syntax. That's a downer.

However, I think it also follows naturally. If `ExceptT` were easily a
clean `MonadUnliftIO` instance, then we would have a clean
interpretation of how such functions would behave under all the
circumstances that unliftio is applicable in, most importantly
threading and when IO appears in negative position (`withX`). As it
happens, we don't.

This also applies to free monads, by the way. When you run the monad,
you still have to decide on an interpretation, figure out how things
will interact, and the idea of doing so does not bring me confidence.

Indeed, I may want to decide on a case-by-case basis whether
`traverse` should optimistically run _all_ actions
([even concurrently](https://hackage.haskell.org/package/unliftio-0.2.13.1/docs/UnliftIO-Async.html#v:pooledMapConcurrentlyN);
which I'm doing in my compiler), only at the end checking all results,
or whether it should fail hard on the _first_ failing action. Happily,
the `pooledMapConcurrentlyN` function doesn't have to know or care
about how I do error handling.

In conclusion, I don't see this as a complete obvious loss, and in
some way it's also a gain. I think there might be some interesting
ideas to explore in this area.

## Lifting hides results

The second point, in my reading, was that with this syntax, a
programmer will get into the habit of lifting all actions that aren't
returning `Either E A`. The downside being that if you use a function
like `timeout`, like this:

```haskell
result <- timeout (someAction :: IO (Either E A))
```

Then you'll hit a type error,

``` haskell
• Couldn't match type: Maybe (Either ErrorType Int)
                 with: Either e0 b0
• In a stmt of a qualified 'do' block:
    result <- timeout 1000 action1
```

So, being in a rush, they ignore the code and just add an `fmap pure`
call to fix the error:

``` haskell
result <- fmap pure (timeout 1000 action1)
```

Now we've lost the error, it didn't short-circuit where it should
have! Actually, **we haven't lose the error**. In fact, we _haven't
finished writing the code_ at this point. Assuming we have compiler
warnings on, we'll be **told that `result` is unused**.

``` haskell
Main.hs:10:7: warning: [-Wunused-matches]
    Defined but not used: ‘result’
   |
10 |       result <- fmap pure (timeout 1000 action1)
   |       ^^^^^^
```

Furthermore, when we decide to use it, we'll see that its type is

``` haskell
Maybe (Either ErrorType Int)
```

The error is still here, we have to *do something* with it. You can't
use it without having to deal with the error in some way. I think if
the compiler tells you something and you ignore it, that's your
responsibility. It can't save you from yourself.

This is the first instance of a running theme in the post that I'm
replying to, which is that code that doesn't use results is dangerous.

## The `try` function

The next case considered was `try`, which has this type:

``` haskell
try :: Exception e => IO a -> IO (Either e a)
```

And what would happen if you wrote:

``` haskell
result <- try (someAction :: IO (Either E A))
```

So that the type would be:

``` haskell
result <- try (someAction :: IO (Either E A)) :: IO (Either E (Either E A))
```

The author claims that you won't get a type error if you do something
with the result. Let's study it:

1. `try` will catch exceptions thrown by the GHC exception mechanism.
2. It will catch only those exceptions that are an instance of
   `Exception`.
3. It will be reported back to the user in the `Left` case of the
   either.

In this example:

* This assumes that one would make their failure result type `E` an
  instance of `Exception`. **I don't think there would be a good reason
  to do that.** But let's continue with this assumption.
* If, further, you actually use the `throw` call from `base` to throw
  your result type (**again, why would you?**), then `try` will catch
  it. So it wasn't lost.
* If the function simply returns the failure type, then you have it.
* Finally, my point above about the `result` variable stands here; it
  has to be used somewhere.

But I believe that this example is contrived. The whole point of the
`Try.do` system is to avoid using the `base` exception system for our
business logic. Why, then, would we implement an instance of
`Exception` and then willingly `throw` our failure type? Finally, the
code doesn't lose any errors. In fact, the double `Either` would be
enough to indicate to a programmer that something is off here.

## Names are not type safety

As an aside, an easy to avoid confusion here is to simply use a type
dedicated to this. Let's call it `Failure`:

``` haskell
data Failure err ok = Failed err | Ok ok
```

Now, it's not possible to be confused about `Either` as some random
data and `Failure` which only ever is produced by a failure. Many have
argued that `Either` itself is a form of
[boolean blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/). I
used `Either` in my post as an aid to demonstrate a point; there's no
compelling reason otherwise to use it.

The author, citing Alexis King, would have us believe that
[names are not type safety](https://lexi-lambda.github.io/blog/2020/11/01/names-are-not-type-safety/). **I
disagree; all types are type safety.** The attempt to put type
techniques into "safe" and "unsafe" categories only serves to
eliminate nuance.

Additionally, the author believes that syntax is not type safety. I
think syntax is the main driving force behind type safety, or so I
feel, having written the type generation stage of various compilers.

## Finally (pun intended)

The last point the author brings up seems to be more of a criticism of
any `IO` action returning `Either`, than the special syntax I proposed
in my post.

In base there are two functions which are notable in one crucial
aspect, **their discarding of values**:

``` haskell
finally :: MonadUnliftIO m => m a -> m b -> m a
bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
```

`finally` discards `b`, and `bracket` discards `b`.

The author writes that for,

``` haskell
someAction1 `finally` someAction2
```

would necessarily discard the result of `someAction2`. I think this is
a good observation. **Your result will be discarded, never to be seen
again.**

This does bring up a wider point, however: the type of bracket and
friends is a problem. Why don't they use `()`? These functions can
swallow results all the time, regardless of whether you're using
`Try.do` or writing normal Haskell. In Scala, Kotlin and Ceylon,
`bracket` uses `void` or `unit`.

I'm glad the author brought this up, because we don't currently have
any tooling to find and catch these mistakes. Essentially,
`bracket`/`finally` are risky functions to use due to this.[^1] This
is what prompted the title of my reply -- is it `Try.do` that's
dangerous, or is it `bracket`/`finally`?

Is it a criticism of `Try.do`? I'm doubtful at the moment.

## Concluding

I don't think there's any reason to call this technique "dangerous". I
think that the examples in the post were slightly contrived (such as
the dubious suggestion that you would implement `Exception` for your
result type, or not using the result of an action that GHC would warn
you about), and in the last point it seemed to be arguing against
`base` rather than my proposed technique, to which I can only agree.

I'm unsure about the tone in the penultimate paragraph[^2], but
overall I liked the criticism, it got me thinking. I do think there's
a wider discussion to be had about "use", and perhaps my slow
absorption of Rust thinking has me wishing there was less of a
complacent attitude to throwing away results in Haskell's base
libraries. It'll raise my alarms more readily when reviewing/auditing
code.

It'll still be a while before I'm even able to take advantage of
`QualifiedDo`, but I still look forward to pushing the boundaries of
this idea.

[^1]: Well, they're risky anyway. Consider the case of an exception
being raised in the handler or releaser. It's complicated.

[^2]: This language seems tonally ambiguous, "shallow unsafe trick
into a single weak spoiled framework", but I'll assume good faith.
