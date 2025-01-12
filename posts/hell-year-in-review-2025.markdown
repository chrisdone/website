---
date: 2025-01-12
title: "Hell: Year in Review"
---

[Hell](https://chrisdone.github.io/hell/) is my little shell scripting
language which is a thin layer[^1] over Haskell, using its syntax, its
standard library and runtime, with a simpler static type system. You
can find [examples here](https://chrisdone.github.io/hell/examples/).

Over the year of 2024, I've written a few non-trivial shell scripts in
Hell at work, and now a few colleagues have used it, too. Now I can
use that to evaluate strengths and weaknesses of it as a tool. I can't
really share those shell scripts because it's not open source, but
I'll drop in some contrived example code at the start of each heading
as an amuse-bouche. The short version of work scripts is:

* Generating an HTML page summarising deployments by talking to Git
  and Terraform and pushing it somewhere to be served up as a web
  page. Like a small, read-only version of ArgoCD.
* Triggering notifications when deploys happen.
* Script to import many "things" via our own customer API in an idempotent way.
* Running end-to-end tests involving launching processes together and
  coordinating where they write and what happens when one fails, etc.
* Generally; talking to various APIs (Notion, Intercom, GitHub, etc.).

In Hell, I also implemented type inference, record types, sum types,
and added a whole bunch of library functions.

## Practical

```haskell
main = do

  -- Run two things concurrently and return both results
  (left, right) <-
    Async.concurrently
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/London")
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/Rome")
  Text.putStrLn left
  Text.putStrLn right

  -- Run two things concurrently and return the one that completes first
  result <-
    Async.race
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/London")
       (Main.curl "https://worldtimeapi.org/api/timezone/Europe/Rome")
  Either.either Text.putStrLn Text.putStrLn result

curl = \url -> do
  (out, err) <- Text.readProcess_ (Process.proc "curl" [url])
  IO.pure out
```

I've seen that it scales quite well e.g. to scripts that are
a few hundred lines, without feeling that the file is getting out of
hand. That's not a surprise and is the main reason I decided to make
this project in the first place.

Because the runtime performance is about as good as GHCi, it seems to
be fast enough for all purposes I've used so far. Really, scripting
languages need to be nimble enough not to show themselves as being
slower than the I/O and sub-process work they inevitably perform.

Concurrency in Haskell is very good. Some say "best in class," which
is debatable, but it's had green threads for decades and has a
maturity and predictability about it. Hell grabs that power tool from
the shelf and puts it under your belt. So, things like "race" or "map
concurrently" are things you often want in a shell script, because
many things are just I/O intensive but embarrassingly parallel
jobs. The other benefit is that you often want to run N sub-processes
concurrently for a long time, such as web services and attendant jobs.

Another absolute win, which I'm sure the PowerScript and Oil Shell
people will tell you, is that using *real* data structures in a script
is phenomenal. I've got a script at work (The Deploy Dashboard) that
uses `Map` and `Set` in non-trivial ways to time slices over a Git
commit history, for example. They're ample efficient, but mostly the
benefit comes from a sensible data structure that has a predictable
behaviour about it.

## Ergonomic

```haskell
main = do
  env <- Environment.getEnvironment

  -- Maybe monad works!
  Maybe.maybe (Text.putStrLn "Oops!") Text.putStrLn
     (do path <- List.lookup "PATH" env
         home <- Functor.fmap Text.reverse $ List.lookup "HOME" env
         Monad.return (Text.concat [path, " and ", home]))

  -- Either monad works!
  Either.either Text.putStrLn Text.putStrLn
    (do x <- Main.parse "foo"
        y <- Main.parse "foo"
        Monad.return (Text.concat [x,y]))

parse = \s ->
  if Eq.eq s "foo"
     then Either.Right "foooo :-)"
     else Either.Left "oh noes!"
```

As the syntax is familiar, it's pretty ergonomic. It's a very, very
small subset of Haskell, as can be seen in [the
examples.](https://chrisdone.github.io/hell/examples/) I think when I
write it, I'm mostly checking what's in [this small
page](https://chrisdone.github.io/hell/api/) of API functions. I think
the familiarity and small scope makes it predictable and that means
one can approach a task using it with confidence.

As a Haskeller, being easy to write is great. But the larger part is
being easy to read. It was easy at the start of this project in early
2024 to say I like reading the code, because I was the only user. Now,
after reading the code of a few colleagues, I still find it very easy
to read the code. That's big win for me.

When writing scripts for work, I use the `--check` flag to typecheck
it regularly. (Pairing with `watchexec`/`entr` is snappy.) The type
system saves me time, as often scripts will run something that's
sluggish like Terraform that takes ages to do anything, and then you
find out you made a silly mistake after all that. So it increases the
feedback loop.

## Low risk

```haskell
main = do
  Temp.withSystemTempFile "example" \filePath handle -> do
    Text.putStrLn $ Text.concat ["Created temp file ", filePath]
    let proc = Process.setStdout (Process.useHandleClose handle) $
         Process.proc "ls" ["-al"]
    Process.runProcess_ proc
    contents <- Text.readFile filePath
    Text.putStrLn contents
```

The nice thing about this project is that because it doesn't
deviate[^2] from Haskell, one could easily translate any script
literally to Haskell by adding some imports and then compile it to a
binary. So any scripts written in Hell has a low buss factor: you can
always just "lift" it into Haskell and be on a maintained
ecosystem. This is incidentally the same answer to the question: what
if my script isn't fast enough? Haskell compiles to competitive
machine code with a high throughput garbage collector.

The other part is that I haven't added any backwards-incompatible
changes, because I made it easy for myself not to.

The first way to do that is to just Learn to Stop Worrying and let
things stay in that I don't like. I've written about this
[elsewhere](https://chrisdone.com/posts/ipp/); not breaking things for
your users doesn't come naturally, because, ew, I hate this function
and regret it being born. In the long-run, though, it makes everything
easier to just accept that some small blemishes.

The other way is to defer 95% of design decisions to the host language
(Haskell) and its libraries. So if you see any API in Hell that you
don't like, well, it's not *my* design, sorry! It's standard Haskell!
None of the below is my design, it's all lifted from Haskell. Getting
into API design is tempting and a sign of hubris; it can drive some
to madness!

A final benefit of not diverging is that one can use any regular
Haskell source formatter. A source formatter is a huge undertaking for
any language with a big surface area like Haskell. So Hell gets that
for free. Use ormolu.

## Weaknesses

It has no editor support. Well, it has an Emacs module I wrote for
myself. But I don't plan on writing editor support for it. That's a
large undertaking; larger than writing the language itself, I
expect. So this might remain rubbish.

The error messages aren't great. They're okay, but they're a bit
barebones. The type errors might point out a location that isn't
obvious. This can be improved, discussion on GitHub here: [Reproduction of a bad type error message](https://github.com/chrisdone/hell/discussions/75)

## Future work

For the year 2025, I will probably work on these:

* Better error messages.
* Move from haskell-src-exts to GHC's own parser, to reduce any
  possible deviations from the norm, and to get recent syntactic
  goodies. This is a big job, but I've got the whole year.
* Move to an efficient unifier for the inference pipeline.
* Probably a steady but decreasing pace of adding library functions.

Otherwise I think my plans are more oriented around using it to write
automation, than adding any new features.

[^1]: AKA shallow embedding: The object language reuses as many features
    of the meta language as possible. Many aspects are delegated to
    the meta language.

[^2]: Not much. There are a few library functions that differ and the
    type-system is dumber.
