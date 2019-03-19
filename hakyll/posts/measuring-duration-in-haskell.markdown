---
date: 2015-01-05
title: Measuring duration in Haskell
description: Measuring duration in Haskell
author: Chris Done
tags: haskell, time
---

Happy new year, everyone. It's a new year and time for new
resolutions. Let's talk about time. Specifically, measuring it in
Haskell.

## A wrong solution

How do you measure how long something takes in Haskell? Here's a naive
attempt:

``` haskell
import Control.Exception
import Data.Time

main = do
    start <- getCurrentTime
    evaluate (sum [1 .. 1000000])
    end <- getCurrentTime
    print (diffUTCTime end start)
```

Running it, we see that it does what we expect:

``` haskell
λ> main
0.316653s
```

## Inaccurate measuring

Here's what's wrong with this implementation:

* The clock can be changed by the user at any moment.
* Time synchronization services regularly update time.

If you're on an Ubuntu desktop, time is updated when you first boot up
from NTP servers. If you're on a server, likely there is a daily cron
job to update your time, because you don't tend to reboot servers. My
laptop has been on for 34 days:

    $ uptime
    21:13:47 up 34 days,  2:06,  3 users,  load average: 0.74, 0.83, 0.84

If I run a manual update, it adjusts my clock by 500 milliseconds:

    $ sudo ntpdate ntp.ubuntu.com
    5 Jan 21:11:53 ntpdate[4805]: adjust time server x.x.x.x offset 0.517166 sec

Because there is a certain amount of "drift" that occurs over time.

Additionally, leap seconds can be introduced at any time and cannot be
predicated systematically, but there is at least a 6 months in advance
notice for time servers. In
[2015](http://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat) there will
be an extra second added to time in-between the 30th of June to the 1st
of July.

These factors mean that if our main function is run during an update,
the reported time could be completely wrong. For something simple like
the above, maybe it doesn't matter. For long term logging and
statistics gathering, this would represent an anomaly. For a one-off,
maybe it's forgivable, because it's convenient. But above all, it's
simply inaccurate reporting.

## Accurate measuring

Readers familiar with this problem will think back to measuring time
in C; it requires inspecting the system clock and dividing by clocks
per second. In fact there are a couple solutions around that use this:

* The
  [timeit](http://hackage.haskell.org/package/timeit-1.0.0.0/docs/System-TimeIt.html)
  package. This is good if your use-case is simple.
* In turn, that package uses
  [System.CPUTime](http://hackage.haskell.org/package/base-4.7.0.2/docs/System-CPUTime.html)
  from base, which is also handy.

These are more reliable, because the time cannot be changed. But they
are limited, as both only measure CPU time and not IO time. So if your
program takes 10 seconds but only does 5 seconds of CPU processing and
5 seconds of waiting for the disk, then you will not have the real
time. Also known as
[wall time](http://en.wikipedia.org/wiki/Wall-clock_time).

In the Criterion package, there's need for fine-grained, fast,
accurate measuring of both real and CPU time, so it includes its own
cross-platform implementations:

* [Here it does a measurement of times](https://github.com/bos/criterion/blob/master/Criterion/Measurement.hs#L54-60)
  using its internal API.
* There is an
  [OS X](https://github.com/bos/criterion/blob/master/cbits/time-osx.c),
  [POSIX](https://github.com/bos/criterion/blob/master/cbits/time-posix.c)
  and
  [Windows](https://github.com/bos/criterion/blob/master/cbits/time-windows.c)
  C binding for each platform.

That's nice, but it's embedded in a specific package built for
benchmarking, which we may not necessarily be doing. For example, I am
dabbling with a program to measure the speed of my key presses. It
turns out there is a package that does similarly to Criterion, already
prepared and similarly cross-platform and only depends on base and
ghc-prim.

## The clock package

I discovered this really nice package called
[clock](http://hackage.haskell.org/package/clock-0.4.1.3) which has
[the option for several time measurements](http://hackage.haskell.org/package/clock-0.4.1.3/docs/System-Clock.html):

* `Monotonic`: a monotonic but not-absolute time which never changes
  after start-up.
* `Realtime`: an absolute Epoch-based time (which is the system clock
  and can change).
* `ProcessCPUTime`: CPU time taken by the process.
* `ThreadCPUTime`: CPU time taken by the thread.

Let's rewrite our example using this package and the formatting
package (which provides a handy `TimeSpec` formatter as of 6.1):

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

main =
  do start <- getTime Monotonic
     evaluate (sum [1 .. 1000000])
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end
```

Running it, we see we get similar information as above, but now it's
accurate.

``` haskell
λ> main
276.05 ms
```

If you just want CPU time for the process, or the OS thread, just
provide a different argument to `getTime`.

## Summary

So next time you want to measure how long something takes, unless
you're doing benchmarking, check out the `clock` package!
