---
date: 2025-02-04
title: Less-Than Estimation
---

## Mechanics

A method of estimation I've used successfully on a bunch of projects goes 
like this. Estimations are one of four choices:

* `<hour`
* `<day`
* `<week`
* `<month`

I call it "less than estimation" because if a given piece of work is obviously under an hour, 
your estimate is `<hour`. If it's not obvious, you bump it up to `<day`. If it doesn't obviously
fit within a day, bump it up to a `<week`. If you're not sure it'll fit into a week,
my experience says that it could take even three weeks, so bump it up to `<month.` 

Finally,
if it doesn't obviously fit into a month, your powers of estimation have been exhausted: break 
the thing down into smaller tasks that can be estimated. This also applies to the fact that
some people may think that a task takes about two weeks, so they don't want to bump it up to 
a month. But if it takes 2 weeks, then you can split it into two tasks.

## Advantages

There are a few advantages to making estimates like this:

1. You don't have to be too precise with granular numbers over something that is inherently 
   imprecise. That makes decision-making quicker. No dithering!
2. People think in terms of time units. An hour, a day, a week, or a month fit neatly into people's heads
   without extra thinking involved.
3. Time units are also concrete: people can use them directly, unlike T-shirt sizing that is abstract 
   for people to think about.
4. Often managers/stakeholders don't care particularly whether it's two or three of something, they just
   want that when you estimate something that it's delivered on time. It's better to be predictable than
   fall short and lose trust.
5. Less experienced devs have trouble breaking down work into digestible units, this method encourages 
   that.
   
## Considerations   
   
One consideration is that this can either be considered non-additive (like T-shirt sizing), or can be additive, but needs
special software to add them up. E.g. 1 month = 4 weeks, 1 week = 5 days, 1 day = 8 hours, etc. Either 
works for me. I think on paper organizations with to work without adding up estimates, but in practice 
that's what everybody needs anyway.

A handy way to think about the priorities in this scheme is: unit up, split down. I.e. estimates always 
jump up to the next unit. But your estimates should never be reduced in unit (by e.g. being pressured by
stakeholders), and instead be split into separate estimates, if possible. If it's easy to split a 
week up into two days, great! If it's not, perhaps the week was the right choice after all.
