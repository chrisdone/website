---
title: "Halogen is better than React at everything"
date: 2024-02-23
---

I wrote these notes in 2022, but found that they still hold up in 2024.

Having used TypeScript + React with its hooks in a code base that fully embraces them for more than a year, I return to my original assessment that PureScript + Halogen is better at almost everything.

PureScript has a sensible, type class based notion of equality, and JavaScript/TypeScript does not, and this comes up regularly, causing both logic bugs and cache invalidation issues.

Halogen components have a clear explicit reason that  a re-render occurs: calling the modify function in the command evaluator. Inputs (“props”) generate a command, which means one can trivially implement custom (i.e. efficient) re-rendering conditions.

Halogen components also have a trivial, built-in way for information messages to travel both upwards and downwards, outside of the input mechanism (“props”), which is exceedingly useful. It’s all in the type signature, too.

Halogen has elegant ways to interact with foreign (non-frameworked) libraries. The built-in event streams lets you react to out of band changes (webhook, CodeMirror edits,…). The explicit life cycle and evaluation-vs-execution distinction makes the interplay predictable.

In contrast, in React everything is hard and experienced users are regularly baffled. Re-rendering, caching, execution of effects, lifecycle, message bubbling and broadcasting, state, are all marvellously unhelpful by comparison.

TypeScript’s type checker regularly falls over, its inference is brittle in ways that interrupt work, type errors unhelpful, union types only make things harder. It doesn’t even have sum types. PureScript’s got all these basic things and were pretty well done from the start.

All in all, my conclusion is that TypeScript + React is a bad investment. It will cost you in dev time, bugs, maintenance, performance. Generally, I think the SPA trend is broadly misapplied, I prefer something like Htmx these days, but if I’m playing the SPA game, there are far better tools out there.

If you know Haskell then PureScript is better than TypeScript at everything, and isn’t a dead-end like Elm.
