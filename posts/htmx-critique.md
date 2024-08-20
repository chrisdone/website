---
date: 2024-08-20
title: A modest critique of Htmx
description: A modest critique of Htmx
---

At work, we really like the basic simple idea of Htmx. Based on using
Htmx in a non-trivial user interface, across a team, we've found that
the following cases are actually not simple and are quite complicated.

## Inheritance of Htmx properties is a definite mistake

Across pieces of code, it's very surprising and it's
    implicit. Like in CSS, inheritance is a cheap hack but you get
    what you pay for.

It contradicts [the author's reasonable argument of locality of
    behaviour.](https://htmx.org/essays/locality-of-behaviour/) It's
    not local, it comes from all the way up there or some other
    module. Pretty much dynamic binding.

Default inheritance differs across various properties
    (e.g. `hx-delete` is not inherited, but `hx-confirm` and `hx-ext`
    are). So you have to remember these exceptions and you end up just
    being explicit about everything, which means inheritance is
    pointless.

## Most interesting web apps cannot replace wholesale a DOM element


Because DOM elements almost always have browser-local state, such
    as the open/closed state of a `<details>` element, the input of an
    `<input>` element, the open/close state of a dropdown element
    (which, note, is not encoded by an attribute of the element when
    you click it). All of this state is lost if you replace outerHTML
    directly with the naive happy path of Htmx.

Even [morphdom](https://v1.htmx.org/extensions/morphdom-swap/)
    overwrites some things you'd expect it not to, so we had to patch
    it to avoid messing with input elements, and details elements.

## Storing state in the DOM element itself is a bad idea

 [Morphdom](https://v1.htmx.org/extensions/morphdom-swap/) is
    intended to correct the pains of the previous heading, but we
    discovered that the way that Htmx works *assumes* it's based on
    replacing elements wholesale: it stores the request queue for the
    element on the DOM element itself. When you kick off a request,
    either from this element or from another that points to it, you
    have a request queue. Some bad failure modes are avoided by
    wholesale-replacing the DOM element, as the queue is reset. But
    with morphdom, the queue is retained because the element is
    retained. You're now in a sort of [undefined behavior
    land](https://en.wikipedia.org/wiki/Undefined_behavior), where the
    designs of Htmx are violated.

## The default queuing mode is bonkers

By default, Htmx will cancel requests that are in-flight if you
    trigger another request on the same queue (element). That's the
    default strategy. We discovered this afterwards. It's highly
    unintuitive, it meant we were losing work.

## Event triggers are non-local

Event triggers often help to make things happen, but they're a
    non-local effect, and suffer from similar issues as property
    inheritance. A bit of DSL work in the server-side language can
    help with this, but it feels like old-school JavaScript
    callback-based programming to some extent; where you "subscribe"
    to an event happening and do something.

## Component state cannot be maintained very well

A broader problem, similar to the DOM element state issue, is that
    your own components have their own state. E.g. if you want a page
    that consists of three sections that have their own state that the
    server needs (e.g. which page of a set of results) and state that
    some e.g. React or WebComponents need, then you have a problem of
    synchronising state between a parent component and the child
    component.

Htmx does not provide a good story for this. We have some ideas,
    but they all have big caveats: use query parameters, use hidden
    form inputs, use event triggers.

React and Halogen (see also [Halogen is better than React at
    everything](https://chrisdone.com/posts/halogen-is-better-than-react/))
    *do* have an answer to this. In both cases, child components have
    their own state, and parents can give them "props" which are
    pretty much "advice", and they also have their own internal state,
    and can choose to ignore/take precedence over props. The props are
    typically sourced from the server or derived from the server, and
    the state is usually some client-side state.

We often do need to use React for off-the-shelf components or
  components that we have to use that are just provided as
  React. React and Htmx do not interact nicely.

  * We've done some unsatisfying work with WebComponents, but those
    things have bizarre limitations that are surprising.
  * We've also made a bridge directly to React components that we use
    from our server-side language, but in general Htmx and React fight
    for control over the flow of state and management of DOM elements.
  * We've played with Alpine, which is nice, but it represents _yet
    another_ client-side-programming library and is therefore
    redundant if React is already in your codebase.

## The up sides

Our current thinking is that being able to use your server side
language is **a huge obvious and uncontroversial win**, no one on the
team would want to go back to writing all this business logic in
TypeScript:

* No serialisation from our DB types to frontend types is needed.
  * No data leaks, and no GraphQL needed.
* We can use our (in our opinion) more powerful abstraction
  facilities of the server-side language.
* We can use the form builder in our server side language; instead of
  doing one frontend *and also* one backend implementation of the same
  validations.

But the above downsides are real.

## Htmx-in-React?

An attractive future direction might be to re-implement Htmx in
React:

* The server sends a JSON blob that React converts into
  virtual DOM components.
* That would solve the component state problem.
* It would mean we require no special bridge to use React components.
* It would let us use our React-connected web fetching library, and
  carefully avoid the queuing choices made by Htmx.
* It would solve the
  morphdom problems and browser DOM input elements problem, too, which
  is pretty much a solved problem in React.

In this way, we could drop the Htmx dependency but retain the benefits
of the idea. That is, given a budget to embark on such a big piece of
work.
