---
date: 2026-05-28
title: System Colic
---

Colic is a thing that some newborn babies go through. Your newborn
baby cries for hours on end, every day, for weeks. And they're by all
measures healthy. Experts don't know what causes it and they don't
have any remedies for it. As a parent, you and your partner pretty
much lose your marbles and start questioning all your life choices
that lead up to this moment. You will try all the remedies; massages,
positions, "anti-colic drops", whatever you can. The [NHS page on
colic](https://www.nhs.uk/conditions/colic/) suggests you ask for
support from friends, family and their helpline.

That's an extreme case of the broader experience of having a baby,
which is often just trying to figure out what's wrong with them, with
very little to go on, because they can't talk to you, and actually
they scream at you, making it harder to discuss nuanced hypotheses
with your partner.

Complex software systems aren't babies. But they can be a bit like
that. They're going wrong, because they're slow, or they've got bugs,
or something, and there isn't one obvious cause. The system is
alerting but you've got nowhere to start from. You try out remedies
that maybe have an affect and maybe don't. Emery Berger did a great
talk called [Performance
Matters](https://thestrangeloop.com/2019/performance-matters.html) at
Strange Loop, showing how, especially in modern systems, everything
from the hardware (think L* caches, branch prediction), the OS (swap,
virtual memory, syscalls, memory layout), runtime (allocators, GC,
etc), and your application code, and then in cloud times, distributed
systems and all their failure modes, are all independent layers with
their own affordances and tolerances that trigger at opposing
intervals. You think you've got something performing reliably and then
some threshold of the layer below is passed and suddenly performance
goes up or down, and you aren't wise as to why.

I'm calling this _system colic_ as a negative term for an undesirable
state of affairs. We know we don't have to accept the illegibility of
a system, and can put work into exposing visibility into them, or
choose better components that do better in this regard.
