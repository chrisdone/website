---
date: 2025-04-09
title: My blog now runs on a Raspberry Pi at home
description: Running my blog on a raspberry pi
---

I discovered that my new house had [OFNL](https://www.ofnl.co.uk/)
internet, which is one of a handful competing fibre providers in England
(others: BT OpenReach, VirginMedia, Hyperoptic, etc). Some of the
infra providers are also the sole ISP, like VirginMedia and
Hyperoptic, whereas BT OpenReach and OFNL just provide the infra and
then companies can offer consumer ISP services on top of it.

I picked one ([Merula](https://www.merula.net/)) who, like a few
others, provide a gigabit connection, and for about £3 per month, a
static IPv4 address.

Some ISPs in the world--even in rich countries--put people behind a
[CGNAT](https://en.wikipedia.org/wiki/Carrier-grade_NAT), as a way to
get more mileage out of the few IPv4 addresses they have. This means
that you can't listen on a port and have someone directly connect to
you, because the NAT wouldn't know who to pick. A very sad state of
affairs. I've also heard about a German ISP that provides IPv6-only
and then handles all IPv4 connections via a proxy!

In that context, it's a pretty fortunate situation to be able to get a
static IP at my home address. As a programmer it's more joyful to have
this.

I grabbed a Raspberry Pi 5 (highly overpowered for a blog), connected
to my router via gigabit Ethernet, is running with a simple SD
card for the root file-system. All logs (journals) are moved to
tamps ('volatile' storage mode). The blog itself is sync's via a
webhook from GitHub, and is stored in tamps, too. Like this, it should
run for a couple years without the SD card wearing out.

The server is [Caddy](https://caddyserver.com/) and the webhook
service is [webhook](https://github.com/adnanh/webhook), both come as
systemd services, so the effort was very minimal.

There's no particular technical advantage over you reading this page
from my home or the Digital Ocean server which only cost me about
10$/mo. But it was a fun little project.
