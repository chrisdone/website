---
date: 2024-06-17
title: Portals in Emacs
description: Portals in Emacs
---

I've been working on a new way to launch and manage processes in Emacs
in [my personal
repo](https://github.com/chrisdone/emacs-config/blob/00e0d01b7f28cff4f16ac2ec5e98581f2969de84/packages/portal/portal.el).
It's an alternative to shells. A bit like a notebook. It also
partially serves the same need that Prodigy-mode does. The name is
evocative of that video game Portal from 2007.

It has a major mode, where `M-!` is rebound to Portal's way of running
shell scripts. It inserts a nano-ID for the portal,
e.g. `portal_abc123456sdkjfal`, as text into the buffer at the current
point. It creates a corresponding directory under `~/.portals/`. All
stdout and stderr are redirected to files under
`~/.portals/{stdout,stderr,command,directory,env}`.

The purpose of this is that Emacs can lock up a bit when you run a
very long running process, or one that spews output very
quickly. Instead of storing it in a buffer or displaying all of it,
Portal just writes it all to a file and displays a little preview of
it anywhere in the buffer that the portal's nano-ID is.

The preview is updated every couple seconds, it's not instant. That
design is intentional, so that no process can ever really overwhelm
Emacs. It's an extremely aggressive design. Even the line length in
the preview is limited to 80 columns.

Another motiviation is simply UX-based, which is that I run a lot of
commands at work that spit out a bunch of useless output. Like
Terraform, Cabal, GHC, etc. Most of the output is not of interest, but
occasionally, one does one to read the output.

Example, below:

```
portal_M2FjMjMyZWIxZjI4MTNmN
# (0) ls
# package.yaml
# readme.md
# scripts
# stack.yaml
# stack.yaml.lock

portal_ZmZhZThiMmFiNzQ1Y2E3N
# (run) stack build
# Preparing to download ghc-tinfo6-libc6-pre232-9.4.8 ...
# ghc-tinfo6-libc6-pre232-9.4.8: download has begun
# ghc-tinfo6-libc6-pre232-9.4.8:  256.00 KiB / 178.01 MiB (  0.14%) downloaded..
# ghc-tinfo6-libc6-pre232-9.4.8:  432.00 KiB / 178.01 MiB (  0.24%) downloaded..
# ghc-tinfo6-libc6-pre232-9.4.8:  736.00 KiB / 178.01 MiB (  0.40%) downloaded..
```

The (0) indicates a successful exit. The command, stdout and stderr
have different colours. In the major mode, you put your cursor over
them and hit RET and it'll pop open the file for you.

The other reason for storing all of this is that one can restart Emacs
and re-open a file containing portals and see what was ran, and
re-launch them if desired, with the same arguments, directory, etc.

I often use `M-x portal-edit` to tweak a command line, which prompts in
the mini-buffer with the existing text to edit. After hitting RET, it
restarts the portal for you. `M-x portal-rerun` does what you'd
expect.

All newly ran portals get a new nano-ID, so you can always go back and
find the outputs of a previously ran portal.

Emacs handles all the stdout and stderr, so it could in theory be
possible to pipe from one portal to another. One could even attach and
detach portals to/from one another, or edit arguments and restart,
while some of processes are still running. But I haven't explored this
because it's more of a side-effect of my design than an intentional
part of it.

It also works perfectly with envrc mode, which is how Emacs integrates
with direnv.

`C-c C-c` on a portal will interrupt the process, as expected.

I've been using this for a few weeks and frankly I'm surprised how
well it works and fits my workflow. At work I have a foo.portals
buffer which has 30 portals in it. Of those, 3 are presently running,
the rest are things I can scroll through and re-run when I feel like
it. I save the file, so I can always restart Emacs and lose nothing.

I'm not interested in maintaining it as a "package." But in the spirit
of Emacs, I am interested in sharing the code, so that others can rip,
riff and remix! Hope someone found this fun!
