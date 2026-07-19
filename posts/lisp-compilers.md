---
date: 2026-07-19
title: Lisp compilers that compile directly to assembly
---

I'd like to point out one cool thing that some Lisp compilers, like
SBCL, do. SBCL compiles functions and expressions directly to
assembly. There aren't other AST interpreter or byte-code interpreter
modes, as seen in many other languages.

Let's turn on debugging up to the fullest:

```lisp
(declaim (optimize (debug 3) (safety 3) (speed 0) (compilation-speed 3)))
```

Declare a simple function:

```lisp
* (defun testfunc (a b) (+ a b))
```

We can immediately ask for the assembly in the REPL:

```lisp
* (disassemble 'testfunc)
; disassembly for TESTFUNC
; Size: 60 bytes. Origin: #x10020101B0                        ; TESTFUNC
; B0:       AA0A40F9         LDR R0, [THREAD, #16]            ; binding-stack-pointer
; B4:       4A0B00F9         STR R0, [CFP, #16]
; B8:       7A0300F9         STR CFP, [CSP]
; BC:       EA030EAA         MOV R0, R4
; C0:       EB030DAA         MOV R1, R3
; C4:       FA031BAA         MOV CFP, CSP
; C8:       3C9C80D2         MOVZ TMP, #1249
; CC:       BE6B7CF8         LDR LR, [NULL, TMP]              ; SB-KERNEL:TWO-ARG-+
; D0:       C0033FD6         BLR LR
; D4:       4EB741A9         LDP R4, R3, [CFP, #24]
; D8:       FB031AAA         MOV CSP, CFP
; DC:       5A7B40A9         LDP CFP, LR, [CFP]
; E0:       BF0300F1         CMP NULL, #0
; E4:       C0035FD6         RET
; E8:       E00120D4         BRK #15                          ; Invalid argument count trap
NIL
```

This assembly isn't unsafe -- it'll handle dynamic typing, wrong
arguments, all the usual stuff that dynamic languages like Lisp or
Python deal with. It keeps the stack frame pointer around so you can
debug it later.

If we turn up optimisations up, turn the safety off, etc. Then the
output is quite different:

```lisp
* (declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
NIL
* (defun testfunc (a b) (+ a b))
WARNING: redefining COMMON-LISP-USER::TESTFUNC in DEFUN
TESTFUNC
* (disassemble 'testfunc)
; disassembly for TESTFUNC
; Size: 16 bytes. Origin: #x1002010268                        ; TESTFUNC
; 68:       3C9C80D2         MOVZ TMP, #1249
; 6C:       BE6B7CF8         LDR LR, [NULL, TMP]              ; SB-KERNEL:TWO-ARG-+
; 70:       DE130091         ADD LR, LR, #4
; 74:       C0031FD6         BR LR
NIL
```

This is still calling the same internal SBCL adder, but doesn't
check for argument count, and doesn't expect control back
either. If we add fixnum declarations to the function, it gets a lot
bigger because then it inlines the previously deferred SBCL adder.

```lisp
* (defun testfunc (a b) (declare (type fixnum a b)) (+ a b))
WARNING: redefining COMMON-LISP-USER::TESTFUNC in DEFUN
TESTFUNC
* (disassemble 'testfunc)
; disassembly for TESTFUNC
; Size: 108 bytes. Origin: #x10020102E8                       ; TESTFUNC
; 2E8:       40FD4193         ASR NL0, R0, #1
; 2EC:       61FD4193         ASR NL1, R1, #1
; 2F0:       0000018B         ADD NL0, NL0, NL1
; 2F4:       0A0000AB         ADDS R0, NL0, NL0
; 2F8:       E7010054         BVC L1
; 2FC:       BD2A00B9         STR WNULL, [THREAD, #40]        ; pseudo-atomic-bits
; 300:       BC7A47A9         LDP TMP, LR, [THREAD, #112]     ; mixed-tlab.{free-pointer, end-addr}
; 304:       8A430091         ADD R0, TMP, #16
; 308:       5F011EEB         CMP R0, LR
; 30C:       C8010054         BHI L2
; 310:       AA3A00F9         STR R0, [THREAD, #112]          ; mixed-tlab
; 314: L0:   8A3F0091         ADD R0, TMP, #15
; 318:       3E2280D2         MOVZ LR, #273
; 31C:       9E0300A9         STP LR, NL0, [TMP]
; 320:       BF3F03D5         DMB SY
; 324:       BF2A00B9         STR WZR, [THREAD, #40]          ; pseudo-atomic-bits
; 328:       BE2E40B9         LDR WLR, [THREAD, #44]          ; pseudo-atomic-bits
; 32C:       5E0000B4         CBZ LR, L1
; 330:       200120D4         BRK #9                          ; Pending interrupt trap
; 334: L1:   FB031AAA         MOV CSP, CFP
; 338:       5A7B40A9         LDP CFP, LR, [CFP]
; 33C:       BF0300F1         CMP NULL, #0
; 340:       C0035FD6         RET
; 344: L2:   1C0280D2         MOVZ TMP, #16
; 348:       0AFCFF58         LDR R0, #x10020102C8            ; SB-VM::ALLOC-TRAMP
; 34C:       40013FD6         BLR R0
; 350:       F1FFFF17         B L0
NIL
```

I'm not sharing these to demonstrate SBCL's optimizer. The particular
details don't matter much. Rather, I think it's really interesting to
have a language implementation that compiles straight to assembly *in
all cases*.

I haven't used Common Lisp in anger since 2007. But how SBCL creates
this kind of mechanical sympathy and simplicity in having "one target"
(sure -- each architecture is different) is really interesting.
