;;; Commentary:
;; Copyright is a the bottome of this file

;; Nterm is meant to be a full vt100 compatible terminal emulator. It
;; has the following features:

;;  - G0 G1 switching with shift in and shift out
;;  - special graphics characters (used for line drawing)
;;  - US and UK character set.
;;  - blinking, bright, underline and reverse rendition
;;  - scroll up and down including within top and bottom margin
;;  - switch terminal background color
;;  - switch between 80 and 132 columns screen
;;  - tabulation set and reset
;;  - all VT100 escape sequences are handled

;; Things that remains to do:
;;  - Double width character
;;  - Double height character
;;  - ANSI color
;;  - VT52 compatibility mode
;;  - Copy paste mechanism
;;  - xterm emulator

;; I think nterm is easier to maintain than term. One look at term's
;; term-emulate-terminal function should convince anyone that term
;; cannot be maintained anymore. Compare with nterm equivalent
;; function nterm-emulate it is only 25 lines long.

;; It has a recording mode (f11) so that you can record and replay
;; traces. It has a terminal memory so that area of the terminal can
;; be redrawn for blinking and changing screen background. There is a
;; memory dump mode (f10) that allows the programmer to examine the
;; memory.

;; The recorder takes a trace of characters received by the
;; terminal. Someone can easily reproduce a bug by replaying the trace
;; both on xterm and nterm.

;; It's not complete yet but it passes the first three tests of
;; vttest. I will get back to it when I have time.

;; The latest version is at http://kanis.fr/hg/lisp/ivan/nterm.el

