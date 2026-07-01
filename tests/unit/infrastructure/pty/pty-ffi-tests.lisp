(in-package #:cl-tmux/test)

;;;; FFI declarations and fd_set helper tests (src/pty-ffi.lisp).

(def-suite pty-ffi-suite :description "FFI declarations and fd_set helpers")
(in-suite pty-ffi-suite)

;;; ── Fixture macro ────────────────────────────────────────────────────────────
;;;
;;; fd_set words are accessed as :UINT32, not :INT32: bit 31 corresponds to
;;; (ash 1 31) = 2147483648, which is out of range for (signed-byte 32).
;;; All tests use with-fresh-fd-set to get a correctly-typed, pre-zeroed buffer.

(defmacro with-fresh-fd-set ((fd-set-var) &body body)
  "Bind FD-SET-VAR to a freshly-allocated, zeroed foreign fd_set for BODY.
   Uses :uint32 to match fd-zero!, fd-set!, and fd-isset-p (which all access
   memory as :uint32 to avoid signed TYPE-ERROR on bit 31)."
  `(cffi:with-foreign-object (,fd-set-var :uint32 cl-tmux/pty::+fd-set-words+)
     (cl-tmux/pty::fd-zero! ,fd-set-var)
     ,@body))

;;; ── Platform constants ───────────────────────────────────────────────────────

(test platform-constants-are-defined
  "+tiocgwinsz+, +tiocswinsz+ are positive fixnums."
  (is (plusp cl-tmux/pty::+tiocgwinsz+))
  (is (plusp cl-tmux/pty::+tiocswinsz+))
  (is (= 32 cl-tmux/pty::+fd-set-words+)))

(test signal-constant-is-positive-fixnum
  "+sighup+ is 1."
  (is (= 1 cl-tmux/pty::+sighup+)
      "SIGHUP must be signal number 1 on POSIX"))

(test fd-set-words-matches-expected-size
  "+fd-set-words+ is 32, matching a 128-byte fd_set on 64-bit platforms."
  (is (= 32 cl-tmux/pty::+fd-set-words+)
      "fd_set must be represented as 32 uint32 words (128 bytes total)"))

;;; ── fd_set helpers (pure CFFI bit-manipulation) ─────────────────────────────

(test fd-zero-clears-all-words
  "fd-zero! zeroes every word in the fd_set buffer."
  (with-fresh-fd-set (rset)
    ;; Dirty the buffer first.
    (dotimes (i cl-tmux/pty::+fd-set-words+)
      (setf (cffi:mem-aref rset :uint32 i) #xFFFFFFFF))
    (cl-tmux/pty::fd-zero! rset)
    (dotimes (i cl-tmux/pty::+fd-set-words+)
      (is (zerop (cffi:mem-aref rset :uint32 i))
          "word ~D must be zero after fd-zero!" i))))

(test fd-zero-idempotent-on-already-cleared-buffer
  "fd-zero! on an already-zeroed buffer leaves all words zero."
  (with-fresh-fd-set (rset)
    (cl-tmux/pty::fd-zero! rset)   ; second call is the idempotency check
    (dotimes (i cl-tmux/pty::+fd-set-words+)
      (is (zerop (cffi:mem-aref rset :uint32 i))
          "word ~D must remain zero after double fd-zero!" i))))

(test fd-set-and-isset-round-trip
  "fd-set! sets exactly the fd's bit; fd-isset-p detects it.
   Includes fd 31 (bit 31 of word 0) and fd 63 (bit 31 of word 1) to verify
   that :uint32 avoids the signed overflow that :int32 would cause at bit 31."
  (with-fresh-fd-set (rset)
    ;; Test fd values spanning different words, including the previously-avoided
    ;; sign bits (fd 31 = word 0 bit 31, fd 63 = word 1 bit 31).
    (dolist (fd '(0 1 5 30 31 32 62 63))
      (cl-tmux/pty::fd-zero! rset)
      (is-false (cl-tmux/pty::fd-isset-p fd rset)
                "fd ~D should be clear before fd-set!" fd)
      (cl-tmux/pty::fd-set! fd rset)
      (is-true  (cl-tmux/pty::fd-isset-p fd rset)
                "fd ~D should be set after fd-set!" fd)
      ;; Adjacent fd should be unaffected.
      (when (> fd 0)
        (is-false (cl-tmux/pty::fd-isset-p (1- fd) rset)
                  "fd ~D (adjacent) should not be affected" (1- fd))))))

(test fd-set-does-not-affect-other-bits
  "Setting fd 5 does not set fd 6 or fd 4."
  (with-fresh-fd-set (rset)
    (cl-tmux/pty::fd-set! 5 rset)
    (is-false (cl-tmux/pty::fd-isset-p 4 rset) "fd 4 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p 6 rset) "fd 6 must remain unset")
    (is-true  (cl-tmux/pty::fd-isset-p 5 rset) "fd 5 must be set")))

(test fd-set-multiple-fds-independently-tracked
  "Setting two fds in the same word does not interfere; both are detected."
  (with-fresh-fd-set (rset)
    ;; fds 3 and 7 share word 0 (both < 32).
    (cl-tmux/pty::fd-set! 3 rset)
    (cl-tmux/pty::fd-set! 7 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 3 rset) "fd 3 must be set")
    (is-true  (cl-tmux/pty::fd-isset-p 7 rset) "fd 7 must be set")
    (is-false (cl-tmux/pty::fd-isset-p 2 rset) "fd 2 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p 8 rset) "fd 8 must remain unset")))

(test fd-set-cross-word-boundary-fds
  "fd 31 (word 0 bit 31) and fd 32 (word 1 bit 0) are in different words.
   fd 31 exercises the high bit of word 0 — safe only with :uint32."
  (with-fresh-fd-set (rset)
    ;; fd 31 is the highest bit of word 0; fd 32 is the lowest bit of word 1.
    (cl-tmux/pty::fd-set! 31 rset)
    (cl-tmux/pty::fd-set! 32 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 31 rset) "fd 31 must be set")
    (is-true  (cl-tmux/pty::fd-isset-p 32 rset) "fd 32 must be set")
    (is-false (cl-tmux/pty::fd-isset-p 30 rset) "fd 30 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p 33 rset) "fd 33 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p  0 rset) "fd 0 must remain unset")))

(test fd-zero-clears-previously-set-bits
  "fd-zero! clears bits that were set by fd-set!."
  (with-fresh-fd-set (rset)
    (cl-tmux/pty::fd-set!  10 rset)
    (cl-tmux/pty::fd-set!  20 rset)
    (is-true (cl-tmux/pty::fd-isset-p 10 rset) "fd 10 must be set before zero")
    (cl-tmux/pty::fd-zero! rset)
    (is-false (cl-tmux/pty::fd-isset-p 10 rset) "fd 10 must be clear after fd-zero!")
    (is-false (cl-tmux/pty::fd-isset-p 20 rset) "fd 20 must be clear after fd-zero!")))

;;; ── Additional fd_set edge cases ─────────────────────────────────────────────

(test fd-set-fd-0-works
  "fd-set! and fd-isset-p work correctly for fd 0 (the lowest possible fd)."
  (with-fresh-fd-set (rset)
    (is-false (cl-tmux/pty::fd-isset-p 0 rset) "fd 0 must be clear initially")
    (cl-tmux/pty::fd-set! 0 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 0 rset) "fd 0 must be set after fd-set!")
    (is-false (cl-tmux/pty::fd-isset-p 1 rset) "fd 1 must remain clear")))

(test fd-isset-p-returns-false-on-zeroed-buffer
  "fd-isset-p returns NIL for any fd after fd-zero!."
  (with-fresh-fd-set (rset)
    (dolist (fd '(0 1 5 15 31 32 63))
      (is-false (cl-tmux/pty::fd-isset-p fd rset)
                "fd ~D must be clear in zeroed buffer" fd))))

(test fd-set-all-fds-in-last-word
  "fd-set! and fd-isset-p work for fds in the last word (word 31, fds 992–1023)."
  (with-fresh-fd-set (rset)
    ;; fd 992 = word 31, bit 0; fd 993 = word 31, bit 1.
    (cl-tmux/pty::fd-set! 992 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 992 rset) "fd 992 must be set")
    (is-false (cl-tmux/pty::fd-isset-p 993 rset) "fd 993 must remain clear")))

(test ioctl-request-codes-are-distinct
  "+tiocgwinsz+ and +tiocswinsz+ are distinct (get ≠ set) ioctl codes."
  (is (not (= cl-tmux/pty::+tiocgwinsz+ cl-tmux/pty::+tiocswinsz+))
      "+tiocgwinsz+ and +tiocswinsz+ must be distinct ioctl codes"))

;;; ── FFI function reachability ────────────────────────────────────────────────

(test cffi-functions-are-fbound
  "All CFFI-declared functions (%read, %write, %select) must be fbound after
   the FFI declarations are loaded."
  (dolist (sym '(cl-tmux/pty::%read
                 cl-tmux/pty::%write
                 cl-tmux/pty::%select))
    (is (fboundp sym)
        "~A must be fbound after CFFI defcfun" sym)))
