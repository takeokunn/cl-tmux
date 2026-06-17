(in-package #:cl-tmux/test)

;;;; FFI declarations and fd_set helper tests (src/pty-ffi.lisp).

(def-suite pty-ffi-suite :description "FFI declarations and fd_set helpers")
(in-suite pty-ffi-suite)

;;; ── Platform constants ───────────────────────────────────────────────────────

(test platform-constants-are-defined
  "+o-rdwr+, +o-noctty+, +tiocgwinsz+, +tiocswinsz+ are all positive fixnums."
  (is (plusp cl-tmux/pty::+o-rdwr+))
  (is (plusp cl-tmux/pty::+o-noctty+))
  (is (plusp cl-tmux/pty::+tiocgwinsz+))
  (is (plusp cl-tmux/pty::+tiocswinsz+))
  (is (= 32 cl-tmux/pty::+fd-set-words+)))

(test signal-and-ioctl-constants-are-positive-fixnums
  "+sighup+ is 1; +tiocsctty+, +tiocgwinsz+, +tiocswinsz+ are positive fixnums."
  (is (= 1 cl-tmux/pty::+sighup+)
      "SIGHUP must be signal number 1 on POSIX")
  (is (plusp cl-tmux/pty::+tiocsctty+)
      "+tiocsctty+ must be a positive fixnum")
  (is (integerp cl-tmux/pty::+tiocsctty+)
      "+tiocsctty+ must be an integer"))

(test fd-set-words-matches-expected-size
  "+fd-set-words+ is 32, matching a 128-byte fd_set on 64-bit platforms."
  (is (= 32 cl-tmux/pty::+fd-set-words+)
      "fd_set must be represented as 32 int32 words (128 bytes total)"))

;;; ── fd_set helpers (pure CFFI bit-manipulation) ─────────────────────────────

(test fd-zero-clears-all-words
  "fd-zero! zeroes every word in the fd_set buffer."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    ;; Dirty the buffer first.
    (dotimes (i cl-tmux/pty::+fd-set-words+)
      (setf (cffi:mem-aref rset :int32 i) -1))
    (cl-tmux/pty::fd-zero! rset)
    (dotimes (i cl-tmux/pty::+fd-set-words+)
      (is (zerop (cffi:mem-aref rset :int32 i))
          "word ~D must be zero after fd-zero!" i))))

(test fd-zero-idempotent-on-already-cleared-buffer
  "fd-zero! on an already-zeroed buffer leaves all words zero."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    (cl-tmux/pty::fd-zero! rset)   ; second call is the idempotency check
    (dotimes (i cl-tmux/pty::+fd-set-words+)
      (is (zerop (cffi:mem-aref rset :int32 i))
          "word ~D must remain zero after double fd-zero!" i))))

(test fd-set-and-isset-round-trip
  "fd-set! sets exactly the fd's bit; fd-isset-p detects it."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    ;; Test a handful of fd values spanning different words.
    ;; Avoid bit 31 (the sign bit of :int32) to prevent signed-overflow
    ;; type errors in CFFI mem-aref; fd 30 and 62 use bit 30 instead.
    (dolist (fd '(0 1 5 30 32 62))
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
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    (cl-tmux/pty::fd-set! 5 rset)
    (is-false (cl-tmux/pty::fd-isset-p 4 rset) "fd 4 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p 6 rset) "fd 6 must remain unset")
    (is-true  (cl-tmux/pty::fd-isset-p 5 rset) "fd 5 must be set")))

(test fd-set-multiple-fds-independently-tracked
  "Setting two fds in the same word does not interfere; both are detected."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    ;; fds 3 and 7 share word 0 (both < 32).
    (cl-tmux/pty::fd-set! 3 rset)
    (cl-tmux/pty::fd-set! 7 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 3 rset) "fd 3 must be set")
    (is-true  (cl-tmux/pty::fd-isset-p 7 rset) "fd 7 must be set")
    (is-false (cl-tmux/pty::fd-isset-p 2 rset) "fd 2 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p 8 rset) "fd 8 must remain unset")))

(test fd-set-cross-word-boundary-fds
  "fd 31 (word 0 bit 31) and fd 32 (word 1 bit 0) are in different words."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    ;; fd 31 is the highest bit of word 0; fd 32 is the lowest bit of word 1.
    ;; Both can be set without signed-overflow because we test each independently.
    (cl-tmux/pty::fd-set! 32 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 32 rset) "fd 32 must be set")
    (is-false (cl-tmux/pty::fd-isset-p 33 rset) "fd 33 must remain unset")
    (is-false (cl-tmux/pty::fd-isset-p  0 rset) "fd 0 must remain unset")))

(test fd-zero-clears-previously-set-bits
  "fd-zero! clears bits that were set by fd-set!."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    (cl-tmux/pty::fd-set!  10 rset)
    (cl-tmux/pty::fd-set!  20 rset)
    (is-true (cl-tmux/pty::fd-isset-p 10 rset) "fd 10 must be set before zero")
    (cl-tmux/pty::fd-zero! rset)
    (is-false (cl-tmux/pty::fd-isset-p 10 rset) "fd 10 must be clear after fd-zero!")
    (is-false (cl-tmux/pty::fd-isset-p 20 rset) "fd 20 must be clear after fd-zero!")))

;;; ── Additional fd_set edge cases ─────────────────────────────────────────────

(test fd-set-fd-0-works
  "fd-set! and fd-isset-p work correctly for fd 0 (the lowest possible fd)."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    (is-false (cl-tmux/pty::fd-isset-p 0 rset) "fd 0 must be clear initially")
    (cl-tmux/pty::fd-set! 0 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 0 rset) "fd 0 must be set after fd-set!")
    (is-false (cl-tmux/pty::fd-isset-p 1 rset) "fd 1 must remain clear")))

(test fd-isset-p-returns-false-on-zeroed-buffer
  "fd-isset-p returns NIL for any fd after fd-zero!."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    (dolist (fd '(0 1 5 15 32 63))
      (is-false (cl-tmux/pty::fd-isset-p fd rset)
                "fd ~D must be clear in zeroed buffer" fd))))

(test fd-set-all-fds-in-last-word
  "fd-set! and fd-isset-p work for fds in the last word (word 31, fds 992–1023)."
  (cffi:with-foreign-object (rset :int32 cl-tmux/pty::+fd-set-words+)
    (cl-tmux/pty::fd-zero! rset)
    ;; fd 992 = word 31, bit 0; fd 993 = word 31, bit 1.
    (cl-tmux/pty::fd-set! 992 rset)
    (is-true  (cl-tmux/pty::fd-isset-p 992 rset) "fd 992 must be set")
    (is-false (cl-tmux/pty::fd-isset-p 993 rset) "fd 993 must remain clear")))

(test platform-constants-distinguish-read-write-flags
  "+o-rdwr+ and +o-noctty+ are distinct non-zero values."
  (is (not (= cl-tmux/pty::+o-rdwr+ cl-tmux/pty::+o-noctty+))
      "+o-rdwr+ and +o-noctty+ must be distinct constants"))

(test ioctl-request-codes-are-distinct
  "+tiocgwinsz+ and +tiocswinsz+ are distinct (get ≠ set) ioctl codes."
  (is (not (= cl-tmux/pty::+tiocgwinsz+ cl-tmux/pty::+tiocswinsz+))
      "+tiocgwinsz+ and +tiocswinsz+ must be distinct ioctl codes"))

;;; ── FFI function reachability ────────────────────────────────────────────────

(test cffi-functions-are-fbound
  "All CFFI-declared functions (%posix-openpt, %grantpt, %unlockpt, %ptsname,
   %read, %write, %select) must be fbound after the FFI declarations are loaded."
  (dolist (sym '(cl-tmux/pty::%posix-openpt
                 cl-tmux/pty::%grantpt
                 cl-tmux/pty::%unlockpt
                 cl-tmux/pty::%ptsname
                 cl-tmux/pty::%read
                 cl-tmux/pty::%write
                 cl-tmux/pty::%select))
    (is (fboundp sym)
        "~A must be fbound after CFFI defcfun" sym)))
