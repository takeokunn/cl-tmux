(in-package #:cl-tmux/test)

;;;; FFI declarations and fd_set helper tests (src/pty-ffi.lisp).

;;; ── Fixture macro ────────────────────────────────────────────────────────────
;;;
;;; fd_set words are accessed as :UINT32, not :INT32: bit 31 corresponds to
;;; (ash 1 31) = 2147483648, which is out of range for (signed-byte 32).
;;; All tests use with-fresh-fd-set to get a correctly-typed, pre-zeroed buffer.
;;;
;;; Must be a genuine top-level DEFMACRO (not nested inside DESCRIBE's body):
;;; DESCRIBE's body only runs as a lambda at suite-registration time, so a
;;; DEFMACRO nested inside it is invisible to the compiler when it compiles
;;; the sibling IT forms in the same file that call it as a macro.
(defmacro with-fresh-fd-set ((fd-set-var) &body body)
  "Bind FD-SET-VAR to a freshly-allocated, zeroed foreign fd_set for BODY.
   Uses :uint32 to match fd-zero!, fd-set!, and fd-isset-p (which all access
   memory as :uint32 to avoid signed TYPE-ERROR on bit 31)."
  `(cffi:with-foreign-object (,fd-set-var :uint32 cl-tmux/pty::+fd-set-words+)
     (cl-tmux/pty::fd-zero! ,fd-set-var)
     ,@body))

(describe "pty-ffi-suite"

  ;;; ── Platform constants ───────────────────────────────────────────────────────

  ;; +tiocgwinsz+, +tiocswinsz+ are positive fixnums.
  (it "platform-constants-are-defined"
    (expect (plusp cl-tmux/pty::+tiocgwinsz+))
    (expect (plusp cl-tmux/pty::+tiocswinsz+))
    (expect (= 32 cl-tmux/pty::+fd-set-words+)))

  ;; +sighup+ is 1.
  (it "signal-constant-is-positive-fixnum"
    (expect (= 1 cl-tmux/pty::+sighup+)))

  ;; +fd-set-words+ is 32, matching a 128-byte fd_set on 64-bit platforms.
  (it "fd-set-words-matches-expected-size"
    (expect (= 32 cl-tmux/pty::+fd-set-words+)))

  ;;; ── fd_set helpers (pure CFFI bit-manipulation) ─────────────────────────────

  ;; fd-zero! zeroes every word in the fd_set buffer.
  (it "fd-zero-clears-all-words"
    (with-fresh-fd-set (rset)
      ;; Dirty the buffer first.
      (dotimes (i cl-tmux/pty::+fd-set-words+)
        (setf (cffi:mem-aref rset :uint32 i) #xFFFFFFFF))
      (cl-tmux/pty::fd-zero! rset)
      (dotimes (i cl-tmux/pty::+fd-set-words+)
        (expect (zerop (cffi:mem-aref rset :uint32 i))))))

  ;; fd-zero! on an already-zeroed buffer leaves all words zero.
  (it "fd-zero-idempotent-on-already-cleared-buffer"
    (with-fresh-fd-set (rset)
      (cl-tmux/pty::fd-zero! rset)   ; second call is the idempotency check
      (dotimes (i cl-tmux/pty::+fd-set-words+)
        (expect (zerop (cffi:mem-aref rset :uint32 i))))))

  ;; fd-set! sets exactly the fd's bit; fd-isset-p detects it.
  ;; Includes fd 31 (bit 31 of word 0) and fd 63 (bit 31 of word 1) to verify
  ;; that :uint32 avoids the signed overflow that :int32 would cause at bit 31.
  (it "fd-set-and-isset-round-trip"
    (with-fresh-fd-set (rset)
      ;; Test fd values spanning different words, including the previously-avoided
      ;; sign bits (fd 31 = word 0 bit 31, fd 63 = word 1 bit 31).
      (dolist (fd '(0 1 5 30 31 32 62 63))
        (cl-tmux/pty::fd-zero! rset)
        (expect (cl-tmux/pty::fd-isset-p fd rset) :to-be-falsy)
        (cl-tmux/pty::fd-set! fd rset)
        (expect (cl-tmux/pty::fd-isset-p fd rset) :to-be-truthy)
        ;; Adjacent fd should be unaffected.
        (when (> fd 0)
          (expect (cl-tmux/pty::fd-isset-p (1- fd) rset) :to-be-falsy)))))

  ;; Setting fd 5 does not set fd 6 or fd 4.
  (it "fd-set-does-not-affect-other-bits"
    (with-fresh-fd-set (rset)
      (cl-tmux/pty::fd-set! 5 rset)
      (expect (cl-tmux/pty::fd-isset-p 4 rset) :to-be-falsy)
      (expect (cl-tmux/pty::fd-isset-p 6 rset) :to-be-falsy)
      (expect (cl-tmux/pty::fd-isset-p 5 rset) :to-be-truthy)))

  ;; Setting two fds in the same word does not interfere; both are detected.
  (it "fd-set-multiple-fds-independently-tracked"
    (with-fresh-fd-set (rset)
      ;; fds 3 and 7 share word 0 (both < 32).
      (cl-tmux/pty::fd-set! 3 rset)
      (cl-tmux/pty::fd-set! 7 rset)
      (expect (cl-tmux/pty::fd-isset-p 3 rset) :to-be-truthy)
      (expect (cl-tmux/pty::fd-isset-p 7 rset) :to-be-truthy)
      (expect (cl-tmux/pty::fd-isset-p 2 rset) :to-be-falsy)
      (expect (cl-tmux/pty::fd-isset-p 8 rset) :to-be-falsy)))

  ;; fd 31 (word 0 bit 31) and fd 32 (word 1 bit 0) are in different words.
  ;; fd 31 exercises the high bit of word 0 — safe only with :uint32.
  (it "fd-set-cross-word-boundary-fds"
    (with-fresh-fd-set (rset)
      ;; fd 31 is the highest bit of word 0; fd 32 is the lowest bit of word 1.
      (cl-tmux/pty::fd-set! 31 rset)
      (cl-tmux/pty::fd-set! 32 rset)
      (expect (cl-tmux/pty::fd-isset-p 31 rset) :to-be-truthy)
      (expect (cl-tmux/pty::fd-isset-p 32 rset) :to-be-truthy)
      (expect (cl-tmux/pty::fd-isset-p 30 rset) :to-be-falsy)
      (expect (cl-tmux/pty::fd-isset-p 33 rset) :to-be-falsy)
      (expect (cl-tmux/pty::fd-isset-p  0 rset) :to-be-falsy)))

  ;; fd-zero! clears bits that were set by fd-set!.
  (it "fd-zero-clears-previously-set-bits"
    (with-fresh-fd-set (rset)
      (cl-tmux/pty::fd-set!  10 rset)
      (cl-tmux/pty::fd-set!  20 rset)
      (expect (cl-tmux/pty::fd-isset-p 10 rset) :to-be-truthy)
      (cl-tmux/pty::fd-zero! rset)
      (expect (cl-tmux/pty::fd-isset-p 10 rset) :to-be-falsy)
      (expect (cl-tmux/pty::fd-isset-p 20 rset) :to-be-falsy)))

  ;;; ── Additional fd_set edge cases ─────────────────────────────────────────────

  ;; fd-set! and fd-isset-p work correctly for fd 0 (the lowest possible fd).
  (it "fd-set-fd-0-works"
    (with-fresh-fd-set (rset)
      (expect (cl-tmux/pty::fd-isset-p 0 rset) :to-be-falsy)
      (cl-tmux/pty::fd-set! 0 rset)
      (expect (cl-tmux/pty::fd-isset-p 0 rset) :to-be-truthy)
      (expect (cl-tmux/pty::fd-isset-p 1 rset) :to-be-falsy)))

  ;; fd-isset-p returns NIL for any fd after fd-zero!.
  (it "fd-isset-p-returns-false-on-zeroed-buffer"
    (with-fresh-fd-set (rset)
      (dolist (fd '(0 1 5 15 31 32 63))
        (expect (cl-tmux/pty::fd-isset-p fd rset) :to-be-falsy))))

  ;; fd-set! and fd-isset-p work for fds in the last word (word 31, fds 992–1023).
  (it "fd-set-all-fds-in-last-word"
    (with-fresh-fd-set (rset)
      ;; fd 992 = word 31, bit 0; fd 993 = word 31, bit 1.
      (cl-tmux/pty::fd-set! 992 rset)
      (expect (cl-tmux/pty::fd-isset-p 992 rset) :to-be-truthy)
      (expect (cl-tmux/pty::fd-isset-p 993 rset) :to-be-falsy)))

  ;; +tiocgwinsz+ and +tiocswinsz+ are distinct (get ≠ set) ioctl codes.
  (it "ioctl-request-codes-are-distinct"
    (expect (not (= cl-tmux/pty::+tiocgwinsz+ cl-tmux/pty::+tiocswinsz+))))

  ;;; ── FFI function reachability ────────────────────────────────────────────────

  ;; The remaining CFFI-declared function (%select) must be fbound after the FFI
  ;; declarations are loaded.  %read/%write were removed when byte-transparent
  ;; master-fd I/O moved to cl-tty-kit's fd-read-octets / fd-write-octets.
  (it "cffi-functions-are-fbound"
    (dolist (sym '(cl-tmux/pty::%select))
      (expect (fboundp sym)))
    ;; %read/%write are gone: their libc access now lives in cl-tty-kit.
    (expect (not (fboundp (find-symbol "%READ" '#:cl-tmux/pty))))
    (expect (not (fboundp (find-symbol "%WRITE" '#:cl-tmux/pty))))))
