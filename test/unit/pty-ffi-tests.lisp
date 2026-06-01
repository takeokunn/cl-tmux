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
