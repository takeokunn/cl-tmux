(in-package #:cl-tmux/test)

;;;; root key-table repeat mode

(in-suite events-suite)

;;; ── Root key-table repeat mode (bind -n -r) ─────────────────────────────

(test root-repeatable-binding-enters-repeat-mode
  "A -r binding in the root table returns :REPEATABLE so process-byte stamps the
   repeat window (parity: tmux sets CLIENT_REPEAT on root, not just prefix)."
  (with-fake-session (s :nwindows 3)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" #\Z :next-window :repeatable t)
      (unwind-protect
           (progn
             ;; First press: fires + arms repeat mode.
             (is (eq :repeatable (cl-tmux::process-byte s (char-code #\Z) state))
                 "a -r root binding must return :REPEATABLE on first press")
             (is (not (null (cl-tmux::input-state-repeat-entered-at state)))
                 "repeat-entered-at must be stamped after a -r root binding")
             (is (eq #'cl-tmux::%after-root-repeat-input-state
                     (cl-tmux::input-state-continuation state))
                 "the continuation must be the root-scoped repeat state"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl)))))))

(test root-repeatable-binding-repeats-without-prefix
  "After a -r root binding arms repeat mode, the SAME key fires again without any
   prefix (the root-repeat state re-looks-up the byte in the root table)."
  (with-fake-session (s :nwindows 3)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" #\Z :next-window :repeatable t)
      (unwind-protect
           (let ((w0 (session-active-window s)))
             (cl-tmux::process-byte s (char-code #\Z) state) ; window 1 -> 2
             (let ((w1 (session-active-window s)))
               (is (not (eq w0 w1)) "first press advances the active window")
               ;; Second press WITHOUT a prefix: repeat state fires it again.
               (is (eq :repeatable (cl-tmux::process-byte s (char-code #\Z) state))
                   "the repeated key must fire (and stay :REPEATABLE) without a prefix")
               (is (not (eq w1 (session-active-window s)))
                   "the repeated key must advance the window a second time")))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl)))))))

(test root-repeat-mode-broken-by-non-repeatable-key
  "A non-repeatable key pressed during root repeat mode exits repeat mode and is
   reprocessed as a normal ground keystroke (clears repeat-entered-at)."
  (with-fake-session (s :nwindows 3)
    (let ((state (cl-tmux::make-input-state)))
      (key-table-bind "root" #\Z :next-window :repeatable t)
      ;; A second, NON-repeatable root binding: pressing it during repeat mode
      ;; still resolves in the root table but must break the repeat sequence.
      (key-table-bind "root" #\Y :prev-window)
      (unwind-protect
           (progn
             (cl-tmux::process-byte s (char-code #\Z) state) ; arm repeat mode
             (is (not (null (cl-tmux::input-state-repeat-entered-at state)))
                 "precondition: repeat mode armed")
             (is (null (cl-tmux::process-byte s (char-code #\Y) state))
                 "a non-repeatable root key must NOT return :REPEATABLE")
             (is (null (cl-tmux::input-state-repeat-entered-at state))
                 "a non-repeatable key must clear repeat-entered-at")
             (is (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state))
                 "the continuation must return to ground after a non-repeatable key"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl) (remhash #\Y tbl)))))))
