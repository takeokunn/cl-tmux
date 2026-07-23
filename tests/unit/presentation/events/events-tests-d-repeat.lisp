(in-package #:cl-tmux/test)

;;;; root key-table repeat mode

(describe "events-suite"

  ;;; ── Root key-table repeat mode (bind -n -r) ─────────────────────────────

  ;; A -r binding in the root table returns :REPEATABLE so process-byte stamps the
  ;; repeat window (parity: tmux sets CLIENT_REPEAT on root, not just prefix).
  (it "root-repeatable-binding-enters-repeat-mode"
    (with-fake-session (s :nwindows 3)
      (let ((state (cl-tmux::make-input-state)))
        (key-table-bind "root" #\Z :next-window :repeatable t)
        (unwind-protect
             (progn
               ;; First press: fires + arms repeat mode.
               (expect (eq :repeatable (cl-tmux::process-byte s (char-code #\Z) state)))
               (expect (not (null (cl-tmux::input-state-repeat-entered-at state))))
               (expect (eq #'cl-tmux::%after-root-repeat-input-state
                           (cl-tmux::input-state-continuation state))))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash #\Z tbl)))))))

  ;; After a -r root binding arms repeat mode, the SAME key fires again without any
  ;; prefix (the root-repeat state re-looks-up the byte in the root table).
  (it "root-repeatable-binding-repeats-without-prefix"
    (with-fake-session (s :nwindows 3)
      (let ((state (cl-tmux::make-input-state)))
        (key-table-bind "root" #\Z :next-window :repeatable t)
        (unwind-protect
             (let ((w0 (session-active-window s)))
               (cl-tmux::process-byte s (char-code #\Z) state) ; window 1 -> 2
               (let ((w1 (session-active-window s)))
                 (expect (not (eq w0 w1)))
                 ;; Second press WITHOUT a prefix: repeat state fires it again.
                 (expect (eq :repeatable (cl-tmux::process-byte s (char-code #\Z) state)))
                 (expect (not (eq w1 (session-active-window s))))))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash #\Z tbl)))))))

  ;; A non-repeatable key pressed during root repeat mode exits repeat mode and is
  ;; reprocessed as a normal ground keystroke (clears repeat-entered-at).
  (it "root-repeat-mode-broken-by-non-repeatable-key"
    (with-fake-session (s :nwindows 3)
      (let ((state (cl-tmux::make-input-state)))
        (key-table-bind "root" #\Z :next-window :repeatable t)
        ;; A second, NON-repeatable root binding: pressing it during repeat mode
        ;; still resolves in the root table but must break the repeat sequence.
        (key-table-bind "root" #\Y :prev-window)
        (unwind-protect
             (progn
               (cl-tmux::process-byte s (char-code #\Z) state) ; arm repeat mode
               (expect (not (null (cl-tmux::input-state-repeat-entered-at state))))
               (expect (null (cl-tmux::process-byte s (char-code #\Y) state)))
               (expect (null (cl-tmux::input-state-repeat-entered-at state)))
               (expect (eq #'cl-tmux::%ground-input-state
                           (cl-tmux::input-state-continuation state))))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash #\Z tbl) (remhash #\Y tbl))))))))
