(in-package #:cl-tmux/test)

;;;; SS3 function keys, prefix function keys, and modified root keys

(in-suite events-suite)

;;; ── SS3 function keys: ESC O P/Q/R/S (F1-F4), ESC O H/F (Home/End) ───────────

(test ss3-key-name-maps-f1-through-f4-and-home-end
  "%ss3-key-name maps the SS3 finals to canonical key names; SS3 arrows and
   unrecognised finals map to NIL (forwarded raw)."
  (dolist (c '((#\P "F1") (#\Q "F2") (#\R "F3") (#\S "F4")
               (#\H "Home") (#\F "End")
               (#\A nil) (#\Z nil)))
    (destructuring-bind (ch expected) c
      (is (equal expected (cl-tmux::%ss3-key-name (char-code ch)))
          "SS3 final ~C → ~S" ch expected))))

(test ss3-introducer-defers-one-byte-and-tracks-buffer
  "ESC O does not resolve immediately (it could be F1-F4); the decoder keeps
   accumulating and exposes the partial buffer for the escape-time flush replay."
  (with-fake-session (s)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (cl-tmux::process-byte s 27 state)              ; ESC
      (cl-tmux::process-byte s (char-code #\O) state) ; O
      (is (not (eq #'cl-tmux::%ground-input-state
                   (cl-tmux::input-state-continuation state)))
          "ESC O must keep accumulating, not resolve as Alt+O at length 2")
      (is (and cl-tmux::*esc-accum-buffer*
               (equalp (coerce (subseq cl-tmux::*esc-accum-buffer* 0
                                       (fill-pointer cl-tmux::*esc-accum-buffer*))
                               'list)
                       '(27 79)))
          "the replay buffer must hold the full partial sequence ESC O"))))

(test ss3-f1-root-binding-fires-from-byte-stream
  "bind -n F1 fires when ESC O P is fed through the input state machine, and the
   buffer-replay state is cleared once the sequence completes (back to ground)."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (key-table-bind "root" "F1" :next-window)
      (unwind-protect
           (progn
             (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC O P must resolve to F1 and fire its root binding")
             (is (eq #'cl-tmux::%ground-input-state
                     (cl-tmux::input-state-continuation state))
                 "the state machine must return to ground after ESC O P")
             (is (null cl-tmux::*esc-accum-buffer*)
                 "the replay buffer must be cleared once back at ground"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "F1" tbl)))))))

(test ss3-unbound-f1-does-not-fire-and-returns-to-ground
  "An unbound F1 (ESC O P) must not trigger a command and must leave the state
   machine at ground — the raw key is forwarded to the pane for transparency."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state  (cl-tmux::make-input-state))
          (before (session-active-window s)))
      (dolist (byte (list 27 (char-code #\O) (char-code #\P)))
        (cl-tmux::process-byte s byte state))
      (is (eq before (session-active-window s))
          "an unbound F1 must not change the active window")
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "the state machine must return to ground after an unbound ESC O P"))))

;;; ── Prefix-table function keys: C-b then F5 / F1 (bind F5, bind F1) ──────────

(test prefix-function-key-csi-binding-fires
  "bind F5 next-window fires on C-b then ESC [ 15 ~ — the prefix-table path now
   resolves CSI function keys (previously the multi-digit tilde was swallowed)."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F5" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        ;; C-b (2) then ESC [ 1 5 ~
        (dolist (byte '(2 27 91 49 53 126))
          (cl-tmux::process-byte s byte state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "C-b F5 must run the prefix-table binding")
        (is (eq #'cl-tmux::%ground-input-state
                (cl-tmux::input-state-continuation state))
            "the state machine must return to ground after C-b F5")))))

(test prefix-function-key-ss3-binding-fires
  "bind F1 next-window fires on C-b then ESC O P — the prefix-table path now
   resolves the SS3 function-key form too."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "F1" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        ;; C-b (2) then ESC O P
        (dolist (byte (list 2 27 (char-code #\O) (char-code #\P)))
          (cl-tmux::process-byte s byte state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "C-b F1 must run the prefix-table binding")))))

(test prefix-arrow-binding-still-fires-after-digit-change
  "Regression guard: widening the 3-byte branch to accumulate on any digit final
   must not break the plain arrow path — C-b then ESC [ A still selects up."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "Up" "next-window"))
    (with-fake-session (s :nwindows 2)
      (let ((cl-tmux::*esc-accum-buffer* nil)
            (state (cl-tmux::make-input-state)))
        ;; C-b (2) then ESC [ A
        (dolist (byte (list 2 27 91 (char-code #\A)))
          (cl-tmux::process-byte s byte state))
        (is (eq (second (session-windows s)) (session-active-window s))
            "C-b Up must still resolve to the prefix-table Up binding")))))

;;; ── Modified function keys & combined-modifier arrows (root bind -n) ─────────

(test modified-function-key-root-binding-fires-from-byte-stream
  "bind -n C-F5 fires when ESC [ 15 ; 5 ~ (Ctrl+F5) is fed through the machine —
   the modified-function-key form the unmodified path previously dropped."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (key-table-bind "root" "C-F5" :next-window)
      (unwind-protect
           (progn
             ;; ESC [ 1 5 ; 5 ~
             (dolist (byte '(27 91 49 53 59 53 126))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC [ 15 ; 5 ~ must resolve to C-F5 and fire its binding"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "C-F5" tbl)))))))

(test combined-modifier-arrow-root-binding-fires-from-byte-stream
  "bind -n C-S-Up fires when ESC [ 1 ; 6 A (Ctrl+Shift+Up) is fed — combined
   modifiers now resolve, matching the CSI-u path's handling of letter keys."
  (with-fake-session (s :nwindows 2)
    (let ((cl-tmux::*esc-accum-buffer* nil)
          (state (cl-tmux::make-input-state)))
      (key-table-bind "root" "C-S-Up" :next-window)
      (unwind-protect
           (progn
             ;; ESC [ 1 ; 6 A
             (dolist (byte (list 27 91 49 59 54 (char-code #\A)))
               (cl-tmux::process-byte s byte state))
             (is (eq (second (session-windows s)) (session-active-window s))
                 "ESC [ 1 ; 6 A must resolve to C-S-Up and fire its binding"))
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash "C-S-Up" tbl)))))))
