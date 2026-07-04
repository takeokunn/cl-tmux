(in-package #:cl-tmux/test)

;;;; Dispatch pane/window/prefix tests.

(in-suite dispatch-suite)

;;; ── :swap-pane-forward dispatch ──────────────────────────────────────────────
;;;
;;; Use the shared fixture macro to avoid repeating pane/window/session setup.


(test cmd-swap-pane-s-t-swaps-specific-panes
  "swap-pane -s 1 -t 3 swaps the two named panes' positions in the window list
   (not just the active pane with a neighbour)."
  (with-fake-session (s :nwindows 1 :npanes 3)
    (let* ((win (session-active-window s))
           (p1  (find 1 (window-panes win) :key #'pane-id))
           (p3  (find 3 (window-panes win) :key #'pane-id)))
      (is (eq p1 (first (window-panes win)))         "pane 1 starts first")
      (is (eq p3 (car (last (window-panes win))))    "pane 3 starts last")
      (cl-tmux::%run-command-line s "swap-pane -s 1 -t 3")
      (is (eq p3 (first (window-panes win)))
          "after swap-pane -s 1 -t 3, pane 3 is first")
      (is (eq p1 (car (last (window-panes win))))
          "after swap, pane 1 is last"))))

(test cmd-swap-pane-t-swaps-active-with-target
  "swap-pane -t 2 swaps the active pane (1) with pane 2 (-s defaults to active)."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p1  (find 1 (window-panes win) :key #'pane-id))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (is (eq p1 (first (window-panes win))) "pane 1 (active) starts first")
      (cl-tmux::%run-command-line s "swap-pane -t 2")
      (is (eq p2 (first (window-panes win)))
          "after swap-pane -t 2, pane 2 is first (swapped with active pane 1)"))))

(test cmd-swap-pane-t-activates-dst-pane
  "Without -d, swap-pane -t 2 makes the -t (dst) pane the active pane,
   matching tmux's window_set_active_pane(dst_wp) for a same-window swap."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p1  (find 1 (window-panes win) :key #'pane-id))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (is (eq p1 (window-active-pane win)) "pane 1 is active before swap")
      (cl-tmux::%run-command-line s "swap-pane -t 2")
      (is (eq p2 (window-active-pane win))
          "after swap-pane -t 2, the -t pane (pane 2) is active"))))

(test cmd-swap-pane-rejects-unsupported-arguments
  "swap-pane rejects unsupported flags, unknown flags, and positional tokens
   before mutating panes."
  (dolist (command '("swap-pane -Z extra"
                     "swap-pane -s 1 -t 3 -d"
                     "swap-pane -x"
                     "swap-pane -s 1 -t 3 extra"))
    (with-fake-session (s :nwindows 1 :npanes 3)
      (let* ((win (session-active-window s))
             (before (copy-list (window-panes win)))
             (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (equal before (window-panes win))
            "~A must not reorder panes" command)
        (assert-overlay-contains "unsupported argument" *overlay* command)))))

;;; ── :swap-pane-forward / :swap-pane-backward dispatch ───────────────────────

(test dispatch-swap-pane-table
  ":swap-pane-forward (p0 active) and :swap-pane-backward (p1 active) both
   move p1 to first position and mark *dirty*."
  (dolist (row '((:swap-pane-forward  nil "forward: p0 active")
                 (:swap-pane-backward t   "backward: p1 active")))
    (destructuring-bind (cmd select-p1 desc) row
      (with-two-pane-h-session (sess win p0 p1)
        (when select-p1 (window-select-pane win p1))
        (cl-tmux::dispatch-command sess cmd nil)
        (is (eq p1 (first  (window-panes win))) "~A: p1 must be first"  desc)
        (is (eq p0 (second (window-panes win))) "~A: p0 must be second" desc)
        (is-true cl-tmux::*dirty*               "~A: must mark *dirty*" desc)))))

;;; ── :kill-pane-confirm dispatch ──────────────────────────────────────────────

(test dispatch-kill-pane-confirm-table
  ":kill-pane-confirm opens a prompt; y kills the active pane; n leaves it."
  (dolist (c '((nil 2 "no answer: pane count unchanged")
               ("y" 1 "y: active pane killed")
               ("n" 2 "n: pane preserved")))
    (destructuring-bind (answer expected-count desc) c
      (with-fake-two-pane-session (s)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :kill-pane-confirm nil)
          (is (prompt-active-p) "prompt must open for ~A" desc)
          (is-true (prompt-single-key *prompt*) "prompt must accept a single key for ~A" desc)
          (when answer
            (cl-tmux::handle-prompt-key (char-code (char answer 0)))
            (is-false (prompt-active-p) "prompt must close after answer for ~A" desc))
          (is (= expected-count
                 (length (window-panes (session-active-window s))))
              "~A" desc))))))

;;; ── :kill-window-confirm dispatch ────────────────────────────────────────────

(test dispatch-kill-window-confirm-table
  ":kill-window-confirm opens a prompt; y kills the active window; n leaves it."
  (dolist (c '((nil 2 "no answer: window count unchanged")
               ("y" 1 "y: active window killed")
               ("n" 2 "n: window preserved")))
    (destructuring-bind (answer expected-count desc) c
      (with-fake-session (s :nwindows 2)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :kill-window-confirm nil)
          (is (prompt-active-p) "prompt must open for ~A" desc)
          (is-true (prompt-single-key *prompt*) "prompt must accept a single key for ~A" desc)
          (when answer
            (cl-tmux::handle-prompt-key (char-code (char answer 0)))
            (is-false (prompt-active-p) "prompt must close after answer for ~A" desc))
          (is (= expected-count (length (session-windows s))) "~A" desc))))))

(test dispatch-kill-window-confirm-prompt-includes-window-name
  ":kill-window-confirm prompt label includes the current window name."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (let ((wname (window-name (session-active-window s))))
        (cl-tmux::dispatch-command s :kill-window-confirm nil)
        (is (prompt-active-p) "prompt must be open")
        (is (search wname (prompt-label *prompt*))
            "prompt label must contain the window name")))))

;;; ── :send-prefix dispatch ────────────────────────────────────────────────────

(test dispatch-send-prefix-command-is-defined
  ":send-prefix command is registered in dispatch-command without error."
  ;; We cannot test actual PTY writes in unit tests (fd=-1), but we verify
  ;; that dispatching :send-prefix does not signal any error and marks dirty.
  (with-fake-session (s)
    ;; Should not error even with fd=-1 (the guard (> fd 0) protects the write).
    (finishes (cl-tmux::dispatch-command s :send-prefix nil))
    (is-true cl-tmux::*dirty* ":send-prefix must mark *dirty*")))

(test dispatch-send-prefix-read-only-does-not-write
  ":send-prefix does not inject the prefix byte when the client is read-only."
  (with-isolated-config
    (with-fake-session (s)
      (let* ((pane (window-active-pane (session-active-window s)))
             (writes nil)
             (orig (fdefinition 'cl-tmux/pty:pty-write)))
        (setf (pane-fd pane) 9999)
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-write)
                     (lambda (fd bytes)
                       (push (list fd (coerce bytes 'list)) writes)))
               (let ((cl-tmux::*client-read-only* t))
                 (cl-tmux::dispatch-command s :send-prefix nil))
               (is (null writes)
                   "read-only clients must not write a prefix byte to the pane"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

;;; ── unbound prefix key no-op ─────────────────────────────────────────────────

(test dispatch-unknown-command-is-noop
  "An unrecognized prefix key is silently discarded (no passthrough corruption)."
  ;; Previously the otherwise clause called %passthrough-prefix, injecting
  ;; raw bytes into the pane.  After the fix it must be a silent no-op.
  (with-fake-session (s)
    ;; Dispatching an unknown command must return NIL and must not error.
    (is (null (cl-tmux::dispatch-command s :no-such-command-xyz nil))
        "unknown command must return NIL")
    (is-true cl-tmux::*dirty*
             "dispatch must mark *dirty* even for unknown commands")))

;;; ── :paste-buffer bracketed-paste wrapping ───────────────────────────────────

(test dispatch-paste-buffer-no-crash-without-buffer
  ":paste-buffer with an empty paste buffer is a no-op (no error, marks dirty)."
  (with-fake-session (s)
    (finishes (cl-tmux::dispatch-command s :paste-buffer nil))
    (is-true cl-tmux::*dirty* ":paste-buffer must mark *dirty*")))
