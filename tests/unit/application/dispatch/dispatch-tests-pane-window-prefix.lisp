(in-package #:cl-tmux/test)

;;;; Dispatch pane/window/prefix tests.

(describe "dispatch-suite"

  ;;; ── :swap-pane-forward dispatch ──────────────────────────────────────────────
  ;;;
  ;;; Use the shared fixture macro to avoid repeating pane/window/session setup.

  ;; swap-pane -s 1 -t 3 swaps the two named panes' positions in the window list
  ;; (not just the active pane with a neighbour).
  (it "cmd-swap-pane-s-t-swaps-specific-panes"
    (with-fake-session (s :nwindows 1 :npanes 3)
      (let* ((win (session-active-window s))
             (p1  (find 1 (window-panes win) :key #'pane-id))
             (p3  (find 3 (window-panes win) :key #'pane-id)))
        (expect (eq p1 (first (window-panes win))))
        (expect (eq p3 (car (last (window-panes win)))))
        (cl-tmux::%run-command-line s "swap-pane -s 1 -t 3")
        (expect (eq p3 (first (window-panes win))))
        (expect (eq p1 (car (last (window-panes win))))))))

  ;; swap-pane -t 2 swaps the active pane (1) with pane 2 (-s defaults to active).
  (it "cmd-swap-pane-t-swaps-active-with-target"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p1  (find 1 (window-panes win) :key #'pane-id))
             (p2  (find 2 (window-panes win) :key #'pane-id)))
        (expect (eq p1 (first (window-panes win))))
        (cl-tmux::%run-command-line s "swap-pane -t 2")
        (expect (eq p2 (first (window-panes win)))))))

  ;; Without -d, swap-pane -t 2 makes the -t (dst) pane the active pane,
  ;; matching tmux's window_set_active_pane(dst_wp) for a same-window swap.
  (it "cmd-swap-pane-t-activates-dst-pane"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p1  (find 1 (window-panes win) :key #'pane-id))
             (p2  (find 2 (window-panes win) :key #'pane-id)))
        (expect (eq p1 (window-active-pane win)))
        (cl-tmux::%run-command-line s "swap-pane -t 2")
        (expect (eq p2 (window-active-pane win))))))

  ;; swap-pane rejects unsupported flags, unknown flags, and positional tokens
  ;; before mutating panes.
  (it "cmd-swap-pane-rejects-unsupported-arguments"
    (dolist (command '("swap-pane -Z extra"
                       "swap-pane -s 1 -t 3 -d"
                       "swap-pane -x"
                       "swap-pane -s 1 -t 3 extra"))
      (with-fake-session (s :nwindows 1 :npanes 3)
        (let* ((win (session-active-window s))
               (before (copy-list (window-panes win)))
               (*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s command)))
          (expect (equal before (window-panes win)))
          (assert-overlay-contains "unsupported argument" *overlay* command)))))

  ;;; ── :swap-pane-forward / :swap-pane-backward dispatch ───────────────────────

  ;; :swap-pane-forward (p0 active) and :swap-pane-backward (p1 active) both
  ;; move p1 to first position and mark *dirty*.
  (it "dispatch-swap-pane-table"
    (dolist (row '((:swap-pane-forward  nil "forward: p0 active")
                   (:swap-pane-backward t   "backward: p1 active")))
      (destructuring-bind (cmd select-p1 desc) row
        (declare (ignore desc))
        (with-two-pane-h-session (sess win p0 p1)
          (when select-p1 (window-select-pane win p1))
          (cl-tmux::dispatch-command sess cmd nil)
          (expect (eq p1 (first  (window-panes win))))
          (expect (eq p0 (second (window-panes win))))
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;;; ── :kill-pane-confirm dispatch ──────────────────────────────────────────────

  ;; :kill-pane-confirm opens a prompt; y kills the active pane; n leaves it.
  (it "dispatch-kill-pane-confirm-table"
    (dolist (c '((nil 2 "no answer: pane count unchanged")
                 ("y" 1 "y: active pane killed")
                 ("n" 2 "n: pane preserved")))
      (destructuring-bind (answer expected-count desc) c
        (declare (ignore desc))
        (with-fake-two-pane-session (s)
          (let ((*prompt* nil))
            (cl-tmux::dispatch-command s :kill-pane-confirm nil)
            (expect (prompt-active-p))
            (expect (prompt-single-key *prompt*) :to-be-truthy)
            (when answer
              (cl-tmux::handle-prompt-key (char-code (char answer 0)))
              (expect (prompt-active-p) :to-be-falsy))
            (expect (= expected-count
                       (length (window-panes (session-active-window s))))))))))

  ;;; ── :kill-window-confirm dispatch ────────────────────────────────────────────

  ;; :kill-window-confirm opens a prompt; y kills the active window; n leaves it.
  (it "dispatch-kill-window-confirm-table"
    (dolist (c '((nil 2 "no answer: window count unchanged")
                 ("y" 1 "y: active window killed")
                 ("n" 2 "n: window preserved")))
      (destructuring-bind (answer expected-count desc) c
        (declare (ignore desc))
        (with-fake-session (s :nwindows 2)
          (let ((*prompt* nil))
            (cl-tmux::dispatch-command s :kill-window-confirm nil)
            (expect (prompt-active-p))
            (expect (prompt-single-key *prompt*) :to-be-truthy)
            (when answer
              (cl-tmux::handle-prompt-key (char-code (char answer 0)))
              (expect (prompt-active-p) :to-be-falsy))
            (expect (= expected-count (length (session-windows s)))))))))

  ;; :kill-window-confirm prompt label includes the current window name.
  (it "dispatch-kill-window-confirm-prompt-includes-window-name"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil))
        (let ((wname (window-name (session-active-window s))))
          (cl-tmux::dispatch-command s :kill-window-confirm nil)
          (expect (prompt-active-p))
          (expect (search wname (prompt-label *prompt*)))))))

  ;;; ── :send-prefix dispatch ────────────────────────────────────────────────────

  ;; :send-prefix command is registered in dispatch-command without error.
  (it "dispatch-send-prefix-command-is-defined"
    ;; We cannot test actual PTY writes in unit tests (fd=-1), but we verify
    ;; that dispatching :send-prefix does not signal any error and marks dirty.
    (with-fake-session (s)
      ;; Should not error even with fd=-1 (the guard (> fd 0) protects the write).
      (finishes (cl-tmux::dispatch-command s :send-prefix nil))
      (expect cl-tmux::*dirty* :to-be-truthy)))

  ;; :send-prefix does not inject the prefix byte when the client is read-only.
  (it "dispatch-send-prefix-read-only-does-not-write"
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
                 (expect (null writes)))
            (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

  ;;; ── unbound prefix key no-op ─────────────────────────────────────────────────

  ;; An unrecognized prefix key is silently discarded (no passthrough corruption).
  (it "dispatch-unknown-command-is-noop"
    ;; Previously the otherwise clause called %passthrough-prefix, injecting
    ;; raw bytes into the pane.  After the fix it must be a silent no-op.
    (with-fake-session (s)
      ;; Dispatching an unknown command must return NIL and must not error.
      (expect (null (cl-tmux::dispatch-command s :no-such-command-xyz nil)))
      (expect cl-tmux::*dirty* :to-be-truthy)))

  ;;; ── :paste-buffer bracketed-paste wrapping ───────────────────────────────────

  ;; :paste-buffer with an empty paste buffer is a no-op (no error, marks dirty).
  (it "dispatch-paste-buffer-no-crash-without-buffer"
    (with-fake-session (s)
      (finishes (cl-tmux::dispatch-command s :paste-buffer nil))
      (expect cl-tmux::*dirty* :to-be-truthy))))
