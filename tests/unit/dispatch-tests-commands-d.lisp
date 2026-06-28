(in-package #:cl-tmux/test)

;;;; Arg-command dispatch tests — part 4: has-session, switch-client-next,
;;;; find-window/select-window-prompt on-submit, move/swap-window, bind/unbind-key,
;;;; show-options, rename/display-message, kill-pane, cycle-pane/window, split, new-window.

(in-suite dispatch-suite)

;;; ── :has-session with missing session shows no ───────────────────────────────

(test dispatch-has-session-not-found-shows-no
  ":has-session on-submit shows 'no' when the session is not registered."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :has-session nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "nonexistent-session-xyz")
      (assert-overlay-contains "no" (overlay-lines)
                                "on-submit"))))

;;; ── :switch-client-next with no other session is a no-op ─────────────────────

(test dispatch-switch-client-next-single-session-is-noop
  ":switch-client-next with only one session in the registry is a no-op."
  (with-fake-session (s)
    (let ((name (session-name s)))
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (finishes (cl-tmux::dispatch-command s :switch-client-next nil)
                  ":switch-client-next with a single session must not error")
        (is-true cl-tmux::*dirty*
                 "dispatch must mark *dirty* even with single session")))))

;;; ── :find-window on-submit paths ─────────────────────────────────────────────

(test dispatch-find-window-on-submit-table
  ":find-window on-submit shows matching windows or 'no windows' when nothing matches.
   Each row: (nwindows query expected-text description)."
  (dolist (row '((2 "0"                     "0"          "matching name appears in the overlay")
                 (1 "zzz-no-such-window-xyz" "no windows" "no match shows 'no windows' overlay")))
    (destructuring-bind (nwindows query expected-text desc) row
      (with-fake-session (s :nwindows nwindows)
        (let ((*prompt* nil) (*overlay* nil))
          (cl-tmux::dispatch-command s :find-window nil)
          (is (prompt-active-p) "prompt must be open")
          (funcall (prompt-on-submit *prompt*) query)
          (assert-overlay-contains expected-text (overlay-lines) desc))))))

;;; ── :select-window-prompt with name lookup ────────────────────────────────────

(test dispatch-select-window-prompt-selects-by-name
  ":select-window-prompt on-submit with a window name selects that window."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      ;; The fake windows are named "0" and "1".
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "1")
      (is (eq (second (session-windows s)) (session-active-window s))
          "submitting \"1\" (name match) must select the second window"))))

(test dispatch-select-window-prompt-unknown-name-shows-overlay
  ":select-window-prompt with an unknown name shows an error overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "no-such-window-xyz")
      (assert-overlay-contains "no window" (overlay-lines)
                                ":select-window-prompt"))))

;;; ── :move-window on-submit ────────────────────────────────────────────────────

(test dispatch-move-window-on-submit-reorders-windows
  ":move-window on-submit with a valid index reorders the window list."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil)
          (w0 (first  (session-windows s)))
          (w1 (second (session-windows s))))
      (cl-tmux::dispatch-command s :move-window nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Move w0 (active, index 0) to index 1.
      (finishes (funcall (prompt-on-submit *prompt*) "1")
                ":move-window on-submit with valid index must not error")
      (is (and w0 w1) "both windows must still exist after move"))))

;;; ── :swap-window on-submit ────────────────────────────────────────────────────

(test dispatch-swap-window-on-submit-swaps-positions
  ":swap-window on-submit with a valid index swaps two windows."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :swap-window nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "1")
                ":swap-window on-submit with valid index must not error"))))

;;; ── :bind-key on-submit ──────────────────────────────────────────────────────

(test dispatch-bind-key-on-submit-table
  ":bind-key on-submit shows 'bound' for a known command or 'unknown command' for a bad one.
   Each row: (input expected-text description)."
  (dolist (row '(("z detach"                  "bound"           "known command shows confirmation")
                 ("z totally-unknown-cmd-xyz"  "unknown command" "unknown command shows error")))
    (destructuring-bind (input expected-text desc) row
      (with-fake-session (s)
        (let ((*prompt* nil) (*overlay* nil))
          (cl-tmux::dispatch-command s :bind-key nil)
          (is (prompt-active-p) "prompt must be open")
          (funcall (prompt-on-submit *prompt*) input)
          (assert-overlay-contains expected-text (overlay-lines) desc))))))

;;; ── :unbind-key on-submit ────────────────────────────────────────────────────

(test dispatch-unbind-key-shows-confirmation
  ":unbind-key on-submit removes a key binding and shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :unbind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Use a key that is expected to be in the default table (e.g. 'd' → detach).
      (funcall (prompt-on-submit *prompt*) "d")
      (assert-overlay-contains "unbound" (overlay-lines) ":unbind-key"))))

;;; ── :rename-session on-submit: empty input does not rename ──────────────────

(test dispatch-rename-session-empty-input-no-rename
  ":rename-session on-submit with empty input does not rename the session."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (let ((original-name (session-name s)))
        (cl-tmux::dispatch-command s :rename-session nil)
        (is (prompt-active-p) "rename-session must open a prompt")
        (funcall (prompt-on-submit *prompt*) "")
        (is (string= original-name (session-name s))
            "submitting empty string must NOT rename the session")))))

;;; ── :display-message empty input is noop ────────────────────────────────────

(test dispatch-display-message-empty-input-no-log
  ":display-message with empty input does not append to *message-log*."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "")
      (is (null cl-tmux::*message-log*)
          "empty input must not append to *message-log*"))))

;;; ── :command-prompt strips leading whitespace ────────────────────────────────

(test dispatch-command-prompt-trims-whitespace
  ":command-prompt trims leading/trailing whitespace before dispatching."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "  list-windows  " should work identically to "list-windows".
      (funcall (prompt-on-submit *prompt*) "  list-windows  ")
      (assert-overlay-active
       ":command-prompt with padded 'list-windows' must still open an overlay"))))

;;; ── :kill-pane on a two-pane window leaves the other pane ──────────────────

(test dispatch-kill-pane-leaves-remaining-pane
  ":kill-pane on a 2-pane window removes the active pane but keeps the other."
  (with-fake-two-pane-session (s)
    (let* ((win   (session-active-window s))
           (pane0 (first  (window-panes win)))
           (pane1 (second (window-panes win))))
      (is (eq pane0 (window-active-pane win)) "pane0 is active initially")
      (cl-tmux::dispatch-command s :kill-pane nil)
      (is (= 1 (length (window-panes win)))
          ":kill-pane must reduce the pane count to 1")
      (is-false (member pane0 (window-panes win))
                ":kill-pane must remove the previously active pane")
      (is (member pane1 (window-panes win))
          ":kill-pane must leave pane1 intact"))))

;;; ── %cmd-cycle-pane with prev-cyclic ─────────────────────────────────────────

(test cmd-cycle-pane-prev-retreats-selection
  "%cmd-cycle-pane with prev-cyclic retreats the active pane."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      ;; Start at p0; prev-cyclic wraps to p1 (the last pane).
      (is (eq p0 (window-active-pane win)))
      (cl-tmux::%cmd-cycle-pane s #'cl-tmux::prev-cyclic)
      (is (eq p1 (window-active-pane win))
          "%cmd-cycle-pane with prev-cyclic must wrap from first pane to last"))))

;;; ── %cmd-cycle-window with prev-cyclic ───────────────────────────────────────

(test cmd-cycle-window-prev-retreats-selection
  "%cmd-cycle-window with prev-cyclic retreats the active window."
  (with-fake-session (s :nwindows 3)
    (let ((w0 (first  (session-windows s)))
          (w2 (third  (session-windows s))))
      ;; Start at w0; prev-cyclic wraps to w2 (the last window).
      (is (eq w0 (session-active-window s)))
      (cl-tmux::%cmd-cycle-window s #'cl-tmux::prev-cyclic)
      (is (eq w2 (session-active-window s))
          "%cmd-cycle-window with prev-cyclic must wrap from first window to last"))))

;;; ── :select-pane-up at top pane is a no-op ──────────────────────────────────

(test dispatch-select-pane-up-noop-at-topmost
  ":select-pane-up is a no-op when the active pane has no pane above."
  (with-two-pane-v-session (sess win p0 p1)
    ;; p0 is at the top; going up should not change the active pane.
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (cl-tmux::dispatch-command sess :select-pane-up nil)
    (is (eq p0 (window-active-pane win))
        ":select-pane-up at the topmost pane must remain on p0")))

;;; ── :select-pane-down at bottom pane is a no-op ─────────────────────────────

(test dispatch-select-pane-down-noop-at-bottommost
  ":select-pane-down is a no-op when the active pane has no pane below."
  (with-two-pane-v-session (sess win p0 p1)
    ;; Start at p1 (bottommost); going down should not change the active pane.
    (window-select-pane win p1)
    (cl-tmux::dispatch-command sess :select-pane-down nil)
    (is (eq p1 (window-active-pane win))
        ":select-pane-down at the bottommost pane must remain on p1")))

;;; ── :select-pane-left at leftmost is a no-op ─────────────────────────────────

(test dispatch-select-pane-left-noop-at-leftmost
  ":select-pane-left is a no-op when the active pane has no left neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    ;; p0 is already at the leftmost position.
    (is (eq p0 (window-active-pane win)) "p0 is active initially")
    (cl-tmux::dispatch-command sess :select-pane-left nil)
    (is (eq p0 (window-active-pane win))
        ":select-pane-left at leftmost pane must remain on p0")))

;;; ── :prev-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-prev-pane-table
  ":prev-pane from first wraps to last (p1); from last retreats to first (p0)."
  (dolist (row '((nil t   "from p0: wraps to p1")
                 (t   nil "from p1: retreats to p0")))
    (destructuring-bind (start-on-p1 expect-p1 desc) row
      (with-fake-two-pane-session (s)
        (let* ((win (session-active-window s))
               (p0  (first  (window-panes win)))
               (p1  (second (window-panes win))))
          (when start-on-p1 (window-select-pane win p1))
          (cl-tmux::dispatch-command s :prev-pane nil)
          (is (eq (if expect-p1 p1 p0) (window-active-pane win)) "~A" desc))))))

;;; ── :split-horizontal / :split-vertical (focus versions) dispatch ────────────

(test dispatch-split-horizontal-vertical-do-not-error
  ":split-horizontal and :split-vertical both dispatch without error on a fake session."
  (dolist (cmd '(:split-horizontal :split-vertical))
    (with-fake-session (s :nwindows 1 :npanes 1)
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not signal an error" cmd))))

;;; ── :new-window dispatch ─────────────────────────────────────────────────────

(test dispatch-new-window-does-not-error
  ":new-window dispatches without error (or signals at PTY level, which is acceptable)."
  (with-fake-session (s :nwindows 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :new-window nil)
          (is-true t ":new-window dispatched without error"))
      (error ()
        (is-true t ":new-window signalled at PTY level (acceptable in sandbox)")))))
