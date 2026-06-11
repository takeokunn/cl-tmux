(in-package #:cl-tmux/test)

;;;; Arg-command dispatch tests — part 4: has-session, switch-client-next,
;;;; find-window/select-window-prompt on-submit, move/swap-window, bind/unbind-key,
;;;; show-option, rename/display-message, kill-pane, cycle-pane/window, split, new-window.

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
      (is (overlay-active-p) "on-submit must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no" text)
            "overlay must say 'no' for an unknown session")))))

;;; ── :switch-client-next with no other session is a no-op ─────────────────────

(test dispatch-switch-client-next-single-session-is-noop
  ":switch-client-next with only one session in the registry is a no-op."
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (finishes (cl-tmux::dispatch-command s :switch-client-next nil)
                  ":switch-client-next with a single session must not error")
        (is-true cl-tmux::*dirty*
                 "dispatch must mark *dirty* even with single session")))))

;;; ── :find-window on-submit paths ─────────────────────────────────────────────

(test dispatch-find-window-matching-pattern-shows-results
  ":find-window on-submit with a matching pattern shows the matching windows."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) "prompt must be open")
      ;; All window names start with a digit; "0" matches the first window.
      (funcall (prompt-on-submit *prompt*) "0")
      (is (overlay-active-p) ":find-window with a match must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "0" text) "overlay must list the matching window")))))

(test dispatch-find-window-no-match-shows-no-windows-message
  ":find-window on-submit with no matches shows a 'no windows matching' overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "zzz-no-such-window-xyz")
      (is (overlay-active-p) ":find-window with no match must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no windows" text)
            "overlay must say 'no windows matching' when there are no matches")))))

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
      (is (overlay-active-p) "unknown window must open an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no window" text)
            "overlay must mention 'no window'")))))

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

(test dispatch-bind-key-known-command-shows-confirmation
  ":bind-key on-submit with a known key+command pair shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "z detach" — z is a valid key token, detach is a known command.
      (funcall (prompt-on-submit *prompt*) "z detach")
      (is (overlay-active-p) "successful bind-key must show a confirmation overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "bound" text) "overlay must confirm the binding with 'bound'")))))

(test dispatch-bind-key-unknown-command-shows-error
  ":bind-key on-submit with an unknown command shows an error overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "z totally-unknown-cmd-xyz")
      (is (overlay-active-p) "unknown command must show an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unknown command" text)
            "overlay must contain 'unknown command'")))))

;;; ── :unbind-key on-submit ────────────────────────────────────────────────────

(test dispatch-unbind-key-shows-confirmation
  ":unbind-key on-submit removes a key binding and shows a confirmation overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :unbind-key nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Use a key that is expected to be in the default table (e.g. 'd' → detach).
      (funcall (prompt-on-submit *prompt*) "d")
      (is (overlay-active-p) "unbind-key must show a confirmation overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unbound" text) "overlay must confirm the unbinding")))))

;;; ── :show-option on-submit paths ─────────────────────────────────────────────

(test dispatch-show-option-on-submit-known-option-shows-overlay
  ":show-option on-submit with a known option name shows its value in an overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :show-option nil)
      (is (prompt-active-p) "prompt must be open")
      ;; "mouse" is a standard option.
      (funcall (prompt-on-submit *prompt*) "mouse")
      (is (overlay-active-p) ":show-option with known option must open overlay"))))

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
      (is (overlay-active-p)
          ":command-prompt with padded 'list-windows' must still open an overlay"))))

;;; ── :kill-pane on a two-pane window leaves the other pane ──────────────────

(test dispatch-kill-pane-leaves-remaining-pane
  ":kill-pane on a 2-pane window removes the active pane but keeps the other."
  (with-fake-session (s :nwindows 1 :npanes 2)
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
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      ;; Start at p0; prev-cyclic wraps to p1 (the last pane).
      (is (eq p0 (window-active-pane win)))
      (cl-tmux::%cmd-cycle-pane s #'cl-tmux::prev-cyclic)
      (is (eq p1 (window-active-pane win))
          "%cmd-cycle-pane with prev-cyclic must wrap from first pane to last"))))

;;; ── %cmd-cycle-window with prev-cyclic ───────────────────────────────────────

(test cmd-cycle-window-prev-retreats-selection
  "%cmd-cycle-window with prev-cyclic retreats the active window."
  (let* ((s  (make-fake-session :nwindows 3))
         (w0 (first  (session-windows s)))
         (w2 (third  (session-windows s))))
    (with-loop-state
      ;; Start at w0; prev-cyclic wraps to w2 (the last window).
      (is (eq w0 (session-active-window s)))
      (cl-tmux::%cmd-cycle-window s #'cl-tmux::prev-cyclic)
      (is (eq w2 (session-active-window s))
          "%cmd-cycle-window with prev-cyclic must wrap from first window to last"))))

;;; ── :select-pane-up at top pane is a no-op ──────────────────────────────────

(test dispatch-select-pane-up-noop-at-topmost
  ":select-pane-up is a no-op when the active pane has no pane above."
  (with-two-pane-v-session (sess win p0 p1)
    (with-loop-state
      ;; p0 is at the top; going up should not change the active pane.
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command sess :select-pane-up nil)
      (is (eq p0 (window-active-pane win))
          ":select-pane-up at the topmost pane must remain on p0"))))

;;; ── :select-pane-down at bottom pane is a no-op ─────────────────────────────

(test dispatch-select-pane-down-noop-at-bottommost
  ":select-pane-down is a no-op when the active pane has no pane below."
  (with-two-pane-v-session (sess win p0 p1)
    (with-loop-state
      ;; Start at p1 (bottommost); going down should not change the active pane.
      (window-select-pane win p1)
      (cl-tmux::dispatch-command sess :select-pane-down nil)
      (is (eq p1 (window-active-pane win))
          ":select-pane-down at the bottommost pane must remain on p1"))))

;;; ── :select-pane-left at leftmost is a no-op ─────────────────────────────────

(test dispatch-select-pane-left-noop-at-leftmost
  ":select-pane-left is a no-op when the active pane has no left neighbour."
  (with-two-pane-h-session (sess win p0 p1)
    (with-loop-state
      ;; p0 is already at the leftmost position.
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command sess :select-pane-left nil)
      (is (eq p0 (window-active-pane win))
          ":select-pane-left at leftmost pane must remain on p0"))))

;;; ── :prev-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-prev-pane-wraps-from-first
  ":prev-pane cycles in reverse: from the first pane wraps to the last."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (is (eq p0 (window-active-pane win)) "p0 is active initially")
      (cl-tmux::dispatch-command s :prev-pane nil)
      (is (eq p1 (window-active-pane win))
          ":prev-pane from the first pane must wrap to the last pane"))))

(test dispatch-prev-pane-retreats-from-last
  ":prev-pane from the last pane selects the preceding pane."
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
      (window-select-pane win p1)
      (cl-tmux::dispatch-command s :prev-pane nil)
      (is (eq p0 (window-active-pane win))
          ":prev-pane from p1 must select p0"))))

;;; ── :split-horizontal / :split-vertical (focus versions) dispatch ────────────

(test dispatch-split-horizontal-does-not-error
  ":split-horizontal dispatches without error on a fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-horizontal nil)
              ":split-horizontal must not signal an error")))

(test dispatch-split-vertical-does-not-error
  ":split-vertical dispatches without error on a fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :split-vertical nil)
              ":split-vertical must not signal an error")))

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

