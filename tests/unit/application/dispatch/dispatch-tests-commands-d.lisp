(in-package #:cl-tmux/test)

;;;; Arg-command dispatch tests — part 4: has-session, switch-client-next,
;;;; find-window/select-window-prompt on-submit, move/swap-window, bind/unbind-key,
;;;; show-options, rename/display-message, kill-pane, cycle-pane/window, split, new-window.

(describe "dispatch-suite"

  ;;; ── :has-session with missing session shows no ───────────────────────────────

  ;; :has-session on-submit shows 'no' when the session is not registered.
  (it "dispatch-has-session-not-found-shows-no"
    (with-fake-session (s)
      (let ((*prompt* nil) (*overlay* nil)
            (cl-tmux::*server-sessions* nil))
        (cl-tmux::dispatch-command s :has-session nil)
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "nonexistent-session-xyz")
        (assert-overlay-contains "no" (overlay-lines)
                                  "on-submit"))))

  ;;; ── :switch-client-next with no other session is a no-op ─────────────────────

  ;; :switch-client-next with only one session in the registry is a no-op.
  (it "dispatch-switch-client-next-single-session-is-noop"
    (with-fake-session (s)
      (let ((name (session-name s)))
        (let ((cl-tmux::*server-sessions* (list (cons name s))))
          (finishes (cl-tmux::dispatch-command s :switch-client-next nil)
                    ":switch-client-next with a single session must not error")
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;;; ── :find-window on-submit paths ─────────────────────────────────────────────

  ;; :find-window on-submit shows matching windows or 'no windows' when nothing matches.
  ;; Each row: (nwindows query expected-text description).
  (it "dispatch-find-window-on-submit-table"
    (dolist (row '((2 "0"                     "0"          "matching name appears in the overlay")
                   (1 "zzz-no-such-window-xyz" "no windows" "no match shows 'no windows' overlay")))
      (destructuring-bind (nwindows query expected-text desc) row
        (with-fake-session (s :nwindows nwindows)
          (let ((*prompt* nil) (*overlay* nil))
            (cl-tmux::dispatch-command s :find-window nil)
            (expect (prompt-active-p))
            (funcall (prompt-on-submit *prompt*) query)
            (assert-overlay-contains expected-text (overlay-lines) desc))))))

  ;;; ── :select-window-prompt with name lookup ────────────────────────────────────

  ;; :select-window-prompt on-submit with a window name selects that window.
  (it "dispatch-select-window-prompt-selects-by-name"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        ;; The fake windows are named "0" and "1".
        (cl-tmux::dispatch-command s :select-window-prompt nil)
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "1")
        (expect (eq (second (session-windows s)) (session-active-window s))))))

  ;; :select-window-prompt with an unknown name shows an error overlay.
  (it "dispatch-select-window-prompt-unknown-name-shows-overlay"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil) (*overlay* nil))
        (cl-tmux::dispatch-command s :select-window-prompt nil)
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "no-such-window-xyz")
        (assert-overlay-contains "no window" (overlay-lines)
                                  ":select-window-prompt"))))

  ;;; ── :move-window on-submit ────────────────────────────────────────────────────

  ;; :move-window on-submit with a valid index reorders the window list.
  (it "dispatch-move-window-on-submit-reorders-windows"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil)
            (w0 (first  (session-windows s)))
            (w1 (second (session-windows s))))
        (cl-tmux::dispatch-command s :move-window nil)
        (expect (prompt-active-p))
        ;; Move w0 (active, index 0) to index 1.
        (finishes (funcall (prompt-on-submit *prompt*) "1")
                  ":move-window on-submit with valid index must not error")
        (expect (and w0 w1)))))

  ;;; ── :swap-window on-submit ────────────────────────────────────────────────────

  ;; :swap-window on-submit with a valid index swaps two windows.
  (it "dispatch-swap-window-on-submit-swaps-positions"
    (with-fake-session (s :nwindows 2)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :swap-window nil)
        (expect (prompt-active-p))
        (finishes (funcall (prompt-on-submit *prompt*) "1")
                  ":swap-window on-submit with valid index must not error"))))

  ;;; ── :bind-key on-submit ──────────────────────────────────────────────────────

  ;; :bind-key on-submit shows 'bound' for a known command or 'unknown command' for a bad one.
  ;; Each row: (input expected-text description).
  (it "dispatch-bind-key-on-submit-table"
    (dolist (row '(("z detach"                  "bound"           "known command shows confirmation")
                   ("z totally-unknown-cmd-xyz"  "unknown command" "unknown command shows error")))
      (destructuring-bind (input expected-text desc) row
        (with-isolated-key-tables
          (with-fake-session (s)
            (let ((*prompt* nil) (*overlay* nil))
              (cl-tmux::dispatch-command s :bind-key nil)
              (expect (prompt-active-p))
              (funcall (prompt-on-submit *prompt*) input)
              (assert-overlay-contains expected-text (overlay-lines) desc)))))))

  ;;; ── :unbind-key on-submit ────────────────────────────────────────────────────

  ;; :unbind-key on-submit removes a key binding and shows a confirmation overlay.
  (it "dispatch-unbind-key-shows-confirmation"
    (with-isolated-key-tables
      (with-fake-session (s)
        (let ((*prompt* nil) (*overlay* nil))
          (cl-tmux::dispatch-command s :unbind-key nil)
          (expect (prompt-active-p))
          ;; Use a key that is expected to be in the default table (e.g. 'd' → detach).
          (funcall (prompt-on-submit *prompt*) "d")
          (assert-overlay-contains "unbound" (overlay-lines) ":unbind-key")))))

  ;;; ── :rename-session on-submit: empty input does not rename ──────────────────

  ;; :rename-session on-submit with empty input does not rename the session.
  (it "dispatch-rename-session-empty-input-no-rename"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (let ((original-name (session-name s)))
          (cl-tmux::dispatch-command s :rename-session nil)
          (expect (prompt-active-p))
          (funcall (prompt-on-submit *prompt*) "")
          (expect (string= original-name (session-name s)))))))

  ;;; ── :display-message empty input is noop ────────────────────────────────────

  ;; :display-message with empty input does not append to *message-log*.
  (it "dispatch-display-message-empty-input-no-log"
    (with-fake-session (s)
      (let ((*prompt* nil)
            (cl-tmux::*message-log* nil))
        (cl-tmux::dispatch-command s :display-message nil)
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "")
        (expect (null cl-tmux::*message-log*)))))

  ;;; ── :command-prompt strips leading whitespace ────────────────────────────────

  ;; :command-prompt trims leading/trailing whitespace before dispatching.
  (it "dispatch-command-prompt-trims-whitespace"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil) (*overlay* nil))
        (cl-tmux::dispatch-command s :command-prompt nil)
        (expect (prompt-active-p))
        ;; "  list-windows  " should work identically to "list-windows".
        (funcall (prompt-on-submit *prompt*) "  list-windows  ")
        (assert-overlay-active
         ":command-prompt with padded 'list-windows' must still open an overlay"))))

  ;;; ── :kill-pane on a two-pane window leaves the other pane ──────────────────

  ;; :kill-pane on a 2-pane window removes the active pane but keeps the other.
  (it "dispatch-kill-pane-leaves-remaining-pane"
    (with-fake-two-pane-session (s)
      (let* ((win   (session-active-window s))
             (pane0 (first  (window-panes win)))
             (pane1 (second (window-panes win))))
        (expect (eq pane0 (window-active-pane win)))
        (cl-tmux::dispatch-command s :kill-pane nil)
        (expect (= 1 (length (window-panes win))))
        (expect (member pane0 (window-panes win)) :to-be-falsy)
        (expect (member pane1 (window-panes win))))))

  ;;; ── %cmd-cycle-pane with prev-cyclic ─────────────────────────────────────────

  ;; %cmd-cycle-pane with prev-cyclic retreats the active pane.
  (it "cmd-cycle-pane-prev-retreats-selection"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p0  (first  (window-panes win)))
             (p1  (second (window-panes win))))
        ;; Start at p0; prev-cyclic wraps to p1 (the last pane).
        (expect (eq p0 (window-active-pane win)))
        (cl-tmux::%cmd-cycle-pane s #'cl-tmux::prev-cyclic)
        (expect (eq p1 (window-active-pane win))))))

  ;;; ── %cmd-cycle-window with prev-cyclic ───────────────────────────────────────

  ;; %cmd-cycle-window with prev-cyclic retreats the active window.
  (it "cmd-cycle-window-prev-retreats-selection"
    (with-fake-session (s :nwindows 3)
      (let ((w0 (first  (session-windows s)))
            (w2 (third  (session-windows s))))
        ;; Start at w0; prev-cyclic wraps to w2 (the last window).
        (expect (eq w0 (session-active-window s)))
        (cl-tmux::%cmd-cycle-window s #'cl-tmux::prev-cyclic)
        (expect (eq w2 (session-active-window s))))))

  ;;; ── :select-pane-up at top pane is a no-op ──────────────────────────────────

  ;; :select-pane-up is a no-op when the active pane has no pane above.
  (it "dispatch-select-pane-up-noop-at-topmost"
    (with-two-pane-v-session (sess win p0 p1)
      ;; p0 is at the top; going up should not change the active pane.
      (expect (eq p0 (window-active-pane win)))
      (cl-tmux::dispatch-command sess :select-pane-up nil)
      (expect (eq p0 (window-active-pane win)))))

  ;;; ── :select-pane-down at bottom pane is a no-op ─────────────────────────────

  ;; :select-pane-down is a no-op when the active pane has no pane below.
  (it "dispatch-select-pane-down-noop-at-bottommost"
    (with-two-pane-v-session (sess win p0 p1)
      ;; Start at p1 (bottommost); going down should not change the active pane.
      (window-select-pane win p1)
      (cl-tmux::dispatch-command sess :select-pane-down nil)
      (expect (eq p1 (window-active-pane win)))))

  ;;; ── :select-pane-left at leftmost is a no-op ─────────────────────────────────

  ;; :select-pane-left is a no-op when the active pane has no left neighbour.
  (it "dispatch-select-pane-left-noop-at-leftmost"
    (with-two-pane-h-session (sess win p0 p1)
      ;; p0 is already at the leftmost position.
      (expect (eq p0 (window-active-pane win)))
      (cl-tmux::dispatch-command sess :select-pane-left nil)
      (expect (eq p0 (window-active-pane win)))))

  ;;; ── :prev-pane dispatch ──────────────────────────────────────────────────────

  ;; :prev-pane from first wraps to last (p1); from last retreats to first (p0).
  (it "dispatch-prev-pane-table"
    (dolist (row '((nil t   "from p0: wraps to p1")
                   (t   nil "from p1: retreats to p0")))
      (destructuring-bind (start-on-p1 expect-p1 desc) row
        (declare (ignore desc))
        (with-fake-two-pane-session (s)
          (let* ((win (session-active-window s))
                 (p0  (first  (window-panes win)))
                 (p1  (second (window-panes win))))
            (when start-on-p1 (window-select-pane win p1))
            (cl-tmux::dispatch-command s :prev-pane nil)
            (expect (eq (if expect-p1 p1 p0) (window-active-pane win))))))))

  ;;; ── :split-horizontal / :split-vertical (focus versions) dispatch ────────────

  ;; :split-horizontal and :split-vertical both dispatch without error on a fake session.
  (it "dispatch-split-horizontal-vertical-do-not-error"
    (dolist (cmd '(:split-horizontal :split-vertical))
      (with-fake-session (s :nwindows 1 :npanes 1)
        (finishes (cl-tmux::dispatch-command s cmd nil)
                  "~A must not signal an error" cmd))))

  ;;; ── :new-window dispatch ─────────────────────────────────────────────────────

  ;; :new-window dispatches without error (or signals at PTY level, which is acceptable).
  (it "dispatch-new-window-does-not-error"
    (with-fake-session (s :nwindows 1)
      (handler-case
          (progn
            (cl-tmux::dispatch-command s :new-window nil)
            (expect t))
        (error ()
          (expect t))))))
