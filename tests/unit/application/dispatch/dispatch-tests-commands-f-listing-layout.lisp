(in-package #:cl-tmux/test)

;;;; Dispatch tests — part F2: tree/session listing and window layout helpers.

(in-suite dispatch-suite)

;;; ── %format-tree-entry helper ────────────────────────────────────────────────

(test format-tree-entry-current-and-non-current-prefix
  "%format-tree-entry uses '* ' for the current session and '  ' for others."
  ;; Current session: marked with asterisk and includes window name.
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen))
         (win    (make-window :id 0 :name "test-win" :width 20 :height 5
                              :panes (list pane)
                              :tree  (make-layout-leaf pane))))
    (window-select-pane win pane)
    (let ((output (with-output-to-string (s)
                    (cl-tmux::%format-tree-entry s "mysess" "mysess"
                                                (list win) win))))
      (is (search "* mysess" output)
          "current session must be marked with '* ' prefix")
      (is (search "test-win" output)
          "window name must appear in the output")))
  ;; Non-current session: space prefix, no asterisk.
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen))
         (win    (make-window :id 0 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree  (make-layout-leaf pane))))
    (window-select-pane win pane)
    (let ((output (with-output-to-string (s)
                    (cl-tmux::%format-tree-entry s "other" "current"
                                                (list win) win))))
      (is-false (search "* other" output)
                "non-current session must not start with '* '")
      (is (search "  other" output)
          "non-current session must start with '  '"))))

;;; ── :choose-session / :list-sessions-full aliases ────────────────────────────

(test dispatch-choose-session-shows-session-list
  ":choose-session shows the session list overlay (same body as :list-sessions)."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :choose-session nil)
      (assert-overlay-contains (session-name s) *overlay*
                               ":choose-session"))))

(test dispatch-list-sessions-full-shows-session-list
  ":list-sessions-full shows the session list overlay."
  (with-dispatch-overlay (s :list-sessions-full
                            :context ":list-sessions-full must open an overlay")
    (assert-overlay-active ":list-sessions-full must open an overlay")))

;;; ── :resize-left/:resize-right/:resize-up/:resize-down dispatch ──────────────

(test dispatch-resize-commands-do-not-error
  "The four resize commands dispatch without signalling an error."
  (with-fake-session (s)
    (dolist (cmd '(:resize-left :resize-right :resize-up :resize-down))
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not signal an error" cmd))))

;;; ── :rotate-window / :rotate-window-reverse / :split-*-no-focus dispatch ────

(test dispatch-rotate-and-split-no-focus-do-not-error
  "rotate-window, rotate-window-reverse, and the no-focus split variants
   dispatch without error.  Each command gets a fresh session so that the
   reader thread started by a split does not block the next fork."
  (dolist (cmd '(:rotate-window :rotate-window-reverse
                 :split-horizontal-no-focus :split-vertical-no-focus))
    (with-fake-session (s :nwindows 1 :npanes 1)
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not signal an error" cmd))))
