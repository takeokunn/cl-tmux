(in-package #:cl-tmux/test)

;;;; Dispatch tests — part F (from commands): display-message-logs, clock-mode,
;;;; capture-pane, send-keys, choose-tree, set-window/session-option, confirm-before,
;;;; set-option-from-prompt, paste-to-pane, format-tree-entry, choose-session/list-sessions-full,
;;;; resize/rotate, split-horizontal/vertical (no-focus).

(in-suite dispatch-suite)

;;; ── :display-message logs to *message-log* ───────────────────────────────────

(test dispatch-display-message-logs-to-message-log
  ":display-message on-submit calls add-message-log, appending the message."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) ":display-message must open a prompt")
      ;; Submit a non-empty message.
      (funcall (prompt-on-submit *prompt*) "test-log-entry")
      (is-false (null cl-tmux::*message-log*)
                "*message-log* must be non-nil after submitting a message")
      (let ((last-msg (cdr (first cl-tmux::*message-log*))))
        (is (string= "test-log-entry" last-msg)
            "first message-log entry must be the submitted message (got ~S)" last-msg)))))

;;; ── :clock-mode dispatch ─────────────────────────────────────────────────────

(test dispatch-clock-mode-toggles-pane-id
  ":clock-mode sets *clock-mode-pane-id* to the active pane's id."
  (with-fake-session (s)
    (let ((cl-tmux::*clock-mode-pane-id* nil))
      (cl-tmux::dispatch-command s :clock-mode nil)
      (let ((ap (session-active-pane s)))
        (is (eql (pane-id ap) cl-tmux::*clock-mode-pane-id*)
            "*clock-mode-pane-id* must be set to active pane id after first :clock-mode")
        ;; Toggle off
        (cl-tmux::dispatch-command s :clock-mode nil)
        (is (null cl-tmux::*clock-mode-pane-id*)
            "*clock-mode-pane-id* must be nil after second :clock-mode (toggle off)")))))

;;; ── :capture-pane dispatch ───────────────────────────────────────────────────

(test dispatch-capture-pane-shows-overlay
  ":capture-pane opens an overlay containing the pane content."
  (with-fake-session (s)
    (let ((*overlay* nil))
      ;; Feed some text into the active pane's screen.
      (let ((ap (session-active-pane s)))
        (when ap
          (feed (pane-screen ap) "CAPTEST")))
      (cl-tmux::dispatch-command s :capture-pane nil)
      (is (overlay-active-p) ":capture-pane must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "CAPTEST" text)
            "capture-pane overlay must contain the pane's fed content")))))

;;; ── :send-keys dispatch ──────────────────────────────────────────────────────

(test dispatch-send-keys-opens-prompt
  ":send-keys opens a prompt for the keys string."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :send-keys nil)
      (is (prompt-active-p) ":send-keys must open a prompt")
      (is (string= "send-keys" (prompt-label *prompt*))
          ":send-keys prompt label must be \"send-keys\""))))

(test dispatch-send-keys-no-crash-with-no-pty
  ":send-keys on-submit with a no-PTY pane (fd=-1) does not signal an error."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :send-keys nil)
      (is (prompt-active-p) "prompt must be open")
      ;; Submitting keys to fd=-1 pane must not error.
      (finishes (funcall (prompt-on-submit *prompt*) "hello")
                "send-keys on-submit must not error with fd=-1 pane"))))

;;; ── :choose-tree dispatch ────────────────────────────────────────────────────

(test dispatch-choose-tree-shows-overlay
  ":choose-tree opens an overlay with session and window entries."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :choose-tree nil)
      (is (overlay-active-p) ":choose-tree must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search (session-name s) text)
            "overlay must contain the session name")))))

(test dispatch-choose-tree-with-server-sessions
  ":choose-tree with multiple server sessions lists them all."
  (with-fake-session (s1 :nwindows 1)
    (let* ((s2  (make-fake-session :nwindows 2))
           (reg (list (cons (session-name s1) s1)
                      (cons (session-name s2) s2))))
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* reg))
        (cl-tmux::dispatch-command s1 :choose-tree nil)
        (is (overlay-active-p) ":choose-tree must open an overlay with server sessions")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search (session-name s1) text)
              "overlay must contain session 1 name")
          (is (search (session-name s2) text)
              "overlay must contain session 2 name"))))))

;;; ── :set-window-option dispatch ──────────────────────────────────────────────

(test dispatch-set-window-option-opens-prompt
  ":set-window-option opens a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-window-option nil)
      (is (prompt-active-p) ":set-window-option must open a prompt")
      (is (string= "set-window-option" (prompt-label *prompt*))
          ":set-window-option prompt label must be \"set-window-option\""))))

;;; ── :set-session-option dispatch ─────────────────────────────────────────────

(test dispatch-set-session-option-opens-prompt
  ":set-session-option opens a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-session-option nil)
      (is (prompt-active-p) ":set-session-option must open a prompt")
      (is (string= "set-session-option" (prompt-label *prompt*))
          ":set-session-option prompt label must be \"set-session-option\""))))

;;; ── :confirm-before dispatch ─────────────────────────────────────────────────
;;;
;;; confirm-before is implemented in dispatch.lisp as a prompt with an
;;; on-submit lambda.  The helper below eliminates repetition across the input
;;; variants.

(defmacro with-confirm-before-prompt ((on-submit-var) &body body)
  "Activate a confirm-before prompt in an isolated environment and bind
   ON-SUBMIT-VAR to the prompt's on-submit function, then execute BODY."
  `(with-fake-session (sess)
     (with-clean-prompt
       (let ((*overlay* nil))
         (cl-tmux::dispatch-command sess :confirm-before nil)
         (is-true (prompt-active-p)
                  "dispatch-command :confirm-before must activate a prompt")
         (let ((,on-submit-var (prompt-on-submit *prompt*)))
           ,@body)))))

(test confirm-before-input-table
  ":confirm-before on-submit: \"y\" shows an overlay; \"n\" and \"\" do not."
  (dolist (row '(("y"  t   "y confirms → overlay visible")
                 ("n"  nil "n cancels → no overlay")
                 (""   nil "empty string cancels → no overlay")))
    (destructuring-bind (input overlay-p desc) row
      (with-confirm-before-prompt (on-submit)
        (funcall on-submit input)
        (is (if overlay-p (overlay-active-p) (null (overlay-active-p))) "~A" desc)))))

(test confirm-before-arg-is-single-key-and-y-runs-command
  "confirm-before COMMAND opens a SINGLE-KEY prompt: one 'y' keypress (no Enter)
   runs the command."
  (with-isolated-config
    (with-fake-session (s)
      (let ((cl-tmux/prompt:*prompt* nil))
        (cl-tmux::%cmd-confirm-before-arg s '("set" "-g" "status-left" "YES"))
        (is (prompt-active-p) "confirm-before must open a prompt")
        (is-true (prompt-single-key *prompt*) "the prompt must be single-key")
        (cl-tmux::handle-prompt-key (char-code #\y))   ; single key, no Enter
        (is (null (prompt-active-p)) "prompt must dismiss after the single key")
        (is (string= "YES" (cl-tmux/options:get-option "status-left"))
            "'y' must run the confirmed command")))))

(test confirm-before-arg-single-key-other-cancels
  "A non-y single key cancels confirm-before without running the command."
  (with-isolated-config
    (with-fake-session (s)
      (let ((cl-tmux/prompt:*prompt* nil))
        (cl-tmux/options:set-option "status-left" "ORIG")
        (cl-tmux::%cmd-confirm-before-arg s '("set" "-g" "status-left" "YES"))
        (cl-tmux::handle-prompt-key (char-code #\n))   ; 'n' cancels
        (is (null (prompt-active-p)) "prompt must dismiss on a non-y key")
        (is (string= "ORIG" (cl-tmux/options:get-option "status-left"))
            "a non-y key must NOT run the command")))))

(test command-prompt-1-single-key-substitutes-one-keypress
  "command-prompt -1 -p k: 'set -g status-left %1' is a single-key prompt: one
   keypress (no Enter) is substituted for %1 and the command runs."
  (with-isolated-config
    (with-fake-session (s)
      (let ((cl-tmux/prompt:*prompt* nil))
        (cl-tmux::%cmd-command-prompt-arg
         s '("-1" "-p" "k:" "set -g status-left %1"))
        (is (prompt-active-p) "command-prompt -1 must open a prompt")
        (is-true (prompt-single-key *prompt*) "the prompt must be single-key")
        (cl-tmux::handle-prompt-key (char-code #\Z))   ; one key, no Enter
        (is (null (prompt-active-p)) "prompt must dismiss after one key")
        (is (string= "Z" (cl-tmux/options:get-option "status-left"))
            "%1 must be substituted with the single keypress 'Z'")))))

;;; ── %set-option-from-prompt helper ──────────────────────────────────────────

(test set-option-from-prompt-helper-opens-prompt
  "%set-option-from-prompt opens a prompt with the given label."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%set-option-from-prompt "test-label")
      (is (prompt-active-p) "%set-option-from-prompt must open a prompt")
      (is (string= "test-label" (prompt-label *prompt*))
          "%set-option-from-prompt prompt label must match the argument"))))

(test set-option-from-prompt-sets-option
  "%set-option-from-prompt on-submit with 'name value' calls set-option."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%set-option-from-prompt "set-window-option")
      (is (prompt-active-p) "prompt must be open")
      ;; Submit "mouse on" which maps to set-option "mouse" "on"
      (finishes (funcall (prompt-on-submit *prompt*) "mouse on")
                "%set-option-from-prompt on-submit must not error"))))

;;; ── %paste-to-pane helper ────────────────────────────────────────────────────

(test paste-to-pane-no-crash-fd-minus-one
  "%paste-to-pane with fd=-1 pane is a no-op (guard skips the write)."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen)))
    (finishes (cl-tmux::%paste-to-pane pane "hello world")
              "%paste-to-pane must not error with fd=-1 pane")))

(test paste-to-pane-nil-text-is-noop
  "%paste-to-pane with nil text is a no-op."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                            :screen screen)))
    (finishes (cl-tmux::%paste-to-pane pane nil)
              "%paste-to-pane with nil text must not error")))

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
      (is (overlay-active-p) ":choose-session must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search (session-name s) text)
            ":choose-session overlay must contain the session name")))))

(test dispatch-list-sessions-full-shows-session-list
  ":list-sessions-full shows the session list overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*server-sessions* nil))
      (cl-tmux::dispatch-command s :list-sessions-full nil)
      (is (overlay-active-p) ":list-sessions-full must open an overlay"))))

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

