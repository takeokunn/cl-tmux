(in-package #:cl-tmux/test)

;;;; Dispatch tests - part F0: display-message, clock-mode, capture-pane,
;;;; send-keys, choose-tree, and option prompts.

(in-suite dispatch-suite)

;;; - :display-message logs to *message-log* ----------------------------------

(test dispatch-display-message-logs-to-message-log
  ":display-message on-submit calls add-message-log, appending the message."
  (let ((*prompt* nil) (*overlay* nil)
        (cl-tmux::*message-log* nil))
    (with-dispatch-prompt (s :display-message :label "display-message"
                             :context ":display-message must open a prompt")
      ;; Submit a non-empty message.
      (funcall (prompt-on-submit *prompt*) "test-log-entry")
      (is-false (null cl-tmux::*message-log*)
                "*message-log* must be non-nil after submitting a message")
      (let ((last-msg (cdr (first cl-tmux::*message-log*))))
        (is (string= "test-log-entry" last-msg)
            "first message-log entry must be the submitted message (got ~S)" last-msg)))))

;;; - :clock-mode dispatch -----------------------------------------------------

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

;;; - :capture-pane dispatch ---------------------------------------------------

(test dispatch-capture-pane-shows-overlay
  ":capture-pane opens an overlay containing the pane content."
  (with-fake-session (s)
    (let ((*overlay* nil))
      ;; Feed some text into the active pane's screen.
      (let ((ap (session-active-pane s)))
        (when ap
          (feed (pane-screen ap) "CAPTEST")))
      (cl-tmux::dispatch-command s :capture-pane nil)
      (assert-overlay-contains "CAPTEST" *overlay*
                               ":capture-pane"))))

;;; - :send-keys dispatch ------------------------------------------------------

(test dispatch-send-keys-opens-prompt
  ":send-keys opens a prompt for the keys string."
  (with-dispatch-prompt (s :send-keys :label "send-keys"
                           :context ":send-keys must open a prompt")))

(test dispatch-send-keys-no-crash-with-no-pty
  ":send-keys on-submit with a no-PTY pane (fd=-1) does not signal an error."
  (with-dispatch-prompt (s :send-keys :context ":send-keys must open a prompt")
    ;; Submitting keys to fd=-1 pane must not error.
    (finishes (funcall (prompt-on-submit *prompt*) "hello")
              "send-keys on-submit must not error with fd=-1 pane")))

;;; - :choose-tree dispatch ----------------------------------------------------

(test dispatch-choose-tree-shows-overlay
  ":choose-tree opens an overlay with session and window entries."
  (with-dispatch-overlay (s :choose-tree
                            :context ":choose-tree must open an overlay")
    (assert-overlay-contains (session-name s) *overlay*
                             ":choose-tree")))

(test dispatch-choose-tree-with-server-sessions
  ":choose-tree with multiple server sessions lists them all."
  (with-fake-session (s1)
    (let* ((s2 (make-fake-session :nwindows 2))
           (reg (list (cons (session-name s1) s1)
                      (cons (session-name s2) s2))))
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* reg))
        (cl-tmux::dispatch-command s1 :choose-tree nil)
        (assert-overlay-contains (session-name s1) *overlay*
                                 ":choose-tree with server sessions")
        (assert-overlay-contains (session-name s2) *overlay*
                                 ":choose-tree with server sessions")))))

;;; - :set-window-option / :set-session-option dispatch -----------------------

(test dispatch-set-window-option-opens-prompt
  ":set-window-option opens a prompt."
  (with-dispatch-prompt (s :set-window-option :label "set-window-option"
                           :context ":set-window-option must open a prompt")))

(test dispatch-set-session-option-opens-prompt
  ":set-session-option opens a prompt."
  (with-dispatch-prompt (s :set-session-option :label "set-session-option"
                           :context ":set-session-option must open a prompt")))
