(in-package #:cl-tmux/test)

;;;; Dispatch tests - part F0: display-message, clock-mode, capture-pane,
;;;; send-keys, choose-tree, and option prompts.

(describe "dispatch-suite"

  ;; - :display-message logs to *message-log* ----------------------------------

  ;; :display-message on-submit calls add-message-log, appending the message.
  (it "dispatch-display-message-logs-to-message-log"
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux::*message-log* nil))
      (with-dispatch-prompt (s :display-message :label "display-message"
                               :context ":display-message must open a prompt")
        ;; Submit a non-empty message.
        (funcall (prompt-on-submit *prompt*) "test-log-entry")
        (expect (null cl-tmux::*message-log*) :to-be-falsy)
        (let ((last-msg (cdr (first cl-tmux::*message-log*))))
          (expect (string= "test-log-entry" last-msg))))))

  ;; - :clock-mode dispatch -----------------------------------------------------

  ;; :clock-mode sets *clock-mode-pane-id* to the active pane's id.
  (it "dispatch-clock-mode-toggles-pane-id"
    (with-fake-session (s)
      (let ((cl-tmux::*clock-mode-pane-id* nil))
        (cl-tmux::dispatch-command s :clock-mode nil)
        (let ((ap (session-active-pane s)))
          (expect (eql (pane-id ap) cl-tmux::*clock-mode-pane-id*))
          ;; Toggle off
          (cl-tmux::dispatch-command s :clock-mode nil)
          (expect (null cl-tmux::*clock-mode-pane-id*))))))

  ;; - :capture-pane dispatch ---------------------------------------------------

  ;; :capture-pane opens an overlay containing the pane content.
  (it "dispatch-capture-pane-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        ;; Feed some text into the active pane's screen.
        (let ((ap (session-active-pane s)))
          (when ap
            (feed (pane-screen ap) "CAPTEST")))
        (cl-tmux::dispatch-command s :capture-pane nil)
        (assert-overlay-contains "CAPTEST" *overlay*
                                 ":capture-pane"))))

  ;; - :send-keys dispatch ------------------------------------------------------

  ;; :send-keys opens a prompt for the keys string.
  (it "dispatch-send-keys-opens-prompt"
    (with-dispatch-prompt (s :send-keys :label "send-keys"
                             :context ":send-keys must open a prompt")))

  ;; :send-keys on-submit with a no-PTY pane (fd=-1) does not signal an error.
  (it "dispatch-send-keys-no-crash-with-no-pty"
    (with-dispatch-prompt (s :send-keys :context ":send-keys must open a prompt")
      ;; Submitting keys to fd=-1 pane must not error.
      (finishes (funcall (prompt-on-submit *prompt*) "hello")
                "send-keys on-submit must not error with fd=-1 pane")))

  ;; - :choose-tree dispatch ----------------------------------------------------

  ;; :choose-tree opens an overlay with session and window entries.
  (it "dispatch-choose-tree-shows-overlay"
    (with-dispatch-overlay (s :choose-tree
                              :context ":choose-tree must open an overlay")
      (assert-overlay-contains (session-name s) *overlay*
                               ":choose-tree")))

  ;; :choose-tree with multiple server sessions lists them all.
  (it "dispatch-choose-tree-with-server-sessions"
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

  ;; - :set-window-option / :set-session-option dispatch -----------------------

  ;; :set-window-option opens a prompt.
  (it "dispatch-set-window-option-opens-prompt"
    (with-dispatch-prompt (s :set-window-option :label "set-window-option"
                             :context ":set-window-option must open a prompt")))

  ;; :set-session-option opens a prompt.
  (it "dispatch-set-session-option-opens-prompt"
    (with-dispatch-prompt (s :set-session-option :label "set-session-option"
                             :context ":set-session-option must open a prompt"))))
