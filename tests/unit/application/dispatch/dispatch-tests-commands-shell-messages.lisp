(in-package #:cl-tmux/test)

;;;; Shell conditional, named dispatch, and message command tests

(in-suite dispatch-suite)

;;; ── if-shell -F <cond> <then> [<else>] (format-conditional) ──────────────────

(test run-command-line-if-shell-F-overlay-table
  "if-shell -F with truthy/falsey/format conditions and accepted flags each produce
   the expected overlay fragment.  Each row: (command expected-fragment message)."
  (dolist (row '(("if-shell -F 1 \"display-message yes\""
                  "yes" "truthy if-shell -F")
                 ("if-shell -F 0 \"display-message yes\" \"display-message no\""
                  "no" "falsey if-shell -F")
                 ("if-shell -F \"#{window_count}\" \"display-message named\""
                  "named" "a non-zero #{window_count} (1)")
                 ("if-shell -b -t %1 -F 1 \"display-message if-target-ok\""
                  "if-target-ok" "if-shell -b/-t")))
    (destructuring-bind (cmd expected msg) row
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s cmd)
          (assert-overlay-contains expected (overlay-lines) msg))))))

(test run-command-line-if-shell-F-uses-target-context
  "if-shell -t <pane> -F evaluates the condition in the target pane's context,
   not the active pane's context."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (target (find 2 (window-panes win) :key #'pane-id))
           (*overlay* nil))
      (setf (cl-tmux/model:pane-title target) "target-title")
      (cl-tmux::%run-command-line
       s "if-shell -t %2 -F \"#{pane_title}\" \"display-message target\" \"display-message active\"")
      (assert-overlay-contains "target" (overlay-lines)
                                "if-shell -F must evaluate against the target pane")
      (is (null (search "active" (format nil "~{~A~%~}" (overlay-lines))))
          "if-shell -F must not fall back to the active pane context"))))

(test run-command-line-if-shell-F-empty-condition-no-then
  "if-shell -F with an empty condition and no else runs nothing."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "if-shell -F \"\" \"display-message x\"")
      (assert-overlay-inactive
       "an empty (falsey) condition with no else must not run THEN"))))


(test run-command-line-if-shell-rejects-unsupported-arguments
  "if-shell rejects unknown flags and extra positionals before running anything."
  (dolist (command '("if-shell -Z -F 1 \"display-message x\""
                     "if-shell -F 1 \"display-message x\" \"display-message y\" extra"))
    (with-fake-session (s)
      (let ((*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (assert-overlay-contains "if-shell: unsupported argument"
                                  (overlay-lines) command)))))

;;; ── %dispatch-named-command helper ──────────────────────────────────────────

(test dispatch-named-command-new-window
  "%dispatch-named-command \"next-window\" selects the next window."
  ;; Use next-window (no fork/no thread) to avoid leaking reader threads
  ;; that would prevent later PTY tests from forking.
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "next-window")
      (is-true cl-tmux::*dirty*
               "%dispatch-named-command 'next-window' must mark *dirty*")
      (is (eq (second (session-windows s)) (session-active-window s))
          "next-window must switch to the second window"))))

(test dispatch-named-command-unknown-shows-overlay
  "%dispatch-named-command with an unrecognized name shows an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "totally-unknown-xyz")
      (assert-overlay-contains "unknown command" (overlay-lines)
                                "unknown command")
      (assert-overlay-contains "totally-unknown-xyz" (overlay-lines)
                                "unknown command"))))

;;; ── :show-messages dispatch ──────────────────────────────────────────────────

(test dispatch-show-messages-empty-log-shows-overlay
  ":show-messages with empty *message-log* opens an overlay saying '(no messages)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :show-messages nil)
      (assert-overlay-contains "no messages" (overlay-lines)
                                ":show-messages"))))

(test dispatch-show-messages-populated-log-shows-entries
  ":show-messages with entries in *message-log* lists them."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* (list (cons 0 "hello") (cons 1 "world"))))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (equal '("hello" "world") (overlay-lines))
          ":show-messages must render one message per line")
      (assert-overlay-contains "hello" (overlay-lines)
                                ":show-messages")
      (assert-overlay-contains "world" (overlay-lines)
                                ":show-messages"))))

(test dispatch-show-messages-defaults-to-current-client-log
  ":show-messages with a current client context uses that client's log."
  (with-fake-session (s)
    (let* ((*overlay* nil)
           (cl-tmux::*message-log* (list (cons 0 "global")))
           (cl-tmux::*current-client-conn*
             (cl-tmux::%make-client-conn
              :state (cl-tmux::make-input-state)
              :message-log (list (cons 1 "client")))))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (equal '("client") (overlay-lines))
          ":show-messages must prefer the current client's log")
      (assert-overlay-not-contains "global" (overlay-lines)
                                   ":show-messages"))))

(test run-command-line-show-messages-targets-client-log
  "show-messages -t client-1 shows the targeted client's message log."
  (with-fake-session (s)
    (let* ((*overlay* nil)
           (a (cl-tmux::%make-client-conn
               :state (cl-tmux::make-input-state)
               :message-log (list (cons 0 "alpha"))))
           (b (cl-tmux::%make-client-conn
               :state (cl-tmux::make-input-state)
               :message-log (list (cons 0 "beta"))))
           (cl-tmux::*clients* (list a b))
           (cl-tmux::*current-client-conn* a)
           (cl-tmux::*message-log* (list (cons 0 "global"))))
      (cl-tmux::%run-command-line s "show-messages -t client-1")
      (is (equal '("beta") (overlay-lines))
          "show-messages -t client-1 must show the second client's log")
      (assert-overlay-not-contains "alpha" (overlay-lines)
                                   "show-messages -t client-1")
      (assert-overlay-not-contains "global" (overlay-lines)
                                   "show-messages -t client-1"))))

(test run-command-line-show-messages-rejects-stale-flags
  "show-messages rejects stale tmux parity flags."
  (with-fake-session (s)
    (let* ((client (cl-tmux::%make-client-conn
                    :state (cl-tmux::make-input-state)
                    :message-log (list (cons 0 "alpha") (cons 1 "beta"))))
           (cl-tmux::*clients* (list client))
           (cl-tmux::*message-log* (list (cons 0 "alpha") (cons 1 "beta"))))
      (dolist (line '("show-messages -J"
                      "show-messages -T"))
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-active line)
          (assert-overlay-contains "show-messages: unsupported argument"
                                   (overlay-lines) line)
          (assert-overlay-not-contains "alpha" (overlay-lines) line)
          (assert-overlay-not-contains "beta" (overlay-lines) line))))))
