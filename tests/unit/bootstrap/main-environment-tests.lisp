(in-package #:cl-tmux/test)

;;;; Tests for bootstrap environment initialization and teardown helpers.

(describe "main-suite"

  ;;; ── Coverage: hostname / environment helpers ─────────────────────────────────

  ;; %hostname-short strips the domain suffix, passes through when no dot, returns empty for empty.
  (it "hostname-short-table"
    (dolist (c '(("myhost.example.com" "myhost" "FQDN → short hostname")
                 ("solo"               "solo"   "no dot → full string unchanged")
                 (""                   ""       "empty string → empty string")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux::%hostname-short input))))))

  ;; %safe-getenv returns a string for any variable name.
  (it "safe-getenv-returns-string"
    (let ((result (cl-tmux::%safe-getenv "PATH")))
      (expect (stringp result)))
    (let ((result (cl-tmux::%safe-getenv "NONEXISTENT_VAR_XYZ_123")))
      (expect (stringp result))
      (expect (string= "" result))))

  ;; %mode-keys-from-editor-string mirrors tmux's basename/substring vi detection.
  (it "mode-keys-from-editor-string-detects-vi-and-emacs"
    (dolist (c '(("vi"               "vi")
                 ("vim"              "vi")
                 ("/usr/bin/vi"      "vi")
                 ("/usr/local/bin/nvim" "vi")
                 ("nano"             "emacs")
                 ("/usr/bin/emacs"   "emacs")
                 ("emacsclient -c"   "emacs")))
      (destructuring-bind (input expected) c
        (expect (string= expected (cl-tmux::%mode-keys-from-editor-string input)))))
    (expect (null (cl-tmux::%mode-keys-from-editor-string nil)))
    (expect (null (cl-tmux::%mode-keys-from-editor-string ""))))

  ;; %build-hostname-context returns a plist with :hostname, :term, :version, etc.
  (it "build-hostname-context-has-expected-keys"
    (let ((ctx (cl-tmux::%build-hostname-context)))
      (expect (stringp (getf ctx :hostname)))
      (expect (stringp (getf ctx :version)))
      (expect (stringp (getf ctx :term)))
      (expect (string= (cl-tmux/version:version-string) (getf ctx :version)))))

  ;; %make-format-condition-evaluator returns a callable closure that returns a string.
  (it "make-format-condition-evaluator"
    (let ((evaluator (cl-tmux::%make-format-condition-evaluator)))
      (expect (functionp evaluator))
      (expect (stringp (funcall evaluator "1")))))

  ;;; ── %enable-negotiated-terminal-features ──────────────────────────────────────

  ;; %enable-negotiated-terminal-features is defined as a function (extracted from
  ;; run-standalone's raw-mode setup).
  (it "enable-negotiated-terminal-features-is-fbound"
    (expect (fboundp 'cl-tmux::%enable-negotiated-terminal-features)))

  ;; %enable-negotiated-terminal-features only emits mouse/focus escape sequences
  ;; when the corresponding session option is on; with mouse, extended-keys, and
  ;; focus-events all off, calling it produces no escape-sequence output.
  (it "enable-negotiated-terminal-features-honors-option-gating"
    (with-isolated-options ("mouse" nil "extended-keys" "off" "focus-events" nil)
      (let ((output (with-output-to-string (*standard-output*)
                      (cl-tmux::%enable-negotiated-terminal-features))))
        (expect (zerop (length output))))))

  ;; %enable-negotiated-terminal-features emits an escape sequence when the mouse
  ;; option is on.
  (it "enable-negotiated-terminal-features-emits-mouse-sequence-when-on"
    (with-isolated-options ("mouse" t "extended-keys" "off" "focus-events" nil)
      (let ((output (with-output-to-string (*standard-output*)
                      (cl-tmux::%enable-negotiated-terminal-features))))
        (expect (plusp (length output)))
        (expect (find #\Escape output)))))

  ;;; ── %close-all-pane-ptys ──────────────────────────────────────────────────────

  ;; %close-all-pane-ptys closes the PTY fd of every pane in the session; a
  ;; subsequent low-level close(2) on any pane's fd then fails with EBADF,
  ;; proving the fd was already released.
  (it "close-all-pane-ptys-closes-every-pane-fd"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (let ((session (create-initial-session 24 80)))
      (cl-tmux::%close-all-pane-ptys session)
      (dolist (pane (all-panes session))
        (signals sb-posix:syscall-error
          (sb-posix:close (pane-fd pane))))))

  ;; %close-all-pane-ptys tolerates being called twice (already-closed fds do not
  ;; signal an error).
  (it "close-all-pane-ptys-ignores-already-closed-fds"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (let ((session (create-initial-session 24 80)))
      (cl-tmux::%close-all-pane-ptys session)
      (finishes (cl-tmux::%close-all-pane-ptys session)
                "a second call on already-closed fds must not error")))

  ;;; ── %cleanup-after-session ────────────────────────────────────────────────────

  ;; %cleanup-after-session joins the passed reader threads, clears *status-timer*,
  ;; and closes every pane's PTY fd.
  (it "cleanup-after-session-closes-ptys-and-clears-status-timer"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (with-isolated-options ()
      (let ((session (create-initial-session 24 80))
            (cl-tmux::*status-timer* nil)
            (cl-tmux::*running* t))
        (cl-tmux::%cleanup-after-session session nil)
        (expect (null cl-tmux::*status-timer*))
        (finishes (cl-tmux::%close-all-pane-ptys session)
                  "panes must already have closed fds (idempotent close)"))))

  ;;; ── %initialize-session-environment ───────────────────────────────────────────

  ;; %initialize-session-environment wires *config-condition-evaluator*, applies
  ;; editor-derived mode-keys, wires the terminal option callbacks, and calls
  ;; load-config-file.
  (it "initialize-session-environment-wires-condition-evaluator-and-loads-config"
    (with-isolated-options ()
      (let ((load-called nil)
            (cl-tmux/config:*config-condition-evaluator*
              cl-tmux/config:*config-condition-evaluator*)
            (cl-tmux/terminal:*history-limit-function*
              cl-tmux/terminal:*history-limit-function*)
            (cl-tmux/terminal:*alternate-screen-enabled-function*
              cl-tmux/terminal:*alternate-screen-enabled-function*)
            (cl-tmux/terminal:*scroll-on-clear-function*
              cl-tmux/terminal:*scroll-on-clear-function*))
        (with-stubbed-fdefinition
            ((cl-tmux::load-config-file
              (lambda (&rest args) (declare (ignore args)) (setf load-called t) 0)))
          (cl-tmux::%initialize-session-environment)
          (expect load-called :to-be-truthy)
          (expect (functionp cl-tmux/config:*config-condition-evaluator*))
          (expect (functionp cl-tmux/terminal:*history-limit-function*))))))

  ;;; ── %apply-editor-mode-keys ────────────────────────────────────────────────────

  ;; %apply-editor-mode-keys sets status-keys/mode-keys to "vi" when $VISUAL
  ;; names a vi-like editor.
  (it "apply-editor-mode-keys-sets-vi-from-visual-env"
    (with-isolated-options ()
      (with-stubbed-fdefinition
          ((cl-tmux::%safe-getenv
            (lambda (name) (if (string= name "VISUAL") "/usr/bin/vim" ""))))
        (cl-tmux::%apply-editor-mode-keys)
        (expect (string= "vi" (cl-tmux/options:get-option "status-keys")))
        (expect (string= "vi" (cl-tmux/options:get-option "mode-keys"))))))

  ;; %apply-editor-mode-keys leaves the registry defaults untouched when neither
  ;; $VISUAL nor $EDITOR is set.
  (it "apply-editor-mode-keys-no-op-when-no-editor-env"
    (with-isolated-options ()
      (let ((before (cl-tmux/options:get-option "mode-keys")))
        (with-stubbed-fdefinition
            ((cl-tmux::%safe-getenv (lambda (name) (declare (ignore name)) "")))
          (cl-tmux::%apply-editor-mode-keys)
          (expect (equal before (cl-tmux/options:get-option "mode-keys")))))))

  ;;; ── %die-with-message ─────────────────────────────────────────────────────────

  ;; %die-with-message writes the formatted message to *error-output* and exits
  ;; with code 1.
  (it "die-with-message-formats-and-exits-with-code-1"
    (let (exit-code (output (make-string-output-stream)))
      (let ((*error-output* output))
        (with-stubbed-exit exit-code
          (cl-tmux::%die-with-message "boom: ~A~%" "reason")))
      (expect (eql 1 exit-code))
      (expect (search "boom: reason" (get-output-stream-string output)))))

  ;;; ── %wire-option-callbacks ────────────────────────────────────────────────────

  ;; %wire-option-callbacks wires cl-tmux/terminal:*history-limit-function*.
  (it "wire-option-callbacks-sets-history-limit-function"
    (let ((old cl-tmux/terminal:*history-limit-function*))
      (unwind-protect
           (progn
             (cl-tmux::%wire-option-callbacks)
             (expect (functionp cl-tmux/terminal:*history-limit-function*)))
        (setf cl-tmux/terminal:*history-limit-function* old))))

  ;; %wire-option-callbacks wires cl-tmux/terminal:*alternate-screen-enabled-function*.
  (it "wire-option-callbacks-sets-alternate-screen-function"
    (let ((old cl-tmux/terminal:*alternate-screen-enabled-function*))
      (unwind-protect
           (progn
             (cl-tmux::%wire-option-callbacks)
             (expect (functionp cl-tmux/terminal:*alternate-screen-enabled-function*)))
        (setf cl-tmux/terminal:*alternate-screen-enabled-function* old))))

  ;; %wire-option-callbacks wires cl-tmux/terminal:*scroll-on-clear-function*.
  (it "wire-option-callbacks-sets-scroll-on-clear-function"
    (let ((old cl-tmux/terminal:*scroll-on-clear-function*))
      (unwind-protect
           (progn
             (cl-tmux::%wire-option-callbacks)
             (expect (functionp cl-tmux/terminal:*scroll-on-clear-function*)))
        (setf cl-tmux/terminal:*scroll-on-clear-function* old)))))
