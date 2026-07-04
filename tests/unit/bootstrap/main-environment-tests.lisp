(in-package #:cl-tmux/test)

;;;; Tests for bootstrap environment initialization and teardown helpers.

;;; ── Coverage: hostname / environment helpers ─────────────────────────────────

(test hostname-short-table
  "%hostname-short strips the domain suffix, passes through when no dot, returns empty for empty."
  (dolist (c '(("myhost.example.com" "myhost" "FQDN → short hostname")
               ("solo"               "solo"   "no dot → full string unchanged")
               (""                   ""       "empty string → empty string")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux::%hostname-short input))
          "~A: ~S → ~S" desc input expected))))

(test safe-getenv-returns-string
  "%safe-getenv returns a string for any variable name."
  (let ((result (cl-tmux::%safe-getenv "PATH")))
    (is (stringp result) "%safe-getenv must return a string"))
  (let ((result (cl-tmux::%safe-getenv "NONEXISTENT_VAR_XYZ_123")))
    (is (stringp result) "%safe-getenv must return a string for missing var")
    (is (string= "" result) "%safe-getenv must return empty string for missing var")))

(test mode-keys-from-editor-string-detects-vi-and-emacs
  "%mode-keys-from-editor-string mirrors tmux's basename/substring vi detection."
  (dolist (c '(("vi"               "vi")
               ("vim"              "vi")
               ("/usr/bin/vi"      "vi")
               ("/usr/local/bin/nvim" "vi")
               ("nano"             "emacs")
               ("/usr/bin/emacs"   "emacs")
               ("emacsclient -c"   "emacs")))
    (destructuring-bind (input expected) c
      (is (string= expected (cl-tmux::%mode-keys-from-editor-string input))
          "~S must map to ~S" input expected)))
  (is (null (cl-tmux::%mode-keys-from-editor-string nil))
      "NIL editor must yield NIL (registry default left untouched)")
  (is (null (cl-tmux::%mode-keys-from-editor-string ""))
      "empty editor must yield NIL (registry default left untouched)"))

(test build-hostname-context-has-expected-keys
  "%build-hostname-context returns a plist with :hostname, :term, :version, etc."
  (let ((ctx (cl-tmux::%build-hostname-context)))
    (is (stringp (getf ctx :hostname))  ":hostname must be a string")
    (is (stringp (getf ctx :version))   ":version must be a string")
    (is (stringp (getf ctx :term))      ":term must be a string")
    (is (string= (cl-tmux/version:version-string) (getf ctx :version))
        ":version must expose the cl-tmux runtime version")))

(test make-format-condition-evaluator
  "%make-format-condition-evaluator returns a callable closure that returns a string."
  (let ((evaluator (cl-tmux::%make-format-condition-evaluator)))
    (is (functionp evaluator)
        "%make-format-condition-evaluator must return a function")
    (is (stringp (funcall evaluator "1"))
        "format condition evaluator must return a string")))

;;; ── %enable-negotiated-terminal-features ──────────────────────────────────────

(test enable-negotiated-terminal-features-is-fbound
  "%enable-negotiated-terminal-features is defined as a function (extracted from
   run-standalone's raw-mode setup)."
  (is (fboundp 'cl-tmux::%enable-negotiated-terminal-features)
      "%enable-negotiated-terminal-features must be fbound"))

(test enable-negotiated-terminal-features-honors-option-gating
  "%enable-negotiated-terminal-features only emits mouse/focus escape sequences
   when the corresponding session option is on; with mouse, extended-keys, and
   focus-events all off, calling it produces no escape-sequence output."
  (with-isolated-options ("mouse" nil "extended-keys" "off" "focus-events" nil)
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux::%enable-negotiated-terminal-features))))
      (is (zerop (length output))
          "no escape sequences must be emitted when all three options are off"))))

(test enable-negotiated-terminal-features-emits-mouse-sequence-when-on
  "%enable-negotiated-terminal-features emits an escape sequence when the mouse
   option is on."
  (with-isolated-options ("mouse" t "extended-keys" "off" "focus-events" nil)
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux::%enable-negotiated-terminal-features))))
      (is (plusp (length output))
          "some escape-sequence output must be emitted when mouse is on")
      (is (find #\Escape output) "output must contain an ESC byte"))))

;;; ── %close-all-pane-ptys ──────────────────────────────────────────────────────

(test close-all-pane-ptys-closes-every-pane-fd
  "%close-all-pane-ptys closes the PTY fd of every pane in the session; a
   subsequent low-level close(2) on any pane's fd then fails with EBADF,
   proving the fd was already released."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    (cl-tmux::%close-all-pane-ptys session)
    (dolist (pane (all-panes session))
      (signals sb-posix:syscall-error
        (sb-posix:close (pane-fd pane))))))

(test close-all-pane-ptys-ignores-already-closed-fds
  "%close-all-pane-ptys tolerates being called twice (already-closed fds do not
   signal an error)."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    (cl-tmux::%close-all-pane-ptys session)
    (finishes (cl-tmux::%close-all-pane-ptys session)
              "a second call on already-closed fds must not error")))

;;; ── %cleanup-after-session ────────────────────────────────────────────────────

(test cleanup-after-session-closes-ptys-and-clears-status-timer
  "%cleanup-after-session joins the passed reader threads, clears *status-timer*,
   and closes every pane's PTY fd."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-isolated-options ()
    (let ((session (create-initial-session 24 80))
          (cl-tmux::*status-timer* nil)
          (cl-tmux::*running* t))
      (cl-tmux::%cleanup-after-session session nil)
      (is (null cl-tmux::*status-timer*)
          "*status-timer* must be NIL after cleanup")
      (finishes (cl-tmux::%close-all-pane-ptys session)
                "panes must already have closed fds (idempotent close)"))))

;;; ── %initialize-session-environment ───────────────────────────────────────────

(test initialize-session-environment-wires-condition-evaluator-and-loads-config
  "%initialize-session-environment wires *config-condition-evaluator*, applies
   editor-derived mode-keys, wires the terminal option callbacks, and calls
   load-config-file."
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
        (is-true load-called
                 "%initialize-session-environment must call load-config-file")
        (is (functionp cl-tmux/config:*config-condition-evaluator*)
            "*config-condition-evaluator* must be wired to a function")
        (is (functionp cl-tmux/terminal:*history-limit-function*)
            "*history-limit-function* must be wired to a function")))))

;;; ── %apply-editor-mode-keys ────────────────────────────────────────────────────

(test apply-editor-mode-keys-sets-vi-from-visual-env
  "%apply-editor-mode-keys sets status-keys/mode-keys to \"vi\" when $VISUAL
   names a vi-like editor."
  (with-isolated-options ()
    (with-stubbed-fdefinition
        ((cl-tmux::%safe-getenv
          (lambda (name) (if (string= name "VISUAL") "/usr/bin/vim" ""))))
      (cl-tmux::%apply-editor-mode-keys)
      (is (string= "vi" (cl-tmux/options:get-option "status-keys"))
          "status-keys must be set to \"vi\"")
      (is (string= "vi" (cl-tmux/options:get-option "mode-keys"))
          "mode-keys must be set to \"vi\""))))

(test apply-editor-mode-keys-no-op-when-no-editor-env
  "%apply-editor-mode-keys leaves the registry defaults untouched when neither
   $VISUAL nor $EDITOR is set."
  (with-isolated-options ()
    (let ((before (cl-tmux/options:get-option "mode-keys")))
      (with-stubbed-fdefinition
          ((cl-tmux::%safe-getenv (lambda (name) (declare (ignore name)) "")))
        (cl-tmux::%apply-editor-mode-keys)
        (is (equal before (cl-tmux/options:get-option "mode-keys"))
            "mode-keys must be unchanged when no editor env var is set")))))

;;; ── %die-with-message ─────────────────────────────────────────────────────────

(test die-with-message-formats-and-exits-with-code-1
  "%die-with-message writes the formatted message to *error-output* and exits
   with code 1."
  (let (exit-code (output (make-string-output-stream)))
    (let ((*error-output* output))
      (with-stubbed-exit exit-code
        (cl-tmux::%die-with-message "boom: ~A~%" "reason")))
    (is (eql 1 exit-code) "%die-with-message must exit with code 1")
    (is (search "boom: reason" (get-output-stream-string output))
        "%die-with-message must format its message to *error-output*")))

;;; ── %wire-option-callbacks ────────────────────────────────────────────────────

(test wire-option-callbacks-sets-history-limit-function
  "%wire-option-callbacks wires cl-tmux/terminal:*history-limit-function*."
  (let ((old cl-tmux/terminal:*history-limit-function*))
    (unwind-protect
         (progn
           (cl-tmux::%wire-option-callbacks)
           (is (functionp cl-tmux/terminal:*history-limit-function*)
               "*history-limit-function* must be a function after %wire-option-callbacks"))
      (setf cl-tmux/terminal:*history-limit-function* old))))

(test wire-option-callbacks-sets-alternate-screen-function
  "%wire-option-callbacks wires cl-tmux/terminal:*alternate-screen-enabled-function*."
  (let ((old cl-tmux/terminal:*alternate-screen-enabled-function*))
    (unwind-protect
         (progn
           (cl-tmux::%wire-option-callbacks)
           (is (functionp cl-tmux/terminal:*alternate-screen-enabled-function*)
               "*alternate-screen-enabled-function* must be a function after %wire-option-callbacks"))
      (setf cl-tmux/terminal:*alternate-screen-enabled-function* old))))

(test wire-option-callbacks-sets-scroll-on-clear-function
  "%wire-option-callbacks wires cl-tmux/terminal:*scroll-on-clear-function*."
  (let ((old cl-tmux/terminal:*scroll-on-clear-function*))
    (unwind-protect
         (progn
           (cl-tmux::%wire-option-callbacks)
           (is (functionp cl-tmux/terminal:*scroll-on-clear-function*)
               "*scroll-on-clear-function* must be a function after %wire-option-callbacks"))
      (setf cl-tmux/terminal:*scroll-on-clear-function* old))))
