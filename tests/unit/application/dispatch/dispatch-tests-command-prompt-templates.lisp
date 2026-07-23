(in-package #:cl-tmux/test)

;;;; Dispatch command-prompt template and focus tests.

(describe "dispatch-suite"

  ;;; ── command-prompt: template without -p, and -i incremental ──────────────────

  ;; %substitute-prompt-response replaces both %% and %1 with the response.
  ;; Each row: (template input expected description).
  (it "substitute-prompt-response-table"
    (dolist (row '(("rename-window '%%'" "shell" "rename-window 'shell'" "%% form")
                   ("select-window -t %1" "3"    "select-window -t 3"    "%1 form")
                   ("echo %% and %1"      "x"    "echo x and x"          "both forms mix")
                   ("no placeholders"     "y"    "no placeholders"       "no-op template")))
      (destructuring-bind (template input expected desc) row
        (declare (ignore desc))
        (expect (string= expected (cl-tmux::%substitute-prompt-response template input))))))

  ;; command-prompt TEMPLATE (no -p) runs the template with the prompt response
  ;; replacing %% — the classic rename-window binding shape (previously the
  ;; template was silently ignored and raw input ran).
  (it "command-prompt-template-without-p-substitutes-response"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::%run-command-line s "command-prompt \"set-option -g @cp 'X-%%'\"")
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "foo")
        (expect (string= "X-foo" (cl-tmux/options:get-option "@cp"))))))

  ;; command-prompt -i wires the template to the prompt's on-change hook so it
  ;; runs with the in-progress input on every edit (tmux incremental mode).
  (it "command-prompt-i-runs-template-on-each-edit"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::%run-command-line s "command-prompt -i \"set-option -g @inc '%1'\"")
        (expect (prompt-active-p))
        (expect (prompt-on-change *prompt*) :to-be-truthy)
        (funcall (prompt-on-change *prompt*) "ab")
        (expect (string= "ab" (cl-tmux/options:get-option "@inc")))
        (funcall (prompt-on-change *prompt*) "abc")
        (expect (string= "abc" (cl-tmux/options:get-option "@inc"))))))

  ;; command-prompt -N marks the prompt numeric-only: prompt-input drops
  ;; non-digit characters and keeps digits.
  (it "command-prompt-N-accepts-digits-only"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::%run-command-line s "command-prompt -N \"select-window -t %1\"")
        (expect (prompt-active-p))
        (expect (cl-tmux/prompt:prompt-numeric-only *prompt*) :to-be-truthy)
        (cl-tmux/prompt:prompt-input #\a)
        (cl-tmux/prompt:prompt-input #\3)
        (expect (string= "3" (cl-tmux/prompt:prompt-buffer *prompt*))))))

  ;; command-prompt -F expands the substituted command line as a format string
  ;; before it runs.
  (it "command-prompt-F-expands-command-as-format"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::%run-command-line
         s "command-prompt -F \"set-option -g @fmt '#{session_name}-%1'\"")
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "tail")
        (let ((value (cl-tmux/options:get-option "@fmt")))
          (expect (and (stringp value)
                       (let ((len (length "-tail")))
                         (and (>= (length value) len)
                              (string= "-tail" value :start2 (- (length value) len))))
                       (null (search "#{" value))))))))

  ;; copy-mode -s src-pane enters copy mode on the SOURCE pane's screen.
  (it "copy-mode-s-enters-on-source-pane-screen"
    (with-two-pane-h-session (s win p0 p1)
      (with-command-test-state (s :overlay t)
        (cl-tmux::%run-command-line s "copy-mode -s %2")
        (expect (cl-tmux/terminal/types:screen-copy-mode-p
                 (cl-tmux/model:pane-screen p1))
                :to-be-truthy)
        (expect (null (cl-tmux/terminal/types:screen-copy-mode-p
                       (cl-tmux/model:pane-screen p0)))))))

  ;; command-prompt -l requests a one-keypress prompt (like -1; cl-tmux's -1
  ;; already submits the untranslated character, which is -l's semantics).
  (it "command-prompt-l-is-single-key"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::%run-command-line s "command-prompt -l \"send-keys %1\"")
        (expect (prompt-active-p))
        (expect (cl-tmux/prompt:prompt-single-key *prompt*) :to-be-truthy))))

  ;; command-prompt -e closes the prompt when the client loses focus (?1004
  ;; focus-out report); a focus-in leaves it open.
  (it "command-prompt-e-closes-on-focus-out"
    (with-fake-session (s)
      (let ((*prompt* nil)
            (cl-tmux::*dirty* nil))
        (cl-tmux::%run-command-line s "command-prompt -e")
        (expect (prompt-active-p))
        ;; Focus-in: prompt stays.
        (cl-tmux::%handle-escape-focus-change s (vector 27 91 73)) ; ESC [ I
        (expect (prompt-active-p))
        ;; Focus-out: prompt closes.
        (cl-tmux::%handle-escape-focus-change s (vector 27 91 79)) ; ESC [ O
        (expect (not (prompt-active-p))))))

  ;; A prompt without -e is unaffected by focus changes.
  (it "command-prompt-without-e-survives-focus-out"
    (with-fake-session (s)
      (let ((*prompt* nil)
            (cl-tmux::*dirty* nil))
        (cl-tmux::%run-command-line s "command-prompt")
        (cl-tmux::%handle-escape-focus-change s (vector 27 91 79))
        (expect (prompt-active-p))))))
