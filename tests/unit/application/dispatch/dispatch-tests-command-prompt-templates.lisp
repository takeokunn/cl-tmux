(in-package #:cl-tmux/test)

;;;; Dispatch command-prompt template and focus tests.

(in-suite dispatch-suite)

;;; ── command-prompt: template without -p, and -i incremental ──────────────────

(test substitute-prompt-response-table
  "%substitute-prompt-response replaces both %% and %1 with the response.
   Each row: (template input expected description)."
  (dolist (row '(("rename-window '%%'" "shell" "rename-window 'shell'" "%% form")
                 ("select-window -t %1" "3"    "select-window -t 3"    "%1 form")
                 ("echo %% and %1"      "x"    "echo x and x"          "both forms mix")
                 ("no placeholders"     "y"    "no placeholders"       "no-op template")))
    (destructuring-bind (template input expected desc) row
      (is (string= expected (cl-tmux::%substitute-prompt-response template input))
          "~A" desc))))

(test command-prompt-template-without-p-substitutes-response
  "command-prompt TEMPLATE (no -p) runs the template with the prompt response
   replacing %% — the classic rename-window binding shape (previously the
   template was silently ignored and raw input ran)."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "command-prompt \"set -g @cp 'X-%%'\"")
      (is (prompt-active-p) "the prompt must open")
      (funcall (prompt-on-submit *prompt*) "foo")
      (is (string= "X-foo" (cl-tmux/options:get-option "@cp"))
          "the response must replace %% in the template"))))

(test command-prompt-i-runs-template-on-each-edit
  "command-prompt -i wires the template to the prompt's on-change hook so it
   runs with the in-progress input on every edit (tmux incremental mode)."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "command-prompt -i \"set -g @inc '%1'\"")
      (is (prompt-active-p) "the prompt must open")
      (is-true (prompt-on-change *prompt*)
               "-i must install an on-change hook")
      (funcall (prompt-on-change *prompt*) "ab")
      (is (string= "ab" (cl-tmux/options:get-option "@inc"))
          "each edit must run the template with the current input")
      (funcall (prompt-on-change *prompt*) "abc")
      (is (string= "abc" (cl-tmux/options:get-option "@inc"))
          "later edits must re-run the template with the newer input"))))

(test command-prompt-N-accepts-digits-only
  "command-prompt -N marks the prompt numeric-only: prompt-input drops
   non-digit characters and keeps digits."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "command-prompt -N \"select-window -t %1\"")
      (is (prompt-active-p) "the prompt must open")
      (is-true (cl-tmux/prompt:prompt-numeric-only *prompt*)
               "-N must mark the prompt numeric-only")
      (cl-tmux/prompt:prompt-input #\a)
      (cl-tmux/prompt:prompt-input #\3)
      (is (string= "3" (cl-tmux/prompt:prompt-buffer *prompt*))
          "only the digit must be inserted"))))

(test command-prompt-F-expands-command-as-format
  "command-prompt -F expands the substituted command line as a format string
   before it runs."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line
       s "command-prompt -F \"set -g @fmt '#{session_name}-%1'\"")
      (is (prompt-active-p) "the prompt must open")
      (funcall (prompt-on-submit *prompt*) "tail")
      (let ((value (cl-tmux/options:get-option "@fmt")))
        (is (and (stringp value)
                 (let ((len (length "-tail")))
                   (and (>= (length value) len)
                        (string= "-tail" value :start2 (- (length value) len))))
                 (null (search "#{" value)))
            "-F must expand #{session_name} and substitute %1 (got ~S)" value)))))

(test copy-mode-s-enters-on-source-pane-screen
  "copy-mode -s src-pane enters copy mode on the SOURCE pane's screen."
  (with-two-pane-h-session (s win p0 p1)
    (with-command-test-state (s :overlay t)
      (cl-tmux::%run-command-line s "copy-mode -s %2")
      (is-true (cl-tmux/terminal/types:screen-copy-mode-p
                (cl-tmux/model:pane-screen p1))
               "-s %2 must enter copy mode on pane 2's screen")
      (is (null (cl-tmux/terminal/types:screen-copy-mode-p
                 (cl-tmux/model:pane-screen p0)))
          "the active pane's screen must not enter copy mode"))))

(test command-prompt-l-is-single-key
  "command-prompt -l requests a one-keypress prompt (like -1; cl-tmux's -1
   already submits the untranslated character, which is -l's semantics)."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "command-prompt -l \"send-keys %1\"")
      (is (prompt-active-p) "the prompt must open")
      (is-true (cl-tmux/prompt:prompt-single-key *prompt*)
               "-l must request a single-key prompt"))))

(test command-prompt-e-closes-on-focus-out
  "command-prompt -e closes the prompt when the client loses focus (?1004
   focus-out report); a focus-in leaves it open."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux::*dirty* nil))
      (cl-tmux::%run-command-line s "command-prompt -e")
      (is (prompt-active-p) "the prompt must open")
      ;; Focus-in: prompt stays.
      (cl-tmux::%handle-escape-focus-change s (vector 27 91 73)) ; ESC [ I
      (is (prompt-active-p) "focus-in must leave the -e prompt open")
      ;; Focus-out: prompt closes.
      (cl-tmux::%handle-escape-focus-change s (vector 27 91 79)) ; ESC [ O
      (is (not (prompt-active-p)) "focus-out must close the -e prompt"))))

(test command-prompt-without-e-survives-focus-out
  "A prompt without -e is unaffected by focus changes."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux::*dirty* nil))
      (cl-tmux::%run-command-line s "command-prompt")
      (cl-tmux::%handle-escape-focus-change s (vector 27 91 79))
      (is (prompt-active-p)
          "focus-out must not close a prompt opened without -e"))))
