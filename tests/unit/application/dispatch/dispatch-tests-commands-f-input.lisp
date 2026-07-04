(in-package #:cl-tmux/test)

;;;; Dispatch tests — part F1: confirm-before, command-prompt, prompt-seeded
;;;; option helpers, and paste-to-pane.

(in-suite dispatch-suite)

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
        (if overlay-p
            (assert-overlay-active desc)
            (assert-overlay-inactive desc))))))

(test confirm-before-arg-is-single-key-and-y-runs-command
  "confirm-before COMMAND opens a SINGLE-KEY prompt: one 'y' keypress (no Enter)
   runs the command."
  (with-isolated-config
    (with-fake-session (s)
      (let ((cl-tmux/prompt:*prompt* nil))
        (cl-tmux::%cmd-confirm-before-arg s '("set-option" "-g" "status-left" "YES"))
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
        (cl-tmux::%cmd-confirm-before-arg s '("set-option" "-g" "status-left" "YES"))
        (cl-tmux::handle-prompt-key (char-code #\n))   ; 'n' cancels
        (is (null (prompt-active-p)) "prompt must dismiss on a non-y key")
        (is (string= "ORIG" (cl-tmux/options:get-option "status-left"))
            "a non-y key must NOT run the command")))))

(test confirm-before-arg-p-expands-format-prompt
  "confirm-before -p expands tmux formats against the active window and pane."
  (with-isolated-config
    (with-fake-session (s)
      (let* ((win (session-active-window s))
             (pane (window-active-pane win))
             (cl-tmux/prompt:*prompt* nil))
        (setf (window-name win) "work")
        (cl-tmux::%cmd-confirm-before-arg
         s '("-p" "kill-window #W pane #P? (y/n)"
             "display-message" "ok"))
        (is (prompt-active-p) "confirm-before -p must open a prompt")
        (is (string= (format nil "kill-window work pane ~D? (y/n)"
                             (pane-id pane))
                     (prompt-label *prompt*))
            "confirm-before -p prompt must expand #W and #P")))))

(test command-prompt-1-single-key-substitutes-one-keypress
  "command-prompt -1 -p k: 'set-option -g status-left %1' is a single-key prompt: one
   keypress (no Enter) is substituted for %1 and the command runs."
  (with-isolated-config
    (with-fake-session (s)
      (let ((cl-tmux/prompt:*prompt* nil))
        (cl-tmux::%cmd-command-prompt-arg
         s '("-1" "-p" "k:" "set-option -g status-left %1"))
        (is (prompt-active-p) "command-prompt -1 must open a prompt")
        (is-true (prompt-single-key *prompt*) "the prompt must be single-key")
        (cl-tmux::handle-prompt-key (char-code #\Z))   ; one key, no Enter
        (is (null (prompt-active-p)) "prompt must dismiss after one key")
        (is (string= "Z" (cl-tmux/options:get-option "status-left"))
            "%1 must be substituted with the single keypress 'Z'")))))

(test command-prompt-initial-text-seeds-buffer
  "command-prompt -I seeds the prompt buffer before editing begins."
  (with-isolated-config
    (with-fake-session (s)
      (let ((cl-tmux/prompt:*prompt* nil))
        (cl-tmux::%cmd-command-prompt-arg s '("-I" "ls"))
        (is (prompt-active-p) "command-prompt -I must open a prompt")
        (is (string= "ls" (prompt-buffer *prompt*))
            "the prompt buffer must start with the -I text")
        (is (= 2 (prompt-cursor-index *prompt*))
            "the cursor must start at the end of the initial text")
        (cl-tmux::handle-prompt-key (char-code #\!))
        (is (string= "ls!" (prompt-buffer *prompt*))
            "typed input must append after the seeded text")))))

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

(test paste-to-pane-noop-cases-table
  "%paste-to-pane is a no-op for fd=-1 panes and for nil text.
   Each row: (text description)."
  (dolist (row '(("hello world" "fd=-1: guard skips the write, no error")
                 (nil           "nil text: no-op, no error")))
    (destructuring-bind (text desc) row
      (let* ((screen (make-screen 20 5))
             (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                                :screen screen)))
        (finishes (cl-tmux::%paste-to-pane pane text) desc)))))

(test paste-to-pane-bracket-p-controls-wrapping
  "%paste-to-pane wraps in ESC[200~/ESC[201~ only when the application enabled
   bracketed paste AND bracket-p (tmux paste-buffer -p) is true.
   Each row: (app-bracketed bracket-p expect-wrapped description)."
  (dolist (row '((t   t   t   "-p with app bracketing must wrap")
                 (t   nil nil "no -p must NOT wrap even when the app enabled it")
                 (nil t   nil "-p without app bracketing must not wrap")))
    (destructuring-bind (app-bracketed bracket-p expect-wrapped desc) row
      (let* ((screen (make-screen 20 5))
             (pane   (make-pane :id 1 :fd 9999 :pid -1 :x 0 :y 0 :width 20 :height 5
                                :screen screen))
             (written nil)
             (real-pty-write (fdefinition 'cl-tmux/pty:pty-write)))
        (setf (cl-tmux/terminal/types:screen-bracketed-paste screen) app-bracketed)
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-write)
                     (lambda (fd octets)
                       (declare (ignore fd))
                       (push (babel:octets-to-string octets :encoding :utf-8)
                             written)))
               (cl-tmux::%paste-to-pane pane "hi" bracket-p))
          (setf (fdefinition 'cl-tmux/pty:pty-write) real-pty-write))
        (let ((all (apply #'concatenate 'string (nreverse written))))
          (is (search "hi" all) "the text must be written (~A)" desc)
          (if expect-wrapped
              (is (search "[200~" all) "~A" desc)
              (is (null (search "[200~" all)) "~A" desc)))))))
