(in-package #:cl-tmux/test)

;;;; Dispatch tests — part F1: confirm-before, command-prompt, prompt-seeded
;;;; option helpers, and paste-to-pane.

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
         (expect (prompt-active-p) :to-be-truthy)
         (let ((,on-submit-var (prompt-on-submit *prompt*)))
           ,@body)))))

(describe "dispatch-suite"

  ;; :confirm-before on-submit: "y" shows an overlay; "n" and "" do not.
  (it "confirm-before-input-table"
    (dolist (row '(("y"  t   "y confirms → overlay visible")
                   ("n"  nil "n cancels → no overlay")
                   (""   nil "empty string cancels → no overlay")))
      (destructuring-bind (input overlay-p desc) row
        (with-confirm-before-prompt (on-submit)
          (funcall on-submit input)
          (if overlay-p
              (assert-overlay-active desc)
              (assert-overlay-inactive desc))))))

  ;; confirm-before COMMAND opens a SINGLE-KEY prompt: one 'y' keypress (no Enter)
  ;; runs the command.
  (it "confirm-before-arg-is-single-key-and-y-runs-command"
    (with-isolated-config
      (with-fake-session (s)
        (let ((cl-tmux/prompt:*prompt* nil))
          (cl-tmux::%cmd-confirm-before-arg s '("set-option" "-g" "status-left" "YES"))
          (expect (prompt-active-p))
          (expect (prompt-single-key *prompt*) :to-be-truthy)
          (cl-tmux::handle-prompt-key (char-code #\y))   ; single key, no Enter
          (expect (null (prompt-active-p)))
          (expect (string= "YES" (cl-tmux/options:get-option "status-left")))))))

  ;; A non-y single key cancels confirm-before without running the command.
  (it "confirm-before-arg-single-key-other-cancels"
    (with-isolated-config
      (with-fake-session (s)
        (let ((cl-tmux/prompt:*prompt* nil))
          (cl-tmux/options:set-option "status-left" "ORIG")
          (cl-tmux::%cmd-confirm-before-arg s '("set-option" "-g" "status-left" "YES"))
          (cl-tmux::handle-prompt-key (char-code #\n))   ; 'n' cancels
          (expect (null (prompt-active-p)))
          (expect (string= "ORIG" (cl-tmux/options:get-option "status-left")))))))

  ;; confirm-before -p expands tmux formats against the active window and pane.
  (it "confirm-before-arg-p-expands-format-prompt"
    (with-isolated-config
      (with-fake-session (s)
        (let* ((win (session-active-window s))
               (pane (window-active-pane win))
               (cl-tmux/prompt:*prompt* nil))
          (setf (window-name win) "work")
          (cl-tmux::%cmd-confirm-before-arg
           s '("-p" "kill-window #W pane #P? (y/n)"
               "display-message" "ok"))
          (expect (prompt-active-p))
          (expect (string= (format nil "kill-window work pane ~D? (y/n)"
                                   (pane-id pane))
                           (prompt-label *prompt*)))))))

  ;; command-prompt -1 -p k: 'set-option -g status-left %1' is a single-key prompt: one
  ;; keypress (no Enter) is substituted for %1 and the command runs.
  (it "command-prompt-1-single-key-substitutes-one-keypress"
    (with-isolated-config
      (with-fake-session (s)
        (let ((cl-tmux/prompt:*prompt* nil))
          (cl-tmux::%cmd-command-prompt-arg
           s '("-1" "-p" "k:" "set-option -g status-left %1"))
          (expect (prompt-active-p))
          (expect (prompt-single-key *prompt*) :to-be-truthy)
          (cl-tmux::handle-prompt-key (char-code #\Z))   ; one key, no Enter
          (expect (null (prompt-active-p)))
          (expect (string= "Z" (cl-tmux/options:get-option "status-left")))))))

  ;; command-prompt -I seeds the prompt buffer before editing begins.
  (it "command-prompt-initial-text-seeds-buffer"
    (with-isolated-config
      (with-fake-session (s)
        (let ((cl-tmux/prompt:*prompt* nil))
          (cl-tmux::%cmd-command-prompt-arg s '("-I" "ls"))
          (expect (prompt-active-p))
          (expect (string= "ls" (prompt-buffer *prompt*)))
          (expect (= 2 (prompt-cursor-index *prompt*)))
          (cl-tmux::handle-prompt-key (char-code #\!))
          (expect (string= "ls!" (prompt-buffer *prompt*)))))))

  ;;; ── %set-option-from-prompt helper ──────────────────────────────────────────

  ;; %set-option-from-prompt opens a prompt with the given label.
  (it "set-option-from-prompt-helper-opens-prompt"
    (with-loop-state
      (let ((*prompt* nil))
        (cl-tmux::%set-option-from-prompt "test-label")
        (expect (prompt-active-p))
        (expect (string= "test-label" (prompt-label *prompt*))))))

  ;; %set-option-from-prompt on-submit with 'name value' calls set-option.
  (it "set-option-from-prompt-sets-option"
    (with-loop-state
      (let ((*prompt* nil))
        (cl-tmux::%set-option-from-prompt "set-window-option")
        (expect (prompt-active-p))
        ;; Submit "mouse on" which maps to set-option "mouse" "on"
        (finishes (funcall (prompt-on-submit *prompt*) "mouse on")
                  "%set-option-from-prompt on-submit must not error"))))

  ;;; ── %paste-to-pane helper ────────────────────────────────────────────────────

  ;; %paste-to-pane is a no-op for fd=-1 panes and for nil text.
  ;; Each row: (text description).
  (it "paste-to-pane-noop-cases-table"
    (dolist (row '(("hello world" "fd=-1: guard skips the write, no error")
                   (nil           "nil text: no-op, no error")))
      (destructuring-bind (text desc) row
        (let* ((screen (make-screen 20 5))
               (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                                  :screen screen)))
          (finishes (cl-tmux::%paste-to-pane pane text) desc)))))

  ;; %paste-to-pane wraps in ESC[200~/ESC[201~ only when the application enabled
  ;; bracketed paste AND bracket-p (tmux paste-buffer -p) is true.
  ;; Each row: (app-bracketed bracket-p expect-wrapped description).
  (it "paste-to-pane-bracket-p-controls-wrapping"
    (dolist (row '((t   t   t   "-p with app bracketing must wrap")
                   (t   nil nil "no -p must NOT wrap even when the app enabled it")
                   (nil t   nil "-p without app bracketing must not wrap")))
      (destructuring-bind (app-bracketed bracket-p expect-wrapped desc) row
        (declare (ignore desc))
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
            (expect (search "hi" all))
            (if expect-wrapped
                (expect (search "[200~" all))
                (expect (null (search "[200~" all))))))))))
