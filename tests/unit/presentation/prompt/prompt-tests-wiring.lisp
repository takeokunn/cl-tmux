(in-package #:cl-tmux/test)

(in-suite prompt-suite)

;;;; Wiring and display tests split out from prompt-tests.lisp so the base file
;;;; stays focused on prompt state, cursor, and kill/edit behaviour.

;;; -- handle-prompt-key wiring (real window, no PTY) --------------------------

(test handle-prompt-key-types-and-applies
  "Typing characters then Enter renames the target window and dismisses the prompt."
  (with-rename-window (win)
    (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key (char-code #\n))
    (cl-tmux::handle-prompt-key (char-code #\e))
    (cl-tmux::handle-prompt-key (char-code #\w))
    (cl-tmux::handle-prompt-key 13)            ; Enter
    (is (string= "new" (window-name win)) "Enter should apply the typed name")
    (is (null (prompt-active-p)) "Enter should dismiss the prompt")))

(test handle-prompt-key-escape-cancels
  "Esc cancels the prompt and leaves the target window's name unchanged."
  (with-rename-window (win)
    (prompt-start "rename-window" "old" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key (char-code #\x))
    (cl-tmux::handle-prompt-key 27)            ; Esc
    (is (null (prompt-active-p)) "Esc should dismiss the prompt")
    (is (string= "old" (window-name win)) "Esc must not rename")))

(test handle-prompt-key-backspace-table
  "Bytes 127 (DEL) and 8 (C-H) both act as backspace."
  (dolist (byte '(127 8))
    (with-clean-prompt
      (prompt-start "rename-window" "ab" (make-noop-submit))
      (cl-tmux::handle-prompt-key byte)
      (is (string= "a" (prompt-buffer *prompt*))
          "byte ~D must backspace to \"a\"" byte))))

;;; -- handle-prompt-key edge bytes (new coverage) -----------------------------

(test handle-prompt-key-ignores-control-byte
  "A control byte that matches no clause (Tab, 9) is ignored: it must not insert,
   clear, or submit -- the buffer and active prompt are untouched."
  (with-clean-prompt
    (prompt-start "rename-window" "ab" (make-noop-submit))
    (cl-tmux::handle-prompt-key 9)              ; Tab -- matches no clause
    (is (string= "ab" (prompt-buffer *prompt*)) "control byte must not edit the buffer")
    (is (prompt-active-p) "control byte must not dismiss the prompt")
    (is (eq t cl-tmux::*dirty*) "handle-prompt-key always marks the screen dirty")))

(test handle-prompt-key-enter-empty-is-noop-for-rename
  "Enter on an empty buffer dismisses the prompt but does NOT rename the window
   because rename-window ignores empty strings (matching real tmux behaviour)."
  (with-rename-window (win)
    (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
    (cl-tmux::handle-prompt-key 13)
    (is (string= "old" (window-name win))
        "Empty input must not rename the window -- original name preserved")
    (is (null (prompt-active-p)) "Enter should dismiss the prompt")))

(test handle-prompt-key-cc-cancels
  "C-c (byte 3) cancels the prompt, same as Escape."
  (with-clean-prompt
    (prompt-start "p" "hello" (make-noop-submit))
    (is (prompt-active-p))
    (cl-tmux::handle-prompt-key 3)   ; byte 3 = C-c
    (is (not (prompt-active-p)) "C-c must clear the prompt")))

;;; -- Status-bar display ------------------------------------------------------

(test status-bar-shows-prompt
  "render-status-bar shows the prompt text while a prompt is active."
  (let ((sess (make-renderer-test-session 40 10 :content "")))
    (with-clean-prompt
      (prompt-start "rename-window" "foo" (make-noop-submit))
      (let ((out (render-status-bar-output sess 10 40)))
        (is (search "rename-window: foo" out))))))
