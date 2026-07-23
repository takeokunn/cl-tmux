(in-package #:cl-tmux/test)

;;;; Wiring and display tests split out from prompt-tests.lisp so the base file
;;;; stays focused on prompt state, cursor, and kill/edit behaviour.

(describe "prompt-suite"

  ;;; -- handle-prompt-key wiring (real window, no PTY) --------------------------

  ;; Typing characters then Enter renames the target window and dismisses the prompt.
  (it "handle-prompt-key-types-and-applies"
    (with-rename-window (win)
      (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
      (cl-tmux::handle-prompt-key (char-code #\n))
      (cl-tmux::handle-prompt-key (char-code #\e))
      (cl-tmux::handle-prompt-key (char-code #\w))
      (cl-tmux::handle-prompt-key 13)            ; Enter
      (expect (string= "new" (window-name win)))
      (expect (null (prompt-active-p)))))

  ;; Esc cancels the prompt and leaves the target window's name unchanged.
  (it "handle-prompt-key-escape-cancels"
    (with-rename-window (win)
      (prompt-start "rename-window" "old" (lambda (name) (rename-window win name)))
      (cl-tmux::handle-prompt-key (char-code #\x))
      (cl-tmux::handle-prompt-key 27)            ; Esc
      (expect (null (prompt-active-p)))
      (expect (string= "old" (window-name win)))))

  ;; Bytes 127 (DEL) and 8 (C-H) both act as backspace.
  (it "handle-prompt-key-backspace-table"
    (dolist (byte '(127 8))
      (with-clean-prompt
        (prompt-start "rename-window" "ab" (make-noop-submit))
        (cl-tmux::handle-prompt-key byte)
        (expect (string= "a" (prompt-buffer *prompt*))))))

  ;;; -- handle-prompt-key edge bytes (new coverage) -----------------------------

  ;; A control byte that matches no clause (Tab, 9) is ignored: it must not insert,
  ;; clear, or submit -- the buffer and active prompt are untouched.
  (it "handle-prompt-key-ignores-control-byte"
    (with-clean-prompt
      (prompt-start "rename-window" "ab" (make-noop-submit))
      (cl-tmux::handle-prompt-key 9)              ; Tab -- matches no clause
      (expect (string= "ab" (prompt-buffer *prompt*)))
      (expect (prompt-active-p))
      (expect (eq t cl-tmux::*dirty*))))

  ;; Enter on an empty buffer dismisses the prompt but does NOT rename the window
  ;; because rename-window ignores empty strings (matching real tmux behaviour).
  (it "handle-prompt-key-enter-empty-is-noop-for-rename"
    (with-rename-window (win)
      (prompt-start "rename-window" "" (lambda (name) (rename-window win name)))
      (cl-tmux::handle-prompt-key 13)
      (expect (string= "old" (window-name win)))
      (expect (null (prompt-active-p)))))

  ;; C-c (byte 3) cancels the prompt, same as Escape.
  (it "handle-prompt-key-cc-cancels"
    (with-clean-prompt
      (prompt-start "p" "hello" (make-noop-submit))
      (expect (prompt-active-p))
      (cl-tmux::handle-prompt-key 3)   ; byte 3 = C-c
      (expect (not (prompt-active-p)))))

  ;;; -- Status-bar display ------------------------------------------------------

  ;; render-status-bar shows the prompt text while a prompt is active.
  (it "status-bar-shows-prompt"
    (let ((sess (make-renderer-test-session 40 10 :content "")))
      (with-clean-prompt
        (prompt-start "rename-window" "foo" (make-noop-submit))
        (let ((out (render-status-bar-output sess 10 40)))
          (expect (search "rename-window: foo" out)))))))
