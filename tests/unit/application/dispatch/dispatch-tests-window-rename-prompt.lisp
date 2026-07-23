(in-package #:cl-tmux/test)

;;;; Window rename prompt dispatch tests.

(describe "dispatch-suite"

  ;; C-b , opens a rename prompt seeded with the active window's name, and its
  ;; on-submit closure renames the active window.
  (it "dispatch-rename-window-opens-prompt"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :rename-window nil)
        (expect (prompt-active-p))
        (expect (string= "0" (prompt-buffer *prompt*)))
        (expect (functionp (prompt-on-submit *prompt*)))
        (funcall (prompt-on-submit *prompt*) "renamed")
        (expect (string= "renamed" (window-name (session-active-window s))))))))
