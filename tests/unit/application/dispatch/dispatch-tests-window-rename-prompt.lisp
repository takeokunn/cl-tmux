(in-package #:cl-tmux/test)

;;;; Window rename prompt dispatch tests.

(in-suite dispatch-suite)

(test dispatch-rename-window-opens-prompt
  "C-b , opens a rename prompt seeded with the active window's name, and its
   on-submit closure renames the active window."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :rename-window nil)
      (is (prompt-active-p) "rename should open a prompt")
      (is (string= "0" (prompt-buffer *prompt*))
          "prompt seeded with current window name")
      (is (functionp (prompt-on-submit *prompt*))
          "prompt should carry an on-submit closure")
      (funcall (prompt-on-submit *prompt*) "renamed")
      (is (string= "renamed" (window-name (session-active-window s)))
          "on-submit closure should rename the active window"))))
