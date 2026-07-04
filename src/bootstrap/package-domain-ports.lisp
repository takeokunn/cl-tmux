;;; Domain abstraction packages.

(defpackage #:cl-tmux/ports
  (:use #:cl)
  (:export
   #:*spawn-pty*
   #:*write-pty*
   #:*resize-pty*
   #:*close-pty*
   #:spawn-pty
   #:write-pty
   #:resize-pty
   #:close-pty))

(defpackage #:cl-tmux/repository
  (:use #:cl)
  (:export
   #:repo-find-session
   #:repo-add-session
   #:repo-remove-session
   #:repo-all-sessions
   #:repo-current-session
   #:*session-repo*))
