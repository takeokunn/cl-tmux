(in-package #:cl-tmux)

(defparameter *dispatch-command-specs-compat-misc-entries*
  '((%cmd-run-shell-arg ("run-shell"))
    (%cmd-if-shell-arg ("if-shell"))
    (%cmd-list-commands-arg ("list-commands"))
    (%cmd-customize-mode ("customize-mode"))))
