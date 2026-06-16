(in-package #:cl-tmux)

(defparameter *dispatch-command-specs-core-misc-entries*
  '((:set-buffer nil nil :public-name "set-buffer")
    (:display-message '%cmd-display-message ("display-message"))
    (:show-options '%cmd-show-options-arg ("show-options"))
    (:show-window-options '%cmd-show-window-options-arg ("show-window-options"))
    (:show-session-options '%cmd-show-session-options-arg ("show-session-options"))
    (:show-server-options '%cmd-show-server-options-arg ("show-server-options"))
    (:run-shell '%cmd-run-shell-arg ("run-shell"))
    (:if-shell '%cmd-if-shell-arg ("if-shell"))
    (:source-file '%cmd-source-file ("source-file"))
    (:list-commands '%cmd-list-commands-arg ("list-commands"))
    (:customize-mode '%cmd-customize-mode ("customize-mode"))))
