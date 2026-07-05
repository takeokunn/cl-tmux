(in-package #:cl-tmux/test)

;;;; Dispatch hook and command-listing tests.

(in-suite dispatch-suite)

(test set-hook-after-select-window-fires-config-command
  "set-hook -g after-select-window <cmd> (the .tmux.conf path) fires the command
   when select-window runs — i.e. the hook reaches run-command-hooks, not just the
   programmatic add-hook registry."
  (with-fake-session (s :nwindows 2)
    (cl-tmux/hooks:clear-command-hooks "after-select-window")
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "set-hook -g after-select-window \"display-message hooked\"")
      (cl-tmux::%run-command-line s "select-window -n")
      (assert-overlay-contains "hooked" *overlay*
                               "the set-hook after-select-window command")
      (cl-tmux/hooks:clear-command-hooks "after-select-window"))))

(test set-hook-after-select-pane-fires-config-command
  "set-hook -g after-select-pane <cmd> fires when select-pane runs (config path)."
  (with-fake-two-pane-session (s)
    (cl-tmux/hooks:clear-command-hooks "after-select-pane")
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "set-hook -g after-select-pane \"display-message picked\"")
      (cl-tmux::%run-command-line s "select-pane -t 2")
      (assert-overlay-contains "picked" *overlay*
                               "the set-hook after-select-pane command")
      (cl-tmux/hooks:clear-command-hooks "after-select-pane"))))

(test set-hook-select-pane-session-lookup-table
  "window-pane-changed and pane-focus-in both fire on select-pane -t 2 via session lookup."
  (dolist (row '(("window-pane-changed" "swapped" "must fire via session lookup")
                 ("pane-focus-in"       "focused" "must fire via session lookup")))
    (destructuring-bind (hook-name word desc) row
      (with-fake-two-pane-session (s)
        (cl-tmux/hooks:clear-command-hooks hook-name)
        (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
              (*overlay* nil))
          (cl-tmux::%run-command-line
           s (format nil "set-hook -g ~A \"display-message ~A\"" hook-name word))
          (cl-tmux::%run-command-line s "select-pane -t 2")
          (assert-overlay-contains word *overlay*
                                   (format nil "~A ~A" hook-name desc))
          (cl-tmux/hooks:clear-command-hooks hook-name))))))

(test set-hook-after-rename-window-fires-config-via-unified-run-hooks
  "set-hook -g after-rename-window <cmd> fires on rename — proving the unified
   run-hooks now drives .tmux.conf set-hook for hooks whose firing point only
   called run-hooks (after-rename-window was previously config-broken)."
  (with-fake-session (s :nwindows 1)
    (cl-tmux/hooks:clear-command-hooks "after-rename-window")
    (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
          (*overlay* nil))
      (cl-tmux::%run-command-line
       s "set-hook -g after-rename-window \"display-message renamed-hook\"")
      (cl-tmux::%run-command-line s "rename-window newname")
      (assert-overlay-contains "renamed-hook" *overlay*
                               "after-rename-window set-hook")
      (cl-tmux/hooks:clear-command-hooks "after-rename-window"))))

(test cmd-list-commands-resolves-prefixes-and-reports-ambiguity
  "tmux 3.6a resolves unique prefixes for list-commands and reports the full
   ambiguous candidate list."
  (with-fake-session (s)
    (dolist (case '(("list-commands new-w"
                     "new-window [-abdkPS] [-c start-directory] [-e environment] [-F format] [-n window-name] [-t target-window] [shell-command [argument ...]]")
                    ("list-commands list-s"
                     "list-sessions [-F format] [-f filter]")
                    ("list-commands list"
                     "ambiguous command: list, could be: list-buffers, list-clients, list-commands, list-keys, list-panes, list-sessions, list-windows")))
      (destructuring-bind (line expected) case
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected *overlay* line))))))

(test cmd-list-commands-format-canonical-fields
  "list-commands expands only the canonical local command_list fields."
  (with-fake-session (s)
    (dolist (case '(("list-commands -F '#{command_list_name}|#{command_list_usage}' list-sessions"
                     "list-sessions|[-F format] [-f filter]")
                    ("list-commands -F '#{command_list_name}|#{command_list_usage}' list-commands"
                     "list-commands|[-F format] [command]")))
      (destructuring-bind (line expected) case
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected *overlay* line)
          (assert-overlay-not-contains "#{command_list_name}" *overlay* line)
          (assert-overlay-not-contains "#{command_list_usage}" *overlay* line))))))

(test cmd-list-commands-unsupported-arguments-are-rejected-before-output
  "tmux 3.6a rejects invalid list-commands flags and excess arguments before
   output."
  (with-fake-session (s)
    (dolist (case '(("list-commands -Z" "command list-commands: unknown flag -Z")
                    ("list-commands -F" "command list-commands: -F expects an argument")
                    ("list-commands new-window kill-pane"
                     "command list-commands: too many arguments (need at most 1)")))
      (destructuring-bind (line expected) case
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected *overlay* line)
          (assert-overlay-not-contains "new-window" *overlay* line))))))

(test run-command-line-rename-window-no-arg-opens-prompt
  "'rename-window' with no argument falls through to the prompt (name table)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "rename-window")
      (is (prompt-active-p)
          "no-arg rename-window must open the rename prompt"))))
