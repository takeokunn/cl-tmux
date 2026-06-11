(in-package #:cl-tmux)

;;; -- Arg-command dispatch table + command-line runners ------------------------------
;;;
;;; *arg-command-table* maps (list-of-names . #'handler) pairs; consulted by
;;; %run-command-tokens before the no-arg named-command table.  Defined LAST so
;;; all #'%cmd-* function references are resolved after their definitions in
;;; dispatch-commands{,-buffer,-option,-lifecycle,-pane,-shell,-auto}.lisp.

(defparameter *arg-command-table*
  (list
   (cons '("display-message" "display") #'%cmd-display-message)
   (cons '("set-hook" "hook")           #'%cmd-set-hook)
   (cons '("bind-key" "bind")           #'%cmd-bind-key-arg)
   (cons '("unbind-key" "unbind")       #'%cmd-unbind-key-arg)
   (cons '("set" "set-option" "sets" "set-session-option")
         #'%cmd-set-option)
   ;; setw / set-window-option default to window scope (= set -w).
   (cons '("setw" "set-window-option") #'%cmd-set-window-option)
   (cons '("rename-window" "renamew")   #'%cmd-rename-window)
   (cons '("rename-session" "rename")   #'%cmd-rename-session)
   (cons '("select-window" "selectw")   #'%cmd-select-window)
   (cons '("select-pane" "selectp")     #'%cmd-select-pane)
   (cons '("kill-window" "killw")       #'%cmd-kill-window)
   (cons '("kill-pane" "killp")         #'%cmd-kill-pane)
   (cons '("kill-session")              #'%cmd-kill-session-arg)
   (cons '("swap-window" "swapw")       #'%cmd-swap-window)
   (cons '("move-window" "movew")       #'%cmd-move-window)
   (cons '("link-window" "linkw")       #'%cmd-link-window)
   (cons '("unlink-window" "unlinkw")   #'%cmd-unlink-window)
   (cons '("if-shell" "if")             #'%cmd-if-shell)
   (cons '("source-file" "source")      #'%cmd-source-file)
   (cons '("select-layout" "selectl")   #'%cmd-select-layout)
   (cons '("list-panes" "lsp")          #'%cmd-list-panes)
   (cons '("new-window" "neww")         #'%cmd-new-window-arg)
   (cons '("split-window" "splitw")     #'%cmd-split-window)
   (cons '("new-session" "new")         #'%cmd-new-session-arg)
   (cons '("set-environment" "setenv")  #'%cmd-set-environment-prompt)
   (cons '("resize-window" "resizew")   #'%cmd-resize-window-arg)
   (cons '("detach-client" "detachc")   #'%cmd-detach-client-arg)
   (cons '("send-keys" "send-key" "send") #'%cmd-send-keys-arg)
   (cons '("resize-pane" "resizep")     #'%cmd-resize-pane-arg)
   (cons '("capture-pane" "capturep")   #'%cmd-capture-pane-arg)
   (cons '("run-shell" "run")           #'%cmd-run-shell-arg)
   (cons '("if-shell" "if")             #'%cmd-if-shell-arg)
   (cons '("pipe-pane" "pipep")         #'%cmd-pipe-pane-arg)
   (cons '("list-sessions" "ls")        #'%cmd-list-sessions-arg)
   (cons '("list-windows" "lsw")        #'%cmd-list-windows-arg)
   (cons '("list-panes" "lsp")          #'%cmd-list-panes-arg-full)
   ;; copy-mode [-u]: -u flag pre-scrolls to oldest content on entry.
   (cons '("copy-mode")                 #'%cmd-copy-mode-arg)
   ;; list-keys [-T table]: filter by key table when -T is supplied.
   (cons '("list-keys" "lsk")           #'%cmd-list-keys-arg)
   ;; swap-pane [-U|-D|-L|-R]: directional swap including up/down.
   (cons '("swap-pane" "swapp")         #'%cmd-swap-pane-arg)
   ;; join-pane / move-pane (move-pane is join-pane): scriptable -s/-t/-h/-v/-d.
   (cons '("join-pane" "joinp")         #'%cmd-join-pane-arg)
   (cons '("move-pane" "movep")         #'%cmd-join-pane-arg)
   ;; break-pane: scriptable -d/-n/-s (move a pane out to its own new window).
   (cons '("break-pane" "breakp")       #'%cmd-break-pane-arg)
   ;; clear-history / rotate-window: scriptable -t (and rotate -D/-U).
   (cons '("clear-history" "clearhist") #'%cmd-clear-history-arg)
   (cons '("rotate-window" "rotatew")   #'%cmd-rotate-window-arg)
   ;; find-window: scriptable search-and-select (match-string positional).
   (cons '("find-window" "findw")       #'%cmd-find-window-arg)
   ;; list-commands [command]: list all commands, or filter to one by name.
   (cons '("list-commands" "lscm")      #'%cmd-list-commands-arg)
   ;; respawn-pane / respawn-window: scriptable -k/-t (restart process(es); -k forces).
   (cons '("respawn-pane" "respawnp")   #'%cmd-respawn-pane-arg)
   (cons '("respawn-window" "respawnw") #'%cmd-respawn-window-arg)
   ;; next/previous-window: scriptable -t target-session window cycling.
   (cons '("next-window" "next")        #'%cmd-next-window-arg)
   (cons '("previous-window" "prev")    #'%cmd-previous-window-arg)
   ;; confirm-before [-p prompt] cmd: gate COMMAND behind a y/n prompt.
   (cons '("confirm-before" "confirm")  #'%cmd-confirm-before-arg)
   ;; command-prompt [-p prompts] [template]: interactive prompt with substitution.
   (cons '("command-prompt" "commandp") #'%cmd-command-prompt-arg)
   ;; display-menu [-T title] [-x x] [-y y] [label key cmd ...]: interactive menu.
   (cons '("display-menu" "menu")       #'%cmd-display-menu-arg)
   ;; display-popup [-E] [-w W] [-h H] [-T title] [cmd]: run cmd, show output in a
   ;; popup (the `bind C-p popup -E "cmd"` form); no cmd opens the prompt.
   (cons '("display-popup" "popup")     #'%cmd-display-popup)
   ;; Named paste-buffer commands: -b <name> targets a specific named buffer.
   (cons '("set-buffer" "setb")         #'%cmd-set-buffer-arg)
   (cons '("paste-buffer" "pasteb")     #'%cmd-paste-buffer-arg)
   (cons '("delete-buffer" "deleteb")   #'%cmd-delete-buffer-arg)
   (cons '("show-buffer" "showb")       #'%cmd-show-buffer-arg)
   ;; has-session [-t name]: check if a named session exists (0 = yes, 1 = no).
   (cons '("has-session" "has")         #'%cmd-has-session-arg)
   ;; switch-client -T <key-table>: activate a custom key table (modal keymaps).
   (cons '("switch-client" "switchc")   #'%cmd-switch-client)
   ;; last-pane [-Z]: select last pane, optionally toggling zoom.
   (cons '("last-pane" "lastp")         #'%cmd-last-pane-arg)
   ;; server-access [-adlrwk] [user]: manage the server access-control list.
   (cons '("server-access")             #'%cmd-server-access)
   ;; customize-mode [-NF f -f filter -t pane]: options/bindings customize tree.
   (cons '("customize-mode" "customize") #'%cmd-customize-mode)
   ;; wait-for [-SLU] channel: channel-synchronization (block/signal/lock/unlock).
   (cons '("wait-for")                   #'%cmd-wait-for-arg))
  "Arg-taking commands: (list-of-names . handler), handler a function of
   (SESSION ARGS).  Consulted by %run-command-line before the no-argument
   %dispatch-named-command name table.")

(defun %run-command-tokens (session tokens)
  "Run a command line given as an already-tokenised TOKENS list (first = command
   name, rest = arguments).  Dispatch order:
   1. command-alias lookup (expand alias + append remaining tokens)
   2. arg-taking commands in *arg-command-table* (consume their arguments)
   3. no-arg named commands via %dispatch-named-command
   Taking pre-split tokens lets arg-bearing key bindings run without lossy
   re-tokenisation.  Returns the handler's return value."
  (let ((cmd  (first tokens))
        (rest (rest tokens)))
    (when cmd
      ;; 1. Command alias: expand and re-dispatch with remaining args appended.
      (let ((alias-exp (cl-tmux/options:lookup-command-alias cmd)))
        (if alias-exp
            (%run-command-line session
                               (format nil "~A~@[ ~{~A~^ ~}~]" alias-exp rest))
            ;; 2. Arg-taking commands (only when there are arguments to consume).
            (let ((entry (and rest
                              (find-if (lambda (e)
                                         (member cmd (car e) :test #'string-equal))
                                       *arg-command-table*))))
              (if entry
                  (funcall (cdr entry) session rest)
                  ;; 3. No-arg named commands (includes arg-cmds invoked with no args).
                  (%dispatch-named-command session cmd))))))))

(defun %run-command-line (session input)
  "Tokenise INPUT (one command line, shell-style) and run it.
   When the tokenised line contains \";\" tokens, splits into multiple commands
   and runs each in sequence, matching tmux's command-prompt behaviour."
  (let* ((tokens    (cl-tmux/commands:tokenize-command-string input))
         (sequences (cl-tmux/config::%split-on-semicolons tokens)))
    (if (= (length sequences) 1)
        (%run-command-tokens session (first sequences))
        (dolist (subcmd sequences)
          (%run-command-tokens session subcmd)))))

