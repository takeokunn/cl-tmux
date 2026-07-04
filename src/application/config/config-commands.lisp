(in-package #:cl-tmux/config)

;;;; Config command-name registry: bindable keyword set, known canonical command
;;;; names, and the resolution helpers.
;;;;
;;;; Splitting these ~160 lines out of config-tokenizer.lisp keeps the tokenizer
;;;; focused on lexical analysis (character scanning, token splitting, key-name
;;;; canonicalization) while this file owns the command-namespace data.
;;;;
;;;; Loaded by config-tokenizer.lisp via a fragment-loader eval-when block,
;;;; after the key-parsing utilities it depends on are defined.

;;; ── Bindable keyword set ─────────────────────────────────────────────────
;;;
;;; These are the keywords that can appear as the command argument of a bind
;;; directive and are dispatched directly by cl-tmux:dispatch-command.  They
;;; form the directly-bindable subset — commands whose names canonically map
;;; to a single keyword without any argument parsing at key-press time.

(defparameter *bindable-commands*
  '(;; Window lifecycle
    :new-window :next-window :prev-window :last-window :find-window
    :rename-window :choose-window :list-windows :move-window-prompt :swap-window
    :rotate-window :rotate-window-reverse :next-layout
    :select-layout-even-h :select-layout-even-v :select-layout-tiled
    :select-layout-main-h :select-layout-main-v :select-layout-spread
    ;; Pane lifecycle
    :next-pane :prev-pane :last-pane :display-panes
    :split-horizontal :split-vertical
    :split-horizontal-no-focus :split-vertical-no-focus
    :kill-pane :kill-pane-confirm :kill-window :kill-window-confirm
    :respawn-pane :break-pane :join-pane
    :swap-pane-forward :swap-pane-backward
    :resize-left :resize-right :resize-up :resize-down
    :zoom-toggle :mark-pane :clear-mark
    :synchronize-panes :pipe-pane :display-info
    ;; Session lifecycle
    :new-session :kill-session :rename-session :detach
    :list-sessions :list-sessions-full :choose-session
    :switch-client-next :switch-client-prev :last-session
    :has-session :lock-session :unlock-session
    ;; Key bindings / config
    :list-keys :source-file
    ;; Selection / navigation
    :select-window ; the pressed digit chooses the window
    :select-window-prompt :select-pane-left :select-pane-right
    :select-pane-up :select-pane-down
    ;; Copy / paste / buffers
    :paste-buffer :copy-mode-enter :send-prefix
    :list-buffers :show-buffer :choose-buffer :delete-buffer
    :save-buffer :load-buffer
    ;; Display / info
    :show-options
    :show-window-options :show-session-options :show-server-options
    :show-messages :show-hooks
    :display-message :display-popup
    :capture-pane :clear-history :clock-mode
    ;; Scripting / hooks
    :run-shell :if-shell :command-prompt :wait-for
    ;; Client management
    :choose-client :choose-tree :refresh-client :suspend-client :customize-mode
    ;; Server management
    :server-info :list-clients :lock-server :detach-all-clients
    :kill-server :start-server :lock-client
    ;; Window management (additional)
    :resize-window :respawn-window :attach-session :move-pane
    :previous-layout :link-window :unlink-window
    ;; Pane management (additional)
    :list-panes :set-buffer
    ;; Info / listing
    :list-commands
    ;; Environment
    :show-environment :set-environment
    ;; Prompt history
    :show-prompt-history :clear-prompt-history
    ;; Set-option (interactive)
    :set-window-option :set-session-option)
  "Command keywords a config-file bind directive may target.
   Type: list of keyword symbols.
   This is the user-bindable subset of commands cl-tmux:dispatch-command handles.
   It deliberately EXCLUDES copy-mode-internal commands (:copy-mode-exit,
   :copy-mode-begin-selection, :copy-mode-yank), which are produced by copy-mode
   interception rather than by key lookup.  Prompt-only dispatcher IDs are also
   excluded from the public command list.
   Updated whenever a new dispatchable command is added to dispatch-handlers.")

;;; ── Command alias policy ─────────────────────────────────────────────────
;;;
;;; cl-tmux accepts canonical command names only.  tmux short aliases such as
;;; neww/splitw/killp are deliberately not kept as a compatibility layer.

(defparameter *tmux-command-aliases*
  '()
  "Canonical-only command registry: no tmux short aliases are accepted.")

(defun %canonical-command-name (name)
  "Return NAME unchanged.  cl-tmux accepts canonical command names only."
  name)

;;; ── Known canonical command names ────────────────────────────────────────
;;;
;;; This list covers all primary command names from tmux's cmd_table that
;;; cl-tmux either implements or accepts as valid bind targets.  Combined with
;;; *bindable-commands*, it allows %known-command-name-p to accept canonical
;;; commands while rejecting aliases and genuine typos.

(defparameter *known-command-names*
  '(;; tmux cmd_table primary names (transparently bindable / dispatchable).
    "attach-session" "bind-key" "break-pane" "capture-pane" "choose-buffer"
    "choose-client" "choose-tree" "choose-window" "clear-history"
    "clear-prompt-history" "clock-mode" "command-prompt" "confirm-before"
    "copy-mode" "customize-mode" "delete-buffer" "detach-client" "display-menu"
    "display-message" "display-panes" "display-popup" "find-window" "has-session"
    "if-shell" "join-pane" "kill-pane" "kill-server" "kill-session" "kill-window"
    "last-pane" "last-window" "link-window" "list-buffers" "list-clients"
    "list-commands" "list-keys" "list-panes" "list-sessions" "list-windows"
    "load-buffer" "lock-client" "lock-server" "lock-session" "move-pane"
    "move-window" "new-session" "new-window" "next-layout" "next-window"
    "paste-buffer" "pipe-pane" "previous-layout" "previous-window"
    "refresh-client" "rename-session" "rename-window" "resize-pane"
    "resize-window" "respawn-pane" "respawn-window" "rotate-window" "run-shell"
    "save-buffer" "select-layout" "select-pane" "select-window" "send-keys"
    "send-prefix" "server-access" "set-buffer" "set-environment" "set-hook"
    "set-option" "set-window-option" "show-buffer" "show-environment"
    "show-hooks" "show-messages" "show-options" "show-prompt-history"
    "show-window-options" "source-file" "split-window" "start-server"
    "suspend-client" "swap-pane" "swap-window" "switch-client" "unbind-key"
    "unlink-window" "wait-for"
    ;; cl-tmux additions / internal command names that are valid bind targets.
    "copy-mode-enter" "choose-session" "detach" "detach-all-clients"
    "show-server-options" "show-session-options" "set-session-option"
    "server-info" "display-info" "mark-pane" "clear-mark" "zoom-toggle"
    "synchronize-panes")
  "Canonical tmux/cl-tmux command names accepted as bind targets.  Combined with
   *bindable-commands*, this lets %known-command-name-p accept canonical command
   names while rejecting aliases and typos.")

(defun %known-command-name-p (name)
  "True when NAME is a recognised canonical command or bindable keyword name.
   tmux short aliases are rejected at load time."
  (or (%command-keyword name)
      (member name *known-command-names* :test #'string-equal)))

(defun %command-keyword (name)
  "Return the bindable command keyword named by NAME (case-insensitive), or NIL
   if NAME is not a directly-bindable command keyword.  Canonical command names
   are resolved via FIND-SYMBOL so unknown names are never interned into the
   keyword package.  Canonical arg-only commands like previous-window return NIL;
   the bind parser stores them as deferred command token lists."
  (let ((keyword (find-symbol (string-upcase name) :keyword)))
    (and keyword (member keyword *bindable-commands*) keyword)))
