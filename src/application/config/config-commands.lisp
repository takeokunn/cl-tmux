(in-package #:cl-tmux/config)

;;;; Config command-name registry: bindable keyword set, tmux short aliases,
;;;; known canonical command names, and the resolution helpers.
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

;;; ── tmux short-alias table ───────────────────────────────────────────────
;;;
;;; tmux's cmd_table carries a .alias field for most commands (e.g. neww =
;;; new-window, splitw = split-window).  This table maps those aliases to the
;;; canonical command names cl-tmux registers.  Users who copy standard
;;; .tmux.conf files that use the short forms will have them transparently
;;; resolved rather than rejected.

(defparameter *tmux-command-aliases*
  '(;; tmux cmd_table short aliases (cmd_entry.alias) → cl-tmux's canonical name.
    ("attach"    . "attach-session")   ("breakp"    . "break-pane")
    ("capturep"  . "capture-pane")     ("clearhist" . "clear-history")
    ("commandp"  . "command-prompt")   ("confirmb"  . "confirm-before")
    ("copy"      . "copy-mode-enter")  ("copy-mode" . "copy-mode-enter")
    ("deleteb"   . "delete-buffer")    ("display"   . "display-message")
    ("displayp"  . "display-panes")    ("findw"     . "find-window")
    ("has"       . "has-session")      ("if"        . "if-shell")
    ("joinp"     . "join-pane")        ("killp"     . "kill-pane")
    ("killw"     . "kill-window")      ("loadb"     . "load-buffer")
    ("lockc"     . "lock-client")      ("locks"     . "lock-session")
    ("ls"        . "list-sessions")    ("lsb"       . "list-buffers")
    ("lsc"       . "list-clients")     ("lscm"      . "list-commands")
    ("lsk"       . "list-keys")        ("lsp"       . "list-panes")
    ("lsw"       . "list-windows")     ("menu"      . "display-menu")
    ("movep"     . "move-pane")        ("movew"     . "move-window")
    ("new"       . "new-session")      ("neww"      . "new-window")
    ("pasteb"    . "paste-buffer")     ("popup"     . "display-popup")
    ("renames"   . "rename-session")   ("renamew"   . "rename-window")
    ("resizep"   . "resize-pane")      ("resizew"   . "resize-window")
    ("respawnp"  . "respawn-pane")     ("respawnw"  . "respawn-window")
    ("saveb"     . "save-buffer")      ("selectp"   . "select-pane")
    ("selectw"   . "select-window")    ("send"      . "send-keys")
    ("set"       . "set-option")       ("sets"      . "set-session-option")
    ("setw"      . "set-window-option")("show"      . "show-options")
    ("showb"     . "show-buffer")      ("showw"     . "show-window-options")
    ("source"    . "source-file")      ("splitw"    . "split-window")
    ("start"     . "start-server")     ("suspendc"  . "suspend-client")
    ("swapp"     . "swap-pane")        ("swapw"     . "swap-window")
    ("switchc"   . "switch-client")    ("bind-key"  . "bind")
    ;; Standard tmux short aliases verified against the upstream cmd_table
    ;; (deepwiki diff 2026-07-03) — real configs use these freely.  The
    ;; earlier confirmb/renames/unlink spellings are kept as tolerant extras.
    ("confirm"   . "confirm-before")   ("kills"     . "kill-session")
    ("next"      . "next-window")      ("prev"      . "previous-window")
    ("nextl"     . "next-layout")      ("prevl"     . "previous-layout")
    ("pipe"      . "pipe-pane")        ("pipep"     . "pipe-pane")
    ("refresh"   . "refresh-client")   ("rename"    . "rename-session")
    ("rotatew"   . "rotate-window")    ("selectl"   . "select-layout")
    ("showenv"   . "show-environment") ("showmsgs"  . "show-messages")
    ("unlinkw"   . "unlink-window")
    ;; new-pane (newp): recent tmux addition — a pane-creating command whose
    ;; behaviour maps onto split-window in this model.
    ("new-pane"  . "split-window")     ("newp"      . "split-window")
    ("unbind-key" . "unbind")          ("unlink"    . "unlink-window"))
  "tmux command-name aliases (the cmd_entry .alias field) mapped to the canonical
   name cl-tmux registers.  Consulted by %canonical-command-name so a .tmux.conf
   using the short forms (neww, splitw, killp, …) resolves transparently.")

(defun %canonical-command-name (name)
  "Return the canonical cl-tmux command name for NAME, resolving tmux short
   aliases (case-insensitive).  Returns NAME unchanged when it is not an alias."
  (or (cdr (assoc name *tmux-command-aliases* :test #'string-equal))
      name))

;;; ── Known canonical command names ────────────────────────────────────────
;;;
;;; This list covers all primary command names from tmux's cmd_table that
;;; cl-tmux either implements or accepts as valid bind targets.  Combined with
;;; *tmux-command-aliases* and *bindable-commands*, it allows %known-command-name-p
;;; to accept any real command (canonical or alias) while rejecting genuine typos.

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
   *tmux-command-aliases* and *bindable-commands*, this lets %known-command-name-p
   accept any real command (canonical or alias) while still rejecting typos.")

(defun %known-command-name-p (name)
  "True when NAME is a recognised command — a bindable keyword, a known canonical
   command name, or a tmux short alias for one.  Used to accept real commands
   (canonical or abbreviated) in a binding while rejecting genuine typos."
  (let ((canon (%canonical-command-name name)))
    (or (%command-keyword canon)
        (member canon *known-command-names* :test #'string-equal))))

(defun %command-keyword (name)
  "Return the bindable command keyword named by NAME (case-insensitive), or NIL
   if NAME is not a directly-bindable command keyword.  Canonical command names
   are resolved via FIND-SYMBOL so unknown names are never interned into the
   keyword package.  Names that are not bindable keywords (tmux short aliases
   like neww, or arg-only commands like previous-window) return NIL; the bind
   parser then stores them as a deferred command token-list resolved through the
   alias-aware command dispatch at key-press time."
  (let ((keyword (find-symbol (string-upcase name) :keyword)))
    (and keyword (member keyword *bindable-commands*) keyword)))
