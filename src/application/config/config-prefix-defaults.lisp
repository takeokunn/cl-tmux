(in-package #:cl-tmux/config)

;;;; Default prefix-table key-binding data.
;;;;
;;;; This file contains the define-initial-key-bindings invocation (the
;;;; char/digit -> command pairs bound into the prefix key-table) plus the
;;;; DATA tables for the named-key (arrow) and C-/M- resize-pane bindings
;;;; that install-default-prefix-string-bindings installs into the prefix
;;;; key-table.  Splitting this data out of config.lisp keeps that file
;;;; focused on structure/protocol (prefix-key parsing, key-table constants,
;;;; initialization entry point) and mirrors the existing
;;;; config-copy-mode-defaults.lisp split for the copy-mode binding tables.
;;;;
;;;; Loaded by config.lisp via a fragment-loader eval-when block, after
;;;; define-initial-key-bindings and %install-key-bindings are defined, and
;;;; before install-default-prefix-string-bindings uses these tables.

(define-initial-key-bindings
  (#\c :new-window)
  (#\n :next-window)
  (#\p :prev-window)
  (#\" :split-horizontal)
  (#\% :split-vertical)
  (#\o :next-pane)
  (#\d :detach)
  (#\? :list-keys)
  (#\[ :copy-mode-enter)
  (#\] :paste-buffer)
  (#\x :kill-pane-confirm)
  (#\& :kill-window-confirm)
  (#\, :rename-window)
  (#\H :resize-left)
  (#\J :resize-down)
  (#\K :resize-up)
  ;; These are the bootstrap defaults. events-loop.lisp installs the live
  ;; bindings after startup (#\L -> last-session, #\! -> break-pane).
  (#\L :resize-right)
  (#\$ :rename-session)
  (#\! :if-shell)
  (:digits :select-window))

(defparameter +default-prefix-arrow-bindings+
  '(("Up"    :select-pane-up)
    ("Down"  :select-pane-down)
    ("Left"  :select-pane-left)
    ("Right" :select-pane-right))
  "Prefix-table arrow-key pane-selection bindings (non-repeatable).")

(defparameter +default-prefix-resize-bindings+
  '(("C-Up"    ("resize-pane" "-U" "1"))
    ("C-Down"  ("resize-pane" "-D" "1"))
    ("C-Left"  ("resize-pane" "-L" "1"))
    ("C-Right" ("resize-pane" "-R" "1"))
    ("M-Up"    ("resize-pane" "-U" "5"))
    ("M-Down"  ("resize-pane" "-D" "5"))
    ("M-Left"  ("resize-pane" "-L" "5"))
    ("M-Right" ("resize-pane" "-R" "5")))
  "Prefix-table C-/M- arrow resize-pane bindings (repeatable, per tmux repeat-time).")

(defparameter +default-copy-mode-named-navigation-bindings+
  '(("Up"       :copy-mode-cursor-up)
    ("Down"     :copy-mode-cursor-down)
    ("Left"     :copy-mode-cursor-left)
    ("Right"    :copy-mode-cursor-right)
    ("C-Up"     :copy-mode-scroll-up-line)
    ("C-Down"   :copy-mode-scroll-down-line)
    ("PageUp"   :copy-mode-page-up)
    ("PageDown" :copy-mode-page-down)
    ("Home"     :copy-mode-line-start)
    ("End"      :copy-mode-line-end))
  "Named-key copy-mode navigation bindings shared by the emacs and vi copy-mode
   key tables.")
