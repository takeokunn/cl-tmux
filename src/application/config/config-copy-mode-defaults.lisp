(in-package #:cl-tmux/config)

;;;; Default copy-mode key-binding data tables.
;;;;
;;;; This file contains only the +default-copy-mode-bindings+ and
;;;; +default-copy-mode-vi-bindings+ defparameter data plus the two
;;;; install-default-copy-mode-* functions that populate the key tables
;;;; at runtime.  Splitting the ~130-line binding data out of config.lisp
;;;; keeps that file focused on structure/protocol (key table types,
;;;; prefix table defaults, initialization entry point) and makes the
;;;; copy-mode defaults easy to audit against tmux source without noise.
;;;;
;;;; Loaded by config.lisp via a fragment-loader eval-when block, after
;;;; %install-key-bindings and %bind-copy-mode-named-navigation are defined.

;;; ── Default emacs copy-mode bindings ─────────────────────────────────────

(defparameter +default-copy-mode-bindings+
  '(("M-f" :copy-mode-word-end)
    ("M-b" :copy-mode-word-backward)
    ("M-e" :copy-mode-word-end)
    ("C-M-f" :copy-mode-next-matching-bracket)
    ("C-M-b" :copy-mode-previous-matching-bracket)
    ("C-Space" :copy-mode-begin-selection)
    ("C-a" :copy-mode-line-start)
    ("C-c" :copy-mode-exit)
    ("C-e" :copy-mode-line-end)
    ("C-f" :copy-mode-cursor-right)
    ("C-b" :copy-mode-cursor-left)
    ("C-g" :copy-mode-clear-selection)
    ("C-l" :copy-mode-cursor-centre-vertical)
    ("C-k" :copy-mode-copy-pipe-end-of-line-and-cancel)
    ("C-n" :copy-mode-cursor-down)
    ("C-p" :copy-mode-cursor-up)
    ("C-r" :copy-mode-search-backward-incremental)
    ("C-s" :copy-mode-search-forward-incremental)
    ("C-v" :copy-mode-page-down)
    ("C-w" :copy-mode-copy-pipe-and-cancel)
    ("M-<" :copy-mode-top)
    ("M->" :copy-mode-bottom)
    ("M-v" :copy-mode-page-up)
    ("M-Up" :copy-mode-half-page-up)
    ("M-Down" :copy-mode-half-page-down)
    ("M-l" :copy-mode-cursor-centre-horizontal)
    ("M-r" :copy-mode-middle)
    ("M-R" :copy-mode-high)
    ("M-w" :copy-mode-yank)
    ("M-m" :copy-mode-back-to-indentation)
    ("M-x" :copy-mode-jump-to-mark)
    (#\f :copy-mode-jump-forward)
    (#\F :copy-mode-jump-backward)
    (#\t :copy-mode-jump-to)
    (#\T :copy-mode-jump-to-backward)
    (#\g :copy-mode-goto-line)
    ("M-{" :copy-mode-prev-paragraph)
    ("M-}" :copy-mode-next-paragraph)
    ("Escape" :copy-mode-exit)
    (#\q :copy-mode-exit)
    (#\Space :copy-mode-page-down)
    (#\, :copy-mode-jump-reverse)
    (#\; :copy-mode-jump-again)
    (#\N :copy-mode-search-prev)
    (#\P :copy-mode-other-end)
    (#\R :copy-mode-rectangle-toggle)
    (#\X :copy-mode-set-mark)
    (#\n :copy-mode-search-next)
    (#\r :copy-mode-refresh-from-pane))
  "Default tmux copy-mode bindings for the emacs-style table.
   Matches tmux 3.x key_bindings.c copy-mode emacs defaults.")

(defparameter +copy-mode-repeatable-commands+
  '(:copy-mode-cursor-left
    :copy-mode-cursor-right
    :copy-mode-cursor-down
    :copy-mode-cursor-up
    :copy-mode-scroll-down-line
    :copy-mode-scroll-up-line
    :copy-mode-page-down
    :copy-mode-page-up
    :copy-mode-half-page-up
    :copy-mode-half-page-down
    :copy-mode-word-forward
    :copy-mode-word-backward
    :copy-mode-word-end
    :copy-mode-space-forward
    :copy-mode-space-backward
    :copy-mode-space-end
    :copy-mode-prev-paragraph
    :copy-mode-next-paragraph
    :copy-mode-next-matching-bracket
    :copy-mode-previous-matching-bracket
    :copy-mode-jump-again
    :copy-mode-jump-reverse)
  "Copy-mode commands that consume numeric prefixes as repeat counts.")

;;; ── Default vi copy-mode bindings ────────────────────────────────────────

(defparameter +default-copy-mode-vi-bindings+
  '((#\q :copy-mode-exit)
    (#\i :copy-mode-exit)
    (#\h :copy-mode-cursor-left)
    (#\j :copy-mode-cursor-down)
    (#\k :copy-mode-cursor-up)
    (#\l :copy-mode-cursor-right)
    (#\Space :copy-mode-begin-selection)
    (#\v :copy-mode-begin-selection)
    (#\V :copy-mode-begin-line-selection)
    (#\y :copy-mode-yank)
    (#\w :copy-mode-word-forward)
    (#\b :copy-mode-word-backward)
    (#\e :copy-mode-word-end)
    (#\W :copy-mode-space-forward)
    (#\B :copy-mode-space-backward)
    (#\E :copy-mode-space-end)
    (#\0 :copy-mode-line-start)
    (#\^ :copy-mode-back-to-indentation)
    (#\$ :copy-mode-line-end)
    (#\% :copy-mode-next-matching-bracket)
    (#\, :copy-mode-jump-reverse)
    (#\; :copy-mode-jump-again)
    (#\g :copy-mode-top)
    (#\G :copy-mode-bottom)
    (#\H :copy-mode-high)
    (#\J :copy-mode-scroll-down-line)
    (#\K :copy-mode-scroll-up-line)
    (#\M :copy-mode-middle)
    (#\L :copy-mode-low)
    (#\D :copy-mode-copy-pipe-end-of-line-and-cancel)
    (#\Y :copy-mode-copy-line)
    (#\A :copy-mode-append-selection-and-cancel)
    (#\P :copy-mode-other-end)
    (#\R :copy-mode-rectangle-toggle)
    (#\X :copy-mode-set-mark)
    (#\# :copy-mode-search-backward-word)
    (#\* :copy-mode-search-forward-word)
    (#\n :copy-mode-search-next)
    (#\N :copy-mode-search-prev)
    (#\f :copy-mode-jump-forward)
    (#\F :copy-mode-jump-backward)
    (#\t :copy-mode-jump-to)
    (#\T :copy-mode-jump-to-backward)
    (#\o :copy-mode-other-end)
    (#\/ :copy-mode-search-forward-prompt)
    (#\? :copy-mode-search-backward-prompt)
    (#\= :copy-mode-choose-buffer)
    (#\{ :copy-mode-prev-paragraph)
    (#\} :copy-mode-next-paragraph)
    (#\z :copy-mode-scroll-middle)
    ("M-x" :copy-mode-jump-to-mark)
    ("Escape" :copy-mode-clear-selection)
    ("C-c" :copy-mode-exit)
    ("C-d" :copy-mode-half-page-down)
    ("C-e" :copy-mode-scroll-down-line)
    ("C-b" :copy-mode-page-up)
    ("C-f" :copy-mode-page-down)
    ("C-h" :copy-mode-cursor-left)
    ("C-j" :copy-mode-copy-pipe-and-cancel)
    ("Enter" :copy-mode-copy-pipe-and-cancel)
    ("C-u" :copy-mode-half-page-up)
    ("C-v" :copy-mode-rectangle-toggle)
    ("C-y" :copy-mode-scroll-up-line)
    ("BSpace" :copy-mode-cursor-left)
    (#\r :copy-mode-refresh-from-pane)
    (#\: :copy-mode-goto-line))
  "Default tmux copy-mode bindings for the vi-style table.
   Matches tmux 3.x key_bindings.c copy-mode-vi defaults.")

;;; ── Install functions ─────────────────────────────────────────────────────

(defun copy-mode-count-command-p (command)
  "Does COMMAND consume the copy-mode numeric prefix as a repeat count?
   This is a property of the copy-mode command itself, distinct from the
   key-table -r (repeat-time) flag: tmux's default copy-mode bindings carry
   no -r in list-keys, yet motions like cursor-down honour vi-style counts."
  (not (null (member command +copy-mode-repeatable-commands+))))

(defun %install-copy-mode-bindings (table-name bindings)
  "Populate TABLE-NAME with copy-mode BINDINGS.
   Deliberately does NOT set :repeatable — count consumption is derived from
   the command via COPY-MODE-COUNT-COMMAND-P, so list-keys output matches
   tmux (no -r on default copy-mode bindings)."
  (dolist (binding bindings)
    (destructuring-bind (key command) binding
      (key-table-bind table-name key command))))

(defun install-default-copy-mode-bindings ()
  "Populate the 'copy-mode' (emacs) key table with tmux 3.x default bindings.
   Meta bindings use names like \"M-f\" so they match what %meta-key-name produces
   when ESC+key arrives in the input stream.  Idempotent."
  (%install-copy-mode-bindings +table-copy-mode+ +default-copy-mode-bindings+)
  (%bind-copy-mode-named-navigation +table-copy-mode+))

(defun install-default-copy-mode-vi-bindings ()
  "Populate the 'copy-mode-vi' key table with tmux 3.x default bindings."
  (%install-copy-mode-bindings +table-copy-mode-vi+ +default-copy-mode-vi-bindings+)
  (%bind-copy-mode-named-navigation +table-copy-mode-vi+))
