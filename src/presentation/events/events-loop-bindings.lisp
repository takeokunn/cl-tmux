(in-package #:cl-tmux)

;;;; Extended prefix key-binding table installation.

;;; ── Additional key bindings ─────────────────────────────────────────────────
;;;
;;; These extend config.lisp's prefix defaults.  They live in a FUNCTION (not
;;; top-level side effects) so test isolation (with-isolated-config) can reinstall
;;; them onto a fresh *key-tables*, keeping the isolated table consistent with the
;;; live image (e.g. C-b z = zoom-toggle, C-b L = last-session).

(defparameter *extended-prefix-bindings*
  `(;;; ── Session / client navigation ────────────────────────────────────────
    (#\s :choose-session)
    (#\( :switch-client-prev)
    (#\) :switch-client-next)
    (#\L :last-session)
    (#\D :choose-client)
    ;;; ── Window navigation ───────────────────────────────────────────────────
    (#\w :choose-window)
    (#\l :last-window)
    (#\f :find-window)
    (#\. :move-window-prompt)
    (#\' :select-window-prompt)
    ;;; ── Layout ──────────────────────────────────────────────────────────────
    (#\E :select-layout-spread)
    (,(code-char 32) :next-layout)              ; Space
    ("M-1" '("select-layout" "even-horizontal"))
    ("M-2" '("select-layout" "even-vertical"))
    ("M-3" '("select-layout" "main-horizontal"))
    ("M-4" '("select-layout" "main-vertical"))
    ("M-5" '("select-layout" "tiled"))
    ("M-n" '("next-window" "-a"))
    ("M-p" '("previous-window" "-a"))
    ("M-o" '("rotate-window" "-D"))
    ;;; ── Pane operations ─────────────────────────────────────────────────────
    (#\! :break-pane)
    (#\{ :swap-pane-backward)
    (#\} :swap-pane-forward)
    (#\; :last-pane)
    (#\q :display-panes)
    (#\z :zoom-toggle)
    (#\m :mark-pane)
    (,(code-char 77) :clear-mark)               ; M
    ;;; ── Prompt / command ────────────────────────────────────────────────────
    ("PageUp" '("copy-mode-enter" "-u"))
    (,(code-char 2) :send-prefix)               ; C-b (literal prefix forward)
    (,(code-char 35) :list-buffers)             ; #
    (,(code-char 61) :choose-buffer)            ; =
    (,(code-char 45) :delete-buffer)            ; -
    (#\: :command-prompt)
    (#\C '("customize-mode"))
    (#\r :refresh-client)
    (#\t :clock-mode)
    (#\i :display-info)
    (,(code-char 126) :show-messages)           ; ~
    (,(code-char 15) :rotate-window)            ; C-o
    (,(code-char 26) :suspend-client))          ; C-z
  "Prefix bindings that extend config.lisp's defaults.")

(defun install-extended-key-bindings ()
  "Install the prefix bindings that extend config.lisp's defaults.  Idempotent.
   Called once at load time, and again by with-isolated-config under test."
  (mapc (lambda (binding)
          (destructuring-bind (key command) binding
            (key-table-bind +table-prefix+ key command)))
        *extended-prefix-bindings*)
  (values))

;; Install once at load time so the running image has the full default set.
(install-extended-key-bindings)
