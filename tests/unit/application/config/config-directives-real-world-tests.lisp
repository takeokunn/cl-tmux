(in-package #:cl-tmux/test)

;;;; Real-world .tmux.conf end-to-end fixture for config directives.
;;;; config-directives-tests.lisp declares config-directives-suite.

;;; A representative config exercising the constructs real configs
;;; (oh-my-tmux/gpakosz, common tutorials) actually use: canonical commands,
;;; -g/-s/-r/-n/-T flags, user options, style strings, format strings,
;;; top-level `;` sequences, if-shell, run-shell -b, source-file -q, and the
;;; %if preprocessor.  Loaded whole-file so the preprocessor runs too.

(defparameter +real-world-tmux-conf-lines+
  '("# ── general ──────────────────────────"
    "set-option -g default-terminal \"screen-256color\""
    "set-option -s escape-time 10"
    "set-option -g prefix2 C-a"
    "bind C-a send-prefix"
    "set-option -g history-limit 5000"
    "set-option -g mouse on"
    "set-window-option -g automatic-rename on"
    "set-option -g renumber-windows on"
    "set-option -g set-titles on"
    "set-option -g set-titles-string \"#h - #S - #I #W\""
    ""
    "# ── display ──────────────────────────"
    "set-option -g status-interval 10"
    "set-option -g status-left \"#[fg=green](#S) \""
    "set-option -g status-right \"#[fg=yellow]%H:%M\""
    "set-window-option -g window-status-current-style \"fg=black,bg=white\""
    "set-option -g monitor-activity on"
    "set-option -g visual-activity off"
    "set-option -g @plugin \"tmux-plugins/tmux-sensible\""
    ""
    "# ── navigation (canonical commands + repeat/root flags) ──"
    "bind - split-window -v"
    "bind _ split-window -h"
    "bind -r h select-pane -L"
    "bind -r l select-pane -R"
    "bind Tab last-window"
    ""
    "# ── copy mode vi ─────────────────────"
    "set-window-option -g mode-keys vi"
    "bind -T copy-mode-vi v send-keys -X begin-selection"
    "bind -T copy-mode-vi y send-keys -X copy-selection-and-cancel"
    ""
    "# ── conditionals / shell / sequences ─"
    "if \"true\" \"set-option -g @cond-ok yes\""
    "run-shell -b \"true\""
    "source-file -q /nonexistent-cl-tmux-local.conf"
    "set-option -g @multi 1; set-option -g @multi2 2"
    "%if #{==:never,ever}"
    "set-option -g @never 1"
    "%endif"
    ""
    "# ── wave 2: terminal-overrides / env / rebind / hooks / continuation ─"
    "set-option -ga terminal-overrides \",xterm-256color:Tc\""
    "set-environment -g CLTMUX_FIXTURE_ENV fixture-value"
    "bind x kill-pane"
    "unbind x"
    "bind r source-file -q /nonexistent-reload.conf \\; display-message \"reloaded\""
    "set-hook -g after-new-window \"set-option -g @hooked yes\""
    "set-option -g @continued \\"
    "joined")
  "Line list for the real-world config fixture (kept as data for readability).")

(describe "config-directives-suite"

  ;; The representative real-world .tmux.conf loads end-to-end and its directives
  ;; take observable effect (options, user options, bindings, %if suppression).
  ;; Binds the production format-based %if condition evaluator (main.lisp installs
  ;; the same shape at startup; the test default of NIL treats every %if as true).
  (it "real-world-tmux-conf-loads-with-effects"
    (with-isolated-config
     (with-isolated-hooks
      (let ((cl-tmux/config:*config-condition-evaluator*
              (lambda (cond-str) (cl-tmux/format:expand-format cond-str nil)))
            (path (merge-pathnames
                   (format nil "cl-tmux-realworld-~D.conf" (random 1000000))
                   (uiop:temporary-directory))))
        (unwind-protect
             (progn
               (with-open-file (s path :direction :output :if-exists :supersede)
                 (dolist (line +real-world-tmux-conf-lines+)
                   (write-line line s)))
               (expect (cl-tmux/config:load-config-file path) :to-be-truthy)
               ;; Options (typed coercion + strings + styles + formats).
               (expect (eql 5000 (cl-tmux/options:get-option "history-limit")))
               (expect (eq t (cl-tmux/options:get-option "mouse")))
               (expect (eq t (cl-tmux/options:get-option "renumber-windows")))
               (expect (string= "#[fg=green](#S) "
                            (cl-tmux/options:get-option "status-left")))
               (expect (string= "fg=black,bg=white"
                            (cl-tmux/options:get-option "window-status-current-style")))
               (expect (string= "vi" (cl-tmux/options:get-option "mode-keys")))
               ;; User options (@-prefixed) including the `;` sequence line.
               (expect (string= "tmux-plugins/tmux-sensible"
                            (cl-tmux/options:get-option "@plugin")))
               (expect (string= "yes" (cl-tmux/options:get-option "@cond-ok")))
               (expect (string= "1" (cl-tmux/options:get-option "@multi")))
               (expect (string= "2" (cl-tmux/options:get-option "@multi2")))
               ;; %if false branch must NOT have run.
               (expect (null (cl-tmux/options:get-option "@never" nil)))
               ;; Bindings: prefix table, repeat flag path,
               ;; and the copy-mode-vi table.
               (expect (cl-tmux/config:key-table-lookup "prefix" #\-) :to-be-truthy)
               (expect (cl-tmux/config:key-table-lookup "prefix" #\h) :to-be-truthy)
               (expect (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v) :to-be-truthy)
               (expect (cl-tmux/config:key-table-lookup "copy-mode-vi" #\y) :to-be-truthy)
               ;; Wave 2: terminal-overrides append (present in virtually every
               ;; real config), env, unbind, reload binding, set-hook, and
               ;; backslash line continuation.
               (expect (search "xterm-256color:Tc"
                           (or (cl-tmux/options:get-option "terminal-overrides" nil) "")))
               (expect (string= "fixture-value"
                            (or (sb-ext:posix-getenv "CLTMUX_FIXTURE_ENV") "")))
               (expect (null (cl-tmux/config:key-table-lookup "prefix" #\x)))
               (expect (cl-tmux/config:key-table-lookup "prefix" #\r) :to-be-truthy)
               (expect (gethash "after-new-window" cl-tmux/hooks::*command-hooks*) :to-be-truthy)
               (expect (string= "joined" (cl-tmux/options:get-option "@continued"))))
          (ignore-errors (delete-file path))))))))
