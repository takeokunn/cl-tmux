(in-package #:cl-tmux/test)

;;;; rename hooks, server-access, customize-mode, copy-mode-toggle/append/copy-pipe — part VIII

(describe "commands-suite"

  ;;; ── rename-window: fires hook ────────────────────────────────────────────────

  ;; rename-window fires +hook-after-rename-window+ with the window and new name.
  (it "rename-window-fires-after-rename-window-hook"
    (with-isolated-hooks
      (let ((hook-win nil)
            (hook-name nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+
                                (lambda (w n) (setf hook-win w hook-name n)))
        (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
          (rename-window win "new")
          (expect (eq win hook-win)))
        (expect (stringp hook-name))
        (expect (string= "new" hook-name)))))

  ;; A manual rename-window (default) disables automatic-rename; passing
  ;; :disable-automatic-rename NIL (the auto-rename path) keeps it on.
  (it "rename-window-disable-automatic-rename-flag"
    (let ((win (make-window :id 1 :name "x" :width 20 :height 5 :panes nil)))
      (setf (window-automatic-rename-p win) t)
      (rename-window win "manual")
      (expect (window-automatic-rename-p win) :to-be-falsy)
      (setf (window-automatic-rename-p win) t)
      (rename-window win "auto" :disable-automatic-rename nil)
      (expect (window-automatic-rename-p win) :to-be-truthy)))

  ;; rename-window also fires +hook-window-renamed+ (tmux's window-renamed hook).
  (it "rename-window-fires-window-renamed-hook"
    (with-isolated-hooks
      (let ((fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-window-renamed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
          (rename-window win "new"))
        (expect fired :to-be-truthy))))

  ;; %cmd-rename-session fires +hook-session-renamed+.
  (it "cmd-rename-session-fires-session-renamed-hook"
    (with-isolated-hooks
      (let ((cl-tmux::*server-sessions* nil)
            (s (make-fake-session :nwindows 1))
            (fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-renamed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-rename-session s '("newname"))
        (expect fired :to-be-truthy))))

  ;; %cmd-select-pane fires +hook-after-select-pane+ regardless of which form it took.
  (it "cmd-select-pane-fires-after-select-pane-hook"
    (with-isolated-hooks
      (with-fake-two-pane-session (s)
        (let ((fired nil))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-select-pane+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%cmd-select-pane s '("-m"))
          (expect fired :to-be-truthy)))))

  ;; %cmd-select-window fires +hook-after-select-window+ (tmux's per-command hook).
  (it "cmd-select-window-fires-after-select-window-hook"
    (with-isolated-hooks
      (with-fake-session (s :nwindows 2)
        (let ((fired nil))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-select-window+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%cmd-select-window s '("-n"))   ; select next window
          (expect fired :to-be-truthy)))))

  ;; session-window-changed fires when the active window actually changes (the
  ;; focus-transition diff covers any switch path, not just select-window).
  (it "session-window-changed-hook-fires-on-window-switch"
    (with-isolated-hooks
      (with-fake-session (s :nwindows 2)
        (let ((fired nil))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-window-changed+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%cmd-select-window s '("-n"))   ; switch to the next window
          (expect fired :to-be-truthy)))))

  ;; window-pane-changed fires when a window's active pane changes (any select-pane
  ;; path routes through %select-pane-with-focus's diff).
  (it "window-pane-changed-hook-fires-on-pane-switch"
    (with-isolated-hooks
      (with-fake-two-pane-session (s)
        (let ((fired nil))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-window-pane-changed+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%run-command-line s "select-pane -t 2")   ; switch to pane 2
          (expect fired :to-be-truthy)))))

  ;; resize-pane fires +hook-after-resize-pane+ (covers both the resize-pane command
  ;; and the C-b H/J/K/L keybind path, which share this function).
  (it "resize-pane-fires-after-resize-pane-hook"
    (with-isolated-hooks
      (with-fake-two-pane-session (s)
        (let* ((win (cl-tmux/model:session-active-window s))
               (fired nil))
          (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-resize-pane+
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (resize-pane win :up 2)
          (expect fired :to-be-truthy)))))

  ;;; ── server-access: access-control-list management ──────────────────────────

  ;; server-access -a adds a user with the correct permission (default read-write; -r read-only).
  (it "server-access-add-permission-table"
    (dolist (row '(("server-access -a alice"   "alice" :read-write "default → :read-write")
                   ("server-access -a -r bob"  "bob"   :read-only  "-r → :read-only")))
      (destructuring-bind (cmd user expected desc) row
        (declare (ignore desc))
        (with-fake-session (s :nwindows 1)
          (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
            (cl-tmux::%run-command-line s cmd)
            (expect (equal expected
                           (alist-value user cl-tmux::*server-access-list*
                                        :test #'string=))))))))

  ;; A bare `server-access -w USER` (no -a/-d) upgrades an existing entry to read-write.
  (it "server-access-w-modifies-existing-user-permission"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*server-access-list* (list (cons "carol" :read-only)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -w carol")
        (expect (equal :read-write
                       (alist-value "carol" cl-tmux::*server-access-list*
                                    :test #'string=))))))

  ;; Modifying (no -a) an unknown user is an error and must NOT create an entry,
  ;; matching tmux's `server-access user` semantics.
  (it "server-access-modify-unknown-user-is-error-no-entry-created"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -w nobody")
        (expect (null cl-tmux::*server-access-list*)))))

  ;; server-access -d USER removes USER from the access list.
  (it "server-access-delete-removes-user"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*server-access-list*
              (list (cons "alice" :read-write) (cons "bob" :read-only)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -d alice")
        (expect (null (assoc "alice" cl-tmux::*server-access-list* :test #'string=)))
        (expect (equal :read-only
                       (alist-value "bob" cl-tmux::*server-access-list*
                                    :test #'string=))))))

  ;; server-access -l renders each entry as `name: permission` in the overlay.
  (it "server-access-l-lists-entries-in-overlay"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*server-access-list*
              (list (cons "alice" :read-write)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -l")
        (assert-overlay-contains "alice" (overlay-lines)
                                 "server-access -l listing")
        (assert-overlay-contains "read-write" (overlay-lines)
                                 "server-access -l listing"))))

  ;; server-access rejects unsupported flags and extra positionals before changing the access list.
  (it "server-access-rejects-unimplemented-kill-flag"
    (with-fake-session (s :nwindows 1)
      (dolist (args '(("-a" "-k" "dave")
                      ("-a" "dave" "extra")))
        (let* ((initial (list (cons "alice" :read-write)))
               (cl-tmux::*server-access-list* (copy-tree initial))
               (*overlay* nil))
          (expect (cl-tmux::%cmd-server-access s args) :to-be-falsy)
          (expect (equal initial cl-tmux::*server-access-list*))
          (assert-overlay-contains "unsupported argument" *overlay*
                                   args)))))

  ;;; ── bare (no-arg) forms of list-commands / list-panes ───────────────────────

  ;; Bare `list-commands` (no args) must list commands, not error as unknown —
  ;; it falls through *arg-command-table* (args-only) to the named-command table.
  (it "bare-list-commands-lists-commands-not-unknown"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-commands")
        (assert-overlay-not-contains "unknown command" (overlay-lines)
                                     "bare list-commands")
        (assert-overlay-contains "new-window" (overlay-lines)
                                 "bare list-commands"))))

  ;; Bare `list-panes` (no args) must list the current window's panes.
  (it "bare-list-panes-lists-panes-not-unknown"
    (with-fake-two-pane-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-panes")
        (assert-overlay-not-contains "unknown command" (overlay-lines)
                                     "bare list-panes")
        (assert-overlay-contains "(active)" (overlay-lines)
                                 "bare list-panes"))))

  ;;; ── customize-mode: options/bindings customize tree ─────────────────────────

  ;; customize-mode renders the customize tree: grouped Session/Window Options with
  ;; a known option name + value, plus the Key Bindings group.
  (it "customize-mode-renders-grouped-tree-with-option-values"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "customize-mode")
        (assert-overlay-contains "Session/Window Options" (overlay-lines)
                                 "customize-mode tree")
        (assert-overlay-contains "mode-keys" (overlay-lines)
                                 "customize-mode tree")
        (assert-overlay-contains "Key Bindings" (overlay-lines)
                                 "customize-mode tree"))))

  ;; customize-mode -f FILTER keeps only entries whose name/line contains FILTER
  ;; (case-insensitive substring) and drops the rest.
  (it "customize-mode-f-filter-restricts-to-matching-entries"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "customize-mode -f mode-keys")
        (assert-overlay-contains "mode-keys" (overlay-lines)
                                 "customize-mode -f mode-keys")
        (assert-overlay-not-contains "status-interval" (overlay-lines)
                                      "customize-mode -f mode-keys"))))

  ;; customize-mode rejects positional tokens it does not implement.  (The tmux
  ;; flags -N/-Z/-F/-t are accepted; only positional tokens are rejected.)
  (it "customize-mode-rejects-unsupported-arguments-before-rendering"
    (with-fake-session (s :nwindows 1)
      (dolist (line '("customize-mode mode-keys"))
        (let ((*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s line)))
          (assert-overlay-contains "customize-mode: unsupported argument"
                                   (overlay-lines) line)
          (assert-overlay-not-contains "Session/Window Options"
                                       (overlay-lines) line)))))

  ;; customize-mode accepts the tmux flags -N/-Z and -F/-t (whose arguments are
  ;; consumed) and renders the customize tree.
  (it "customize-mode-accepts-tmux-flags"
    (with-fake-session (s :nwindows 1)
      (dolist (line '("customize-mode -N"
                      "customize-mode -Z"
                      "customize-mode -t :.0"
                      "customize-mode -F #{pane_id}"))
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-not-contains "unsupported argument"
                                       (overlay-lines) line)))))

  ;; The bare :customize-mode keybinding form opens the customize overlay.
  (it "customize-mode-keyword-dispatch-opens-overlay"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :customize-mode nil)
        (assert-overlay-active ":customize-mode must open an overlay")))))
