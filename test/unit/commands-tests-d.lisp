(in-package #:cl-tmux/test)

;;;; rename hooks, server-access, customize-mode, copy-mode-toggle/append/copy-pipe — part VIII

(in-suite commands-suite)

;;; ── rename-window: fires hook ────────────────────────────────────────────────

(test rename-window-fires-after-rename-window-hook
  "rename-window fires +hook-after-rename-window+ with the window and new name."
  (with-isolated-hooks
    (let ((hook-win nil)
          (hook-name nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+
                              (lambda (w n) (setf hook-win w hook-name n)))
      (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
        (rename-window win "new"))
      (is (stringp hook-name)
          "hook must receive the new name as a string")
      (is (string= "new" hook-name)
          "hook name argument must equal the new name"))))

(test rename-window-disable-automatic-rename-flag
  "A manual rename-window (default) disables automatic-rename; passing
   :disable-automatic-rename NIL (the auto-rename path) keeps it on."
  (let ((win (make-window :id 1 :name "x" :width 20 :height 5 :panes nil)))
    (setf (window-automatic-rename-p win) t)
    (rename-window win "manual")
    (is-false (window-automatic-rename-p win)
              "manual rename disables automatic-rename")
    (setf (window-automatic-rename-p win) t)
    (rename-window win "auto" :disable-automatic-rename nil)
    (is-true (window-automatic-rename-p win)
             ":disable-automatic-rename NIL keeps automatic-rename on")))

(test rename-window-fires-window-renamed-hook
  "rename-window also fires +hook-window-renamed+ (tmux's window-renamed hook)."
  (with-isolated-hooks
    (let ((fired nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-window-renamed+
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
        (rename-window win "new"))
      (is-true fired "window-renamed hook must fire on rename"))))

(test cmd-rename-session-fires-session-renamed-hook
  "%cmd-rename-session fires +hook-session-renamed+."
  (with-isolated-hooks
    (let ((cl-tmux::*server-sessions* nil)
          (s (make-fake-session :nwindows 1))
          (fired nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-renamed+
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%cmd-rename-session s '("newname"))
      (is-true fired "session-renamed hook must fire on rename-session"))))

(test cmd-select-pane-fires-after-select-pane-hook
  "%cmd-select-pane fires +hook-after-select-pane+ regardless of which form it took."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let ((fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-select-pane+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-select-pane s '("-m"))
        (is-true fired "after-select-pane hook must fire")))))

(test cmd-select-window-fires-after-select-window-hook
  "%cmd-select-window fires +hook-after-select-window+ (tmux's per-command hook)."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 2)
      (let ((fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-select-window+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-select-window s '("-n"))   ; select next window
        (is-true fired "after-select-window hook must fire")))))

(test session-window-changed-hook-fires-on-window-switch
  "session-window-changed fires when the active window actually changes (the
   focus-transition diff covers any switch path, not just select-window)."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 2)
      (let ((fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-window-changed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-select-window s '("-n"))   ; switch to the next window
        (is-true fired
                 "session-window-changed must fire when the active window changes")))))

(test window-pane-changed-hook-fires-on-pane-switch
  "window-pane-changed fires when a window's active pane changes (any select-pane
   path routes through %select-pane-with-focus's diff)."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let ((fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-window-pane-changed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%run-command-line s "select-pane -t 2")   ; switch to pane 2
        (is-true fired
                 "window-pane-changed must fire when the active pane changes")))))

(test resize-pane-fires-after-resize-pane-hook
  "resize-pane fires +hook-after-resize-pane+ (covers both the resize-pane command
   and the C-b H/J/K/L keybind path, which share this function)."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let* ((win (cl-tmux/model:session-active-window s))
             (fired nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-resize-pane+
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (resize-pane win :up 2)
      (is-true fired "after-resize-pane hook must fire")))))

;;; ── server-access: access-control-list management ──────────────────────────

(test server-access-add-permission-table
  "server-access -a adds a user with the correct permission (default read-write; -r read-only)."
  (dolist (row '(("server-access -a alice"   "alice" :read-write "default → :read-write")
                 ("server-access -a -r bob"  "bob"   :read-only  "-r → :read-only")))
    (destructuring-bind (cmd user expected desc) row
      (with-fake-session (s :nwindows 1)
        (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
          (cl-tmux::%run-command-line s cmd)
          (is (equal expected
                     (cdr (assoc user cl-tmux::*server-access-list* :test #'string=)))
              "~A" desc))))))

(test server-access-w-modifies-existing-user-permission
  "A bare `server-access -w USER` (no -a/-d) upgrades an existing entry to read-write."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-access-list* (list (cons "carol" :read-only)))
          (*overlay* nil))
      (cl-tmux::%run-command-line s "server-access -w carol")
      (is (equal :read-write
                 (cdr (assoc "carol" cl-tmux::*server-access-list* :test #'string=)))
          "-w must upgrade carol from read-only to read-write"))))

(test server-access-modify-unknown-user-is-error-no-entry-created
  "Modifying (no -a) an unknown user is an error and must NOT create an entry,
   matching tmux's `server-access user` semantics."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
      (cl-tmux::%run-command-line s "server-access -w nobody")
      (is (null cl-tmux::*server-access-list*)
          "modifying an unknown user must not add it to the list"))))

(test server-access-delete-removes-user
  "server-access -d USER removes USER from the access list."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-access-list*
            (list (cons "alice" :read-write) (cons "bob" :read-only)))
          (*overlay* nil))
      (cl-tmux::%run-command-line s "server-access -d alice")
      (is (null (assoc "alice" cl-tmux::*server-access-list* :test #'string=))
          "alice must be removed")
      (is (equal :read-only
                 (cdr (assoc "bob" cl-tmux::*server-access-list* :test #'string=)))
          "bob must be left untouched"))))

(test server-access-l-lists-entries-in-overlay
  "server-access -l renders each entry as `name: permission` in the overlay."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-access-list*
            (list (cons "alice" :read-write)))
          (*overlay* nil))
      (cl-tmux::%run-command-line s "server-access -l")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "alice" text) "listing must contain the user name")
        (is (search "read-write" text)
            "listing must contain the user's permission")))))

(test server-access-k-flag-accepted-without-error
  "server-access -k USER (kill clients) is accepted as a no-op in single-user
   cl-tmux and still applies the add when combined with -a."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
      (finishes (cl-tmux::%run-command-line s "server-access -a -k dave"))
      (is (assoc "dave" cl-tmux::*server-access-list* :test #'string=)
          "-k must not prevent the -a add"))))

;;; ── bare (no-arg) forms of list-commands / list-panes ───────────────────────

(test bare-list-commands-lists-commands-not-unknown
  "Bare `list-commands` (no args) must list commands, not error as unknown —
   it falls through *arg-command-table* (args-only) to the named-command table."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "list-commands")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (not (search "unknown command" text))
            "bare list-commands must not be an unknown command")
        (is (search "new-window" text)
            "list-commands output must include a known command name")))))

(test bare-list-panes-lists-panes-not-unknown
  "Bare `list-panes` (no args) must list the current window's panes."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "list-panes")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (not (search "unknown command" text))
            "bare list-panes must not be an unknown command")
        (is (search "(active)" text)
            "list-panes output must mark the active pane")))))

;;; ── customize-mode: options/bindings customize tree ─────────────────────────

(test customize-mode-renders-grouped-tree-with-option-values
  "customize-mode renders the customize tree: grouped Session/Window Options with
   a known option name + value, plus the Key Bindings group."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "customize-mode")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Session/Window Options" text)
            "tree must group the session/window options")
        (is (search "mode-keys" text)
            "tree must list a known registered option name")
        (is (search "Key Bindings" text)
            "tree must include the key-bindings group")))))

(test customize-mode-f-filter-restricts-to-matching-entries
  "customize-mode -f FILTER keeps only entries whose name/line contains FILTER
   (case-insensitive substring) and drops the rest."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "customize-mode -f mode-keys")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "mode-keys" text)
            "filter must keep the matching option")
        (is (not (search "status-interval" text))
            "filter must drop options that do not match")))))

(test customize-mode-keyword-dispatch-opens-overlay
  "The bare :customize-mode keybinding form opens the customize overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :customize-mode nil)
      (is (overlay-active-p)
          ":customize-mode must open an overlay"))))
