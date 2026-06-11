(in-package #:cl-tmux/test)

;;;; commands tests — part D: rename hooks, server-access, customize-mode, copy-mode
;;;; rectangle/append/copy-pipe, renumber-windows, jump-to-char, set-mark, search-incr.

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
    (let ((s (make-fake-session :nwindows 1 :npanes 2))
          (fired nil))
      (with-loop-state
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-select-pane+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-select-pane s '("-m"))
        (is-true fired "after-select-pane hook must fire")))))

(test cmd-select-window-fires-after-select-window-hook
  "%cmd-select-window fires +hook-after-select-window+ (tmux's per-command hook)."
  (with-isolated-hooks
    (let ((s (make-fake-session :nwindows 2))
          (fired nil))
      (with-loop-state
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-select-window+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-select-window s '("-n"))   ; select next window
        (is-true fired "after-select-window hook must fire")))))

(test session-window-changed-hook-fires-on-window-switch
  "session-window-changed fires when the active window actually changes (the
   focus-transition diff covers any switch path, not just select-window)."
  (with-isolated-hooks
    (let ((s (make-fake-session :nwindows 2))
          (fired nil))
      (with-loop-state
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-window-changed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-select-window s '("-n"))   ; switch to the next window
        (is-true fired
                 "session-window-changed must fire when the active window changes")))))

(test window-pane-changed-hook-fires-on-pane-switch
  "window-pane-changed fires when a window's active pane changes (any select-pane
   path routes through %select-pane-with-focus's diff)."
  (with-isolated-hooks
    (let ((s (make-fake-session :nwindows 1 :npanes 2))
          (fired nil))
      (with-loop-state
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-window-pane-changed+
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%run-command-line s "select-pane -t 2")   ; switch to pane 2
        (is-true fired
                 "window-pane-changed must fire when the active pane changes")))))

(test resize-pane-fires-after-resize-pane-hook
  "resize-pane fires +hook-after-resize-pane+ (covers both the resize-pane command
   and the C-b H/J/K/L keybind path, which share this function)."
  (with-isolated-hooks
    (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
           (win (cl-tmux/model:session-active-window s))
           (fired nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-resize-pane+
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (resize-pane win :up 2)
      (is-true fired "after-resize-pane hook must fire"))))

;;; ── server-access: access-control-list management ──────────────────────────

(test server-access-add-records-user-read-write-by-default
  "server-access -a USER adds USER to the access list as read-write."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -a alice")
        (is (equal :read-write
                   (cdr (assoc "alice" cl-tmux::*server-access-list* :test #'string=)))
            "alice must be added with the default read-write permission")))))

(test server-access-add-r-records-user-read-only
  "server-access -a -r USER adds USER as read-only."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -a -r bob")
        (is (equal :read-only
                   (cdr (assoc "bob" cl-tmux::*server-access-list* :test #'string=)))
            "-r must record bob as read-only")))))

(test server-access-w-modifies-existing-user-permission
  "A bare `server-access -w USER` (no -a/-d) upgrades an existing entry to read-write."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list* (list (cons "carol" :read-only)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -w carol")
        (is (equal :read-write
                   (cdr (assoc "carol" cl-tmux::*server-access-list* :test #'string=)))
            "-w must upgrade carol from read-only to read-write")))))

(test server-access-modify-unknown-user-is-error-no-entry-created
  "Modifying (no -a) an unknown user is an error and must NOT create an entry,
   matching tmux's `server-access user` semantics."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -w nobody")
        (is (null cl-tmux::*server-access-list*)
            "modifying an unknown user must not add it to the list")))))

(test server-access-delete-removes-user
  "server-access -d USER removes USER from the access list."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list*
              (list (cons "alice" :read-write) (cons "bob" :read-only)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -d alice")
        (is (null (assoc "alice" cl-tmux::*server-access-list* :test #'string=))
            "alice must be removed")
        (is (equal :read-only
                   (cdr (assoc "bob" cl-tmux::*server-access-list* :test #'string=)))
            "bob must be left untouched")))))

(test server-access-l-lists-entries-in-overlay
  "server-access -l renders each entry as `name: permission` in the overlay."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list*
              (list (cons "alice" :read-write)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s "server-access -l")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "alice" text) "listing must contain the user name")
          (is (search "read-write" text)
              "listing must contain the user's permission"))))))

(test server-access-k-flag-accepted-without-error
  "server-access -k USER (kill clients) is accepted as a no-op in single-user
   cl-tmux and still applies the add when combined with -a."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((cl-tmux::*server-access-list* nil) (*overlay* nil))
        (finishes (cl-tmux::%run-command-line s "server-access -a -k dave"))
        (is (assoc "dave" cl-tmux::*server-access-list* :test #'string=)
            "-k must not prevent the -a add")))))

;;; ── bare (no-arg) forms of list-commands / list-panes ───────────────────────

(test bare-list-commands-lists-commands-not-unknown
  "Bare `list-commands` (no args) must list commands, not error as unknown —
   it falls through *arg-command-table* (args-only) to the named-command table."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-commands")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (not (search "unknown command" text))
              "bare list-commands must not be an unknown command")
          (is (search "new-window" text)
              "list-commands output must include a known command name"))))))

(test bare-list-panes-lists-panes-not-unknown
  "Bare `list-panes` (no args) must list the current window's panes."
  (let ((s (make-fake-session :nwindows 1 :npanes 2)))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-panes")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (not (search "unknown command" text))
              "bare list-panes must not be an unknown command")
          (is (search "(active)" text)
              "list-panes output must mark the active pane"))))))

;;; ── customize-mode: options/bindings customize tree ─────────────────────────

(test customize-mode-renders-grouped-tree-with-option-values
  "customize-mode renders the customize tree: grouped Session/Window Options with
   a known option name + value, plus the Key Bindings group."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "customize-mode")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "Session/Window Options" text)
              "tree must group the session/window options")
          (is (search "mode-keys" text)
              "tree must list a known registered option name")
          (is (search "Key Bindings" text)
              "tree must include the key-bindings group"))))))

(test customize-mode-f-filter-restricts-to-matching-entries
  "customize-mode -f FILTER keeps only entries whose name/line contains FILTER
   (case-insensitive substring) and drops the rest."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "customize-mode -f mode-keys")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "mode-keys" text)
              "filter must keep the matching option")
          (is (not (search "status-interval" text))
              "filter must drop options that do not match"))))))

(test customize-mode-keyword-dispatch-opens-overlay
  "The bare :customize-mode keybinding form opens the customize overlay."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :customize-mode nil)
        (is (overlay-active-p)
            ":customize-mode must open an overlay")))))

;;; ── copy-mode-begin-line-selection: multi-row window ────────────────────────

(test copy-mode-begin-line-selection-selects-correct-width
  "copy-mode-begin-line-selection marks col width-1 on a non-default screen width."
  (let ((s (make-screen 40 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::copy-mode-begin-line-selection s)
    (is (= 39 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be width-1=39 for 40-column screen")
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-mark s)))
        "mark col must be 0 for line selection")))

;;; ── copy-mode-copy-line: preserves content without trailing spaces ───────────

(test copy-mode-copy-line-right-trims-trailing-spaces
  "copy-mode-copy-line right-trims trailing spaces before pushing to paste buffer."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hi")          ; "hi" followed by 18 spaces on row 0
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-copy-line s)
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (string= "hi" yanked))
            "copy-mode-copy-line must right-trim spaces (got ~S)" yanked)))))

;;; ── copy-mode-copy-end-of-line: cursor at column 0 ──────────────────────────

(test copy-mode-copy-end-of-line-from-col-0-copies-entire-row
  "copy-mode-copy-end-of-line from col 0 copies the full row content."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (search "hello world" yanked))
            "D from col 0 must copy 'hello world' (got ~S)" yanked)))))

;;; ── with-shell-timeout macro coverage ───────────────────────────────────────

(test with-shell-timeout-returns-result-on-success
  "with-shell-timeout macro returns the result when thunk completes in time."
  (let ((result (cl-tmux/commands::with-shell-timeout (shell 30)
                  (string= "/bin/sh" shell)
                  42)))
    ;; result is the value of the last form in the body
    (is (= 42 result)
        "with-shell-timeout must return the last form result when no timeout")))

;;; ── %window-after-kill: empty list returns nil ───────────────────────────────

(test window-after-kill-empty-list-returns-nil
  "%window-after-kill with an empty remaining list returns NIL."
  (is (null (cl-tmux/commands::%window-after-kill nil 5))
      "%window-after-kill with empty list must return NIL"))

;;; ── kill-pane: fires hook ────────────────────────────────────────────────────

(test kill-pane-fires-after-kill-pane-hook
  "kill-pane fires +hook-after-kill-pane+ with the killed pane."
  (with-isolated-hooks
    (let ((hooked-pane nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                              (lambda (p) (setf hooked-pane p)))
      (let* ((win  (%vsplit-window 20))
             (p0   (first  (window-panes win)))
             (p1   (second (window-panes win)))
             (sess (make-session :id 1 :name "0" :windows (list win))))
        (session-select-window sess win)
        (window-select-pane win p0)
        (kill-pane sess p1)
        (is (eq p1 hooked-pane)
            "+hook-after-kill-pane+ must be called with the killed pane")))))

;;; ── kill-window: fires hook ──────────────────────────────────────────────────

(test kill-window-fires-after-kill-window-hook
  "kill-window fires +hook-after-kill-window+ with the killed window."
  (with-isolated-hooks
    (let ((hooked-win nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-window+
                              (lambda (w) (setf hooked-win w)))
      (let* ((p0   (%make-test-pane))
             (w1   (make-window :id 1 :name "a" :width 20 :height 5
                                :tree (make-layout-leaf p0) :panes (list p0)))
             (w2   (make-window :id 2 :name "b" :width 20 :height 5
                                :panes (list (%make-test-pane :id 2))))
             (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
        (session-select-window sess w1)
        (kill-window sess w1)
        (is (eq w1 hooked-win)
            "+hook-after-kill-window+ must be called with the killed window")))))

;;; ── copy-mode-toggle-rectangle ───────────────────────────────────────────────

(test copy-mode-toggle-rectangle-flips-flag
  "copy-mode-toggle-rectangle toggles screen-copy-rect-select-p between NIL and T."
  (let ((s (%copy-mode-screen)))
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must start NIL")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-true  (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must be T after first toggle")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must return to NIL after second toggle")))

(test copy-mode-toggle-rectangle-noop-outside-copy-mode
  "copy-mode-toggle-rectangle does nothing when not in copy mode."
  (let ((s (make-screen 20 5)))
    (is-false (screen-copy-mode-p s) "precondition: not in copy mode")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must remain NIL outside copy mode")))

(test copy-mode-exit-resets-rect-select
  "copy-mode-exit clears screen-copy-rect-select-p."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t)
    (cl-tmux/commands::copy-mode-exit s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must be NIL after exit")))

;;; ── copy-mode-append-selection ───────────────────────────────────────────────

(test copy-mode-append-selection-appends-to-existing-buffer
  "copy-mode-append-selection appends selected text to the current paste buffer entry."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    ;; Seed a buffer entry.
    (cl-tmux/buffer:add-paste-buffer "hello")
    (let ((s (make-screen 20 5)))
      (feed s " world")
      (cl-tmux/commands::copy-mode-enter s)
      ;; Manually set a selection spanning " world" on row 0.
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
      (cl-tmux/commands::copy-mode-append-selection s)
      ;; Exactly one buffer entry (appended, not pushed).
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "append-selection must not add a second paste buffer entry")
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (search "hello" buf))
            "appended buffer must contain original text")
        (is (and (stringp buf) (search " world" buf))
            "appended buffer must contain the newly appended text")))))

(test copy-mode-append-selection-creates-new-entry-when-empty
  "copy-mode-append-selection pushes a new entry when the paste buffer is empty."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection s)
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "append-selection must create one entry when buffer is empty")
      (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
          "new entry must equal the selected text"))))

(test copy-mode-append-selection-stays-in-copy-mode
  "copy-mode-append-selection must NOT exit copy mode (tmux append-selection stays in copy mode)."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection s)
      (is (cl-tmux/terminal/types:screen-copy-mode-p s)
          "append-selection must leave copy mode active"))))

(test copy-mode-append-selection-and-cancel-exits-copy-mode
  "copy-mode-append-selection-and-cancel exits copy mode after appending."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection-and-cancel s)
      (is (not (cl-tmux/terminal/types:screen-copy-mode-p s))
          "append-selection-and-cancel must exit copy mode")
      (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
          "buffer entry must be created"))))

;;; ── copy-mode-copy-pipe ──────────────────────────────────────────────────────

(test copy-mode-copy-pipe-puts-text-in-paste-buffer
  "copy-mode-copy-pipe adds the selected text to the paste buffer."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "pipe-me")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 7))
      ;; Pass an empty CMD so only the buffer side runs (no real shell invoked).
      (cl-tmux/commands::copy-mode-copy-pipe s "")
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "copy-pipe must push selected text to paste buffers")
      (is (string= "pipe-me" (cl-tmux/buffer:get-paste-buffer 0))
          "paste buffer must contain the selected text"))))

(test copy-mode-copy-pipe-exits-copy-mode
  "copy-mode-copy-pipe exits copy mode after yanking."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "data")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 4))
      (cl-tmux/commands::copy-mode-copy-pipe s "")
      (is-false (screen-copy-mode-p s)
                "copy mode must be inactive after copy-pipe"))))

;;; ── rectangle selection text ─────────────────────────────────────────────────

(test copy-mode-yank-rectangle-uses-fixed-columns
  "When rect-select is T, yank uses column bounds from mark and cursor on every row."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 10 5)))
      ;; Write row 0 "abcde" and row 1 "ABCDE" using CR+LF to ensure row 1 starts at col 0.
      (feed s (format nil "abcde~C~CABCDE" #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Rectangle col 1-3, rows 0-1.
      ;; %extract-row-chars from-col=1 to-col=3 → 2 chars at cols 1 and 2.
      ;; Row 0: "bc"; row 1: "BC".
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t
            (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 1)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
      (cl-tmux/commands::copy-mode-yank s)
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (search "bc" buf))
            "rectangle yank must include chars from first row")
        (is (and (stringp buf) (search "BC" buf))
            "rectangle yank must include chars from second row")))))

;;; ── renumber-windows option ───────────────────────────────────────────────────

(test renumber-windows-renumbers-after-kill
  "kill-window renumbers remaining windows from base-index when renumber-windows is on."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "renumber-windows" h) t
                 (gethash "base-index"       h) 0)
           h)))
    (let* ((s    (make-fake-session :nwindows 3))
           (wins (cl-tmux/model:session-windows s))
           ;; Manually give them non-contiguous IDs as if gaps already existed.
           (_ (setf (cl-tmux/model:window-id (first  wins)) 1
                    (cl-tmux/model:window-id (second wins)) 3
                    (cl-tmux/model:window-id (third  wins)) 5))
           ;; Kill the first window (id=1); remaining are 3 and 5.
           (_2 (kill-window s (first wins))))
      (declare (ignore _ _2))
      (let ((ids (mapcar #'cl-tmux/model:window-id (cl-tmux/model:session-windows s))))
        (is (equal '(0 1) ids)
            "After kill with renumber-windows, windows should be renumbered 0,1; got ~S" ids)))))

(test renumber-windows-off-preserves-ids
  "kill-window does not renumber windows when renumber-windows is off."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "renumber-windows" h) nil)
           h)))
    (let* ((s    (make-fake-session :nwindows 3))
           (wins (cl-tmux/model:session-windows s))
           (_ (setf (cl-tmux/model:window-id (first  wins)) 1
                    (cl-tmux/model:window-id (second wins)) 3
                    (cl-tmux/model:window-id (third  wins)) 5))
           (_2 (kill-window s (first wins))))
      (declare (ignore _ _2))
      (let ((ids (mapcar #'cl-tmux/model:window-id (cl-tmux/model:session-windows s))))
        (is (equal '(3 5) ids)
            "Without renumber-windows, IDs stay as-is; got ~S" ids)))))

;;; ── %rectangle-selection-text (direct unit tests) ────────────────────────────
;;;
;;; %rectangle-selection-text is exercised transitively through copy-mode-yank
;;; with rect-select=T.  These direct tests make boundary conditions explicit.

(test rectangle-selection-text-returns-nil-when-no-selection
  "%rectangle-selection-text returns NIL when no selection is active."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (is (null (cl-tmux/commands::%rectangle-selection-text s))
        "%rectangle-selection-text must return NIL when copy-selecting is NIL")))

(test rectangle-selection-text-returns-nil-when-mark-nil
  "%rectangle-selection-text returns NIL when mark is NIL even if selecting is T."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (is (null (cl-tmux/commands::%rectangle-selection-text s))
        "%rectangle-selection-text must return NIL when mark is NIL")))

(test rectangle-selection-text-single-row
  "%rectangle-selection-text returns the correct column slice for a single-row selection."
  ;; Feed "hello world" to row 0; rectangle from col 0 to col 5 on row 0 only.
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting    s) t
          (cl-tmux/terminal/types:screen-copy-mark         s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor       s) (cons 0 5))
    (let ((text (cl-tmux/commands::%rectangle-selection-text s)))
      (is (stringp text) "%rectangle-selection-text must return a string")
      (is (string= "hello" text)
          "%rectangle-selection-text must return cols 0-4 (got ~S)" text))))

(test rectangle-selection-text-multi-row-fixed-columns
  "%rectangle-selection-text extracts the same column range on every row."
  ;; Row 0 = "abcde", row 1 = "ABCDE"; rectangle col 1-3 (2 chars per row).
  (let ((s (make-screen 10 5)))
    (feed s (format nil "abcde~C~CABCDE" #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 1)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
    (let ((text (cl-tmux/commands::%rectangle-selection-text s)))
      (is (stringp text) "%rectangle-selection-text must return a string")
      (is (search "bc" text)
          "%rectangle-selection-text must include cols 1-2 from row 0 (got ~S)" text)
      (is (search "BC" text)
          "%rectangle-selection-text must include cols 1-2 from row 1 (got ~S)" text)
      (is (find #\Newline text)
          "%rectangle-selection-text must separate rows with newlines"))))

;;; ── %run-copy-command (direct unit tests) ────────────────────────────────────
;;;
;;; %run-copy-command is exercised only transitively through copy-mode-yank when
;;; the 'copy-command' option is set.  These direct tests cover the no-op branch
;;; (empty option / empty text) and the error-handling contract.

(test run-copy-command-noop-when-text-is-nil
  "%run-copy-command is a no-op when TEXT is NIL."
  (finishes (cl-tmux/commands::%run-copy-command nil)
            "%run-copy-command with nil text must not signal"))

(test run-copy-command-noop-when-text-is-empty
  "%run-copy-command is a no-op when TEXT is an empty string."
  (finishes (cl-tmux/commands::%run-copy-command "")
            "%run-copy-command with empty text must not signal"))

(test run-copy-command-noop-when-option-unset
  "%run-copy-command is a no-op when the 'copy-command' option is not set."
  ;; Fresh option table: 'copy-command' is absent.
  (with-fresh-global-options
    (finishes (cl-tmux/commands::%run-copy-command "some text")
              "%run-copy-command with no copy-command option must not signal")))

(test run-copy-command-does-not-crash-on-bad-command
  "%run-copy-command swallows errors from a malformed copy-command."
  ;; Set copy-command to a command that will fail (exit non-zero or not found).
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "copy-command" h) "false")
           h)))
    (finishes (cl-tmux/commands::%run-copy-command "hello")
              "%run-copy-command must not signal when the copy-command fails")))

;;; ── copy-mode-set-cursor (direct unit tests in commands group) ───────────────
;;;
;;; copy-mode-set-cursor is exported from cl-tmux/commands and tested in
;;; events-tests.lisp (via keystroke dispatch), but that test lives outside the
;;; commands audit scope.  Direct tests here make the commands group self-contained.

(test copy-mode-set-cursor-positions-cursor
  "copy-mode-set-cursor sets the cursor to the given row and column."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 2 7)
    (is (equal (cons 2 7) (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-mode-set-cursor must set cursor to (2 . 7)")))

(test copy-mode-set-cursor-clamps-row-to-bounds
  "copy-mode-set-cursor clamps the row to [0, height-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 99 0)
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row > height-1 must clamp to height-1=4")
    (cl-tmux/commands:copy-mode-set-cursor s -1 0)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row < 0 must clamp to 0")))

(test copy-mode-set-cursor-clamps-col-to-bounds
  "copy-mode-set-cursor clamps the column to [0, width-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 0 99)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "col > width-1 must clamp to width-1=19")
    (cl-tmux/commands:copy-mode-set-cursor s 0 -1)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "col < 0 must clamp to 0")))

(test copy-mode-set-cursor-noop-outside-copy-mode
  "copy-mode-set-cursor is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    ;; Do NOT enter copy mode.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 1))
    (cl-tmux/commands:copy-mode-set-cursor s 3 7)
    (is (equal (cons 1 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged outside copy mode")))

;;; ── send-keys -l (literal) vs translated ────────────────────────────────────
;;;
;;; send-keys-to-pane (pane string &key literal) is the production entry point,
;;; but it needs a pane with a real PTY (fd > -1) to observe output; fake panes
;;; have fd -1, where pty-write is a harmless no-op.  We therefore test the
;;; byte-production logic that distinguishes the two modes:
;;;   - non-literal: %translate-send-keys maps the key name "Enter" → CR (13).
;;;   - literal (-l): the string is emitted as raw UTF-8 bytes, so "Enter"
;;;     stays the 5 bytes E-n-t-e-r with no key-name interpretation.

(test send-keys-translated-enter-produces-cr
  "Without -l, %translate-send-keys maps the key name \"Enter\" to a single CR byte (13)."
  (let ((bytes (cl-tmux/commands::%translate-send-keys "Enter")))
    (is (= 1 (length bytes))
        "translated \"Enter\" must be exactly one byte (got length ~D)" (length bytes))
    (is (= 13 (aref bytes 0))
        "translated \"Enter\" must be CR (char code 13), got ~D" (aref bytes 0))))

(test send-keys-literal-enter-stays-five-bytes
  "With -l, the string \"Enter\" is written as raw UTF-8 bytes — five literal
   characters E-n-t-e-r — NOT translated to a CR.  This is the byte payload
   send-keys-to-pane writes when :literal is true."
  (let ((literal-bytes (babel:string-to-octets "Enter")))
    (is (= 5 (length literal-bytes))
        "literal \"Enter\" must be five bytes (got length ~D)" (length literal-bytes))
    (is (equalp #(69 110 116 101 114) literal-bytes)
        "literal \"Enter\" must be the ASCII bytes for E,n,t,e,r")
    ;; The literal payload must differ from the translated (single-CR) payload.
    (is (not (equalp literal-bytes
                     (cl-tmux/commands::%translate-send-keys "Enter")))
        "literal mode must NOT equal the translated single-CR payload")))

(test send-keys-literal-multibyte-utf8-preserves-bytes
  "With -l, a multi-byte UTF-8 string is emitted as its raw UTF-8 octets:
   \"café\" is 4 characters but encodes to 5 bytes (é = 2 bytes), so literal
   mode preserves the multi-byte encoding rather than counting characters."
  (let ((literal-bytes (babel:string-to-octets "café" :encoding :utf-8)))
    (is (= 5 (length literal-bytes))
        "literal \"café\" must be 5 UTF-8 bytes (got length ~D)" (length literal-bytes))
    (is (> (length literal-bytes) (length "café"))
        "byte count (~D) must exceed the 4-character count, proving multi-byte preservation"
        (length literal-bytes))
    ;; The é (U+00E9) encodes to the two-byte sequence C3 A9; assert the tail.
    (is (equalp #(195 169) (subseq literal-bytes 3))
        "the é must encode to the two UTF-8 bytes C3 A9 (got ~S)"
        (subseq literal-bytes 3))))

;;; ── Jump-to-char (vi f/F/t/T/;/,) ──────────────────────────────────────────

(defun %jump-screen (&optional (content "hello world"))
  "Return a copy-mode screen with CONTENT on row 0 and cursor at col 0."
  (let ((s (%copy-mode-screen :w 20 :h 3 :content content)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    s))

(test copy-mode-jump-forward-finds-char
  "jump-forward moves cursor to the next occurrence of the target char on the line."
  (let ((s (%jump-screen "hello world")))
    (cl-tmux/commands::copy-mode-jump-forward s #\l)
    (is (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-forward 'l' from col 0 must land on col 2 (first 'l')")))

(test copy-mode-jump-forward-no-match-stays-put
  "jump-forward does not move the cursor when the char is not found ahead."
  (let ((s (%jump-screen "hello world")))
    (setf (cdr (cl-tmux/terminal/types:screen-copy-cursor s)) 10) ; col 10 = 'd'
    (cl-tmux/commands::copy-mode-jump-forward s #\z)
    (is (= 10 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "no-match forward must leave cursor unchanged")))

(test copy-mode-jump-backward-finds-char
  "jump-backward moves cursor to the previous occurrence of the target char."
  (let ((s (%jump-screen "hello world")))
    (setf (cdr (cl-tmux/terminal/types:screen-copy-cursor s)) 10) ; start near end
    (cl-tmux/commands::copy-mode-jump-backward s #\l)
    (is (= 9 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-backward 'l' from col 10 must land on col 9 ('l' in 'world')")))

(test copy-mode-jump-to-stops-before-char
  "jump-to (vi t) lands one column BEFORE the target char (till)."
  (let ((s (%jump-screen "hello world")))
    (cl-tmux/commands::copy-mode-jump-to s #\l)
    (is (= 1 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-to 'l' from col 0 must land on col 1 (one before col 2)")))

(test copy-mode-jump-again-repeats-last
  "jump-again (vi ;) repeats the last jump-forward."
  (let ((s (%jump-screen "hello world")))
    (cl-tmux/commands::copy-mode-jump-forward s #\l)   ; lands col 2
    (cl-tmux/commands::copy-mode-jump-again s)         ; next 'l'
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-again must advance to col 3 (second 'l')")))

(test copy-mode-jump-reverse-reverses-forward
  "jump-reverse (vi ,) performs the jump in the opposite direction."
  (let ((s (%jump-screen "hello world")))
    (cl-tmux/commands::copy-mode-jump-forward s #\l)   ; lands col 2
    (cl-tmux/commands::copy-mode-jump-again  s)        ; lands col 3
    (cl-tmux/commands::copy-mode-jump-reverse s)       ; back to col 2
    (is (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "jump-reverse after two forward jumps must return to col 2")))

;;; ── copy-mode-set-mark ───────────────────────────────────────────────────────

(test copy-mode-set-mark-stores-current-cursor
  "copy-mode-set-mark stores the current cursor position as the mark."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-offset s)  2
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 7)
          (cl-tmux/terminal/types:screen-copy-mark   s) nil
          (cl-tmux/terminal/types:screen-copy-mark-offset s) 0)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is (equal (cons 3 7) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be set to current cursor position (row=3, col=7)")
    (is (= 2 (cl-tmux/terminal/types:screen-copy-mark-offset s))
        "mark-offset must match the current copy-offset")))

(test copy-mode-set-mark-does-not-start-selection
  "copy-mode-set-mark must NOT begin a visual selection."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-offset s)  0
          (cl-tmux/terminal/types:screen-copy-cursor s)    (cons 1 4)
          (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "set-mark must not activate selection mode")))

(test copy-mode-set-mark-noop-outside-copy-mode
  "copy-mode-set-mark is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  nil
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-mark   s) nil)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is-false (cl-tmux/terminal/types:screen-copy-mark s)
              "mark must remain nil when not in copy mode")))

(test copy-mode-set-mark-noop-without-cursor
  "copy-mode-set-mark is a no-op when copy-cursor is nil."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s)  t
          (cl-tmux/terminal/types:screen-copy-cursor s) nil
          (cl-tmux/terminal/types:screen-copy-mark   s) nil)
    (cl-tmux/commands::copy-mode-set-mark s)
    (is-false (cl-tmux/terminal/types:screen-copy-mark s)
              "mark must remain nil when cursor is nil")))

;;; ── copy-mode-goto-line ──────────────────────────────────────────────────────

(test copy-mode-goto-line-jumps-to-live-row
  "copy-mode-goto-line N with no scrollback jumps to viewport row N-1."
  ;; 10-wide, 5-row screen, no scrollback: vrow = viewport-row (offset=0, sb-n=0).
  ;; goto-line 3 = vrow 2 = viewport row 2.
  (let ((s (make-screen 10 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-goto-line s 3)
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "goto-line 3 with no scrollback must land on viewport row 2 (vrow 2)")))

(test copy-mode-goto-line-clamps-over-max
  "copy-mode-goto-line clamps to the last valid row when N exceeds total rows."
  (let ((s (make-screen 10 3)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; 999 is way past the total row count (3-row screen, no scrollback = vrows 0-2)
    (cl-tmux/commands::copy-mode-goto-line s 999)
    ;; After clamping, cursor row must be within [0, height-1]
    (is (<= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)) 2)
        "goto-line out-of-range must clamp cursor to a valid viewport row")))

(test copy-mode-goto-line-noop-outside-copy-mode
  "copy-mode-goto-line is a no-op when not in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  nil
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; Should not signal any error, screen must stay out of copy mode.
    (cl-tmux/commands::copy-mode-goto-line s 1)
    (is-false (cl-tmux/terminal/types:screen-copy-mode-p s)
              "screen must remain out of copy mode")))

;;; ── copy-mode-search-forward-incremental ─────────────────────────────────────

(test copy-mode-search-forward-incremental-noop-outside-copy-mode
  "Does not open a prompt when not in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s) nil
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (cl-tmux/commands::copy-mode-search-forward-incremental s)
    (is-false *prompt* "no prompt must open outside copy mode")))

(test copy-mode-search-forward-incremental-opens-prompt
  "Opens a prompt labelled search-forward when in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 2 3)
          (screen-copy-offset  s)  0
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (unwind-protect
        (progn
          (cl-tmux/commands::copy-mode-search-forward-incremental s)
          (is-true  *prompt* "prompt must be open")
          (is (string= "search-forward" (prompt-label *prompt*))
              "prompt label must be search-forward"))
      (setf *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil))))

(test copy-mode-search-forward-incremental-saves-origin
  "Saves cursor+offset in *copy-mode-isearch-origin* when prompt opens."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 2 3)
          (screen-copy-offset  s)  5
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (unwind-protect
        (progn
          (cl-tmux/commands::copy-mode-search-forward-incremental s)
          (let ((origin cl-tmux/commands::*copy-mode-isearch-origin*))
            (is-true origin "origin must be non-nil after prompt open")
            (is (equal (cons 2 3) (car origin)) "origin cursor must match pre-search cursor")
            (is (= 5 (cdr origin))              "origin offset must match pre-search offset")))
      (setf *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil))))

(test copy-mode-search-forward-incremental-cancel-restores-cursor
  "prompt-clear (ESC/C-g) restores cursor and offset to pre-search position."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 2 3)
          (screen-copy-offset  s)  0
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (cl-tmux/commands::copy-mode-search-forward-incremental s)
    ;; Simulate the search having moved the cursor away.
    (setf (screen-copy-cursor s) (cons 0 1)
          (screen-copy-offset s) 2)
    ;; Cancel — must invoke the on-cancel closure which restores origin.
    (prompt-clear)
    (is (equal (cons 2 3) (screen-copy-cursor s))
        "cursor must be restored to pre-search position after cancel")
    (is (= 0 (screen-copy-offset s))
        "offset must be restored to pre-search value after cancel")
    (is-false cl-tmux/commands::*copy-mode-isearch-origin*
              "isearch origin must be cleared after cancel")))

;;; ── copy-mode-search-backward-incremental ────────────────────────────────────

(test copy-mode-search-backward-incremental-noop-outside-copy-mode
  "Does not open a prompt when not in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s) nil
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (cl-tmux/commands::copy-mode-search-backward-incremental s)
    (is-false *prompt* "no prompt must open outside copy mode")))

(test copy-mode-search-backward-incremental-opens-prompt
  "Opens a prompt labelled search-backward when in copy mode."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 3 5)
          (screen-copy-offset  s)  0
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (unwind-protect
        (progn
          (cl-tmux/commands::copy-mode-search-backward-incremental s)
          (is-true  *prompt* "prompt must be open")
          (is (string= "search-backward" (prompt-label *prompt*))
              "prompt label must be search-backward"))
      (setf *prompt* nil
            cl-tmux/commands::*copy-mode-isearch-origin* nil))))

(test copy-mode-search-backward-incremental-cancel-restores-cursor
  "prompt-clear (ESC/C-g) restores cursor and offset when backward search is cancelled."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-mode-p s)  t
          (screen-copy-cursor  s)  (cons 3 5)
          (screen-copy-offset  s)  1
          *prompt* nil
          cl-tmux/commands::*copy-mode-isearch-origin* nil)
    (cl-tmux/commands::copy-mode-search-backward-incremental s)
    ;; Simulate search having moved the cursor away.
    (setf (screen-copy-cursor s) (cons 1 0)
          (screen-copy-offset s) 3)
    (prompt-clear)
    (is (equal (cons 3 5) (screen-copy-cursor s))
        "cursor must be restored to pre-search position after cancel")
    (is (= 1 (screen-copy-offset s))
        "offset must be restored to pre-search value after cancel")
    (is-false cl-tmux/commands::*copy-mode-isearch-origin*
              "isearch origin must be cleared after cancel")))

;;; ── copy-mode-next-matching-bracket ─────────────────────────────────────────

(test copy-mode-next-matching-bracket-open-paren-finds-close
  "When cursor is on '(' the bracket scan jumps to the matching ')'."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) t)
    ;; Write "( foo )" directly into row 2 cells.
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell s i 2)
            (cl-tmux/terminal/types:make-cell :char (char "( foo )" i))))
    (setf (screen-copy-cursor s) (cons 2 0)   ; on the '('
          (screen-copy-offset  s) 0)
    (cl-tmux/commands::copy-mode-next-matching-bracket s)
    (is (= 6 (cdr (screen-copy-cursor s)))
        "cursor column must be on the ')' at col 6")))

(test copy-mode-next-matching-bracket-close-paren-finds-open
  "When cursor is on ')' the bracket scan jumps backward to the matching '('."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) t)
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell s i 2)
            (cl-tmux/terminal/types:make-cell :char (char "( foo )" i))))
    (setf (screen-copy-cursor s) (cons 2 6)   ; on the ')'
          (screen-copy-offset  s) 0)
    (cl-tmux/commands::copy-mode-next-matching-bracket s)
    (is (= 0 (cdr (screen-copy-cursor s)))
        "cursor column must be on the '(' at col 0")))

(test copy-mode-next-matching-bracket-nested-brackets
  "Nested brackets: cursor on outer '(' jumps to the outer matching ')'."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) t)
    ;; Write "(a(b)c)" at row 0.
    (dotimes (i 7)
      (setf (cl-tmux/terminal/types:screen-cell s i 0)
            (cl-tmux/terminal/types:make-cell :char (char "(a(b)c)" i))))
    (setf (screen-copy-cursor s) (cons 0 0)
          (screen-copy-offset  s) 0)
    (cl-tmux/commands::copy-mode-next-matching-bracket s)
    (is (= 6 (cdr (screen-copy-cursor s)))
        "cursor must land on the outer ')' at column 6")))

(test copy-mode-next-matching-bracket-noop-outside-copy-mode
  "Bracket matching is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-mode-p s) nil
          (screen-copy-cursor  s) (cons 0 3))
    (cl-tmux/commands::copy-mode-next-matching-bracket s)
    (is (equal (cons 0 3) (screen-copy-cursor s))
        "cursor must remain at (0,3) when not in copy mode")))
