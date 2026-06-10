(in-package #:cl-tmux/hooks)

;;; A hooks system that allows user-defined Lisp functions to run on events.
;;;
;;; Hook event names follow the tmux convention:
;;;   "after-new-window"    — after a new window is created
;;;   "after-new-pane"      — after a pane is split
;;;   "pane-exited"         — when a pane's process exits
;;;   "after-rename-window" — after rename-window is called
;;;   "session-created"     — when the session starts
;;;   "after-kill-pane"     — after a pane is killed
;;;   "after-kill-window"   — after a window is killed
;;;   "client-attached"     — when a client attaches to the server
;;;   "client-detached"     — when a client detaches from the server
;;;   "alert-bell"          — when a BEL character is received in a pane

;;; ── Hook event constant table ────────────────────────────────────────────

(defmacro define-hook-events (&rest specs)
  "Declare known hook events as a fact table.
Each SPEC is (constant-name event-string description-string).
Generates a DEFCONSTANT for each event-string constant.
Uses the safe SBCL idiom to avoid string-constant redefinition errors."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (constant-name event-string description-string) spec
                   (declare (ignore description-string))
                   `(defconstant ,constant-name
                      (if (boundp ',constant-name)
                          (symbol-value ',constant-name)
                          ,event-string))))
               specs)))

(define-hook-events
  (+hook-after-new-window+       "after-new-window"       "Fired after a new window is created")
  (+hook-after-new-pane+         "after-new-pane"         "Fired after a pane is split")
  (+hook-pane-exited+            "pane-exited"            "Fired when a pane's process exits")
  (+hook-after-rename-window+    "after-rename-window"    "Fired after rename-window is called")
  (+hook-session-created+        "session-created"        "Fired when a session is first created")
  (+hook-after-kill-pane+        "after-kill-pane"        "Fired after a pane is killed")
  (+hook-after-kill-window+      "after-kill-window"      "Fired after a window is killed")
  (+hook-after-split-window+     "after-split-window"     "Fired after a window is split")
  (+hook-client-attached+        "client-attached"        "Fired when a client attaches to the server")
  (+hook-client-detached+        "client-detached"        "Fired when a client detaches from the server")
  (+hook-alert-bell+             "alert-bell"             "Fired when a BEL character is received in a pane")
  (+hook-alert-activity+         "alert-activity"         "Fired when monitor-activity detects activity in a window")
  (+hook-alert-silence+          "alert-silence"          "Fired when monitor-silence detects silence in a window")
  (+hook-pane-focus-in+          "pane-focus-in"          "Fired when a pane gains focus")
  (+hook-pane-focus-out+         "pane-focus-out"         "Fired when a pane loses focus")
  (+hook-after-select-pane+      "after-select-pane"      "Fired after the select-pane command")
  (+hook-after-select-window+    "after-select-window"    "Fired after the select-window command")
  (+hook-session-window-changed+ "session-window-changed"  "Fired when a session's active window changes")
  (+hook-window-pane-changed+    "window-pane-changed"     "Fired when the active pane in a window changes")
  (+hook-window-renamed+         "window-renamed"         "Fired when a window is renamed")
  (+hook-session-renamed+        "session-renamed"        "Fired when a session is renamed")
  (+hook-after-resize-pane+      "after-resize-pane"      "Fired after a pane is resized")
  (+hook-client-resized+         "client-resized"         "Fired when the client terminal is resized")
  (+hook-window-linked+          "window-linked"          "Fired when a window is linked into a session")
  (+hook-window-unlinked+        "window-unlinked"        "Fired when a window is unlinked from a session")
  (+hook-session-closed+         "session-closed"         "Fired when a session is destroyed (kill-session)"))

(defvar *hook-registry* (make-hash-table :test #'equal)
  "Maps event-name (string) to a list of callback functions.
   The first element of the list is the most recently added hook (front-push).")

(defun add-hook (event-name callback)
  "Push CALLBACK to the front of the hook list for EVENT-NAME.
   Subsequent add-hook calls for the same event-name prepend additional hooks,
   so hooks run newest-first."
  (setf (gethash event-name *hook-registry*)
        (cons callback (gethash event-name *hook-registry*))))

(defun remove-hook (event-name callback)
  "Remove CALLBACK (tested with #'eq) from the hook list for EVENT-NAME.
   All occurrences are removed."
  (setf (gethash event-name *hook-registry*)
        (remove callback (gethash event-name *hook-registry*) :test #'eq)))

(defun run-hooks (event-name &rest args)
  "Call each registered hook for EVENT-NAME with ARGS.
   Errors signalled by individual hooks are silently suppressed so that
   a broken hook never prevents the rest from running.
   Trade-off: silent suppression makes debugging a broken hook difficult;
   if a hook never fires, check whether it signals an unhandled condition.
   A future *hooks-error-handler* hook-point could surface these at debug time."
  (dolist (cb (gethash event-name *hook-registry*))
    (handler-case (apply cb args)
      (error () nil)))
  ;; Also fire .tmux.conf set-hook command hooks for this event, against the
  ;; session derived from the hook TARGET (the first arg — a session/window/pane).
  ;; This unifies the two hook registries so every event supports `set-hook`, not
  ;; just the ones whose firing point happened to call run-command-hooks directly.
  (ignore-errors (run-command-hooks-via-runner event-name (first args))))

(defun clear-hooks (event-name)
  "Remove all hooks registered for EVENT-NAME."
  (remhash event-name *hook-registry*))

(defun list-hooks ()
  "Return an alist of (event-name . hook-count) for all registered events.
   Note: iteration order over the registry is undefined — callers must not
   rely on the order of entries in the returned alist."
  (let (result)
    (maphash (lambda (name hooks)
               (push (cons name (length hooks)) result))
             *hook-registry*)
    result))

;;; ── Command hooks (the user-facing `set-hook` directive) ──────────────────
;;;
;;; Distinct from the lisp-function hooks above: *command-hooks* maps an event
;;; name to a list of tmux command KEYWORDS to dispatch when the event fires.
;;; It is populated by the `set-hook <event> <command>` config directive.  The
;;; actual dispatch (run-command-hooks) lives in the cl-tmux package because it
;;; needs DISPATCH-COMMAND and a session; this layer only stores the bindings.

(defvar *command-hooks* (make-hash-table :test #'equal)
  "Maps event-name (string) to an ordered list of commands to dispatch
   when the corresponding hook fires.  Each entry is either a keyword
   (legacy programmatic hook) or a raw command-line string (from
   set-hook in .tmux.conf).  String hooks support format expansion.")

(defun set-command-hook (event-name command)
  "Append COMMAND to the list dispatched when hook EVENT-NAME fires.
   COMMAND may be a keyword (built-in command) or a string (command line,
   potentially with arguments and #{format} variables).
   Appending preserves declaration order across multiple set-hook calls."
  (setf (gethash event-name *command-hooks*)
        (append (gethash event-name *command-hooks*) (list command))))

(defun command-hooks (event-name)
  "Return the ordered list of commands registered for EVENT-NAME."
  (gethash event-name *command-hooks*))

(defun clear-command-hooks (event-name)
  "Remove all command hooks registered for EVENT-NAME."
  (remhash event-name *command-hooks*))

(defun %list-command-hooks ()
  "Return an alist of (event-name . command-keyword-list) for all command hooks.
   Internal helper; callers that need a sorted copy must sort the result themselves."
  (let (result)
    (maphash (lambda (name kws) (push (cons name kws) result)) *command-hooks*)
    result))

(defun describe-command-hooks ()
  "Return a newline-separated, event-sorted listing of the registered command
   hooks (\"<event> -> <command>, ...\" per line) for the show-hooks overlay."
  (let ((entries (sort (copy-list (%list-command-hooks)) #'string< :key #'car)))
    (if (null entries)
        "no command hooks set"
        (with-output-to-string (out)
          (write-string "command hooks:" out)
          (dolist (entry entries)
            (format out "~%  ~A -> ~{~(~A~)~^, ~}" (car entry) (cdr entry)))))))

;;; The command-hook RUNNER breaks a package layering cycle: kill-pane and
;;; kill-window live in cl-tmux/commands, which cannot reference the cl-tmux
;;; package's run-command-hooks (that one needs dispatch-command).  The cl-tmux
;;; package installs its run-command-hooks here at load; lower layers fire
;;; command hooks indirectly through the runner.

(defvar *command-hook-runner* nil
  "A function (event-name session) that dispatches the command hooks for an
   event, installed by the cl-tmux package at load.  NIL means no command-hook
   dispatch yet (e.g. before the top-level package has finished loading).")

(defun run-command-hooks-via-runner (event-name session)
  "Fire the command hooks for EVENT-NAME on SESSION through *command-hook-runner*.
   A no-op when no runner is installed, so lower layers may call it freely."
  (when *command-hook-runner*
    (funcall *command-hook-runner* event-name session)))
