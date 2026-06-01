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
  (+hook-after-new-window+    "after-new-window"    "Fired after a new window is created")
  (+hook-after-new-pane+      "after-new-pane"      "Fired after a pane is split")
  (+hook-pane-exited+         "pane-exited"         "Fired when a pane's process exits")
  (+hook-after-rename-window+ "after-rename-window" "Fired after rename-window is called")
  (+hook-session-created+     "session-created"     "Fired when a session is first created")
  (+hook-after-kill-pane+     "after-kill-pane"     "Fired after a pane is killed")
  (+hook-after-kill-window+   "after-kill-window"   "Fired after a window is killed"))

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
   a broken hook never prevents the rest from running."
  (dolist (cb (gethash event-name *hook-registry*))
    (handler-case (apply cb args)
      (error () nil))))

(defun clear-hooks (event-name)
  "Remove all hooks registered for EVENT-NAME."
  (remhash event-name *hook-registry*))

(defun list-hooks ()
  "Return an alist of (event-name . hook-count) for all registered events."
  (let (result)
    (maphash (lambda (name hooks)
               (push (cons name (length hooks)) result))
             *hook-registry*)
    result))
