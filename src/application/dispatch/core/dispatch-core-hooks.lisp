(in-package #:cl-tmux)

;;;; Command-hook dispatch helpers.
;;;;
;;;; run-command-hooks is installed as cl-tmux/hooks:*command-hook-runner* so
;;;; lower layers (cl-tmux/commands kill-pane / kill-window, etc.) can fire
;;;; command hooks without depending on this (higher) dispatch layer directly.

(defun %derive-hook-session (target)
  "Resolve a hook TARGET — a session, window, or pane — to its owning session, so
   command hooks (which run against a session) can fire from any run-hooks call
   regardless of what object the firing point had.  NIL when unresolvable."
  (cond
    ((null target) nil)
    ((cl-tmux/model::session-p target) target)
    ((cl-tmux/model::window-p  target) (%session-of-window target))
    ((cl-tmux/model::pane-p    target) (%session-of-pane   target))
    (t nil)))

(defun %hook-scope-matches-p (kind value session target)
  "True when a scoped hook (KIND VALUE) applies to this firing: session scope
   matches the derived SESSION's name; window scope matches when the fire-time
   TARGET is that window (or a pane inside it); pane scope matches that pane."
  (ecase kind
    (:scoped-session
     (string= value (session-name session)))
    (:scoped-window
     (let ((win (cond ((cl-tmux/model::window-p target) target)
                      ((cl-tmux/model::pane-p target)
                       (cl-tmux/model:pane-window target)))))
       (and win (eql value (cl-tmux/model:window-id win)))))
    (:scoped-pane
     (and (cl-tmux/model::pane-p target)
          (eql value (cl-tmux/model:pane-id target))))))

(defun %dispatch-hook-entry (session entry &optional target)
  "Dispatch a single hook ENTRY against SESSION.
   STRING entries are run as command lines via %run-command-line; errors are
   reported as an overlay instead of being silently swallowed.
   KEYWORD entries dispatch directly via dispatch-command.
   Scoped entries (set-hook -t / -w -t / -p -t) fire only when the firing
   session/window/pane matches — tmux stores hooks per target and fires them
   for that scope only."
  (cond
    ((cl-tmux/hooks:scoped-hook-entry-p entry)
     (destructuring-bind (kind value command) entry
       (when (%hook-scope-matches-p kind value session target)
         (%dispatch-hook-entry session command target))))
    ((stringp entry)
     (with-overlay-on-error ("hook") (%run-command-line session entry)))
    ((keywordp entry)
     (dispatch-command session entry 0))))

(defun run-command-hooks (event-name target)
  "Dispatch every command registered for hook EVENT-NAME against the session
   derived from TARGET (a session/window/pane).  String hooks (from set-hook
   in .tmux.conf) run via %run-command-line for format expansion; keyword
   hooks (programmatic set-command-hook calls) dispatch directly.  TARGET is
   also passed through for window/pane-scoped hook matching."
  (let ((session (%derive-hook-session target)))
    (when session
      (dolist (entry (cl-tmux/hooks:command-hooks event-name))
        (%dispatch-hook-entry session entry target)))))

;; Install run-command-hooks as the command-hook runner so lower layers
;; (cl-tmux/commands kill-pane / kill-window) can fire command hooks too.
(setf cl-tmux/hooks:*command-hook-runner* #'run-command-hooks)
