(in-package #:cl-tmux/format)

;;; -- W:/S:/P: iteration expanders --------------------------------------------
;;;
;;; #{W:ACTIVE,INACTIVE} iterates the windows of the context's session.
;;; #{S:ACTIVE,INACTIVE} iterates every server session.
;;; #{P:ACTIVE,INACTIVE} iterates the panes of the context's current window.
;;; Each renders the "current" item with ACTIVE and every other item with
;;; INACTIVE, via the shared %iterate-fmt helper.

(defun %iterate-fmt (items active-item active-fmt inactive-fmt context-fn &optional separator)
  "Iterate ITEMS: format each with ACTIVE-FMT when it is EQ to ACTIVE-ITEM, else
   INACTIVE-FMT.  CONTEXT-FN is called with each item to produce its format context.
   SEPARATOR is written between items when non-NIL."
  (with-output-to-string (s)
    (loop for item in items
          for first = t then nil
          do (when (and separator (not first)) (write-string separator s))
             (write-string
              (expand-format (if (eq item active-item) active-fmt inactive-fmt)
                             (funcall context-fn item))
              s))))

(defun %expand-window-iteration (rest context)
  "Expand a #{W:ACTIVE,INACTIVE} window-list modifier.  Iterates the windows of
   the context's session: the current window is formatted with ACTIVE, the others
   with INACTIVE.  Results are joined with the window-status-separator option.
   Returns \"\" when there is no session."
  (let ((session (getf context :%session)))
    (if (null session)
        ""
        (multiple-value-bind (active-fmt inactive-fmt) (%split-active-inactive rest)
          (%iterate-fmt
           (cl-tmux/model:session-windows session)
           (cl-tmux/model:session-active-window session)
           active-fmt inactive-fmt
           (lambda (win)
             (format-context-from-session session win (cl-tmux/model:window-active-pane win)))
           (or (cl-tmux/options:get-option "window-status-separator") " "))))))

(defun %all-server-sessions ()
  "The list of live session objects from cl-tmux's *server-sessions* registry,
   read by runtime symbol lookup to avoid a compile-time dependency on the umbrella
   package (the same indirection #{session_count} uses).  NIL when empty/unbound."
  (ignore-errors
    (mapcar #'cdr (symbol-value (find-symbol "*SERVER-SESSIONS*" "CL-TMUX")))))

(defun %expand-session-iteration (rest context)
  "Expand a #{S:ACTIVE,INACTIVE} session-list modifier.  Iterates every server
   session: the context's current session is formatted with ACTIVE, the others with
   INACTIVE.  Results are concatenated without separator.  Falls back to the single
   context session when the registry is empty."
  (multiple-value-bind (active-fmt inactive-fmt) (%split-active-inactive rest)
    (let* ((cur-session (getf context :%session))
           (sessions    (or (%all-server-sessions)
                            (and cur-session (list cur-session)))))
      (%iterate-fmt
       sessions cur-session active-fmt inactive-fmt
       (lambda (sess)
         (let* ((win  (cl-tmux/model:session-active-window sess))
                (pane (and win (cl-tmux/model:window-active-pane win))))
           (format-context-from-session sess win pane)))))))

(defun %expand-pane-iteration (rest context)
  "Expand a #{P:ACTIVE,INACTIVE} pane-list modifier.  Iterates the panes of the
   context's current window: the active pane is formatted with ACTIVE, the others
   with INACTIVE.  Results are concatenated without separator.  Returns \"\" when
   there is no window."
  (let* ((session (getf context :%session))
         (window  (and session (cl-tmux/model:session-active-window session))))
    (if (null window)
        ""
        (multiple-value-bind (active-fmt inactive-fmt) (%split-active-inactive rest)
          (%iterate-fmt
           (cl-tmux/model:window-panes window)
           (cl-tmux/model:window-active-pane window)
           active-fmt inactive-fmt
           (lambda (pane) (format-context-from-session session window pane)))))))
