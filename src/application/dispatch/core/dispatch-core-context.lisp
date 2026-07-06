(in-package #:cl-tmux)

;;;; Declarative command dispatch - target, guard, and focus context helpers.

(defun %flag-value (flags char)
  "Return the value associated with CHAR in FLAGS, or NIL when absent."
  (cdr (assoc char flags)))

(defun %flag-present-p (flags char)
  "Return true when FLAGS contains an entry for CHAR, even if its value is NIL."
  (not (null (assoc char flags))))

(defmacro with-target-session ((target-session target-str session
                               &key message (on-missing :skip))
                               &body body)
  "Bind TARGET-SESSION from TARGET-STR or SESSION."
  (let ((target-str-var (gensym "TARGET-STR-"))
        (resolved-var (gensym "TARGET-SESSION-"))
        (session-var (gensym "SESSION-")))
    `(let* ((,session-var ,session)
            (,target-str-var ,target-str)
            (,resolved-var (and ,target-str-var
                                (find-session-by-target *server-sessions*
                                                        ,target-str-var)))
            (,target-session (or ,resolved-var ,session-var)))
       (cond
         ((or (null ,target-str-var) ,resolved-var)
          ,@body)
         ((eq ,on-missing :current)
          ,@body)
         ((eq ,on-missing :error)
          (progn
            ,(when message
               `(%overlayf ,message ,target-str-var))
            nil))
         (t nil)))))

(defmacro with-target-context ((target-session target-window target-pane session target-str)
                               &body body)
  "Bind TARGET-SESSION, TARGET-WINDOW, and TARGET-PANE from TARGET-STR or SESSION."
  `(multiple-value-bind (,target-session ,target-window ,target-pane)
       (resolve-target-context *server-sessions* ,session ,target-str)
     ,@body))

(defmacro with-active-pane ((pane-var session) &body body)
  "Bind PANE-VAR to SESSION's active pane and evaluate BODY."
  `(let ((,pane-var (session-active-pane ,session)))
     (when ,pane-var ,@body)))

(defun %handle-kill-result (result)
  "Set *running* nil when RESULT is :quit, then return RESULT."
  (when (eq result :quit) (setf *running* nil))
  result)

(defmacro with-active-window ((win-var session) &body body)
  "Bind WIN-VAR to SESSION's active window and evaluate BODY only when present."
  `(let ((,win-var (session-active-window ,session)))
     (when ,win-var ,@body)))

(defmacro %with-window-focus-transition ((session) &body body)
  "Run BODY and deliver focus/window-changed hooks when SESSION changes windows."
  (let ((sess (gensym "SESSION")) (old-win (gensym "OLD-WIN"))
        (old-pane (gensym "OLD-PANE")) (new-win (gensym "NEW-WIN")))
    `(let* ((,sess     ,session)
            (,old-win  (session-active-window ,sess))
            (,old-pane (and ,old-win (window-active-pane ,old-win))))
       (prog1 (progn ,@body)
         (let ((,new-win (session-active-window ,sess)))
           (unless (eq ,old-win ,new-win)
             (%notify-pane-focus ,old-pane nil)
             (%notify-pane-focus (and ,new-win (window-active-pane ,new-win)) t)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-window-changed+ ,sess)))))))

(defun %active-window-pane (session)
  "Return SESSION's active window and its active pane as two values."
  (let ((win (session-active-window session)))
    (values win (and win (window-active-pane win)))))

(defun %active-screen (session)
  "Return SESSION's active-pane screen, or NIL when there is no active pane."
  (with-active-pane (ap session)
    (pane-screen ap)))

(defun %copy-mode-active-p (session)
  "Return T when the active pane's screen is in copy mode."
  (multiple-value-bind (_win ap) (%active-window-pane session)
    (declare (ignore _win))
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))
