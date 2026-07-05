(in-package #:cl-tmux)

;;;; Copy-mode -X dispatch (send-keys -X).

(defun %lookup-send-keys-x-explicit-arg-spec (command-name specs)
  "Return KIND and HANDLER for COMMAND-NAME from SPECS."
  (let ((spec (find command-name specs :key #'first :test #'string-equal)))
    (if spec
        (destructuring-bind (name kind handler) spec
          (declare (ignore name))
          (values kind handler))
        (values nil nil))))

(defun %send-keys-x-explicit-arg-spec (command-name)
  "Return KIND and HANDLER for canonical COMMAND-NAME."
  (%lookup-send-keys-x-explicit-arg-spec
   command-name *send-keys-x-explicit-arg-specs*))

(defun %send-keys-x-explicit-arg-string (kind extra-args)
  "Return the explicit argument string for KIND from EXTRA-ARGS."
  (ecase kind
    ((:char :line) (first extra-args))
    (:text (format nil "~{~A~^ ~}" extra-args))))

(defun %send-keys-x-coerce-explicit-arg (kind handler screen arg)
  "Apply KIND-specific coercion to ARG and call HANDLER on SCREEN."
  (when (and screen arg (plusp (length arg)))
    (ecase kind
      (:char (funcall handler screen (char arg 0)))
      (:line (let ((line-number (%parse-integer-or-nil arg)))
               (when line-number
                 (funcall handler screen line-number)
                 t)))
      (:text (funcall handler screen arg) t))))

(defun %dispatch-send-keys-x-explicit-arg (screen command-name extra-args)
  "Dispatch COMMAND-NAME with an explicit positional argument when it has one."
  (multiple-value-bind (kind handler)
      (%send-keys-x-explicit-arg-spec command-name)
    (when handler
      (%send-keys-x-coerce-explicit-arg kind handler screen
                                        (%send-keys-x-explicit-arg-string kind
                                                                         extra-args)))))

(defun %dispatch-send-keys-x-with-temporary-focus (session target-pane target-window thunk)
  "Run THUNK while TARGET-PANE is temporarily focused in TARGET-WINDOW.
   Restores the real session/window focus afterward without delivering focus
   events or updating recency metadata."
  (let ((prev-win  (session-active-window session))
        (prev-pane (and target-window (window-active-pane target-window))))
    (unwind-protect
         (progn
           (setf (session-active session) target-window
                 (window-active target-window) target-pane)
           (funcall thunk))
      (when target-window
        (setf (window-active target-window) prev-pane))
      (setf (session-active session) prev-win))))

(defun %dispatch-send-keys-X (session command-name &optional target-pane target-window extra-args)
  "Dispatch a send-keys -X COMMAND-NAME against TARGET-PANE's copy mode (default:
   the active pane).  Copy-mode -X commands act on the session's ACTIVE screen, so
   when TARGET-PANE is a non-active pane the command runs with a temporary focus
   swap so it operates on the target while leaving the real focus unchanged.
   Returns T when COMMAND-NAME is a recognised copy-mode command.
   EXTRA-ARGS (a list of strings) holds any positional arguments after the command
   name; used by the copy-pipe commands to carry the pipe-command string."
  (let* ((pane   (or target-pane (session-active-pane session)))
         (screen (and pane (cl-tmux/model:pane-screen pane))))
    (cond
      ((and extra-args
            (%dispatch-send-keys-x-explicit-arg screen command-name extra-args))
       t)
      ;; Standard keyword dispatch.
       (t
       (let ((kw (cdr (assoc command-name *copy-mode-x-commands* :test #'string-equal))))
         (when kw
           (if (and target-pane target-window
                    (not (eq target-pane (session-active-pane session))))
               (%dispatch-send-keys-x-with-temporary-focus
                session target-pane target-window
                (lambda ()
                  (dispatch-command session kw nil)))
               (dispatch-command session kw nil))
           t))))))
