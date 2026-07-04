(in-package #:cl-tmux)

;;; -- List command read-model queries -----------------------------------------

(declaim (special cl-tmux::*clients*))

(defun %list-clients-records ()
  "Return (NAME ROWS COLS) records for attached clients, or a local fallback."
  (if *clients*
      (loop for conn in *clients*
            for i from 0
            collect (list (format nil "client-~D" i)
                          (client-conn-rows conn)
                          (client-conn-cols conn)))
      (list (list "local" *term-rows* *term-cols*))))

(defun %registered-sessions-or-current (session)
  "Return the registered sessions, or SESSION when none are registered."
  (or (mapcar #'cdr *server-sessions*)
      (list session)))

(defun %window-targets-for-session (target-session)
  "Return (SESSION . WINDOW) targets for every window in TARGET-SESSION."
  (mapcar (lambda (win)
            (cons target-session win))
          (cl-tmux/model:session-windows-in-index-order target-session)))

(defun %list-pane-targets (session target-str all-p session-p)
  "Return the target windows for list-panes based on flags and target input."
  (cond
    (all-p
     (loop for target-session in (%registered-sessions-or-current session)
           append (%window-targets-for-session target-session)))
    (session-p
     (with-target-session (target-session target-str session
                           :on-missing :current)
       (%window-targets-for-session target-session)))
    (target-str
     (multiple-value-bind (target-session target-window)
         (%resolve-target-session-window
          session target-str
          (session-active-window session)
          (session-active-pane session))
       (when target-window
         (list (cons target-session target-window)))))
    (t
     (let ((win (session-active-window session)))
       (when win
         (list (cons session win)))))))

(defun %format-list-window-entry (session win fmt)
  "Format one list-windows row using either FMT or the default tmux-style text."
  (if fmt
      (cl-tmux/format:expand-format
       fmt
       (cl-tmux/format:format-context-from-window session win))
      (format nil "~A: ~A (~Dx~D) [~D pane~:P]~A"
              (window-id win) (window-name win)
              (window-width win) (window-height win)
              (length (window-panes win))
              (if (eq win (session-active-window session))
                  " [active]"
                  ""))))

(defun %format-list-pane-entry (session win pane fmt)
  "Format one list-panes row using either FMT or the default tmux-style text."
  (if fmt
      (cl-tmux/format:expand-format
       fmt
       (cl-tmux/format:format-context-from-session session win pane))
      (format nil "~D: [~Dx~D] [~D,~D] pane ~D~A"
              (pane-id pane)
              (pane-width pane) (pane-height pane)
              (pane-x pane) (pane-y pane)
              (pane-id pane)
              (if (eq pane (window-active-pane win))
                  " (active)"
                  ""))))

(defun %format-list-client-entry (session record fmt)
  "Format one list-clients row using either FMT or the default tmux-style text."
  (destructuring-bind (name rows cols) record
    (if fmt
        (cl-tmux/format:expand-format
         fmt
         (cl-tmux/format:format-context-from-session
          session
          (and session (session-active-window session))
          (and session (session-active-pane session))
          :client-width cols
          :client-height rows
          :client-tty name))
        (format nil "~A: ~A [~Ax~A]"
                name
                (or (and session (session-name session)) "")
                cols
                rows))))

(defun %format-list-session-entry (target-session fmt)
  "Format one list-sessions row using FMT."
  (cl-tmux/format:expand-format
   fmt
   (cl-tmux/format:format-context-from-session
    target-session
    (session-active-window target-session)
    nil)))

(defun %list-session-overlay-lines (session fmt)
  "Return list-sessions overlay lines, optional raw text, and a display flag."
  (if fmt
      (values
       (loop for sess in (%registered-sessions-or-current session)
             collect (%format-list-session-entry sess fmt))
       nil
       t)
      (values nil (%format-session-list session) t)))

(defun %list-client-overlay-lines (session fmt)
  "Return list-clients overlay lines and a display flag."
  (values (loop for record in (%list-clients-records)
                collect (%format-list-client-entry session record fmt))
          t))

(defun %list-window-overlay-lines (session fmt target-str all-p)
  "Return list-windows overlay lines and a display flag."
  (let ((sessions (cond
                    (all-p
                     (%registered-sessions-or-current session))
                    (target-str
                     (with-target-session (target-session target-str session)
                       (list target-session)))
                    (t
                     (list session)))))
    (if sessions
        (values
         (loop for sess in sessions
               append (loop for win in (cl-tmux/model:session-windows-in-index-order sess)
                            collect (%format-list-window-entry sess win fmt)))
         t)
        (values nil nil))))

(defun %list-pane-overlay-lines (session fmt target-str all-p session-p)
  "Return list-panes overlay lines and a display flag."
  (let ((targets (%list-pane-targets session target-str all-p session-p)))
    (values
     (loop for target in targets
           append (let ((target-session (car target))
                        (win            (cdr target)))
                    (loop for pane in (window-panes win)
                          collect (%format-list-pane-entry
                                   target-session win pane fmt))))
     (not (null targets)))))
