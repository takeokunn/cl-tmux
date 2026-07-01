(in-package #:cl-tmux)

;;;; Target-string resolution helpers.
;;;;
;;;; These functions resolve an optional tmux "target" string (e.g. "session:1.2")
;;;; against a current session/window/pane, falling back to the current values
;;;; when the target string is absent or unresolvable.
;;;;
;;;; The with-target-session and with-target-context binding macros that build
;;;; on resolve-target-context live in dispatch-core.lisp.

(defun %resolve-target-window-pane (session target-str current-window current-pane)
  "Resolve TARGET-STR to a window/pane pair.
   When TARGET-STR is absent, return CURRENT-WINDOW and CURRENT-PANE.
   When TARGET-STR names a window but not a pane, return that window's active pane."
  (if target-str
      (multiple-value-bind (target-session target-window target-pane)
          (resolve-target *server-sessions* target-str
                          :current-session session
                          :current-window current-window
                          :current-pane current-pane)
        (declare (ignore target-session))
        (when target-window
          (values target-window
                  (or (and target-pane
                           (member target-pane (window-panes target-window))
                           target-pane)
                      (window-active-pane target-window)))))
      (values current-window current-pane)))

(defun %resolve-target-session-window (session target-str current-window current-pane)
  "Resolve TARGET-STR to a session/window pair.
   When TARGET-STR is absent, return SESSION and CURRENT-WINDOW.
   When TARGET-STR names a pane, return its window."
  (if target-str
      (multiple-value-bind (target-session target-window target-pane)
          (resolve-target *server-sessions* target-str
                          :current-session session
                          :current-window current-window
                          :current-pane current-pane)
        (declare (ignore target-pane))
        (when target-window
          (values target-session target-window)))
      (values session current-window)))

(defun %resolve-window-target-or-active (session target-str)
  "Return TARGET-STR's window or SESSION's active window when TARGET-STR is absent."
  (or (and target-str (%resolve-window-target session target-str))
      (session-active-window session)))
