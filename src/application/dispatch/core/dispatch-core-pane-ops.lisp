(in-package #:cl-tmux)

;;;; Declarative command dispatch - pane, layout, and window-list helpers.

(defun %swap-active-pane (session direction)
  "Swap the active pane of SESSION in DIRECTION (:left or :right)."
  (with-active-window (win session)
    (%pane-navigation-unzoom win)
    (swap-pane win direction)))

(defun %resize-active-window-pane (session direction)
  "Resize the active pane of SESSION's active window in DIRECTION."
  (resize-pane (session-active-window session) direction))

(defun %select-pane-in-direction (session direction)
  "Select the pane adjacent to the active pane in DIRECTION."
  (multiple-value-bind (win ap) (%active-window-pane session)
    (when (and win ap)
      (%pane-navigation-unzoom win)
      (let ((nb (pane-neighbor win ap direction)))
        (when nb (%select-pane-with-focus win nb))))))

(defun %apply-named-layout-to-session (session layout-name)
  "Apply LAYOUT-NAME to SESSION's active window and reassign geometry."
  (let ((win (session-active-window session)))
    (when win
      (cl-tmux/model:apply-named-layout
       win layout-name
       (or (cl-tmux/options:get-option "main-pane-width") 80)
       (or (cl-tmux/options:get-option "main-pane-height") 24)
       (or (cl-tmux/options:get-option "other-pane-width") 0)
       (or (cl-tmux/options:get-option "other-pane-height") 0))
      (%assign-window-tree win (window-width win) (window-height win)))))

(defun %copy-mode-call (session fn)
  "Call FN on SESSION's active screen when one exists."
  (let ((screen (%active-screen session)))
    (when screen (funcall fn screen))))

(defun %format-window-list (session)
  "Return a formatted string listing all windows in SESSION."
  (let* ((win  (session-active-window session))
         (wins (session-windows session)))
    (with-output-to-string (s)
      (dolist (w wins)
        (format s "~A~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                (if (eq w win) "*" " ")
                (window-id w)
                (window-name w)
                (window-width w)
                (window-height w)
                (length (window-panes w))
                (if (eq w win) " [active]" ""))))))
