(in-package #:cl-tmux)

;;; -- select-pane %cmd-* handlers --------------------------------------------
;;;
;;; %cmd-select-pane and its pane-configuration helpers.

(defun %select-pane-disable-input (pane disabled-p)
  (when pane
    (setf (pane-input-disabled pane) disabled-p)))

(defun %select-pane-set-title (pane title)
  (when (and pane title)
    (setf (pane-title pane) title)
    (let ((screen (pane-screen pane)))
      (when screen
        (cl-tmux/terminal/actions:set-screen-title screen title)))))

(defun %select-pane-mark (window pane)
  (when (and window pane)
    (dolist (p (window-panes window))
      (setf (pane-marked p) nil))
    (setf (pane-marked pane) t)))

(defun %select-pane-clear-mark (window)
  (when window
    (dolist (p (window-panes window))
      (setf (pane-marked p) nil))))

(defun %select-pane-select-last (window)
  (when window
    (let ((last (window-last-active window)))
      (when last
        (%select-pane-with-focus window last)))))

(defun %select-pane-select-target (window pane)
  (when (and window pane (not (eq pane (window-active-pane window))))
    (%select-pane-with-focus window pane)))

(defun %select-pane-move-in-direction (session flags)
  (cond
    ((%flag-present-p flags #\L) (%select-pane-in-direction session :left))
    ((%flag-present-p flags #\R) (%select-pane-in-direction session :right))
    ((%flag-present-p flags #\U) (%select-pane-in-direction session :up))
    ((%flag-present-p flags #\D) (%select-pane-in-direction session :down))))

(defun %select-pane-configure-target (window target-pane flags)
  (cond
    ;; -d/-e: disable / enable input to the target pane.
    ((%flag-present-p flags #\d) (%select-pane-disable-input target-pane t))
    ((%flag-present-p flags #\e) (%select-pane-disable-input target-pane nil))
    ;; -T title: set the target pane's title (and its screen title so
    ;; #{pane_title} reflects it).
    ((%flag-present-p flags #\T)
     (%select-pane-set-title target-pane (%flag-value flags #\T)))
    ;; -m: mark the target pane (unmark the others in its window first).
    ((%flag-present-p flags #\m) (%select-pane-mark window target-pane))
    ;; -M: clear the marked pane (unmark all panes in the active window).
    ((%flag-present-p flags #\M) (%select-pane-clear-mark window))
    ;; -l: select the previously active (last) pane in the active window.
    ((%flag-present-p flags #\l) (%select-pane-select-last window))
    ;; Default: select the target pane (no-op when it is already active).
    (t (%select-pane-select-target window target-pane))))

(defun %cmd-select-pane (session args)
  "select-pane [-L|-R|-U|-D|-l|-d|-e|-m|-M] [-t target] [-T title]: select or configure a pane.
   -L/-R/-U/-D: move in the given direction (relative to the active pane).
   -l: select the previously active (last) pane.
   -d/-e: disable / re-enable keyboard input to the TARGET pane.
   -T title: set the TARGET pane's title.
   -m: mark the TARGET pane; -M: clear the marked pane (unmark all).
   -t target: pane-id within the active window (default: the active pane).  The
     pane-configuring forms (-d/-e/-T/-m) and plain selection all act on -t's pane,
     not unconditionally the active one."
  (with-command-input (flags positionals args "tT"
                             :allowed-flags '(#\L #\R #\U #\D #\l #\d #\e #\m #\M #\t #\T)
                             :max-positionals 0
                             :message "select-pane: unsupported argument")
    (let* ((win    (session-active-window session))
           ;; Resolve -t to a pane-id within the active window; default = active pane.
           (target-pane (%resolve-pane-in-window win (%flag-value flags #\t))))
      (cond
        ((or (%flag-present-p flags #\L)
             (%flag-present-p flags #\R)
             (%flag-present-p flags #\U)
             (%flag-present-p flags #\D))
         (%select-pane-move-in-direction session flags))
        (t
         (%select-pane-configure-target win target-pane flags)))
      ;; after-select-pane fires once after the command (run-hooks now fires both
      ;; the add-hook and the .tmux.conf set-hook registries).
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-pane+ session))))
