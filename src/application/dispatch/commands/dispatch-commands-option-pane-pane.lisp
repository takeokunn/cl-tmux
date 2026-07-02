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
  "Mark PANE as the server-wide marked pane (select-pane -m).  Delegates to the
   %toggle-mark-pane chokepoint so the scriptable command path behaves like the
   C-b m keybinding: it sets BOTH the per-pane flag and the *server-marked-pane*
   singleton (which join-pane/swap-pane read as their default source), and
   toggles the mark off when PANE is already the marked pane — matching tmux's
   server_set_marked/server_clear_marked semantics."
  (declare (ignore window))
  (when pane
    (%toggle-mark-pane pane)))

(defun %select-pane-clear-mark (window)
  "Clear the marked pane (select-pane -M): unmark every pane in WINDOW and also
   clear the server-wide *server-marked-pane* singleton, so a mark set via
   `select-pane -m` (or C-b m) is fully cleared rather than left dangling for
   join-pane/swap-pane."
  (when window
    (dolist (p (window-panes window))
      (setf (pane-marked p) nil)))
  (when *server-marked-pane*
    (setf (pane-marked *server-marked-pane*) nil
          *server-marked-pane* nil)))

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

(defun %pane-navigation-unzoom (window flags)
  "tmux window_push/pop_zoom: pane-navigation commands (select-pane, swap-pane,
   rotate-window, last-pane) unzoom a zoomed WINDOW unless -Z (keep zoomed) is
   given.  No-op for an unzoomed window."
  (when (and window
             (window-zoom-p window)
             (not (%flag-present-p flags #\Z)))
    (window-zoom-toggle window)))

(defun %select-pane-selection-form-p (flags)
  "True when this select-pane invocation moves focus (a selection or direction
   move), as opposed to the pane-configuring forms (-d/-e/-T/-m/-M) which tmux
   runs without popping zoom."
  (not (or (%flag-present-p flags #\d)
           (%flag-present-p flags #\e)
           (%flag-present-p flags #\T)
           (%flag-present-p flags #\m)
           (%flag-present-p flags #\M))))

(defun %cmd-select-pane (session args)
  "select-pane [-L|-R|-U|-D|-l|-d|-e|-m|-M] [-t target] [-T title]: select or configure a pane.
   -L/-R/-U/-D: move in the given direction (relative to the active pane).
   -l: select the previously active (last) pane.
   -d/-e: disable / re-enable keyboard input to the TARGET pane.
   -T title: set the TARGET pane's title.
   -m: mark the TARGET pane; -M: clear the marked pane (unmark all).
   -t target: pane-id within the active window (default: the active pane).  The
     pane-configuring forms (-d/-e/-T/-m) and plain selection all act on -t's pane,
     not unconditionally the active one.
   -Z: keep the window zoomed if it was zoomed (tmux window_push_zoom); without
     -Z, selecting a pane in a zoomed window unzooms it first (window_pop_zoom)."
  (with-command-input (flags positionals args "tT"
                             :allowed-flags '(#\L #\R #\U #\D #\l #\d #\e #\m #\M #\t #\T #\Z)
                             :max-positionals 0
                             :message "select-pane: unsupported argument")
    (let* ((win    (session-active-window session))
           ;; Resolve -t to a pane-id within the active window; default = active pane.
           (target-pane (%resolve-pane-in-window win (%flag-value flags #\t))))
      ;; tmux pops zoom before a focus-moving selection unless -Z; the
      ;; pane-configuring forms (-d/-e/-T/-m/-M) leave zoom untouched.
      (when (%select-pane-selection-form-p flags)
        (%pane-navigation-unzoom win flags))
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
