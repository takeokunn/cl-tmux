(in-package #:cl-tmux/commands)

;;; High-level tmux commands that operate on the session/window/pane model.
;;; Each exported function is the CL analogue of a tmux command-line command.

;;; ── Kill ───────────────────────────────────────────────────────────────────

(defun kill-pane (session &optional pane)
  "Close PANE (default: active pane of SESSION).
   Sends SIGHUP to its child process and closes the PTY fd.
   Removes the pane from the window's split tree, collapsing its parent so the
   sibling reclaims the freed rectangle.  If the owning window becomes empty,
   also calls KILL-WINDOW.
   Returns :quit if no windows remain, nil otherwise."
  (let* ((win    (session-active-window session))
         (target (or pane (window-active-pane win))))
    (when target
      (ignore-errors (pty-close (pane-fd target) (pane-pid target))))
    (let ((survivor (window-remove-pane win target)))
      (if (null (window-panes win))
          (kill-window session win)
          (progn
            (window-select-pane win (or survivor (first (window-panes win))))
            nil)))))

(defun kill-window (session &optional window)
  "Destroy WINDOW (default: active window of SESSION).
   Kills all panes in it and removes the window from SESSION.
   Returns :quit if no windows remain, nil otherwise."
  (let* ((target    (or window (session-active-window session)))
         (remaining (remove target (session-windows session))))
    (dolist (pane (window-panes target))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
    (setf (session-windows session) remaining)
    (if (null remaining)
        :quit
        (progn
          (when (eq (session-active-window session) target)
            (session-select-window session (first remaining)))
          nil))))

;;; ── Rename ─────────────────────────────────────────────────────────────────

(defun rename-window (window name)
  "Set WINDOW's name to NAME."
  (when window
    (setf (window-name window) name)))

;;; ── Window selection ───────────────────────────────────────────────────────

(defun select-window-by-number (session n)
  "Select the Nth window (0-based) of SESSION if it exists."
  (let ((win (nth n (session-windows session))))
    (when win
      (session-select-window session win))))

;;; ── Pane resize ────────────────────────────────────────────────────────────

(defun resize-pane (window direction &optional (amount 5))
  "Resize the active pane of WINDOW by AMOUNT cells in DIRECTION (:left/:right/
   :up/:down).

   With a split TREE the border between the active pane and a neighbour in
   DIRECTION is resolved from the tree and moved in ANY direction that has a
   neighbour, reflowing every affected pane (no gaps/overlaps with 3+ panes).
   Without a tree (legacy flat fixtures) the old single-orientation behaviour is
   used: :left/:right adjust a vertical split, :up/:down a horizontal one, and
   an off-axis direction is a no-op.

   Returns the active pane when a resize happened, NIL otherwise."
  (let ((pane (and window (window-active-pane window))))
    (when pane
      (if (window-tree window)
          (window-resize-active window direction amount)
          ;; Legacy flat path (no tree).
          (ecase (window-layout window)
            (:vertical
             (when (member direction '(:left :right))
               (%resize-vertical window pane direction amount)
               pane))
            (:horizontal
             (when (member direction '(:up :down))
               (%resize-horizontal window pane direction amount)
               pane))
            ((nil) nil))))))         ; single pane: nothing to resize against

(defun %resize-vertical (win pane direction delta)
  "Adjust PANE width and its neighbour within a vertical split of WIN."
  (let* ((panes (window-panes win))
         (idx   (position pane panes))
         (adj   (when idx
                  (if (eq direction :right)
                      (nth (min (1+ idx) (1- (length panes))) panes)
                      (when (> idx 0) (nth (1- idx) panes))))))
    (when (and adj (not (eq adj pane)))
      (let ((d (if (eq direction :right) delta (- delta))))
        (when (and (> (+ (pane-width pane) d) 2)
                   (> (- (pane-width adj)  d) 2))
          (setf (pane-width pane) (+ (pane-width pane) d)
                (pane-width adj)  (- (pane-width adj)  d))
          (setf (pane-x adj)
                (+ (pane-x pane) (pane-width pane) 1))
          (pane-reposition pane
                           (pane-x pane) (pane-y pane)
                           (pane-width pane) (pane-height pane))
          (pane-reposition adj
                           (pane-x adj) (pane-y adj)
                           (pane-width adj) (pane-height adj)))))))

(defun %resize-horizontal (win pane direction delta)
  "Adjust PANE height and its neighbour within a horizontal split of WIN."
  (let* ((panes (window-panes win))
         (idx   (position pane panes))
         (adj   (when idx
                  (if (eq direction :down)
                      (nth (min (1+ idx) (1- (length panes))) panes)
                      (when (> idx 0) (nth (1- idx) panes))))))
    (when (and adj (not (eq adj pane)))
      (let ((d (if (eq direction :down) delta (- delta))))
        (when (and (> (+ (pane-height pane) d) 2)
                   (> (- (pane-height adj)  d) 2))
          (setf (pane-height pane) (+ (pane-height pane) d)
                (pane-height adj)  (- (pane-height adj)  d))
          (setf (pane-y adj)
                (+ (pane-y pane) (pane-height pane) 1))
          (pane-reposition pane
                           (pane-x pane) (pane-y pane)
                           (pane-width pane) (pane-height pane))
          (pane-reposition adj
                           (pane-x adj) (pane-y adj)
                           (pane-width adj) (pane-height adj)))))))

;;; ── Copy mode ──────────────────────────────────────────────────────────────

(defun copy-mode-enter (screen)
  "Enter copy/scroll mode on SCREEN: freeze the viewport at the live position."
  (setf (screen-copy-mode-p screen) t
        (screen-copy-offset  screen) 0))

(defun copy-mode-exit (screen)
  "Exit copy mode: resume live PTY output display."
  (setf (screen-copy-mode-p screen) nil
        (screen-copy-offset  screen) 0))

(defun copy-mode-scroll (screen delta)
  "Adjust SCREEN's copy-offset by DELTA lines.
   Positive DELTA scrolls back toward older output; negative scrolls forward.
   Clamped to [0, (length scrollback)]. Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (let ((max-offset (length (screen-scrollback screen))))
      (setf (screen-copy-offset screen)
            (max 0 (min max-offset (+ (screen-copy-offset screen) delta))))
      (setf (screen-dirty-p screen) t))))
