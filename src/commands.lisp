(in-package #:cl-tmux/commands)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "commands-capture-pane.lisp"
                         (make-pathname :name nil :type nil
                                        :defaults (or *compile-file-truename*
                                                      *load-truename*
                                                      *default-pathname-defaults*)))))

;;; ── Pane operations ────────────────────────────────────────────────────────
;;;
;;; swap_pane(Window, Dir)   :- active(Window, AP), neighbor(AP, Dir, Other),
;;;                              swap_positions(AP, Other), swap_list_order(AP, Other).
;;; capture_pane(Pane, Opts) :- lock(screen(Pane)),
;;;                              (scrollback(Opts) -> emit_scrollback ; true),
;;;                              emit_visible_rows.

(defun %swap-pane-geometry (active-pane other)
  "Exchange the screen positions of two panes ACTIVE-PANE and OTHER in-place.
   Updates both pane structs so the renderer sees the swapped layout immediately."
  (let ((saved-x      (pane-x      active-pane))
        (saved-y      (pane-y      active-pane))
        (saved-width  (pane-width  active-pane))
        (saved-height (pane-height active-pane)))
    (pane-reposition active-pane
                     (pane-x other) (pane-y other)
                     (pane-width other) (pane-height other))
    (pane-reposition other saved-x saved-y saved-width saved-height)))

(defun swap-two-panes (window pane-a pane-b)
  "Swap PANE-A and PANE-B within WINDOW: exchange both their list positions and
   their screen geometry, so the renderer sees the swap immediately.  No-op
   (returns NIL) when either pane is missing, they are the same, or either is not
   in WINDOW.  Does NOT change which pane is active.  Returns PANE-A on success."
  (let* ((panes (window-panes window))
         (ia    (and pane-a (position pane-a panes)))
         (ib    (and pane-b (position pane-b panes))))
    (when (and ia ib (/= ia ib))
      (let ((new-panes (copy-list panes)))
        (setf (nth ia new-panes) pane-b
              (nth ib new-panes) pane-a
              (window-panes window) new-panes))
      (%swap-pane-geometry pane-a pane-b)
      pane-a)))

(defun swap-pane (window direction)
  "Swap the active pane with an adjacent pane in WINDOW.
   DIRECTION:
     :right / :forward  — next in panes list (wraps around)
     :left  / :backward — previous in panes list (wraps around)
     :up    — spatially adjacent pane above (via pane-neighbor)
     :down  — spatially adjacent pane below (via pane-neighbor)
   Swaps both list order and screen geometry (via swap-two-panes)."
  (let* ((panes (window-panes window))
         (ap    (window-active-pane window))
         (idx   (position ap panes))
         (n     (length panes)))
    (when (> n 1)
      (let ((other
             (ecase direction
               ((:right :forward)
                (nth (mod (1+ idx) n) panes))
               ((:left :backward)
                (nth (mod (1- idx) n) panes))
               (:up   (pane-neighbor window ap :up))
               (:down (pane-neighbor window ap :down)))))
        (when other
          (swap-two-panes window ap other))))))

;;; ── break-pane ─────────────────────────────────────────────────────────────
;;;
;;; break_pane(Session) :-
;;;   active_window(Session, Win),
;;;   active_pane(Win, Pane),
;;;   (sole_pane(Win) -> no_op ; true),
;;;   remove_pane(Win, Pane),
;;;   new_window(Session, NewWin),
;;;   set_sole_pane(NewWin, Pane),
;;;   select_window(Session, NewWin).

(defun break-pane (session &key src-window pane name (select t))
  "Remove PANE from SRC-WINDOW and place it as the sole pane of a new window.
   SRC-WINDOW defaults to the session's active window and PANE to that window's
   active pane (the no-argument interactive behaviour).  NAME, when given, names
   the new window (break-pane -n); otherwise the shell basename is used.  When
   SELECT is true (default) the session switches to the new window; NIL leaves the
   current window active (break-pane -d).  When the source window has only one
   pane, break-pane is a no-op.  Returns the new window, or NIL."
  (let* ((src-win (or src-window (session-active-window session)))
         (pane    (or pane (and src-win (window-active-pane src-win)))))
    (when (and src-win pane (>= (length (window-panes src-win)) 2))
      ;; Remove pane from its current window (collapses the tree).
    (window-remove-pane src-win pane)
    ;; After removal, re-select a pane in the source window.
    (when (window-panes src-win)
      (window-select-pane src-win (first (window-panes src-win))))
    ;; Create a new window with the pane as the sole full-screen occupant.
    ;; Use the lowest free window id (same rule as session-new-window).
    (let* ((rows    (window-height src-win))
           (cols    (window-width  src-win))
           (new-id  (cl-tmux/model::%next-window-id
                     session
                     (or (cl-tmux/options:get-option "base-index") 0)))
           (wname   (or name (cl-tmux/model::%shell-basename)))
           (new-win (make-window :id new-id :name wname :width cols :height rows)))
      ;; Install the pane as the sole leaf in the new window's tree.
      (setf (window-panes new-win) (list pane)
            (window-tree  new-win) (make-layout-leaf pane)
            (pane-window  pane)    new-win)
      (window-select-pane new-win pane)
      ;; Reposition the pane to fill the new window.
      (pane-reposition pane 0 0 cols rows)
      ;; Attach the new window to the session via the model-layer helper.
      (session-insert-window session new-win)
      (when select (session-select-window session new-win))
      (run-hooks +hook-after-new-window+ new-win)
      new-win))))

;;; ── join-pane / move-pane ───────────────────────────────────────────────────
;;;
;;; join_pane(Session, SrcWin, SrcPane, DstWin, Dir) :-
;;;   remove_pane(SrcWin, SrcPane),
;;;   (empty(SrcWin) -> kill_window(Session, SrcWin) ; true),
;;;   insert_by_split(DstWin, SrcPane, Dir).

(defun %join-pane-kill-empty-src (session src-window)
  "Remove SRC-WINDOW from SESSION when it has no panes remaining.
   Switches the active window to the first surviving window if needed."
  (when (null (window-panes src-window))
    (let ((remaining (remove src-window (session-windows session))))
      (setf (session-windows session) remaining)
      (when (eq (session-active-window session) src-window)
        (session-select-window session (first remaining))))))

(defun %join-pane-insert-into-dst (src-pane dst-window direction)
  "Insert SRC-PANE into DST-WINDOW as a DIRECTION split.
   Returns SRC-PANE on success, NIL when the destination has no active leaf."
  (let* ((active      (window-active-pane dst-window))
         (tree        (window-tree dst-window))
         (active-leaf (and active tree (layout-find-leaf tree active))))
    (when active-leaf
      (multiple-value-bind (px py pw ph)
          (split-child-geometry active direction)
        (pane-reposition src-pane px py pw ph)
        (let ((new-split (make-layout-split direction active-leaf
                                            (make-layout-leaf src-pane) 1/2)))
          (cl-tmux/model::%replace-in-tree dst-window active-leaf new-split)
          (setf (window-panes dst-window)
                (layout-leaves (window-tree dst-window))
                (pane-window src-pane) dst-window)
          (window-relayout dst-window
                           (window-height dst-window)
                           (window-width  dst-window))
          src-pane)))))

(defun join-pane (session src-window src-pane dst-window direction)
  "Move SRC-PANE from SRC-WINDOW into DST-WINDOW as a split in DIRECTION.
   DIRECTION is :h (left/right) or :v (top/bottom).
   If SRC-WINDOW becomes empty after removal, it is killed.
   Returns SRC-PANE on success, NIL on failure."
  (when (and src-window src-pane dst-window)
    (window-remove-pane src-window src-pane)
    (%join-pane-kill-empty-src session src-window)
    (%join-pane-insert-into-dst src-pane dst-window direction)))

;;; ── pipe-pane ───────────────────────────────────────────────────────────────
;;;
;;; pipe_pane(Pane, Cmd) :-
;;;   (existing_pipe(Pane) -> close_pipe(Pane) ; true),
;;;   (Cmd \= nil -> open_pipe(Pane, Cmd) ; true).

(defconstant +pipe-pane-close-timeout+ 1
  "Seconds to wait for a pipe-pane subprocess to exit after stdin closes.")

(defconstant +pipe-pane-open-timeout+ 1
  "Seconds to wait for launching the pipe-pane subprocess.")

(defun pipe-pane-open (pane command)
  "Tee PANE's PTY output to a pipe connected to COMMAND.
   If PANE already has an open pipe, it is closed first.
   Returns the pipe write-fd on success, NIL on failure."
  ;; Close any existing pipe.
  (when (pane-pipe-fd pane)
    (pipe-pane-close pane))
  ;; Open a new pipe to the command.
  (let ((proc nil)
        (stream nil))
    (handler-case
        (bt:with-timeout (+pipe-pane-open-timeout+)
          (let* ((shell (or cl-tmux/config:*default-shell* "/bin/sh"))
                 (new-proc
                   (uiop:launch-program (list shell "-c" command)
                                       :input :stream :output nil
                                       :error-output nil))
                 (new-stream (uiop:process-info-input new-proc)))
            (setf proc new-proc
                  stream new-stream
                  (pane-pipe-fd pane) stream
                  (pane-pipe-process pane) proc)
            stream))
      (bt:timeout ()
        (ignore-errors (when stream (close stream)))
        (ignore-errors (%terminate-pipe-process proc))
        (setf (pane-pipe-fd pane) nil
              (pane-pipe-process pane) nil)
        nil)
      (error ()
        (ignore-errors (when stream (close stream)))
        (ignore-errors (%terminate-pipe-process proc))
        (setf (pane-pipe-fd pane) nil
              (pane-pipe-process pane) nil)
        nil))))

(defun %wait-pipe-process (process)
  "Return true when PROCESS exits before the pipe-pane close timeout."
  (when process
    (handler-case
        (progn
          (bt:with-timeout (+pipe-pane-close-timeout+)
            (uiop:wait-process process))
          t)
      (bt:timeout () nil)
      (error () nil))))

(defun %terminate-pipe-process (process)
  "Reap a pipe-pane subprocess, terminating it only if it ignores stdin EOF."
  (when (and process (not (%wait-pipe-process process)))
    (ignore-errors
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process)))
    (%wait-pipe-process process)))

(defun pipe-pane-close (pane)
  "Close PANE's output pipe if one is open."
  (let ((stream (pane-pipe-fd pane))
        (process (pane-pipe-process pane)))
    (when stream
      (ignore-errors (close stream)))
    (%terminate-pipe-process process)
    (setf (pane-pipe-fd pane) nil
          (pane-pipe-process pane) nil)))

(defun pipe-pane-write (pane bytes)
  "Write BYTES to PANE's output pipe if one is active.
   Silently ignores write errors (pipe may have closed on the other end)."
  (when (pane-pipe-fd pane)
    (ignore-errors
      (write-sequence bytes (pane-pipe-fd pane))
      (force-output (pane-pipe-fd pane)))))
