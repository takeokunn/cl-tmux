(in-package #:cl-tmux/commands)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                   *load-pathname*
                   *compile-file-pathname*
                   *default-pathname-defaults*))
         (src (merge-pathnames #P"src/" root)))
    (load (merge-pathnames #P"commands-capture-pane.lisp" src))))

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

(defun %join-pane-insert-into-dst (src-pane dst-window direction
                                   &key before full size)
  "Insert SRC-PANE into DST-WINDOW as a DIRECTION split.
   Returns SRC-PANE on success, NIL when the destination has no active leaf."
  (let* ((active      (window-active-pane dst-window))
         (tree        (window-tree dst-window))
         (active-leaf (and active tree (layout-find-leaf tree active))))
    (when (and active-leaf
               (if full
                   (cl-tmux/model::%split-axis-fits-p
                    (cl-tmux/model::%window-axis-extent dst-window direction)
                    direction)
                   (cl-tmux/model::%split-fits-p active direction)))
      ;; Refresh the destination layout before deriving the split size.  Test
      ;; fixtures can carry stale pane dimensions even when the window/tree size
      ;; is current, and `-l` must be computed from the rendered pane extent.
      (cl-tmux/model::window-relayout-current dst-window)
      (let* ((active      (window-active-pane dst-window))
             (tree        (window-tree dst-window))
             (active-leaf (layout-find-leaf tree active)))
      ;; Match split-window's layout rules: -f uses the full window extent, -b
      ;; swaps the child order, and -l supplies the target size hint.
      (multiple-value-bind (px py pw ph)
          (split-child-geometry active direction)
        (pane-reposition src-pane px py pw ph)
        (let* ((avail    (1- (if full
                                 (cl-tmux/model::%window-axis-extent dst-window direction)
                                 (cl-tmux/model::%orient-pane-extent active direction))))
               (new-ratio (if size
                              (cl-tmux/model::%ratio-from-size-hint size avail direction)
                              1/2))
               (anchor   (if full tree active-leaf))
               (new-node (make-layout-leaf src-pane))
               ;; `make-layout-split` stores the ratio for the FIRST child.
               ;; `%ratio-from-size-hint` returns the desired share for the new pane.
               (new-split (if before
                              (make-layout-split direction new-node anchor
                                                 new-ratio)
                              (make-layout-split direction anchor new-node
                                                 (- 1 new-ratio)))))
          (if full
              (setf (window-tree dst-window) new-split)
              (cl-tmux/model::%replace-in-tree dst-window active-leaf new-split))
          (setf (window-panes dst-window)
                (layout-leaves (window-tree dst-window))
                (pane-window src-pane) dst-window)
          (window-relayout dst-window
                           (window-height dst-window)
                           (window-width  dst-window))
          src-pane))))))

(defun join-pane (session src-window src-pane dst-window direction
                  &key before full size)
  "Move SRC-PANE from SRC-WINDOW into DST-WINDOW as a split in DIRECTION.
   DIRECTION is :h (left/right) or :v (top/bottom).
   If SRC-WINDOW becomes empty after removal, it is killed.
   Returns SRC-PANE on success, NIL on failure."
  (when (and src-window src-pane dst-window)
    (window-remove-pane src-window src-pane)
    (%join-pane-kill-empty-src session src-window)
    (%join-pane-insert-into-dst src-pane dst-window direction
                                :before before
                                :full full
                                :size size)))

;;; ── pipe-pane ───────────────────────────────────────────────────────────────
;;;
;;; pipe_pane(Pane, Cmd) :-
;;;   (existing_pipe(Pane) -> close_pipe(Pane) ; true),
;;;   (Cmd \= nil -> open_pipe(Pane, Cmd) ; true).

(defconstant +pipe-pane-close-timeout+ 1
  "Seconds to wait for a pipe-pane subprocess to exit after stdin closes.")

(defconstant +pipe-pane-open-timeout+ 1
  "Seconds to wait for launching the pipe-pane subprocess.")

(defun %pipe-pane-copy-output (pane output-stream)
  "Copy OUTPUT-STREAM from the command back into PANE's PTY."
  (unwind-protect
      (handler-case
          (let ((buffer (make-string 4096)))
            (loop
              for count = (read-sequence buffer output-stream)
              while (plusp count) do
                (ignore-errors
                  (pty-write (pane-fd pane) (subseq buffer 0 count)))))
        (end-of-file () nil)
        (error () nil))
    (ignore-errors (close output-stream))))

(defun %pipe-pane-start-output-thread (pane output-stream)
  "Start the background copier for command stdout into PANE."
  (bt:make-thread (lambda () (%pipe-pane-copy-output pane output-stream))
                  :name (format nil "pipe-pane-output-~D" (pane-id pane))))

(defun %pipe-pane-reset (pane)
  "Clear all pipe-pane state slots on PANE."
  (setf (pane-pipe-fd pane) nil
        (pane-pipe-output-stream pane) nil
        (pane-pipe-output-thread pane) nil
        (pane-pipe-process pane) nil))

(defun pipe-pane-open (pane command &key
                            (pane-output-to-command-p t)
                            (command-output-to-pane-p nil))
  "Connect PANE and COMMAND with pipe-pane direction flags.
   PANE-OUTPUT-TO-COMMAND-P routes pane output to the command's stdin.
   COMMAND-OUTPUT-TO-PANE-P routes command stdout back into the pane.
   Returns a non-NIL stream or process handle on success, NIL on failure."
  ;; Close any existing pipe in either direction.
  (when (pane-pipe-active-p pane)
    (pipe-pane-close pane))
  (let ((proc nil)
        (input-stream nil)
        (output-stream nil)
        (output-thread nil))
    (handler-case
        (bt:with-timeout (+pipe-pane-open-timeout+)
          (let* ((shell (or cl-tmux/config:*default-shell* "/bin/sh"))
                 (new-proc
                   (uiop:launch-program (list shell "-c" command)
                                       :input (if pane-output-to-command-p :stream nil)
                                       :output (if command-output-to-pane-p :stream nil)
                                       :error-output nil))
                 (new-input (and pane-output-to-command-p
                                 (uiop:process-info-input new-proc)))
                 (new-output (and command-output-to-pane-p
                                  (uiop:process-info-output new-proc))))
            (setf proc new-proc
                  input-stream new-input
                  output-stream new-output
                  (pane-pipe-fd pane) input-stream
                  (pane-pipe-output-stream pane) output-stream
                  (pane-pipe-process pane) proc)
            (when output-stream
              (setf output-thread
                    (%pipe-pane-start-output-thread pane output-stream)
                    (pane-pipe-output-thread pane) output-thread))
            (or input-stream output-stream proc t)))
      (bt:timeout ()
        (ignore-errors (when input-stream (close input-stream)))
        (ignore-errors (when output-stream (close output-stream)))
        (ignore-errors (%terminate-pipe-process proc))
        (ignore-errors (when output-thread
                         (cl-tmux::%join-thread-with-timeout output-thread
                                                             +pipe-pane-close-timeout+)))
        (%pipe-pane-reset pane)
        nil)
      (error ()
        (ignore-errors (when input-stream (close input-stream)))
        (ignore-errors (when output-stream (close output-stream)))
        (ignore-errors (%terminate-pipe-process proc))
        (ignore-errors (when output-thread
                         (cl-tmux::%join-thread-with-timeout output-thread
                                                             +pipe-pane-close-timeout+)))
        (%pipe-pane-reset pane)
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
  (let ((input-stream (pane-pipe-fd pane))
        (output-stream (pane-pipe-output-stream pane))
        (output-thread (pane-pipe-output-thread pane))
        (process (pane-pipe-process pane)))
    (when input-stream
      (ignore-errors (close input-stream)))
    (when output-stream
      (ignore-errors (close output-stream)))
    (%terminate-pipe-process process)
    (when output-thread
      (ignore-errors
        (cl-tmux::%join-thread-with-timeout output-thread
                                            +pipe-pane-close-timeout+)))
    (%pipe-pane-reset pane)))

(defun pipe-pane-write (pane bytes)
  "Write BYTES to PANE's output pipe if one is active.
   Silently ignores write errors (pipe may have closed on the other end)."
  (when (pane-pipe-fd pane)
    (ignore-errors
      (write-sequence bytes (pane-pipe-fd pane))
      (force-output (pane-pipe-fd pane)))))
