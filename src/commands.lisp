(in-package #:cl-tmux/commands)

;;; ── Pane operations ────────────────────────────────────────────────────────
;;;
;;; swap_pane(Window, Dir)   :- active(Window, AP), neighbor(AP, Dir, Other),
;;;                              swap_positions(AP, Other), swap_list_order(AP, Other).
;;; capture_pane(Pane, Opts) :- lock(screen(Pane)),
;;;                              (scrollback(Opts) -> emit_scrollback ; true),
;;;                              emit_visible_rows.

(defun swap-pane (window direction)
  "Swap the active pane with the next (:right) or previous (:left) pane in WINDOW.
   Swaps the panes in the panes list, reassigns positions, and relayouts."
  (let* ((panes (window-panes window))
         (ap    (window-active-pane window))
         (idx   (position ap panes))
         (n     (length panes)))
    (when (> n 1)
      (let* ((other-idx (ecase direction
                          (:right (mod (1+ idx) n))
                          (:left  (mod (1- idx) n))))
             (other (nth other-idx panes))
             (new-panes (copy-list panes)))
        (setf (nth idx new-panes) other
              (nth other-idx new-panes) ap
              (window-panes window) new-panes)
        ;; Swap x/y/width/height between the two panes using destructuring.
        (let ((ax (pane-x ap)) (ay (pane-y ap))
              (aw (pane-width ap)) (ah (pane-height ap)))
          (pane-reposition ap
                           (pane-x other) (pane-y other)
                           (pane-width other) (pane-height other))
          (pane-reposition other ax ay aw ah))
        ap))))

(defun %screen-row-string (screen row)
  "Return a string of WIDTH characters representing ROW in SCREEN's visible grid.
   Each character is read directly from the cell at (col, ROW).
   This is a pure data-to-string conversion; no side effects."
  (let* ((width (screen-width screen))
         (result (make-string width)))
    (dotimes (col width result)
      (setf (char result col)
            (cell-char (screen-cell screen col row))))))

(defun %scrollback-row-string (cell-vector)
  "Return a string of characters from a scrollback row CELL-VECTOR (a simple-vector of cells)."
  (let* ((n      (length cell-vector))
         (result (make-string n)))
    (dotimes (i n result)
      (setf (char result i) (cell-char (aref cell-vector i))))))

(defun capture-pane (pane &key (include-scrollback nil))
  "Dump the visible content of PANE as a string.
   When INCLUDE-SCROLLBACK is T, also include scrollback history above the visible area.
   The screen lock is held only for snapshot extraction; string rendering happens
   outside the lock so renderer threads are not blocked during I/O."
  (let ((screen (pane-screen pane)))
    ;; Snapshot pure data under lock (I/O-synchronisation concern).
    (let ((scrollback-snapshot nil)
          (visible-rows nil))
      (with-lock-held ((screen-lock screen))
        (when include-scrollback
          (setf scrollback-snapshot (reverse (screen-scrollback screen))))
        (setf visible-rows
              (loop for row from 0 below (screen-height screen)
                    collect (%screen-row-string screen row))))
      ;; Render to string outside the lock (pure I/O).
      (with-output-to-string (out)
        (dolist (row-str scrollback-snapshot)
          (write-string (%scrollback-row-string row-str) out)
          (terpri out))
        (dolist (row-str visible-rows)
          (write-string row-str out)
          (terpri out))))))

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

(defun break-pane (session)
  "Remove the active pane from its window and place it as the sole pane
   of a new window.  When the source window has only one pane, break-pane
   is a no-op (nothing to break out).  Returns the new window, or NIL."
  (let* ((src-win (session-active-window session))
         (pane    (and src-win (window-active-pane src-win))))
    (unless (and src-win pane) (return-from break-pane nil))
    ;; Must have at least 2 panes to break one out.
    (when (< (length (window-panes src-win)) 2)
      (return-from break-pane nil))
    ;; Remove pane from its current window (collapses the tree).
    (window-remove-pane src-win pane)
    ;; After removal, re-select a pane in the source window.
    (when (window-panes src-win)
      (window-select-pane src-win (first (window-panes src-win))))
    ;; Create a new window with the pane as the sole full-screen occupant.
    ;; Use the lowest free window id (same rule as session-new-window).
    (let* ((rows    (window-height src-win))
           (cols    (window-width  src-win))
           (new-id  (cl-tmux/model::%next-window-id session))
           (name    (cl-tmux/model::%shell-basename))
           (new-win (make-window :id new-id :name name :width cols :height rows)))
      ;; Install the pane as the sole leaf in the new window's tree.
      (setf (window-panes new-win) (list pane)
            (window-tree  new-win) (make-layout-leaf pane))
      (window-select-pane new-win pane)
      ;; Reposition the pane to fill the new window.
      (pane-reposition pane 0 0 cols rows)
      ;; Attach the new window to the session via the model-layer helper.
      (session-insert-window session new-win)
      (session-select-window session new-win)
      (run-hooks +hook-after-new-window+ new-win)
      new-win)))

;;; ── join-pane / move-pane ───────────────────────────────────────────────────
;;;
;;; join_pane(Session, SrcWin, SrcPane, DstWin, Dir) :-
;;;   remove_pane(SrcWin, SrcPane),
;;;   (empty(SrcWin) -> kill_window(Session, SrcWin) ; true),
;;;   insert_by_split(DstWin, SrcPane, Dir).

(defun %join-pane-kill-empty-src (session src-window)
  "Remove SRC-WINDOW from SESSION when it has no panes remaining.
   Switches the active window to the nearest survivor if needed."
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
                (layout-leaves (window-tree dst-window)))
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

(defun pipe-pane-open (pane command)
  "Tee PANE's PTY output to a pipe connected to COMMAND.
   If PANE already has an open pipe, it is closed first.
   Returns the pipe write-fd on success, NIL on failure."
  ;; Close any existing pipe.
  (when (pane-pipe-fd pane)
    (pipe-pane-close pane))
  ;; Open a new pipe to the command.
  (handler-case
      (let* ((shell cl-tmux/config:*default-shell*)
             (proc  (uiop:launch-program (list shell "-c" command)
                                         :input :stream :output nil
                                         :error-output nil))
             (stream (uiop:process-info-input proc)))
        (setf (pane-pipe-fd pane) stream)
        stream)
    (error () nil)))

(defun pipe-pane-close (pane)
  "Close PANE's output pipe if one is open."
  (when (pane-pipe-fd pane)
    (ignore-errors (close (pane-pipe-fd pane)))
    (setf (pane-pipe-fd pane) nil)))

(defun pipe-pane-write (pane bytes)
  "Write BYTES to PANE's output pipe if one is active.
   Silently ignores write errors (pipe may have closed on the other end)."
  (when (pane-pipe-fd pane)
    (ignore-errors
      (write-sequence bytes (pane-pipe-fd pane))
      (force-output (pane-pipe-fd pane)))))

;;; ── send-keys-to-pane ───────────────────────────────────────────────────────
;;;
;;; send_keys_to_pane(Pane, String) :-
;;;   pane_fd(Pane, Fd),
;;;   Fd > -1,
;;;   forall(char(Ch, String), write_byte(Fd, Ch)).

(defun send-keys-to-pane (pane string)
  "Write each character of STRING as a UTF-8 byte sequence to PANE's PTY fd.
   Silently ignores the write when PANE has no open PTY (fd <= -1)."
  (when (and pane (> (pane-fd pane) -1))
    (let ((bytes (babel:string-to-octets string :encoding :utf-8)))
      (pty-write (pane-fd pane) bytes))))

;;; ── Shell ──────────────────────────────────────────────────────────────────
;;;
;;; run_shell(cmd)            :- subprocess(cmd, timeout=30, output=string).
;;; if_shell(cmd, then, else) :- subprocess(cmd), exit_code=0 -> then ; else.
;;;
;;; Both run-shell and if-shell accept a :timeout keyword (seconds, default 30).
;;; The foreground (synchronous) paths honour the timeout via a bordeaux-threads
;;; helper; background tasks are fire-and-forget.
;;;
;;; uiop:run-program is used instead of sb-ext:run-program so the code is
;;; portable across all ASDF-supported implementations.
;;;
;;; if-shell is exported and wired to the :if-shell dispatch key in dispatch.lisp
;;; so it is reachable from the prefix-key handler.

(defun %run-with-timeout (thunk timeout-seconds)
  "Run THUNK in a fresh thread; join it up to TIMEOUT-SECONDS.
   Returns (funcall thunk) result or NIL if the timeout expires."
  (handler-case
      (bt:with-timeout (timeout-seconds)
        (funcall thunk))
    (bt:timeout () nil)))

(defmacro with-shell-timeout ((shell-var timeout) &body body)
  "Bind SHELL-VAR to the active shell binary and run BODY with a TIMEOUT (seconds).
   TIMEOUT is evaluated at macro-expansion call time and passed directly to
   %RUN-WITH-TIMEOUT.  Returns the result of BODY or NIL when the timeout fires."
  `(%run-with-timeout
     (lambda ()
       (let ((,shell-var (or *default-shell* "/bin/sh")))
         ,@body))
     ,timeout))

(defun run-shell (command &key background (timeout 30))
  "Run COMMAND in a subshell.  Returns the output string (stdout) when BACKGROUND
   is nil, or T immediately when BACKGROUND is T.
   Uses *default-shell* for the shell binary.
   TIMEOUT (seconds, default 30) limits how long a synchronous command may run;
   when the limit is exceeded NIL is returned."
  (if background
      (progn
        ;; Deliberate no-timeout policy: background shell commands are fire-and-forget.
        ;; The caller requested asynchronous execution and does not need the result.
        ;; If a bounded background job is needed, the caller should wrap in bt:with-timeout.
        (bt:make-thread
          (lambda ()
            (let ((shell (or *default-shell* "/bin/sh")))
              (uiop:run-program (list shell "-c" command)
                                :output nil :ignore-error-status t)))
          :name "shell-bg")
        t)
      (with-shell-timeout (shell timeout)
        (uiop:run-program (list shell "-c" command)
                          :output :string :ignore-error-status t))))

(defun if-shell (command then-fn &key else-fn (timeout 30))
  "Run COMMAND; call THEN-FN if exit code is 0, ELSE-FN otherwise.
   THEN-FN and ELSE-FN are zero-argument functions (keyword arguments).
   TIMEOUT (seconds, default 30) limits how long the command may run;
   when the limit is exceeded ELSE-FN is called."
  (let ((exit-code
          (with-shell-timeout (shell timeout)
            (multiple-value-bind (output error-output code)
                (uiop:run-program (list shell "-c" command)
                                  :output nil :ignore-error-status t)
              (declare (ignore output error-output))
              code))))
    (if (and exit-code (zerop exit-code))
        (when then-fn (funcall then-fn))
        (when else-fn (funcall else-fn)))))
