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
   Returns :quit if no windows remain, NIL otherwise."
  (let* ((target    (or window (session-active-window session)))
         (remaining (remove target (session-windows session))))
    (dolist (pane (window-panes target))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
    (setf (session-windows session) remaining)
    (unless remaining (return-from kill-window :quit))
    (when (eq (session-active-window session) target)
      (session-select-window session (first remaining)))
    nil))

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
  "Resize the active pane via the split tree. Returns the active pane on success, NIL otherwise."
  (when (and window (window-tree window))
    (window-resize-active window direction amount)))

;;; ── Copy mode transitions ──────────────────────────────────────────────────
;;;
;;; Enter and exit are symmetric facts:
;;;   copy_mode(enter, Screen) :- copy_mode_p(Screen) := true,  offset := 0.
;;;   copy_mode(exit,  Screen) :- copy_mode_p(Screen) := false, offset := 0.

(defmacro define-copy-mode-transitions (&rest specs)
  "Build copy-mode transition functions from a Prolog-like fact table.
   Each SPEC is (name active-p docstring): active-p is T or NIL."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name active-p docstring) spec
                   `(defun ,name (screen)
                      ,docstring
                      (setf (screen-copy-mode-p   screen) ,active-p
                            (screen-copy-offset    screen) 0
                            ;; Reset selection on mode transition to avoid stale highlights.
                            (screen-copy-mark      screen) nil
                            (screen-copy-cursor    screen) nil
                            (screen-copy-selecting screen) nil))))
               specs)))

(define-copy-mode-transitions
  (copy-mode-enter t
   "Enter copy/scroll mode on SCREEN: freeze the viewport at the live position.")
  (copy-mode-exit nil
   "Exit copy mode: resume live PTY output display."))

(defun copy-mode-scroll (screen delta)
  "Adjust SCREEN's copy-offset by DELTA lines.
   Positive DELTA scrolls back toward older output; negative scrolls forward.
   Clamped to [0, (length scrollback)]. Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (let ((max-offset (length (screen-scrollback screen))))
      (setf (screen-copy-offset screen)
            (max 0 (min max-offset (+ (screen-copy-offset screen) delta))))
      (setf (screen-dirty-p screen) t))))

(defun rename-session (session name)
  "Set SESSION's name to NAME."
  (when (and session name (not (string= name "")))
    (setf (session-name session) name)))


;;; ── Copy-mode cursor and selection ────────────────────────────────────────

(defun copy-mode-move-cursor (screen direction)
  "Move SCREEN's copy-mode cursor in DIRECTION (:left :right :up :down).
   Initializes the cursor to (0 . 0) if not yet set.  Marks the screen dirty."
  (when (screen-copy-mode-p screen)
    (let* ((cur (or (screen-copy-cursor screen) (cons 0 0)))
           (row (car cur))
           (col (cdr cur))
           (h   (screen-height screen))
           (w   (screen-width  screen)))
      (setf (screen-copy-cursor screen)
            (ecase direction
              (:left  (cons row (max 0 (1- col))))
              (:right (cons row (min (1- w) (1+ col))))
              (:up    (cons (max 0 (1- row)) col))
              (:down  (cons (min (1- h) (1+ row)) col))))
      ;; When selecting, move mark anchor too if not yet placed
      (when (and (screen-copy-selecting screen) (null (screen-copy-mark screen)))
        (setf (screen-copy-mark screen) (screen-copy-cursor screen)))
      (setf (screen-dirty-p screen) t))))

(defun copy-mode-begin-selection (screen)
  "Begin a text selection at the current copy-mode cursor position."
  (when (screen-copy-mode-p screen)
    (let ((cur (or (screen-copy-cursor screen) (cons 0 0))))
      (setf (screen-copy-mark      screen) cur
            (screen-copy-cursor    screen) cur
            (screen-copy-selecting screen) t
            (screen-dirty-p        screen) t))))

(defun copy-mode-cancel-selection (screen)
  "Cancel any active copy-mode selection."
  (setf (screen-copy-mark      screen) nil
        (screen-copy-cursor    screen) nil
        (screen-copy-selecting screen) nil
        (screen-dirty-p        screen) t))

;;; ── Copy-mode yank helpers ────────────────────────────────────────────────
;;;
;;; %selection-bounds extracts the canonical (start-row end-row start-col end-col)
;;; rectangle from the mark and cursor positions — independent of which end the
;;; user anchored first.  %selection-text builds the string from that rectangle.
;;; Both are private (percent-prefixed) and independently testable.

(defun %selection-bounds (screen)
  "Return (values start-r end-r start-c end-c) for the current copy-mode
   selection in SCREEN, normalising mark and cursor order.
   Assumes mark and cursor are already set."
  (let* ((mark   (screen-copy-mark   screen))
         (cursor (screen-copy-cursor screen))
         (mr (car mark))   (mc (cdr mark))
         (cr (car cursor)) (cc (cdr cursor))
         (start-r (min mr cr))
         (end-r   (max mr cr))
         (start-c (if (< mr cr) mc (if (> mr cr) cc (min mc cc))))
         (end-c   (if (< mr cr) cc (if (> mr cr) mc (max mc cc)))))
    (values start-r end-r start-c end-c)))

(defun %selection-text (screen)
  "Compute the text selected by copy-mode in SCREEN.
   Returns a string, or NIL when no valid selection exists.
   Intermediate rows (not the last) are right-trimmed of trailing spaces."
  (unless (and (screen-copy-selecting screen)
               (screen-copy-mark   screen)
               (screen-copy-cursor screen))
    (return-from %selection-text nil))
  (multiple-value-bind (start-r end-r start-c end-c)
      (%selection-bounds screen)
    (let* ((w    (screen-width screen))
           (text (with-output-to-string (out)
                   (loop for row from start-r to end-r do
                     (let ((c0 (if (= row start-r) start-c 0))
                           (c1 (if (= row end-r)   end-c   w)))
                       (let ((row-str (with-output-to-string (rs)
                                        (loop for col from c0 below c1 do
                                          (write-char (cell-char (screen-cell screen col row)) rs)))))
                         ;; Trim trailing spaces from intermediate rows.
                         (write-string (if (< row end-r)
                                           (string-right-trim " " row-str)
                                           row-str)
                                       out))
                       (when (< row end-r) (write-char #\Newline out)))))))
      (if (plusp (length text)) text nil))))

(defun copy-mode-yank (screen)
  "Copy selected text to paste buffer and exit copy mode."
  (let ((text (%selection-text screen)))
    (when (and text (plusp (length text)))
      (cl-tmux/buffer:add-paste-buffer text)))
  (copy-mode-cancel-selection screen)
  (copy-mode-exit screen))

;;; ── Swap-pane ─────────────────────────────────────────────────────────────

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
        ;; Swap x/y/width/height between the two panes
        (let ((ax (pane-x ap)) (ay (pane-y ap)) (aw (pane-width ap)) (ah (pane-height ap)))
          (pane-reposition ap (pane-x other) (pane-y other) (pane-width other) (pane-height other))
          (pane-reposition other ax ay aw ah))
        ap))))

;;; ── Capture-pane ──────────────────────────────────────────────────────────

(defun capture-pane (pane &key (include-scrollback nil))
  "Dump the visible content of PANE as a string.
   When INCLUDE-SCROLLBACK is T, also include scrollback history above the visible area."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (with-output-to-string (out)
        (when include-scrollback
          (dolist (row (reverse (screen-scrollback screen)))
            (dotimes (i (length row))
              (write-char (cell-char (aref row i)) out))
            (terpri out)))
        (dotimes (row (screen-height screen))
          (dotimes (col (screen-width screen))
            (write-char (cell-char (screen-cell screen col row)) out))
          (terpri out))))))

;;; ── Shell execution commands ───────────────────────────────────────────────
;;;
;;; Both run-shell and if-shell accept an optional :timeout keyword (seconds).
;;; When supplied it is forwarded to sb-ext:run-program so a hung command cannot
;;; block the event thread indefinitely.  The foreground (synchronous) paths are
;;; the only ones that honour the timeout; background tasks are fire-and-forget.
;;;
;;; if-shell is exported and wired to the :if-shell dispatch key in dispatch.lisp
;;; so it is reachable from the prefix-key handler.

(defun run-shell (command &key background (timeout nil))
  "Run COMMAND in a subshell.  Returns the output string (stdout) when BACKGROUND
   is nil, or T immediately when BACKGROUND is T.
   Uses *default-shell* for the shell binary.
   TIMEOUT (seconds, optional) limits how long a synchronous command may run;
   when the limit is exceeded sb-ext:run-program signals an error."
  (let ((shell (or *default-shell* "/bin/sh")))
    (if background
        (progn
          (sb-ext:run-program shell (list "-c" command)
                              :wait nil :output nil :error nil)
          t)
        (with-output-to-string (out)
          (apply #'sb-ext:run-program shell (list "-c" command)
                 :wait t :output out :error nil
                 (when timeout (list :timeout timeout)))))))

(defun if-shell (command then-fn &optional else-fn &key (timeout nil))
  "Run COMMAND; call THEN-FN if exit code is 0, ELSE-FN otherwise.
   THEN-FN and ELSE-FN are zero-argument functions.
   TIMEOUT (seconds, optional) is forwarded to sb-ext:run-program so a hung
   command cannot block the event thread indefinitely."
  (let* ((shell (or *default-shell* "/bin/sh"))
         (proc  (apply #'sb-ext:run-program shell (list "-c" command)
                       :wait t :output nil :error nil
                       (when timeout (list :timeout timeout))))
         (exit  (sb-ext:process-exit-code proc)))
    (if (zerop exit)
        (when then-fn (funcall then-fn))
        (when else-fn (funcall else-fn)))))
