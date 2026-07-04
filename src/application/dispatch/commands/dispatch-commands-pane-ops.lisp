(in-package #:cl-tmux)

;;; -- Pane operation commands -------------------------------------------------
;;;
;;; resize-pane, join-pane, move-pane, break-pane, clear-history, rotate-window.

(defun %resize-pane-to-absolute-dimension (win pane target-size size-fn direction)
  (let ((delta (- target-size (funcall size-fn pane))))
    (unless (zerop delta)
      (resize-pane win direction delta))))

;;; define-flag-dispatch expands a declarative (FLAG-CHAR HANDLER-FORM) table into
;;; direct when-forms at compile time, in the style of config-directives-set.lisp's
;;; define-flag-mapping, but for arbitrary handler forms.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expand-flag-dispatch-arm (arm flags-var)
    "Expand one (FLAG-CHAR HANDLER-FORM) ARM into a when-form guarded by FLAG-CHAR."
    (destructuring-bind (flag-char handler-form) arm
      `(when (%flag-present-p ,flags-var ,flag-char)
         ,handler-form))))

(defmacro define-flag-dispatch ((flags-var) &rest arms)
  "Expand ARMS into a sequence of guarded handler forms."
  `(progn ,@(mapcar (lambda (arm) (%expand-flag-dispatch-arm arm flags-var)) arms)))

(defun %resize-pane-apply-absolute-dimensions (win pane flags)
  (define-flag-dispatch (flags)
    (#\x (let ((target-size (%parse-flag-int flags #\x)))
           (when target-size
             (%resize-pane-to-absolute-dimension win pane target-size
                                                 #'cl-tmux/model:pane-width :right))))
    (#\y (let ((target-size (%parse-flag-int flags #\y)))
           (when target-size
             (%resize-pane-to-absolute-dimension win pane target-size
                                                 #'cl-tmux/model:pane-height :down))))))

(defun %resize-pane-apply-relative-directions (flags win amount)
  (define-flag-dispatch (flags)
    (#\L (when win (resize-pane win :left amount)))
    (#\R (when win (resize-pane win :right amount)))
    (#\U (when win (resize-pane win :up amount)))
    (#\D (when win (resize-pane win :down amount)))))

(defun %cmd-resize-pane-arg (session args)
  "resize-pane [-t target] [-L|-R|-U|-D|-Z] [-x width] [-y height] [amount]: resize a pane.
   -t target: target pane by pane-id or 'session:window.pane' (default: active pane).
   -L/-R/-U/-D: resize by AMOUNT (default 5) in the given direction.
   -x N / -y N: resize to an ABSOLUTE width/height of N cells (computed as a delta
   from the pane's current size and applied via the :right/:down border move; both
   may be given together).
   -Z: zoom-toggle the target pane.
   -T: trim the rows below the cursor and pull rows out of the history to
       replace them (the cursor row becomes the bottom row).
   -M: begin a mouse resize — with an in-flight mouse event on a pane border,
       arms the same drag state the MouseDrag1Border path uses."
  (with-command-flags+pos (flags positionals args "txy")
    (let* ((amount-str (first positionals))
           (amount (or (and amount-str (%parse-integer-or-nil amount-str :junk-allowed t))
                       5))
           (x-val (%parse-flag-int flags #\x))
           (y-val (%parse-flag-int flags #\y))
           ;; Resolve target pane; fall back to active window for resize operations.
           (target-str (%flag-value flags #\t)))
      (with-target-context (target-session win pane session target-str)
        (declare (ignore target-session))
        (cond
          ((%flag-present-p flags #\Z)
           (when win (window-zoom-toggle win)))
          ;; -T: history-trim below the cursor on the target pane's screen.
          ((%flag-present-p flags #\T)
           (when pane
             (cl-tmux/terminal/actions:trim-below-cursor (pane-screen pane))
             (setf *dirty* t)))
          ;; -M: arm the border-drag state from the in-flight mouse event.
          ((%flag-present-p flags #\M)
           (when (and win *current-mouse-event*)
             (multiple-value-bind (split orient)
                 (%border-at-position win
                                      (getf *current-mouse-event* :col)
                                      (getf *current-mouse-event* :row))
               (when split
                 (setf *mouse-drag-state* (list split orient))))))
          ;; -x/-y: absolute resize. Move the relevant border by (target - current).
          ((or x-val y-val)
           (when (and win pane)
             (%resize-pane-apply-absolute-dimensions win pane flags)))
          ((some (lambda (flag-char) (%flag-present-p flags flag-char))
                 '(#\L #\R #\U #\D))
           (%resize-pane-apply-relative-directions flags win amount)))))))

(defun %cmd-join-pane-arg (session args)
  "join-pane / move-pane [-bdfhv] [-l size] [-s src-pane] [-t dst-pane]: move
   SRC-PANE out of its window and into DST-PANE's window as a new split.
   -h splits left/right; -v (the default, as for split-window) splits top/bottom.
   -b inserts the moved pane before/above the destination pane.
   -f makes the split span the full window dimension along the split axis.
   -l size sets the new pane size (cells or percentage, tmux split-window syntax).
   -s source pane (default: the active pane); -t destination pane, whose WINDOW
     receives the split (default: the active window).
   -d keeps the current pane active (no switch to the joined pane).
   No-op when source and destination resolve to the same window (nothing to move).
   This is the scriptable form; the interactive :join-pane / :move-pane keybindings
    (which prompt for a window index) are unchanged."
  (with-command-input (flags positionals args "stl"
                               :allowed-flags '(#\b #\d #\f #\h #\l #\s #\t #\v)
                               :max-positionals 0
                               :message "join-pane: unsupported argument")
    (declare (ignore positionals))
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((src-str (%flag-value flags #\s))
             (dst-str (%flag-value flags #\t))
             (dir (if (%flag-present-p flags #\h) :h :v))
             (before (%flag-present-p flags #\b))
             (full (%flag-present-p flags #\f))
             (size-str (%flag-value flags #\l))
             (size (and size-str (%parse-split-size size-str)))
             (src-win (if *server-marked-pane*
                          (pane-window *server-marked-pane*)
                          cur-win))
             (src-pane (or *server-marked-pane*
                           cur-pane))
             (dst-win cur-win))
        ;; -s: resolve the source pane (and its window). When the target names a
        ;; window but no pane, take THAT window's active pane.
        (multiple-value-setq (src-win src-pane)
          (%resolve-target-window-pane session src-str src-win src-pane))
        ;; -t: resolve the destination; only its WINDOW matters.
        (setf dst-win (multiple-value-bind (target-win target-pane)
                           (%resolve-target-window-pane session dst-str cur-win cur-pane)
                         (declare (ignore target-pane))
                         target-win))
        (when (and src-win src-pane dst-win (not (eq src-win dst-win))
                   (join-pane session src-win src-pane dst-win dir
                              :before before
                              :full full
                              :size size))
          ;; tmux makes the joined pane active unless -d.
          (unless (%flag-present-p flags #\d)
            (window-select-pane dst-win src-pane))
          (setf *dirty* t)
          t)))))

(defun %cmd-move-pane (session args)
  "move-pane uses the same scriptable argument surface as join-pane."
  (%cmd-join-pane-arg session args))

(defun %parse-break-pane-window-index (target)
  (when (and target (> (length target) 0))
    (let* ((colon (position #\: target))
           (dot (position #\. target :start (if colon (1+ colon) 0)))
           (start (cond
                    ((and (> (length target) 1) (char= (char target 0) #\@)) 1)
                    (colon (1+ colon))
                    (t 0)))
           (end (or dot (length target))))
      (when (< start end)
        (let ((index (%parse-integer-or-nil target :start start :end end
                                             :junk-allowed t)))
          (when (and index
                     (every #'digit-char-p (subseq target start end)))
            index))))))

(defun %break-pane-target-window-id (target-str target-win cur-win after before)
  (or (%parse-break-pane-window-index target-str)
      (and target-win (window-id target-win))
      (and (or after before) cur-win (window-id cur-win))))

(defun %resolve-break-pane-endpoints (session src-str target-str cur-win cur-pane after before)
  "Resolve the source pane/window and destination window id for break-pane."
  (multiple-value-bind (src-win src-pane)
      (%resolve-target-window-pane session src-str cur-win cur-pane)
    (let* ((target-win (and target-str
                            (nth-value 0
                                       (%resolve-target-window-pane
                                        session target-str cur-win cur-pane))))
           (target-id (%break-pane-target-window-id target-str target-win
                                                    cur-win after before)))
      (values src-win src-pane target-id))))

(defun %cmd-break-pane-arg (session args)
  "break-pane [-abdP] [-F format] [-n window-name] [-s src-pane] [-t dst-window]:
   move a pane out of its window into a new window of its own.
   -d: don't switch to the new window (stay on the current one).
   -a/-b: insert after/before the target window, shifting colliding ids upward.
   -P: print information about the new pane.
   -F format: print format for -P.
   -n name: name the new window (default: the shell basename).
   -s src-pane: the pane to break out (default: the active pane).
   -t dst-window: destination window index for the new window.
   No-op when the source window has fewer than two panes.  This is the scriptable
   form; the interactive :break-pane keybinding is unchanged."
  (with-command-input (flags positionals args "nstF"
                               :allowed-flags '(#\a #\b #\d #\F #\n #\P #\s #\t)
                               :max-positionals 0
                               :message "break-pane: unsupported argument")
      (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
        (let* ((detach   (%flag-present-p flags #\d))
               (after    (%flag-present-p flags #\a))
               (before   (%flag-present-p flags #\b))
               (print-p  (%flag-present-p flags #\P))
               (print-fmt (%flag-value flags #\F))
               (name     (%flag-value flags #\n))
               (src-str  (%flag-value flags #\s))
               (target-str (%flag-value flags #\t)))
          (multiple-value-bind (src-win src-pane target-id)
              (%resolve-break-pane-endpoints session src-str target-str
                                             cur-win cur-pane after before)
            (let ((new-win (cl-tmux/commands:break-pane
                            session :src-window src-win :pane src-pane
                                    :name name :select (not detach)
                                    :target-window-id target-id
                                    :insert-after after
                                    :insert-before before)))
              (when new-win
                (when print-p
                  (%show-pane-info-overlay session new-win src-pane print-fmt))
                (setf *dirty* t)
                t)))))))

(defun %cmd-clear-history-arg (session args)
  "clear-history [-H] [-t target-pane]: clear a pane's scrollback history.
   -t target-pane: the pane to clear (default: the active pane); a window-only
   target clears that window's active pane.
   -H: also clear the pane's hyperlinks (accepted; cl-tmux does not track
       hyperlinks separately, so clearing the scrollback already covers it).
   This is the scriptable form; the interactive :clear-history keybinding (active
   pane) is unchanged.  tmux args \"Ht:\"."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\H #\t)
                             :max-positionals 0
                             :message "clear-history: unsupported argument")
    (declare (ignore positionals))
    (let ((target-str (%flag-value flags #\t)))
      (with-target-context (target-session target-window pane session target-str)
        (declare (ignore target-session target-window))
        (when pane
          (cl-tmux/terminal/actions:clear-scrollback (pane-screen pane))
          (setf *dirty* t)
          t)))))

(defun %cmd-rotate-window-arg (session args)
  "rotate-window [-DU] [-t target-window]: rotate the pane order in a window.
   -U (the default) rotates forward (the first pane moves to the end); -D rotates
   backward.
   -t target-window: the window to rotate (default: the active window).
   This is the scriptable form; the interactive :rotate-window /
   :rotate-window-reverse bindings are unchanged."
  (with-command-input (flags positionals args "t"
                               :allowed-flags '(#\D #\U #\t)
                               :max-positionals 0
                               :message "rotate-window: unsupported argument")
    (declare (ignore positionals))
    (let ((target-str (%flag-value flags #\t))
          (dir (if (%flag-present-p flags #\D) :down :up)))
      (with-target-context (target-session win pane session target-str)
        (declare (ignore target-session pane))
        (when win
          (%pane-navigation-unzoom win)
          (window-rotate win dir)
          (setf *dirty* t)
          t)))))
