(in-package #:cl-tmux/renderer)

;;;; Status bar composition for the cl-tmux renderer.
;;;;
;;;; This file owns the status bar: option lookup, format expansion, justify
;;;; logic, and the render-status-bar entry point.  It has no knowledge of
;;;; session-frame compositing; that lives in renderer-compose.lisp.
;;;;
;;;; Load order: renderer-format → renderer-style → renderer-pane
;;;;             → renderer-overlay → renderer-statusbar
;;;;             → renderer-compose

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((source (or *load-truename* *compile-file-truename*))
         (base (and source
                    (make-pathname :name nil :type nil :defaults source))))
    (dolist (name '("renderer-statusbar-layout.lisp"))
      (let ((path (and base (merge-pathnames name base))))
        (when (and path (probe-file path))
          (load path))))))

;;; ── Status bar data formatters (pure) ─────────────────────────────────────

(defun %status-current-time ()
  "Return current time as a HH:MM string (5 characters)."
  (multiple-value-bind (_ min hour) (get-decoded-time)
    (declare (ignore _))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %status-pane-indicator (active-pane)
  "Pane-number string for the status bar, or empty string when ACTIVE-PANE is NIL."
  (if active-pane (format nil " #~D" (pane-id active-pane)) ""))

(defun %status-copy-indicator (active-pane)
  "Copy-mode scroll offset string, or empty string.
   Returns non-empty only when ACTIVE-PANE is in copy mode with a positive offset."
  (if (and active-pane
           (screen-copy-mode-p (pane-screen active-pane))
           (> (screen-copy-offset (pane-screen active-pane)) 0))
      (format nil " [COPY +~D]" (screen-copy-offset (pane-screen active-pane)))
      ""))

(defun %window-has-bell-p (window)
  "T when any pane in WINDOW has a pending (unconsumed) BEL.
   Mirrors the #{window_bell_flag} computation in format.lisp."
  (some (lambda (p)
          (let ((scr (pane-screen p)))
            (and scr (screen-bell-pending scr))))
        (window-panes window)))

(defun %window-option (window name)
  "Read NAME from WINDOW's option context."
  (cl-tmux/options:get-option-for-context name :window window))

(defun %status-bucket-stream (buckets current)
  "Return the active output stream for CURRENT in BUCKETS."
  (getf buckets current))

(defun %window-status-style (session window active-p)
  "Resolve the status-bar style string for WINDOW's tab.
   Active window → window-status-current-style.  For a non-active window, the
   highest-priority non-empty alert style wins: bell > activity > last
   (previously active) > the normal window-status-style.  Every option is read
   per-window via get-option-for-context, so alert styles can be set per-window."
  (if active-p
      (%window-option window "window-status-current-style")
      (let ((bell-style     (%window-option window "window-status-bell-style"))
            (activity-style (%window-option window "window-status-activity-style"))
            (last-style     (%window-option window "window-status-last-style"))
            (normal-style   (%window-option window "window-status-style")))
        (cond
          ((and (%window-has-bell-p window) (plusp (length bell-style)))
           bell-style)
          ((and (window-activity-flag window) (plusp (length activity-style)))
           activity-style)
          ((and (eq window (session-last-window session))
                (plusp (length last-style)))
           last-style)
          (t normal-style)))))

(defun %status-window-list-styled (session active-win)
  "Window-tab string with current-style applied to the active window entry.
   Uses window-status-format, window-status-current-format, window-status-separator,
   window-status-current-style, window-status-style, and the alert-state styles
   (window-status-{bell,activity,last}-style).
   The format/style options are resolved PER WINDOW via get-option-for-context
   (pane→window→global→default), so e.g. `set-window-option -t :2
   window-status-current-style fg=red` styles only that window's tab.  A non-active window with a pending bell,
   unseen activity, or that is the last (previously active) window picks up the
   corresponding alert style (bell > activity > last > normal).
   window-status-separator stays global — it sits between windows and has no
   single owning window."
  (let ((separator (cl-tmux/options:get-option "window-status-separator" " ")))
    (with-output-to-string (window-stream)
      (let ((first-p t))
        (dolist (window (session-windows session))
          (unless first-p (write-string separator window-stream))
          (setf first-p nil)
          (let* ((context  (cl-tmux/format:format-context-from-window session window))
                 (active-p (eq window active-win))
                 (fmt      (cl-tmux/options:get-option-for-context
                            (if active-p "window-status-current-format" "window-status-format")
                            :window window))
                 ;; Style honors alert state (bell/activity/last) for non-active windows.
                 (style    (%window-status-style session window active-p))
                 (label    (cl-tmux/format:expand-format fmt context)))
            ;; Apply the per-window style, then expand any inline #[attr] blocks
            ;; embedded in the label.  Within a window label, #[default] reverts to
            ;; the window's own style (or the status default when it is unstyled).
            ;; STYLED-P is true when we emitted a wrapper SGR or the label injected
            ;; one, so the trailing reset keeps colour from bleeding into the
            ;; separator / next window.
            (let* ((sgr-code (when (and style (plusp (length style)))
                               (%status-sgr-from-style style)))
                   (expanded (%status-expand-style-blocks
                              label (or sgr-code +sgr-default-status+)))
                   (styled-p (or sgr-code (not (eq expanded label)))))
              (when sgr-code
                (%emit-sgr window-stream sgr-code))
              (write-string expanded window-stream)
              (when styled-p
                (reset-attrs window-stream)))))))))

(defun %status-left-text (session active-win active-pane)
  "Left portion of the status bar: prompt text or session/window/pane info.
   Uses %status-window-list-styled so per-window style options take effect."
  (if (prompt-active-p)
      (prompt-text)
      (format nil " ~A~A~A~A"
              (session-name session)
              (%status-window-list-styled session active-win)
              (%status-pane-indicator active-pane)
              (%status-copy-indicator active-pane))))

(defun %render-status-line (stream status-row sgr-code line)
  "Emit a fully-composed status LINE at STATUS-ROW, wrapped in SGR-CODE, then reset."
  (move-to stream status-row 0)
  (%emit-sgr stream sgr-code)
  (write-string line stream)
  (reset-attrs stream))

(defun render-status-bar (stream session terminal-rows terminal-cols
                          &key (status-row (1- terminal-rows)))
  "Draw the status bar at STATUS-ROW with dynamic format string expansion.
   STATUS-ROW defaults to (1- TERMINAL-ROWS), i.e. the bottom row.
   Respects status-style, status-justify, status-left-length, status-right-length,
   and window-status-current-style options."
  (let* ((active-win  (session-active-window session))
         (active-pane (session-active-pane session))
         ;; Pass terminal dimensions so #{client_width} / #{client_height} work
         ;; in status-left, status-right, and window-status-format strings.
         (context     (cl-tmux/format:format-context-from-session
                       session active-win active-pane
                       :client-width  terminal-cols
                       :client-height (max 0 (- terminal-rows 1))))
         (sgr-code    (%status-sgr-from-style (%effective-status-style)))
         (status-fmt0 (cl-tmux/options:get-option "status-format[0]" "")))
    ;; status-format[0] template path: when SET (and no prompt is active) the bar
    ;; is rendered from that single format, with #[align=…] regions positioned by
    ;; %compose-aligned-line and #{W:…}/#{…} expanded.  Procedural path follows.
    (cond
      ((and (stringp status-fmt0) (plusp (length status-fmt0)) (not (prompt-active-p)))
       (%render-status-line stream status-row sgr-code
                            (%compose-aligned-line
                             (handler-case (cl-tmux/format:expand-format status-fmt0 context)
                               (error () status-fmt0))
                             sgr-code terminal-cols)))
      (t
       ;; Expand inline #[attr] style blocks into SGR escapes; #[default] reverts to
       ;; SGR-CODE (the base status style) so the bar's bg/fg returns between segments.
       (let* ((raw-left    (%status-expand-style-blocks
                            (if (prompt-active-p)
                                (prompt-text)
                                (%status-format-or-default
                                 "status-left" context
                                 (lambda () (%status-left-text session active-win active-pane))))
                            sgr-code))
              (raw-right   (%status-expand-style-blocks
                            (%status-format-or-default
                             "status-right" context #'cl-tmux/format::%current-time-string)
                            sgr-code))
              (left-style-sgr  (%status-segment-style-sgr "status-left-style"  sgr-code))
              (right-style-sgr (%status-segment-style-sgr "status-right-style" sgr-code))
              (left        (%apply-segment-style
                            (%clamp-status-segment
                             raw-left (cl-tmux/options:get-option "status-left-length" 40))
                            left-style-sgr sgr-code))
              (right-str   (%apply-segment-style
                            (%clamp-status-segment
                             raw-right (cl-tmux/options:get-option "status-right-length" 40))
                            right-style-sgr sgr-code))
              (justify     (cl-tmux/options:get-option "status-justify" "left")))
         (%render-status-line stream status-row sgr-code
                              (%status-justify-line left right-str terminal-cols justify)))))))

(defun render-extra-status-line (stream session terminal-cols row index)
  "Render the INDEX-th extra status line (INDEX >= 1) at ROW from the option
   status-format[INDEX], expanded against SESSION's format context and padded to
   TERMINAL-COLS with the base status style.  An unset/blank status-format[INDEX]
   draws a blank styled row (which is still required, since the pane area has
   shrunk to leave this row to the status region)."
  (let* ((fmt      (cl-tmux/options:get-option
                    (format nil "status-format[~D]" index) ""))
         (sgr-code (%status-sgr-from-style
                    (%effective-status-style)))
         (context  (cl-tmux/format:format-context-from-session
                    session (session-active-window session)
                    (session-active-pane session)
                    :client-width terminal-cols))
         ;; Expand #{...} (leaving #[...] markers intact) then compose via the
         ;; same align-aware path as status-format[0], so #[align=right]/#[align=
         ;; centre] work in the extra rows too.  An empty format composes to a
         ;; blank styled row.
         (expanded (if (and (stringp fmt) (plusp (length fmt)))
                       (handler-case (cl-tmux/format:expand-format fmt context)
                         (error () fmt))
                       ""))
         (line     (%compose-aligned-line expanded sgr-code terminal-cols)))
    (%render-status-line stream row sgr-code line)))

(defun status-line-count ()
  "Number of status rows requested by the `status` option, 0..5.
   off/false/0/nil → 0; an explicit positive integer N → min(N,5) (tmux caps at
   5); any other truthy value (on/t) → 1.  This is the renderer's source of truth
   for how many status rows to draw; the pane layout reserves the matching count
   via cl-tmux/config:*status-height* (kept in sync by the `status` side-effect)."
  (let ((v (cl-tmux/options:get-option "status" t)))
    (cond
      ((null v) 0)
      ((integerp v) (max 0 (min v 5)))
      ((stringp v)
       (cond
         ((member v '("off" "false" "0") :test #'equal) 0)
         (t (let ((n (parse-integer v :junk-allowed t)))
              (cond ((and n (> n 0)) (min n 5))
                    (n 0)        ; parsed to <= 0
                    (t 1))))))   ; non-numeric truthy string (e.g. "on")
      (t 1))))                   ; T or any other truthy value

(defun render-status-region (stream session terminal-rows terminal-cols lines position)
  "Render a LINES-row status region.  The main bar (status-left, the window
   list, and status-right) is drawn on the outer edge — the bottom-most row when
   POSITION is \"bottom\" (the default), the top-most row when \"top\" — matching
   the single-line layout.  Additional rows render status-format[1..LINES-1]
   stacked inward from the main bar."
  (let* ((bottom-p (string/= position "top"))
         (main-row (if bottom-p (1- terminal-rows) 0)))
    (render-status-bar stream session terminal-rows terminal-cols
                       :status-row main-row)
    (loop for index from 1 below lines
          for row = (if bottom-p (- main-row index) (+ main-row index))
          do (render-extra-status-line stream session terminal-cols row index))))
