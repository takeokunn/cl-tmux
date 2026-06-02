(in-package #:cl-tmux/renderer)

;;;; Status bar composition for the cl-tmux renderer.
;;;;
;;;; This file owns the status bar: option lookup, format expansion, justify
;;;; logic, and the render-status-bar entry point.  It has no knowledge of
;;;; session-frame compositing; that lives in renderer-compose.lisp.
;;;;
;;;; Load order: renderer-format → renderer-style → renderer-pane
;;;;             → renderer-overlay → renderer-statusbar → renderer-compose

;;; ── Status bar data formatters (pure) ─────────────────────────────────────

(defun %status-current-time ()
  "HH:MM string from the system clock.
   Delegates to cl-tmux/format::%current-time-string — single source of truth."
  (cl-tmux/format::%current-time-string))

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

(defun %status-window-list-styled (session active-win)
  "Window-tab string with current-style applied to the active window entry.
   Uses window-status-format, window-status-current-format, window-status-separator,
   window-status-current-style, and window-status-style options."
  (let ((fmt-normal    (cl-tmux/options:get-option "window-status-format"
                                                   " #{window_index}:#{window_name} "))
        (fmt-current   (cl-tmux/options:get-option "window-status-current-format"
                                                   " #{window_index}:#{window_name}* "))
        (separator     (cl-tmux/options:get-option "window-status-separator" " "))
        (current-style (cl-tmux/options:get-option "window-status-current-style" ""))
        (normal-style  (cl-tmux/options:get-option "window-status-style" "")))
    (with-output-to-string (window-stream)
      (let ((first-p t))
        (dolist (window (session-windows session))
          (unless first-p (write-string separator window-stream))
          (setf first-p nil)
          (let* ((context  (cl-tmux/format:format-context-from-window session window))
                 (active-p (eq window active-win))
                 (style    (if active-p current-style normal-style))
                 (label    (cl-tmux/format:expand-format
                            (if active-p fmt-current fmt-normal)
                            context)))
            ;; Apply per-window style using %status-sgr-from-style for SGR conversion.
            (let ((sgr-code (when (and style (plusp (length style)))
                              (%status-sgr-from-style style))))
              (when sgr-code
                (format window-stream "~C[~Am" +esc+ sgr-code))
              (write-string label window-stream)
              (when sgr-code
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

(defun %status-bar-line (left time-str terminal-cols)
  "Assemble the full status bar string: LEFT text, gap, TIME-STR, truncated to TERMINAL-COLS."
  (let* ((gap  (max 0 (- terminal-cols (length left) (length time-str) 1)))
         (line (format nil "~A~A ~A" left (make-string gap :initial-element #\Space) time-str)))
    (subseq line 0 (min (length line) terminal-cols))))

(defun %status-format-or-default (opt-name context default-fn)
  "Return the expanded format string for OPT-NAME when it differs from its registered default;
   otherwise call DEFAULT-FN.  CONTEXT is the format-expansion plist."
  (let* ((spec    (gethash opt-name cl-tmux/options:*option-registry*))
         (default (when spec (cl-tmux/options:option-spec-default spec)))
         (current (cl-tmux/options:get-option opt-name nil)))
    (if (and current (not (equal current default)))
        (cl-tmux/format:expand-format current context)
        (funcall default-fn))))

(defun %clamp-status-segment (raw-text max-length)
  "Return RAW-TEXT truncated to at most MAX-LENGTH characters."
  (if (> (length raw-text) max-length)
      (subseq raw-text 0 max-length)
      raw-text))

;;; ── Status bar justify strategies (data layer) ───────────────────────────────
;;;
;;; define-justify-strategy is a Prolog-like fact table mapping a justify
;;; keyword string to a layout formula:
;;;   justify_strategy("right",  left, right-str, cols) :- right_formula(…).
;;;   justify_strategy("centre", left, right-str, cols) :- centre_formula(…).
;;;   justify_strategy(default,  left, right-str, cols) :- %status-bar-line(…).
;;;
;;; (Heterogeneous bodies — different formula per arm — so we use the
;;; table to dispatch to per-strategy helpers rather than inlining the bodies.)

(defun %justify-right (left right-str cols)
  "Layout formula for right-justify: place RIGHT-STR flush against the right edge."
  (let* ((gap  (max 0 (- cols (length left) (length right-str) 1)))
         (line (format nil "~A~A ~A" left
                       (make-string gap :initial-element #\Space)
                       right-str)))
    (subseq line 0 (min (length line) cols))))

(defun %justify-centre (left right-str cols)
  "Layout formula for centre-justify: pad before LEFT so the combined text is centred."
  (let* ((llen  (length left))
         (rlen  (length right-str))
         (total (+ llen 1 rlen))   ; 1 = the separator space before right-str
         (pad-l (max 0 (floor (- cols total) 2)))
         (gap   (max 0 (- cols llen pad-l 1 rlen)))
         (line  (format nil "~A~A~A ~A"
                        (make-string pad-l :initial-element #\Space)
                        left
                        (make-string gap :initial-element #\Space)
                        right-str)))
    (subseq line 0 (min (length line) cols))))

(defun %status-justify-line (left right-str cols justify)
  "Assemble the status bar according to JUSTIFY (\"left\" \"centre\" \"right\").
   COLS is the terminal width; result is truncated to COLS."
  (cond
    ((string-equal justify "right")  (%justify-right  left right-str cols))
    ((string-equal justify "centre") (%justify-centre left right-str cols))
    (t                               (%status-bar-line left right-str cols))))

;;; ── Status bar render entry point ────────────────────────────────────────────

(defun render-status-bar (stream session terminal-rows terminal-cols
                          &key (status-row (1- terminal-rows)))
  "Draw the status bar at STATUS-ROW with dynamic format string expansion.
   STATUS-ROW defaults to (1- TERMINAL-ROWS), i.e. the bottom row.
   Respects status-style, status-justify, status-left-length, status-right-length,
   and window-status-current-style options."
  (let* ((active-win  (session-active-window session))
         (active-pane (session-active-pane session))
         (context     (cl-tmux/format:format-context-from-session
                       session active-win active-pane))
         (raw-left    (if (prompt-active-p)
                          (prompt-text)
                          (%status-format-or-default
                           "status-left" context
                           (lambda () (%status-left-text session active-win active-pane)))))
         (raw-right   (%status-format-or-default
                       "status-right" context #'%status-current-time))
         (left        (%clamp-status-segment
                       raw-left (cl-tmux/options:get-option "status-left-length" 40)))
         (right-str   (%clamp-status-segment
                       raw-right (cl-tmux/options:get-option "status-right-length" 40)))
         (justify     (cl-tmux/options:get-option "status-justify" "left"))
         (line        (%status-justify-line left right-str terminal-cols justify))
         (sgr-code    (%status-sgr-from-style
                       (cl-tmux/options:get-option "status-style" ""))))
    (move-to stream status-row 0)
    (format stream "~C[~Am" +esc+ sgr-code)
    (write-string line stream)
    (reset-attrs stream)))
