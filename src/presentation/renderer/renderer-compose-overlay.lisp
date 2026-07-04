(in-package #:cl-tmux/renderer)

;;;; Overlay and mouse-mode composition for the cl-tmux renderer.
;;;;
;;;; This file owns the light-weight overlay text renderer and the mouse-mode
;;;; escape-sequence dispatch table used by renderer-compose.lisp.

;;; ── Overlay (list-keys help) ────────────────────────────────────────────────

(defun %message-line-row (rows)
  "The terminal row a single-line message occupies.  tmux draws messages over
   the status area — the bottom rows by default, the top rows when
   status-position is top — with message-line (0..4) selecting the line within
   a multi-line status bar (clamped to the status height)."
  (let* ((height (max 1 cl-tmux/config:*status-height*))
         (raw    (cl-tmux/options:get-option "message-line" 0))
         (line   (if (integerp raw) (min (max raw 0) (1- height)) 0))
         (top-p  (string-equal
                  (cl-tmux/options:get-option "status-position" "bottom")
                  "top")))
    (if top-p
        line
        (max 0 (+ (- rows height) line)))))

(defun render-overlay (stream cols rows)
  "Draw the active overlay.
   A SINGLE-line overlay is a message: it is drawn over
   the status area on the message-line row (tmux message placement), padded to
   the full width.  Multi-line overlays are pagers (list-keys, show-options
   output) and draw over the top rows.
   Applies the message-style option (or message-command-style when a prompt is
   active) so overlays respect the user's colour scheme."
  (let* ((style-opt (if (prompt-active-p)
                        (cl-tmux/options:get-option "message-command-style" "")
                        (cl-tmux/options:get-option "message-style" "")))
         (sgr-code  (when (and style-opt (plusp (length style-opt)))
                      (%status-sgr-from-style style-opt))))
    (if sgr-code
        (%emit-sgr stream sgr-code)
        (reset-attrs stream)))
  (let ((lines (overlay-lines)))
    (if (= (length lines) 1)
        ;; Message: one line over the status area, padded to clear the row.
        (let* ((line (first lines))
               (shown (subseq line 0 (min (length line) cols))))
          (move-to stream (%message-line-row rows) 0)
          (write-string shown stream)
          (loop repeat (- cols (length shown)) do (write-char #\Space stream)))
        ;; Pager: draw over the top rows.
        (loop for line in lines
              for row from 0
              do (move-to stream row 0)
                 (write-string (subseq line 0 (min (length line) cols)) stream)))))

;;; ── Overlay layer selection ────────────────────────────────────────────────

(defun %render-overlay-layer (buffer active-pane terminal-rows terminal-cols)
  "Render the active overlay layer (popup > menu > overlay > cursor) into BUFFER."
  (cond
    (*active-popup*
     (render-popup buffer *active-popup* terminal-rows terminal-cols))
    (*active-menu*
     (render-menu buffer *active-menu* terminal-rows terminal-cols))
    ((overlay-active-p)
     (render-overlay buffer terminal-cols terminal-rows))
    (t
     (when active-pane
       (let ((screen (pane-screen active-pane)))
         (with-lock-held ((screen-lock screen))
           (move-to buffer
                    (+ (pane-y active-pane) (screen-cursor-y screen))
                    (+ (pane-x active-pane) (screen-cursor-x screen)))))))))

;;; ── Mouse-mode DEC private mode dispatch table ──────────────────────────────
;;;
;;; define-mouse-mode-sequence maps a screen-mouse-mode integer to the
;;; DEC private mode number to enable:
;;;   mouse_mode(1) → ?1000h  (X10: press only)
;;;   mouse_mode(2) → ?1002h  (button-event: press + release + held motion)
;;;   mouse_mode(3) → ?1003h  (any-event: all mouse motion, and default fallback)
;;;
;;; Pattern matches define-csi-rules style: one declarative rule per mode.

(defmacro define-mouse-mode-sequence (&rest rules)
  "Build %MOUSE-MODE-DEC-NUMBER from a declarative (mode-integer dec-mode-number) table.
   The last entry's dec-mode-number is the default for any unmatched mode > 0."
  (let ((default-dec (second (car (last rules))))
        (explicit-rules (butlast rules)))
    `(defun %mouse-mode-dec-number (mode-integer)
       "Return the DEC private mode number for the given SCREEN-MOUSE-MODE integer."
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (mode-val dec-num) rule
                       `((= mode-integer ,mode-val) ,dec-num)))
                   explicit-rules)
         (t ,default-dec)))))

(define-mouse-mode-sequence
  (1 1000)   ; X10 basic mouse tracking (press only)
  (2 1002)   ; button-event tracking (press + release + motion while held)
  (3 1003))  ; any-event tracking (all motion) — also default fallback

(defun %render-mouse-sequences (stream active-pane)
  "Emit mouse-tracking mode sequences according to session and pane settings.
   When the session 'mouse' option is enabled, emit SGR + button-event sequences.
   Otherwise honour ACTIVE-PANE's screen-mouse-mode (X10/button-event/any-event)."
  (let ((session-mouse (cl-tmux/options:get-option "mouse")))
    (if session-mouse
        (progn
          (format stream "~C[?1006h" +esc+)
          (format stream "~C[?1002h" +esc+))
        (when active-pane
          (let* ((screen     (pane-screen active-pane))
                 (mouse-mode (screen-mouse-mode screen))
                 (sgr-mode   (screen-mouse-sgr-mode screen)))
            (when (> mouse-mode 0)
              (format stream "~C[?~Dh" +esc+ (%mouse-mode-dec-number mouse-mode))
              (when sgr-mode (format stream "~C[?1006h" +esc+))))))))
