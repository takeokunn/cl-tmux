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

(defun %window-status-style (session window active-p)
  "Resolve the status-bar style string for WINDOW's tab.
   Active window → window-status-current-style.  For a non-active window, the
   highest-priority non-empty alert style wins: bell > activity > last
   (previously active) > the normal window-status-style.  Every option is read
   per-window via get-option-for-context, so alert styles can be set per-window."
  (flet ((opt (name) (cl-tmux/options:get-option-for-context name :window window)))
    (if active-p
        (opt "window-status-current-style")
        (let ((bell-style     (opt "window-status-bell-style"))
              (activity-style (opt "window-status-activity-style"))
              (last-style     (opt "window-status-last-style"))
              (normal-style   (opt "window-status-style")))
          (cond
            ((and (%window-has-bell-p window) (plusp (length bell-style)))
             bell-style)
            ((and (window-activity-flag window) (plusp (length activity-style)))
             activity-style)
            ((and (eq window (session-last-window session))
                  (plusp (length last-style)))
             last-style)
            (t normal-style))))))

(defun %status-window-list-styled (session active-win)
  "Window-tab string with current-style applied to the active window entry.
   Uses window-status-format, window-status-current-format, window-status-separator,
   window-status-current-style, window-status-style, and the alert-state styles
   (window-status-{bell,activity,last}-style).
   The format/style options are resolved PER WINDOW via get-option-for-context
   (pane→window→global→default), so e.g. `setw -t :2 window-status-current-style
   fg=red` styles only that window's tab.  A non-active window with a pending bell,
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
                (format window-stream "~C[~Am" +esc+ sgr-code))
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

;;; ── SGR-aware length / truncation (inline #[attr] support) ───────────────────
;;;
;;; tmux status strings may embed CSI SGR sequences — both from window-status
;;; styling and from inline #[fg=…] blocks (expanded below).  Those sequences are
;;; zero-width on screen, so gap math and width clamping must count VISIBLE cells,
;;; not raw characters.  For escape-free strings these reduce exactly to
;;; LENGTH / SUBSEQ, so every existing alignment test is unaffected.

(defun %sgr-sequence-end (str start)
  "If STR has a CSI escape starting at START, return the index just past its final byte.
   Otherwise returns NIL.

   CSI encoding: ESC (0x1B) '[' (0x5B) <parameter-bytes 0x30–0x3F>*
                 <intermediate-bytes 0x20–0x2F>* <final-byte 0x40–0x7E>.
   The function skips all bytes until the first final-byte or end of string,
   returning (1+ final-byte-index) on success, or LEN when the sequence is
   unterminated.  Callers should treat an unterminated sequence as consuming
   the rest of the string."
  (let ((len (length str)))
    (when (and (< (1+ start) len)
               (char= (char str start) +esc+)
               (char= (char str (1+ start)) #\[))
      (let ((j (+ start 2)))
        (loop while (and (< j len)
                         (not (<= #x40 (char-code (char str j)) #x7e)))
              do (incf j))
        (if (< j len) (1+ j) len)))))

(defun %visible-length (str)
  "Number of visible cells in STR, skipping CSI SGR escape sequences.
   Equals (LENGTH STR) for strings with no escape sequences."
  (let ((n 0) (i 0) (len (length str)))
    (loop while (< i len)
          for esc-end = (%sgr-sequence-end str i)
          do (if esc-end
                 (setf i esc-end)
                 (progn (incf n) (incf i))))
    n))

(defun %visible-truncate (str n)
  "Prefix of STR holding at most N visible cells; CSI escape sequences are copied
   through without counting toward N.  Equals (SUBSEQ STR 0 (MIN N (LENGTH STR)))
   for escape-free strings."
  (if (>= n (%visible-length str))
      str
      (with-output-to-string (out)
        (let ((seen 0) (i 0) (len (length str)))
          (loop while (and (< i len) (< seen n))
                for esc-end = (%sgr-sequence-end str i)
                do (if esc-end
                       (progn (write-string str out :start i :end esc-end)
                              (setf i esc-end))
                       (progn (write-char (char str i) out)
                              (incf seen)
                              (incf i))))))))

(defun %status-style-block-sgr (body base-sgr)
  "SGR escape string for one inline #[BODY] status block.
   An empty / \"default\" / \"none\" BODY resets to BASE-SGR (reset + base attrs);
   any other BODY is parsed as a tmux style string (e.g. \"fg=green,bold\")."
  (let ((b (string-trim " " body)))
    (if (or (string= b "")
            (string-equal b "default")
            (string-equal b "none"))
        (format nil "~C[0;~Am" +esc+ base-sgr)
        (format nil "~C[~Am" +esc+ (%status-sgr-from-style b)))))

(defun %status-expand-style-blocks (str base-sgr)
  "Replace tmux inline #[…] style blocks in STR with CSI SGR escape sequences.
   #[fg=green,bold] → ESC[1;32m ; #[default] → reset to BASE-SGR.  Returns STR
   unchanged when it contains no #[ block, so default/format paths are untouched."
  (if (not (search "#[" str))
      str
      (with-output-to-string (out)
        (let ((i 0) (len (length str)))
          (loop while (< i len)
                do (if (and (char= (char str i) #\#)
                            (< (1+ i) len)
                            (char= (char str (1+ i)) #\[))
                       (let ((close (position #\] str :start (+ i 2))))
                         (if close
                             (progn
                               (write-string
                                (%status-style-block-sgr (subseq str (+ i 2) close) base-sgr)
                                out)
                               (setf i (1+ close)))
                             (progn (write-char (char str i) out) (incf i))))
                       (progn (write-char (char str i) out) (incf i))))))))

(defun %status-bar-line (left time-str terminal-cols)
  "Assemble the full status bar string: LEFT text, gap, TIME-STR, truncated to TERMINAL-COLS."
  (let* ((gap  (max 0 (- terminal-cols (%visible-length left) (%visible-length time-str) 1)))
         (line (format nil "~A~A ~A" left (make-string gap :initial-element #\Space) time-str)))
    (%visible-truncate line terminal-cols)))

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
  "Return RAW-TEXT truncated to at most MAX-LENGTH visible cells.
   CSI SGR sequences (from inline #[attr] blocks) do not count toward the limit."
  (if (> (%visible-length raw-text) max-length)
      (%visible-truncate raw-text max-length)
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
  (let* ((gap  (max 0 (- cols (%visible-length left) (%visible-length right-str) 1)))
         (line (format nil "~A~A ~A" left
                       (make-string gap :initial-element #\Space)
                       right-str)))
    (%visible-truncate line cols)))

(defun %justify-centre (left right-str cols)
  "Layout formula for centre-justify: pad before LEFT so the combined text is centred."
  (let* ((llen  (%visible-length left))
         (rlen  (%visible-length right-str))
         (total (+ llen 1 rlen))   ; 1 = the separator space before right-str
         (pad-l (max 0 (floor (- cols total) 2)))
         (gap   (max 0 (- cols llen pad-l 1 rlen)))
         (line  (format nil "~A~A~A ~A"
                        (make-string pad-l :initial-element #\Space)
                        left
                        (make-string gap :initial-element #\Space)
                        right-str)))
    (%visible-truncate line cols)))

(defun %status-justify-line (left right-str cols justify)
  "Assemble the status bar according to JUSTIFY (\"left\" \"centre\" \"right\").
   COLS is the terminal width; result is truncated to COLS."
  (cond
    ((string-equal justify "right")  (%justify-right  left right-str cols))
    ((string-equal justify "centre") (%justify-centre left right-str cols))
    (t                               (%status-bar-line left right-str cols))))

;;; ── Status bar render entry point ────────────────────────────────────────────

(defun %status-segment-style-sgr (option-name base-sgr)
  "SGR parameter string for a status-segment style OPTION-NAME (status-left-style /
   status-right-style), falling back to BASE-SGR (the status-style) when the option
   is unset or \"default\"."
  (let ((s (cl-tmux/options:get-option option-name "")))
    (if (or (string= s "") (string-equal s "default"))
        base-sgr
        (%status-sgr-from-style s))))

(defun %apply-segment-style (text seg-sgr base-sgr)
  "Wrap a status-bar segment TEXT in its SEG-SGR style, reverting to BASE-SGR after
   (so inter-segment padding keeps the base status style).  Returns TEXT unchanged
   when SEG-SGR = BASE-SGR.  The wrapping SGR has zero visible length, so it does
   not affect the justify padding (which uses %visible-length)."
  (if (string= seg-sgr base-sgr)
      text
      (format nil "~C[~Am~A~C[~Am" +esc+ seg-sgr text +esc+ base-sgr)))

;;; ── #[align=…] regions + status-format[0] template path ─────────────────────
;;;
;;; tmux's status line is a single format whose #[align=left|centre|right] blocks
;;; divide it into three regions positioned within the terminal width.  cl-tmux
;;; normally renders the bar procedurally (status-left + window-list + status-
;;; right); when status-format[0] is SET it instead expands that template and
;;; composes the regions here.  The procedural default path is unchanged.

(defun %split-align-attr (body)
  "Parse a #[BODY] block's comma-separated attrs.  Returns (values ALIGN REST):
   ALIGN is :left/:centre/:right when an align=… attr is present (else NIL), and
   REST is the remaining attrs re-joined by commas (NIL when none) so combined
   blocks like #[align=right,fg=red] keep their colour."
  (let ((align nil) (rest nil))
    (dolist (a (uiop:split-string body :separator '(#\,)))
      (let ((at (string-trim " " a)))
        (cond
          ((member at '("align=left" "align=l")   :test #'string-equal) (setf align :left))
          ((member at '("align=centre" "align=center" "align=c") :test #'string-equal)
           (setf align :centre))
          ((member at '("align=right" "align=r")  :test #'string-equal) (setf align :right))
          ((plusp (length at)) (push at rest)))))
    (values align (when rest (format nil "~{~A~^,~}" (nreverse rest))))))

(defun %status-align-buckets (raw)
  "Split RAW (a status format) into (values LEFT CENTRE RIGHT) raw substrings by
   its #[align=…] markers.  Text before any marker is LEFT; a combined block's
   non-align attrs are re-emitted as a #[…] prefix so colour is preserved."
  (let ((buckets (list :left (make-string-output-stream)
                       :centre (make-string-output-stream)
                       :right  (make-string-output-stream)))
        (current :left) (i 0) (len (length raw)))
    (flet ((out () (getf buckets current)))
      (loop while (< i len) do
        (if (and (char= (char raw i) #\#) (< (1+ i) len) (char= (char raw (1+ i)) #\[))
            (let ((close (position #\] raw :start (+ i 2))))
              (if close
                  (multiple-value-bind (align rest) (%split-align-attr (subseq raw (+ i 2) close))
                    (cond
                      (align (setf current align)
                             (when rest (format (out) "#[~A]" rest)))
                      (t (write-string (subseq raw i (1+ close)) (out))))
                    (setf i (1+ close)))
                  (progn (write-char (char raw i) (out)) (incf i))))
            (progn (write-char (char raw i) (out)) (incf i)))))
    (values (get-output-stream-string (getf buckets :left))
            (get-output-stream-string (getf buckets :centre))
            (get-output-stream-string (getf buckets :right)))))

(defun %compose-aligned-line (raw base-sgr cols)
  "Render a status format RAW with #[align=…] regions into a COLS-wide line:
   the left region starts at column 0, the right region ends at COLS, and the
   centre region is centred.  Regions carry their own #[…] colours (reset to
   BASE-SGR after each).  Overlapping content is truncated to COLS."
  (multiple-value-bind (lraw craw rraw) (%status-align-buckets raw)
    (let* ((reset (format nil "~C[~Am" +esc+ base-sgr))
           (l (if (plusp (length lraw)) (concatenate 'string (%status-expand-style-blocks lraw base-sgr) reset) ""))
           (c (if (plusp (length craw)) (concatenate 'string (%status-expand-style-blocks craw base-sgr) reset) ""))
           (r (if (plusp (length rraw)) (concatenate 'string (%status-expand-style-blocks rraw base-sgr) reset) ""))
           (lw (%visible-length l)) (cw (%visible-length c)) (rw (%visible-length r)))
      (%visible-truncate
       (with-output-to-string (out)
         (let ((col 0))
           (flet ((pad-to (target)
                    (when (> target col)
                      (dotimes (_ (- target col)) (write-char #\Space out))
                      (setf col target))))
             (write-string l out) (incf col lw)
             (when (plusp cw)
               (pad-to (max col (floor (- cols cw) 2)))
               (when (< col cols) (write-string c out) (incf col cw)))
             (when (plusp rw)
               (pad-to (max col (- cols rw)))
               (when (< col cols) (write-string r out) (incf col rw)))
             (pad-to cols))))
       cols))))

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
         (sgr-code    (%status-sgr-from-style
                       (%effective-status-style)))
         (status-fmt0 (cl-tmux/options:get-option "status-format[0]" "")))
    ;; status-format[0] template path: when SET (and no prompt is active) the bar
    ;; is rendered from that single format, with #[align=…] regions positioned by
    ;; %compose-aligned-line and #{W:…}/#{…} expanded.  Otherwise fall through to
    ;; the procedural status-left + window-list + status-right path below.
    (when (and (stringp status-fmt0) (plusp (length status-fmt0))
               (not (prompt-active-p)))
      (let ((line (%compose-aligned-line
                   (handler-case (cl-tmux/format:expand-format status-fmt0 context)
                     (error () status-fmt0))
                   sgr-code terminal-cols)))
        (move-to stream status-row 0)
        (format stream "~C[~Am" +esc+ sgr-code)
        (write-string line stream)
        (reset-attrs stream))
      (return-from render-status-bar)))
  (let* ((active-win  (session-active-window session))
         (active-pane (session-active-pane session))
         (context     (cl-tmux/format:format-context-from-session
                       session active-win active-pane
                       :client-width  terminal-cols
                       :client-height (max 0 (- terminal-rows 1))))
         (sgr-code    (%status-sgr-from-style
                       (%effective-status-style)))
         ;; Expand inline #[attr] style blocks into SGR escapes; #[default]
         ;; reverts to SGR-CODE (the base status style) so the bar's bg/fg returns.
         (raw-left    (%status-expand-style-blocks
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
         ;; Per-segment styles: status-left-style / status-right-style override the
         ;; base status-style for the left/right text (falling back to it).
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
         (justify     (cl-tmux/options:get-option "status-justify" "left"))
         (line        (%status-justify-line left right-str terminal-cols justify)))
    (move-to stream status-row 0)
    (format stream "~C[~Am" +esc+ sgr-code)
    (write-string line stream)
    (reset-attrs stream)))

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
    (move-to stream row 0)
    (format stream "~C[~Am" +esc+ sgr-code)
    (write-string line stream)
    (reset-attrs stream)))

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
  (let* ((bottom-p (not (equal position "top")))
         (main-row (if bottom-p (1- terminal-rows) 0)))
    (render-status-bar stream session terminal-rows terminal-cols
                       :status-row main-row)
    (loop for index from 1 below lines
          for row = (if bottom-p (- main-row index) (+ main-row index))
          do (render-extra-status-line stream session terminal-cols row index))))
