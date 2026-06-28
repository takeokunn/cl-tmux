(in-package #:cl-tmux/renderer)

;;;; Status bar layout helpers.
;;;;
;;;; This file holds the SGR-aware width math, justify strategies, and aligned
;;;; segment composition used by renderer-statusbar.lisp.

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
    (if (member b '("" "default" "none") :test #'string-equal)
        (format nil "~C[0;~Am" +esc+ base-sgr)
        (format nil "~C[~Am" +esc+ (%status-sgr-from-style b)))))

(defun %status-expand-style-blocks (str base-sgr)
  "Replace tmux inline #[…] style blocks in STR with CSI SGR escape sequences.
   #[fg=green,bold] → ESC[1;32m ; #[default] → reset to BASE-SGR.  Returns STR
   unchanged when it contains no #[ block, so default/format paths are untouched."
  (if (search "#[" str)
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
                       (progn (write-char (char str i) out) (incf i))))))
      str))

(defun %status-format-or-default (opt-name context default-fn)
  "Return the expanded format string for OPT-NAME when it differs from its registered default;
   otherwise call DEFAULT-FN.  CONTEXT is the format-expansion plist."
  (let* ((spec    (gethash opt-name cl-tmux/options:*option-registry*))
         (default (when spec (cl-tmux/options:option-spec-default spec)))
         (current (cl-tmux/options:get-option opt-name nil)))
    (if (and current (not (equal current default)))
        (cl-tmux/format:expand-format current context)
        (funcall default-fn))))

(defun %status-segment-limit (max-length)
  "Return a sane visible-length limit for status segment truncation.
   Missing or malformed values fall back to the tmux default of 40 cells."
  (if (numberp max-length)
      (max 0 (truncate max-length))
      40))

(defun %clamp-status-segment (raw-text max-length)
  "Return RAW-TEXT truncated to at most MAX-LENGTH visible cells.
   CSI SGR sequences (from inline #[attr] blocks) do not count toward the limit."
  (let ((limit (%status-segment-limit max-length)))
    (if (> (%visible-length raw-text) limit)
        (%visible-truncate raw-text limit)
        raw-text)))

(defun %split-comma-attrs (body)
  "Split BODY on commas and preserve empty fields."
  (let ((parts nil)
        (start 0))
    (loop for pos = (position #\, body :start start)
          do (push (subseq body start pos) parts)
          if pos do (setf start (1+ pos))
          else do (return (nreverse parts)))))

;;; ── Status bar justify strategies (data layer) ───────────────────────────────
;;;
;;; define-justify-strategy is a Prolog-like fact table mapping a justify
;;; keyword string to a layout formula:
;;;   justify_strategy("right",  left, right-str, cols) :- %justify-right(…).
;;;   justify_strategy("centre", left, right-str, cols) :- %justify-centre(…).
;;;   justify_strategy(default,  left, right-str, cols) :- %justify-right(…).
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
         (pad-l (%center-coord cols total))
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
  (if (string-equal justify "centre")
      (%justify-centre left right-str cols)
      (%justify-right  left right-str cols)))

(defun %status-segment-style-sgr (option-name base-sgr)
  "SGR parameter string for a status-segment style OPTION-NAME (status-left-style /
   status-right-style), falling back to BASE-SGR (the status-style) when the option
   is unset or \"default\"."
  (let ((s (cl-tmux/options:get-option option-name "")))
    (if (member s '("" "default") :test #'string-equal)
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
    (dolist (a (%split-comma-attrs body))
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
    (loop while (< i len) do
      (if (and (char= (char raw i) #\#) (< (1+ i) len) (char= (char raw (1+ i)) #\[))
          (let ((close (position #\] raw :start (+ i 2))))
            (if close
                (multiple-value-bind (align rest) (%split-align-attr (subseq raw (+ i 2) close))
                  (cond
                    (align (setf current align)
                           (when rest (format (getf buckets current) "#[~A]" rest)))
                    (t (write-string (subseq raw i (1+ close))
                                     (getf buckets current))))
                  (setf i (1+ close)))
                (progn (write-char (char raw i) (getf buckets current))
                       (incf i))))
          (progn (write-char (char raw i) (getf buckets current))
                 (incf i))))
    (values (get-output-stream-string (getf buckets :left))
            (get-output-stream-string (getf buckets :centre))
            (get-output-stream-string (getf buckets :right)))))

(defun %expand-segment-or-empty (raw base-sgr reset)
  "Expand inline #[…] style blocks in RAW then append RESET; returns \"\" when RAW is empty."
  (if (plusp (length raw))
      (concatenate 'string (%status-expand-style-blocks raw base-sgr) reset)
      ""))

(defun %status-pad-to (out current target)
  "Pad OUT with spaces until CURRENT reaches TARGET; return the new column."
  (when (> target current)
    (dotimes (_ (- target current)) (write-char #\Space out))
    (setf current target))
  current)

(defun %status-emit-segment (out current cols seg width pos)
  "Emit SEG at POS in a COLS-wide line, returning the updated column."
  (if (plusp width)
      (let ((current (%status-pad-to out current (max current pos))))
        (when (< current cols)
          (write-string seg out)
          (+ current width)))
      current))

(defun %compose-aligned-line (raw base-sgr cols)
  "Render a status format RAW with #[align=…] regions into a COLS-wide line:
   the left region starts at column 0, the right region ends at COLS, and the
   centre region is centred.  Regions carry their own #[…] colours (reset to
   BASE-SGR after each).  Overlapping content is truncated to COLS."
  (multiple-value-bind (lraw craw rraw) (%status-align-buckets raw)
    (let* ((reset (format nil "~C[~Am" +esc+ base-sgr))
           (l (%expand-segment-or-empty lraw base-sgr reset))
           (c (%expand-segment-or-empty craw base-sgr reset))
           (r (%expand-segment-or-empty rraw base-sgr reset))
           (lw (%visible-length l)) (cw (%visible-length c)) (rw (%visible-length r)))
      (%visible-truncate
       (with-output-to-string (out)
         (let ((col 0))
           (write-string l out)
           (incf col lw)
           (setf col (%status-emit-segment out col cols c cw (floor (- cols cw) 2)))
           (setf col (%status-emit-segment out col cols r rw (- cols rw)))
           (setf col (%status-pad-to out col cols))))
       cols))))
