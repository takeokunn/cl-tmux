(in-package #:cl-tmux/commands)

;;;; Copy-mode selection extraction helpers.
;;;
;;; This file holds the pure data-extraction layer for copy-mode selection:
;;; canonical row/column bounds, virtual-row lookup, and text extraction.

;;; %selection-bounds extracts the canonical (start-row end-row start-col end-col)
;;; rectangle from the mark and cursor positions - independent of which end the
;;; user anchored first.  %selection-text builds the string from that rectangle.
;;; Both are private (percent-prefixed) and independently testable.

(defun %selection-col-range (mark-vrow mark-col cur-vrow cursor-col)
  "Return (values start-col end-col) for a selection spanning MARK-VROW/MARK-COL
   to CUR-VROW/CURSOR-COL.  When mark is topmost, mark-col is start; when cursor
   is topmost, cursor-col is start; when on the same row, min/max applies."
  (cond ((< mark-vrow cur-vrow) (values mark-col cursor-col))
        ((> mark-vrow cur-vrow) (values cursor-col mark-col))
        (t (values (min mark-col cursor-col) (max mark-col cursor-col)))))

(defun %copy-mode-cursor-vrow (screen)
  "Return the current copy-mode cursor as a virtual row.
   Virtual rows are numbered from oldest scrollback (0) toward the live grid.
   A missing cursor is treated as viewport row 0 for callers that probe before
   copy-mode has initialized the cursor."
  (let ((cursor (screen-copy-cursor screen)))
    (+ (length (screen-scrollback screen))
       (if cursor (car cursor) 0)
       (- (screen-copy-offset screen)))))

(defun %selection-bounds (screen)
  "Return (values start-vrow end-vrow start-col end-col) for the current copy-mode
   selection in SCREEN.  Rows are VIRTUAL (0 = oldest scrollback, increasing toward
   the live grid bottom) so the selection is invariant to subsequent viewport scrolling.
   Assumes mark and cursor are already set."
  (let* ((sb-n        (length (screen-scrollback screen)))
         (mark        (screen-copy-mark   screen))
         (cursor      (screen-copy-cursor screen))
         (mark-offset (screen-copy-mark-offset screen))
         (cur-offset  (screen-copy-offset screen))
         ;; Convert viewport rows to virtual rows using the offset in effect at placement.
         (mark-vrow   (+ sb-n (car mark)   (- mark-offset)))
         (cur-vrow    (+ sb-n (car cursor) (- cur-offset))))
    (multiple-value-bind (start-col end-col)
        (%selection-col-range mark-vrow (cdr mark) cur-vrow (cdr cursor))
      (values (min mark-vrow cur-vrow) (max mark-vrow cur-vrow)
              start-col end-col))))

(defun %extract-chars (count char-at)
  "Return a COUNT-length string built by calling CHAR-AT on each index.
   CHAR-AT must return a character for the given 0-based index."
  (let ((result (make-string count)))
    (dotimes (i count result)
      (setf (char result i) (funcall char-at i)))))

(defun %extract-row-chars-from-reader (from-col to-col char-at)
  "Return a string for the half-open column range [FROM-COL, TO-COL).
   CHAR-AT is called with each absolute column index and must return a character.
   Row lookup is intentionally factored out so callers can supply either a
   virtual-row reader or a viewport-row reader."
  (if (>= from-col to-col)
      ""
      (%extract-chars
       (- to-col from-col)
       (lambda (i)
         (funcall char-at (+ from-col i))))))

(defun %extract-vrow-chars (screen vrow from-col to-col)
  "Return a string of characters from SCREEN at virtual row VROW.
   VROW is numbered from oldest scrollback (0) toward the live grid.  The row
   lookup is virtual-row aware; extraction itself is shared with viewport rows."
  (let* ((sb   (screen-scrollback screen))
         (sb-n (length sb)))
    (%extract-row-chars-from-reader
     from-col
     to-col
     (lambda (col)
       (if (< vrow sb-n)
           ;; Scrollback: vrow 0 = oldest = nth(sb-n-1), newest = nth(0).
           (let ((vec (nth (- sb-n 1 vrow) sb)))
             (if (and vec (< col (length vec)))
                 (cell-char (aref vec col))
                 #\Space))
           ;; Live grid row.
           (cell-char (screen-cell screen col (- vrow sb-n))))))))

(defun %extract-row-chars (screen row from-col to-col)
  "Return a string of characters from SCREEN at viewport ROW.
   ROW is a viewport row (0-based, same units as screen-copy-cursor when the
   copy offset is 0).  The viewport and virtual-row readers share the same
   extraction core; only the row lookup differs."
  (%extract-row-chars-from-reader
   from-col
   to-col
   (lambda (col)
     (cell-char (screen-display-cell screen col row)))))

(defun %selection-text (screen)
  "Compute the text selected by copy-mode in SCREEN.
   Returns a string, or NIL when no valid selection exists.
   Intermediate rows (not the last) are right-trimmed of trailing spaces.
   Pure data extraction: no lock held, no I/O."
  (when (and (screen-copy-selecting screen)
             (screen-copy-mark   screen)
             (screen-copy-cursor screen))
    (multiple-value-bind (start-vrow end-vrow start-col end-col)
        (%selection-bounds screen)
      (let* ((w    (screen-width screen))
             (text (with-output-to-string (out)
                     (loop for vrow from start-vrow to end-vrow do
                       (let* ((col-from (if (= vrow start-vrow) start-col 0))
                              (col-to   (if (= vrow end-vrow)   end-col   w))
                              (row-str  (%extract-vrow-chars screen vrow col-from col-to)))
                         ;; Trim trailing spaces from intermediate rows.
                         (write-string (if (< vrow end-vrow)
                                           (string-right-trim " " row-str)
                                           row-str)
                                       out)
                         (when (< vrow end-vrow)
                           (write-char #\Newline out)))))))
        (when (plusp (length text)) text)))))
