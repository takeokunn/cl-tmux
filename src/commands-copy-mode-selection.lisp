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

;;; %extract-row-chars reads characters from a rectangular range as a string.
;;; It accepts either a virtual row (via %copy-mode-virtual-row-string, used by
;;; the selection path where %selection-bounds now returns virtual rows) or a
;;; viewport row (used by %copy-row-range-to-paste-buffer in the nav module).
;;; The selection path uses the virtual-row overload; nav uses viewport overload.
;;; Pure data extraction - no I/O side effects.

(defun %extract-vrow-chars (screen vrow from-col to-col)
  "Return a string of characters from SCREEN at VIRTUAL row VROW (0=oldest
   scrollback, increasing toward live grid), columns FROM-COL to TO-COL (exclusive).
   Inlines the virtual-row lookup so this file has no forward-reference to
   commands-copy-mode-search.  Pure data extraction."
  (if (>= from-col to-col)
      ""
      (let* ((sb    (screen-scrollback screen))
             (sb-n  (length sb))
             (n     (- to-col from-col))
             (result (make-array n :element-type 'character :initial-element #\Space)))
        (dotimes (i n result)
          (let ((col (+ from-col i)))
            (setf (char result i)
                  (if (< vrow sb-n)
                      ;; Scrollback: vrow 0 = oldest = nth(sb-n-1), newest = nth(0).
                      (let ((vec (nth (- sb-n 1 vrow) sb)))
                        (if (and vec (< col (length vec)))
                            (cell-char (aref vec col))
                            #\Space))
                      ;; Live grid row.
                      (cell-char (screen-cell screen col (- vrow sb-n))))))))))

(defun %extract-row-chars (screen row from-col to-col)
  "Return a string of characters from SCREEN at viewport ROW, columns FROM-COL to
   TO-COL (exclusive).  Reads through screen-display-cell so the projection honours
   the copy-mode scroll offset.  ROW is a VIEWPORT row (0-based, same units as
   screen-copy-cursor when copy-offset is 0).  Used by %copy-row-range-to-paste-buffer.
   The selection path uses %extract-vrow-chars instead.  Pure data extraction."
  (let* ((n      (- to-col from-col))
         (result (make-string n)))
    (dotimes (i n result)
      (setf (char result i)
            (cell-char (screen-display-cell screen (+ from-col i) row))))))

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
