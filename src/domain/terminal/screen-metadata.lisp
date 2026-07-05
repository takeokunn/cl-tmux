(in-package #:cl-tmux/terminal/types)

;;;; Screen metadata helpers.
;;;;
;;;; The screen struct owns the slots; this file owns the mutation rules for
;;;; capture line-wrap metadata and OSC palette overrides.

;;; ── Line-wrap flags (capture-pane -J metadata) ──────────────────────────────

(defun %mark-line-wrapped (screen row)
  "Mark that ROW's line wraps (continues onto ROW+1) — set when an autowrap
   actually carries content to the next row."
  (let ((ht (or (screen-wrapped-rows screen)
                (setf (screen-wrapped-rows screen) (make-hash-table :test #'eql)))))
    (setf (gethash row ht) t)))

(defun %line-wrapped-p (screen row)
  "T when ROW's line wraps onto ROW+1 (capture-pane -J join boundary)."
  (let ((ht (screen-wrapped-rows screen)))
    (and ht (gethash row ht) t)))

(defun %clear-line-wrapped (screen row)
  "Clear ROW's wrap flag — its content no longer continues (repositioned/erased)."
  (let ((ht (screen-wrapped-rows screen)))
    (when ht (remhash row ht))))

(defun %clear-all-line-wrapped (screen)
  "Drop all wrap flags — a coarse reset for erase-display / RIS / resize / alt-screen."
  (let ((ht (screen-wrapped-rows screen)))
    (when ht (clrhash ht))))

(defun %shift-line-wrapped-up (screen top bottom)
  "Shift wrap flags to track a scroll-up of region [TOP,BOTTOM]: a flag at row Y in
   (TOP,BOTTOM] moves to Y-1; the flag at TOP scrolls off; BOTTOM's flag is cleared."
  (let ((ht (screen-wrapped-rows screen)))
    (when ht
      (let ((new (make-hash-table :test #'eql)))
        (maphash (lambda (y v)
                   (declare (ignore v))
                   (cond
                     ((and (> y top) (<= y bottom)) (setf (gethash (1- y) new) t))
                     ((or (< y top) (> y bottom))   (setf (gethash y new) t))))
                 ht)
        (setf (screen-wrapped-rows screen) new)))))

;;; ── OSC 4 / OSC 104 palette overrides ───────────────────────────────────────
;;;
;;; A custom palette entry set by OSC 4 shadows the built-in xterm palette for
;;; that index.  Storage is lazily allocated (NIL until the first set) to keep the
;;; common no-override screen cheap.  Mirrors tmux colour_palette_set/_get/_clear.

(defun %palette-override-get (screen index)
  "Return the custom 0xRRGGBB override for palette INDEX, or NIL when INDEX has no
   override (caller falls back to the built-in xterm palette).  INDEX out of the
   0..255 range returns NIL."
  (let ((overrides (screen-palette-overrides screen)))
    (and overrides
         (<= 0 index 255)
         (svref overrides index))))

(defun %palette-override-set (screen index rgb)
  "Set the custom 0xRRGGBB override for palette INDEX (0..255), allocating the
   256-entry override vector on first use.  Out-of-range INDEX is ignored."
  (when (<= 0 index 255)
    (let ((overrides (or (screen-palette-overrides screen)
                         (setf (screen-palette-overrides screen)
                               (make-array 256 :initial-element nil)))))
      (setf (svref overrides index) rgb))))

(defun %palette-override-clear (screen index)
  "Clear the custom override for palette INDEX (0..255), reverting it to the
   built-in xterm palette.  No-op when no overrides exist or INDEX is out of range."
  (let ((overrides (screen-palette-overrides screen)))
    (when (and overrides (<= 0 index 255))
      (setf (svref overrides index) nil))))

(defun %palette-override-clear-all (screen)
  "Drop all custom palette overrides (OSC 104 with an empty body), reverting every
   index to the built-in xterm palette."
  (setf (screen-palette-overrides screen) nil))
