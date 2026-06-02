(in-package #:cl-tmux/terminal/actions)

;;;; Scroll operations and scroll-region setup.
;;;;
;;;; Loads BEFORE cursor.lisp, erase.lisp, and edit.lisp:
;;;;   cursor.lisp needs scroll-up-one;
;;;;   erase.lisp and edit.lisp need %copy-row / %clear-row.

;;; ── Row primitives (data-layer helpers) ─────────────────────────────────────
;;;
;;; Both scroll operations and line-edit operations (insert-lines, delete-lines)
;;; work row-by-row.  %copy-row and %clear-row are the shared building blocks.

(defun %copy-row (screen dst-row src-row)
  "Copy all cells from SRC-ROW to DST-ROW within SCREEN."
  (dotimes (col (screen-width screen))
    (setf (screen-cell screen col dst-row)
          (screen-cell screen col src-row))))

(defun %clear-row (screen row)
  "Fill every cell in ROW with a blank cell."
  (dotimes (col (screen-width screen))
    (setf (screen-cell screen col row) (blank-cell))))

;;; ── Scroll operations ───────────────────────────────────────────────────────

;;; ── Scrollback trimming ────────────────────────────────────────────────────

;;; The history-limit callback is set at startup by the higher-level layer
;;; (buffer.lisp / main.lisp) once the options package is loaded.  NIL means
;;; fall back to the compile-time constant.  Injecting the cap as a callback
;;; rather than probing cl-tmux/options at call time keeps this file pure
;;; (no runtime package discovery) and testable in isolation.
(defvar *history-limit-fn* nil
  "A zero-argument function returning the current history-limit integer, or NIL.
   Install (lambda () (cl-tmux/options:get-option \"history-limit\")) at startup.")

(declaim (inline %effective-history-limit))
(defun %effective-history-limit ()
  "Return the history-limit in effect: callback result if available, else +max-scrollback-lines+."
  (or (and *history-limit-fn* (funcall *history-limit-fn*))
      cl-tmux/config:+max-scrollback-lines+))

(defun trim-scroll-history (screen)
  "Cap the scrollback buffer of SCREEN to the current history-limit.
   The limit is obtained from *history-limit-fn* (injected at startup)
   rather than discovered via find-package at call time.
   Called after every scroll-up to honour runtime configuration changes."
  (let ((cap (%effective-history-limit)))
    (when (> (length (screen-scrollback screen)) cap)
      (let ((tail (nthcdr (1- cap) (screen-scrollback screen))))
        (when tail (setf (cdr tail) nil))))))

(defun scroll-up-one (screen)
  "Scroll the scroll region up one line; the displaced top row is pushed onto
   the scrollback buffer and the new bottom line is cleared to blank cells.
   The scrollback cap (history-limit) is enforced by the caller or by the
   post-scroll trim below — keeping policy out of the primitive operation itself.

   Scrollback cap note: the cap is enforced by trim-scroll-history which
   splices off the tail cons of the list, keeping the operation O(limit).
   The list is maintained newest-first so the tail is always the oldest entry."
  (let* ((top       (screen-scroll-top    screen))
         (bottom    (screen-scroll-bottom screen))
         (w         (screen-width         screen))
         (saved-row (make-array w)))
    (dotimes (col w) (setf (aref saved-row col) (screen-cell screen col top)))
    (push saved-row (screen-scrollback screen))
    (loop for row from top below bottom
          do (%copy-row screen row (1+ row)))
    (%clear-row screen bottom)
    ;; Enforce the scrollback cap after pushing the new entry so the policy
    ;; decision (cap size) stays at this logical boundary rather than inside
    ;; the raw grid-copy operations above.
    (trim-scroll-history screen)
    (setf (screen-dirty-p screen) t)))

(defun scroll-down-one (screen)
  "Scroll the scroll region down one line; the new top line is cleared to blanks."
  (let ((top    (screen-scroll-top    screen))
        (bottom (screen-scroll-bottom screen)))
    (loop for row from bottom above top
          do (%copy-row screen row (1- row)))
    (%clear-row screen top)
    (setf (screen-dirty-p screen) t)))

;;; ── Scroll region ──────────────────────────────────────────────────────────

(defun decstbm (screen top bottom)
  "DECSTBM — set the vertical scroll region.
   TOP and BOTTOM are 0-based inclusive row indices.  The cursor is homed
   to (0,0) after a valid set."
  (let ((clamped-top    (max 0 top))
        (clamped-bottom (min (1- (screen-height screen)) bottom)))
    (when (< clamped-top clamped-bottom)
      (setf (screen-scroll-top    screen) clamped-top
            (screen-scroll-bottom screen) clamped-bottom)
      (set-cursor screen 0 0))))
