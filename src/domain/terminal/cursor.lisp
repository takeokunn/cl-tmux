(in-package #:cl-tmux/terminal/actions)

;;;; Cursor movement.
;;;; Loads AFTER scroll.lisp so cursor-down/scroll can call scroll-up-one
;;;; (defined there) without a forward-reference.
;;;; Character writing (combining chars, DEC graphics, wide/normal cells) lives
;;;; in char-write.lisp, which loads after this file.

;;; ── Constants ──────────────────────────────────────────────────────────────

(defconstant +tab-width+ 8
  "Standard terminal tab column interval: tab stops every 8 columns by default.")

;;; ── Cursor movement ────────────────────────────────────────────────────────
;;;
;;; define-cursor-movements is a Prolog-like table:
;;;   cursor_move(up,    Screen, N) :- cursor-y(Screen) := clamp(cursor-y - N, scroll_top).
;;;   cursor_move(down,  Screen, N) :- cursor-y(Screen) := clamp(cursor-y + N, scroll_bottom).
;;;   cursor_move(right, Screen, N) :- cursor-x(Screen) := clamp(cursor-x + N, width-1).
;;;   cursor_move(left,  Screen, N) :- cursor-x(Screen) := clamp(cursor-x - N, 0).
;;;
;;; Each spec: (name docstring accessor clamped-expression)

(declaim (inline %cancel-wrap))
(defun %cancel-wrap (screen)
  "Cancel any pending (deferred) wrap.  Called by every explicit cursor movement:
   moving the cursor discards the VT100 last-column flag, so a subsequent write
   does not spuriously wrap.  See the screen-pending-wrap slot docstring."
  (setf (screen-pending-wrap screen) nil))

(defmacro define-cursor-movements (&rest specs)
  "Build cursor movement functions from a Prolog-like fact table.
   Each SPEC is (name docstring slot-accessor clamped-new-value-expr)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name docstring accessor limit-expr) spec
            `(defun ,name (screen n)
               ,docstring
               (%cancel-wrap screen)
               (setf (,accessor screen) ,limit-expr))))
        specs)))

(defun set-cursor (screen x y)
  "Move the cursor to (X, Y), clamping both coordinates into bounds.
   Cancels a pending wrap (explicit positioning discards the last-column flag)."
  (%cancel-wrap screen)
  (setf (screen-cursor-x screen) (clamp x 0 (1- (screen-width  screen)))
        (screen-cursor-y screen) (clamp y 0 (1- (screen-height screen))))
  ;; Explicit positioning starts fresh content on the target row — its old wrap
  ;; flag (if any) no longer applies.
  (%clear-line-wrapped screen (screen-cursor-y screen)))

(define-cursor-movements
  (cursor-up    "Move the cursor up N rows, clamping to the scroll-top boundary."
   screen-cursor-y  (max (screen-scroll-top    screen) (- (screen-cursor-y screen) n)))
  (cursor-down  "Move the cursor down N rows, clamping to the scroll-bottom boundary."
   screen-cursor-y  (min (screen-scroll-bottom screen) (+ (screen-cursor-y screen) n)))
  (cursor-right "Move the cursor right N columns, clamping to width-1."
   screen-cursor-x  (min (1- (screen-width     screen)) (+ (screen-cursor-x screen) n)))
  (cursor-left  "Move the cursor left N columns, clamping to column 0."
   screen-cursor-x  (max 0                              (- (screen-cursor-x screen) n))))

(defun cursor-down/scroll (screen)
  "Move cursor down one line, scrolling when at the bottom of the scroll region.
   This is the internal helper used by write-char-at-cursor and the LF handler."
  (if (< (screen-cursor-y screen) (screen-scroll-bottom screen))
      (incf (screen-cursor-y screen))
      (scroll-up-one screen)))

(defun cursor-lf (screen)
  "Line feed: move cursor down, scrolling the scroll region if at the bottom."
  (%cancel-wrap screen)
  (cursor-down/scroll screen))

(defun cursor-nl (screen)
  "Process a C0 line-feed control (LF / VT / FF): a line feed, plus a carriage
   return when LNM (newline mode, CSI 20 h) is on so the cursor returns to column
   0.  IND (ESC D) calls cursor-lf directly and is therefore never affected by LNM."
  (cursor-lf screen)
  (when (screen-newline-mode screen)
    (setf (screen-cursor-x screen) 0)))

(defun %materialize-tab-stops (screen)
  "Return SCREEN's tab stops as a concrete sorted list of columns, expanding the
   :DEFAULT sentinel into the standard every-8-columns stops for the width."
  (let ((stops (screen-tab-stops screen)))
    (if (eq stops :default)
        (loop for c from 8 below (screen-width screen) by 8 collect c)
        stops)))

(defun set-tab-stop (screen)
  "HTS (ESC H) — set a horizontal tab stop at the current cursor column."
  (setf (screen-tab-stops screen)
        (sort (adjoin (screen-cursor-x screen) (%materialize-tab-stops screen)) #'<)))

(defun clear-tab-stops (screen mode)
  "TBC (CSI N g) — clear tab stops.  MODE 3 clears ALL stops; any other value
   (including 0) clears the stop at the current cursor column."
  (setf (screen-tab-stops screen)
        (if (= mode 3)
            '()
            (remove (screen-cursor-x screen) (%materialize-tab-stops screen)))))

(defun %next-tab-stop (stops x max-x)
  "Return the column of the next tab stop after X.
   When STOPS is :DEFAULT the standard every-+TAB-WIDTH+-column grid is used;
   otherwise STOPS is a custom sorted list.  The result is clamped to MAX-X."
  (if (eq stops :default)
      (min (* +tab-width+ (ceiling (1+ x) +tab-width+)) max-x)
      (or (find-if (lambda (c) (> c x)) (sort (copy-list stops) #'<))
          max-x)))

(defun %prev-tab-stop (stops x)
  "Return the column of the previous tab stop before X.
   When STOPS is :DEFAULT the standard every-+TAB-WIDTH+-column grid is used;
   otherwise STOPS is a custom sorted list.  The result is clamped to 0."
  (if (eq stops :default)
      (* +tab-width+ (floor (max 0 (1- x)) +tab-width+))
      (or (find-if (lambda (c) (< c x)) (sort (copy-list stops) #'>))
          0)))

(defun cursor-ht (screen)
  "Horizontal tab: advance the cursor to the next tab stop (default: every
   +TAB-WIDTH+ columns; HTS/TBC can customise the stops), clamping to the last column."
  (%cancel-wrap screen)
  (setf (screen-cursor-x screen)
        (%next-tab-stop (screen-tab-stops screen)
                        (screen-cursor-x screen)
                        (1- (screen-width screen)))))

(defun cursor-cht (screen n)
  "CHT — cursor forward N tab stops (CSI N I).
   Advance the cursor to the Nth next tab stop, clamping to width-1."
  (dotimes (_ (max 1 n))
    (cursor-ht screen)))

(defun cursor-cbt (screen n)
  "CBT — cursor backward N tab stops (CSI N Z).
   Move the cursor back to the Nth previous tab stop, stopping at column 0."
  (%cancel-wrap screen)
  (dotimes (_ (max 1 n))
    (setf (screen-cursor-x screen)
          (%prev-tab-stop (screen-tab-stops screen)
                          (screen-cursor-x screen)))))

(defun cursor-bs (screen)
  "Backspace: move cursor left one column if not already at column 0."
  (%cancel-wrap screen)
  (when (> (screen-cursor-x screen) 0)
    (decf (screen-cursor-x screen))))

(defun cursor-ri (screen)
  "Reverse index (ESC M): move cursor up one line, scrolling down if at top."
  (%cancel-wrap screen)
  (if (= (screen-cursor-y screen) (screen-scroll-top screen))
      (scroll-down-one screen)
      (decf (screen-cursor-y screen))))

(defun cursor-cr (screen)
  "Carriage return: move the cursor to column 0."
  (%cancel-wrap screen)
  (setf (screen-cursor-x screen) 0)
  ;; CR returns to the line start to overwrite it (e.g. progress bars) — clear any
  ;; stale wrap flag for this row.
  (%clear-line-wrapped screen (screen-cursor-y screen)))

(defun cursor-nel (screen)
  "NEL (ESC E) — Next Line: carriage return then line feed, i.e. move the cursor
   to column 0 of the next row (scrolling at the bottom margin like LF)."
  (cursor-cr screen)
  (cursor-lf screen))
