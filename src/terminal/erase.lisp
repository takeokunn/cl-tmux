(in-package #:cl-tmux/terminal/actions)

;;;; Erase operations: region, display, and line.
;;;;
;;;; All erase functions are expressed as Prolog-like rule tables,
;;;; consistent with the define-csi-rules / define-sgr-rules idiom.
;;;; Loads after scroll.lisp (needs blank-cell from cell.lisp, screen-cell).

;;; ── Primitive erase ─────────────────────────────────────────────────────────

(defun erase-region (screen x0 y0 x1 y1)
  "Erase all cells from (X0,Y0) to (X1,Y1) inclusive, treating the range as
   a linear span across rows."
  (loop for y from y0 to y1
        do (let ((bx (if (= y y0) x0 0))
                 (ex (if (= y y1) x1 (1- (screen-width screen)))))
             (loop for x from bx to ex
                   do (setf (screen-cell screen x y) (blank-cell))))))

;;; ── ED (erase-display) rule table ──────────────────────────────────────────
;;;
;;; Prolog-like factual table — each clause maps one mode value to its action.
;;; Parallel structure with define-erase-line-rules.
;;;
;;;   erase_display(0, S) :- erase_region(S, cx, cy, w-1, cy),
;;;                          when(cy+1 < h, erase_region(S, 0, cy+1, w-1, h-1)).
;;;   erase_display(1, S) :- when(cy > 0, erase_region(S, 0, 0, w-1, cy-1)),
;;;                          erase_region(S, 0, cy, cx, cy).
;;;   erase_display(2, S) :- erase_region(S, 0, 0, w-1, h-1).
;;;   erase_display(3, S) :- erase_region(S, 0, 0, w-1, h-1), clear_scrollback(S).

(defmacro define-erase-display-rules (&rest specs)
  "Build ERASE-DISPLAY from a Prolog-like mode rule table.
   Each SPEC is (mode &rest body).
   CX, CY, W, H, SCREEN are bound in every body.
   MODE is the function parameter (NOT in the let binding) — no ignorable needed."
  `(defun erase-display (screen mode)
     "Erase part or all of the display (ED: ESC[Jn).
      Mode 0: cursor to end.  Mode 1: start to cursor.
      Mode 2: entire screen.  Mode 3: entire screen + scrollback."
     ;; cx/cy/w/h are used in rules 0 and 1; declared ignorable so rules
     ;; that don't use them (rules 2 and 3) compile without unused-var warnings.
     ;; mode is a function parameter used in the case dispatch — not ignorable.
     (let ((cx (screen-cx screen)) (cy (screen-cy screen))
           (w  (screen-width  screen))
           (h  (screen-height screen)))
       (declare (ignorable cx cy w h))
       (case mode
         ,@(mapcar (lambda (spec)
                     (destructuring-bind (mode-val &rest body) spec
                       `(,mode-val ,@body)))
                   specs)))))

(define-erase-display-rules
  (0
   (erase-region screen cx cy (1- w) cy)
   (when (< (1+ cy) h)
     (erase-region screen 0 (1+ cy) (1- w) (1- h))))
  (1
   (when (> cy 0)
     (erase-region screen 0 0 (1- w) (1- cy)))
   (erase-region screen 0 cy cx cy))
  (2
   (erase-region screen 0 0 (1- w) (1- h)))
  (3
   (erase-region screen 0 0 (1- w) (1- h))
   (setf (screen-scrollback screen) nil)))

;;; ── EL (erase-line) rule table ─────────────────────────────────────────────
;;;
;;; Prolog-like table: each row maps a mode number to the x-extent of the erase.
;;;   erase_line(0, Screen) :- erase_region(Screen, cx, cy, w-1, cy).
;;;   erase_line(1, Screen) :- erase_region(Screen, 0,  cy, cx,  cy).
;;;   erase_line(2, Screen) :- erase_region(Screen, 0,  cy, w-1, cy).

(defmacro define-erase-line-rules (&rest specs)
  "Build ERASE-LINE from a declarative range table.
   Each SPEC is (mode x0-expr x1-expr); CX, CY, W are bound."
  `(defun erase-line (screen mode)
     "Erase part or all of the current line (EL: ESC[Kn).
      Mode 0: cursor to end.  Mode 1: start to cursor.  Mode 2: entire line."
     (let ((cx (screen-cx screen)) (cy (screen-cy screen)) (w (screen-width screen)))
       (declare (ignorable cx cy w))
       (case mode
         ,@(mapcar (lambda (s) `(,(first s) (erase-region screen ,(second s) cy ,(third s) cy)))
                   specs)))))

(define-erase-line-rules
  (0  cx     (1- w))   ; from cursor to end
  (1  0       cx)      ; from start to cursor
  (2  0      (1- w)))
