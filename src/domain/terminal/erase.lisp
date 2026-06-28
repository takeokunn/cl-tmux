(in-package #:cl-tmux/terminal/actions)

;;;; Erase operations: region, display, and line.
;;;;
;;;; All erase functions are expressed as Prolog-like rule tables,
;;;; consistent with the define-csi-rules / define-sgr-rules idiom.
;;;; Loads after scroll.lisp (needs blank-cell from cell.lisp, screen-cell).

;;; ── Primitive erase ─────────────────────────────────────────────────────────

(defun erase-region (screen x0 y0 x1 y1)
  "Erase all cells from (X0,Y0) to (X1,Y1) inclusive, treating the range as
   a linear span across rows.  Sets screen-dirty-p whenever any cell is written."
  (loop for y from y0 to y1
        do (%clear-line-wrapped screen y)   ; erased content no longer wraps (capture -J)
           (let ((bx (if (= y y0) x0 0))
                 (ex (if (= y y1) x1 (1- (screen-width screen)))))
             (loop for x from bx to ex
                   do (setf (screen-cell screen x y) (%erase-cell screen)))))
  (setf (screen-dirty-p screen) t))

;;; ── ED (erase-display) rule table ──────────────────────────────────────────
;;;
;;; Prolog-like factual table — each clause maps one mode value to its action.
;;; Parallel structure with define-erase-line-rules.
;;;
;;;   erase_display(0, S) :- erase_region(S, cursor-x, cursor-y, w-1, cursor-y),
;;;                          when(cursor-y+1 < h, erase_region(S, 0, cursor-y+1, w-1, h-1)).
;;;   erase_display(1, S) :- when(cursor-y > 0, erase_region(S, 0, 0, w-1, cursor-y-1)),
;;;                          erase_region(S, 0, cursor-y, cursor-x, cursor-y).
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
     (let ((cx (screen-cursor-x screen)) (cy (screen-cursor-y screen))
           (w  (screen-width    screen))
           (h  (screen-height   screen)))
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
   ;; scroll-on-clear (tmux option, on by default): move the visible content into
   ;; history before erasing, so a full-screen clear stays in the scrollback.
   (when (%scroll-on-clear-p) (scroll-screen-to-history screen))
   (erase-region screen 0 0 (1- w) (1- h)))
  (3
   (erase-region screen 0 0 (1- w) (1- h))
   (setf (screen-scrollback screen) nil)))

;;; ── EL (erase-line) rule table ─────────────────────────────────────────────
;;;
;;; Prolog-like table: each row maps a mode number to the x-extent of the erase.
;;;   erase_line(0, Screen) :- erase_region(Screen, cursor-x, cursor-y, w-1, cursor-y).
;;;   erase_line(1, Screen) :- erase_region(Screen, 0,  cursor-y, cursor-x, cursor-y).
;;;   erase_line(2, Screen) :- erase_region(Screen, 0,  cursor-y, w-1, cursor-y).

(defmacro define-erase-line-rules (&rest specs)
  "Build ERASE-LINE from a declarative range table.
   Each SPEC is (mode x0-expr x1-expr); CX (cursor-x), CY (cursor-y), W are bound."
  `(defun erase-line (screen mode)
     "Erase part or all of the current line (EL: ESC[Kn).
      Mode 0: cursor to end.  Mode 1: start to cursor.  Mode 2: entire line."
     (let ((cx (screen-cursor-x screen)) (cy (screen-cursor-y screen)) (w (screen-width screen)))
       (declare (ignorable cx cy w))
       (case mode
         ,@(mapcar (lambda (s) `(,(first s) (erase-region screen ,(second s) cy ,(third s) cy)))
                   specs)))))

(define-erase-line-rules
  (0  cx     (1- w))   ; from cursor to end
  (1  0       cx)      ; from start to cursor
  (2  0      (1- w)))

;;; ── DEC Rectangle operations ─────────────────────────────────────────────────
;;;
;;; DECERA/DECFRA/DECCRA are xterm extensions used by full-screen TUI apps.
;;; Parameters arrive 1-based; helpers convert to 0-based and clamp to bounds.

(defun %rect-bounds (screen top1 left1 bottom1 right1)
  "Convert 1-based inclusive DEC rectangle parameters to 0-based inclusive bounds
   clamped to the screen, returning (values t0 l0 b0 r0).
   A degenerate rectangle (top > bottom or left > right) returns (values 0 0 -1 -1)."
  (let* ((w (screen-width  screen))
         (h (screen-height screen))
         (t0 (1- (max 1 top1)))
         (l0 (1- (max 1 left1)))
         (b0 (min (1- h) (1- (if (zerop bottom1) h bottom1))))
         (r0 (min (1- w) (1- (if (zerop right1)  w right1)))))
    (if (or (> t0 b0) (> l0 r0))
        (values 0 0 -1 -1)
        (values t0 l0 b0 r0))))

(defun decera (screen top1 left1 bottom1 right1)
  "DECERA — Erase Rectangular Area (CSI Pt;Pl;Pb;Pr $ z).
   Parameters are 1-based and inclusive.  Cells are replaced with BCE blanks
   (background-colour-erase), matching DECERA semantics in xterm."
  (multiple-value-bind (t0 l0 b0 r0)
      (%rect-bounds screen top1 left1 bottom1 right1)
    (when (and (<= t0 b0) (<= l0 r0))
      (loop for y from t0 to b0 do
        (loop for x from l0 to r0 do
          (setf (screen-cell screen x y) (%erase-cell screen))))
      (setf (screen-dirty-p screen) t))))

(defun decfra (screen char-code top1 left1 bottom1 right1)
  "DECFRA — Fill Rectangular Area (CSI Pc;Pt;Pl;Pb;Pr $ x).
   CHAR-CODE is the character to fill with (e.g. 65 for 'A').
   Uses the current SGR pen for fg/bg/attrs so themed apps render correctly."
  (multiple-value-bind (t0 l0 b0 r0)
      (%rect-bounds screen top1 left1 bottom1 right1)
    (when (and (<= t0 b0) (<= l0 r0))
      (let* ((ch   (safe-code-char (if (zerop char-code) 32 char-code)))
             (cell (make-cell :char     ch
                              :fg       (screen-cur-fg       screen)
                              :bg       (screen-cur-bg       screen)
                              :attrs    (screen-cur-attrs    screen)
                              :attrs2   (screen-cur-attrs2   screen)
                              :ul-color (screen-cur-ul-color screen))))
        (loop for y from t0 to b0 do
          (loop for x from l0 to r0 do
            (setf (screen-cell screen x y) cell))))
      (setf (screen-dirty-p screen) t))))

(defun %copy-rect-buffered (screen src-top src-left rows cols tgt-top tgt-left)
  "Copy a ROWS × COLS rectangle from (SRC-TOP, SRC-LEFT) to (TGT-TOP, TGT-LEFT).
   All coordinates are 0-based.  Source cells are buffered before writing so that
   overlapping source/target regions are handled correctly.  Marks screen dirty."
  (let* ((w   (screen-width  screen))
         (h   (screen-height screen))
         (tb0 (min (1- h) (+ tgt-top  rows -1)))
         (tr0 (min (1- w) (+ tgt-left cols -1)))
         (buffer (make-array (* rows cols))))
    ;; Read phase: buffer all source cells before any writes.
    (loop for sy from src-top to (+ src-top rows -1)
          for ri from 0 do
      (loop for sx from src-left to (+ src-left cols -1)
            for ci from 0 do
        (setf (aref buffer (+ (* ri cols) ci))
              (screen-cell screen sx sy))))
    ;; Write phase: copy buffer to target rectangle.
    (loop for ty from tgt-top to tb0
          for ri from 0 do
      (loop for tx from tgt-left to tr0
            for ci from 0 do
        (setf (screen-cell screen tx ty)
              (aref buffer (+ (* ri cols) ci)))))
    (setf (screen-dirty-p screen) t)))

(defun deccra (screen src-top1 src-left1 src-bottom1 src-right1
                      tgt-top1 tgt-left1)
  "DECCRA — Copy Rectangular Area (CSI Pt;Pl;Pb;Pr;Pp;Ptp;Plp;Ppp $ v).
   Page parameters are ignored (only page 0 exists).
   Source and target rectangles are clamped independently; overlapping regions
   are handled correctly by buffering source cells before writing."
  (multiple-value-bind (st sl sb sr)
      (%rect-bounds screen src-top1 src-left1 src-bottom1 src-right1)
    (when (and (<= st sb) (<= sl sr))
      (let* ((rows (1+ (- sb st)))
             (cols (1+ (- sr sl)))
             (tt0  (1- (max 1 tgt-top1)))
             (tl0  (1- (max 1 tgt-left1))))
        (%copy-rect-buffered screen st sl rows cols tt0 tl0)))))
