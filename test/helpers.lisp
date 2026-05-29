;;;; Test DSL helpers for cl-tmux.
;;;;
;;;; Provides screen builder macros, byte-feeding utilities, grid inspection
;;;; accessors, a table-driven test macro, and layout invariant checkers.

(in-package #:cl-tmux/test)

;;; ── Screen builder ──────────────────────────────────────────────────────────

(defmacro with-screen ((var w h) &body body)
  "Bind VAR to a fresh screen of width W and height H for BODY."
  `(let ((,var (make-screen ,w ,h))) ,@body))

;;; ── Byte feeding ────────────────────────────────────────────────────────────

(defun octets (string)
  "Convert STRING to an (unsigned-byte 8) vector (Latin-1; each char maps
   directly to its char-code, so #\\Escape = #x1B)."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun feed (screen string)
  "Process STRING (one byte per character) through SCREEN's emulator."
  (screen-process-bytes screen (octets string))
  screen)

;;; ── Semantic escape sequence builders ──────────────────────────────────────

(defun esc (fmt &rest args)
  "Build an escape sequence string with ESC (char code 27) prefix.
   FMT and ARGS are passed to FORMAT after the ESC character."
  (format nil "~C~?" #\Escape fmt args))

(defun csi (params final)
  "Build the string ESC [ PARAMS FINAL."
  (format nil "~C[~A~A" #\Escape params (string final)))

;;; ── Grid inspection ─────────────────────────────────────────────────────────

(defun row-string (screen y &key (start 0) end)
  "Return the characters of row Y from START to END (default: full width)."
  (let* ((w (screen-width screen))
         (e (or end w)))
    (with-output-to-string (s)
      (loop for x from start below (min e w)
            do (write-char (cell-char (screen-cell screen x y)) s)))))

(defun cell-at  (screen x y) (screen-cell screen x y))
(defun char-at  (screen x y) (cell-char   (screen-cell screen x y)))
(defun fg-at    (screen x y) (cell-fg     (screen-cell screen x y)))
(defun bg-at    (screen x y) (cell-bg     (screen-cell screen x y)))
(defun attrs-at (screen x y) (cell-attrs  (screen-cell screen x y)))

;;; ── Table-driven test macro ─────────────────────────────────────────────────

(defmacro test-table (test-name description &rest cases)
  "Run a table of cases as a single fiveam test named TEST-NAME.
   DESCRIPTION is a documentation string (currently unused at runtime).
   Each CASE has the form:
     (input-string &key x y char fg bg attrs cx cy row)
   where:
     X, Y     -- cell coordinates for char/fg/bg/attrs checks (default 0)
     CHAR     -- expected character at (X, Y)
     FG       -- expected foreground colour index at (X, Y)
     BG       -- expected background colour index at (X, Y)
     ATTRS    -- expected attribute bitmask at (X, Y)
     CX, CY   -- expected cursor position after processing INPUT
     ROW      -- expected prefix string starting at column 0 of row 0

   Each case creates a fresh 20x5 screen, feeds INPUT to it, then checks
   every non-nil keyword assertion with fiveam IS."
  (declare (ignore description))
  (let ((cases-sym (gensym "CASES")))
    `(test ,test-name
       (let ((,cases-sym
              (list ,@(mapcar
                       (lambda (case-form)
                         (destructuring-bind (input &key (x 0) (y 0)
                                                    char fg bg attrs
                                                    (cx nil) (cy nil) row)
                             case-form
                           `(list ,input ,x ,y ,char ,fg ,bg ,attrs ,cx ,cy ,row)))
                       cases))))
         (dolist (c ,cases-sym)
           (destructuring-bind (input x y expected-char expected-fg expected-bg
                                expected-attrs expected-cx expected-cy expected-row)
               c
             (with-screen (s 20 5)
               (when (plusp (length input))
                 (feed s input))
               (when expected-char
                 (is (char= expected-char (char-at s x y))
                     "char-at ~D,~D: expected ~C got ~C"
                     x y expected-char (char-at s x y)))
               (when expected-fg
                 (is (= expected-fg (fg-at s x y))
                     "fg-at ~D,~D: expected ~D got ~D"
                     x y expected-fg (fg-at s x y)))
               (when expected-bg
                 (is (= expected-bg (bg-at s x y))
                     "bg-at ~D,~D: expected ~D got ~D"
                     x y expected-bg (bg-at s x y)))
               (when expected-attrs
                 (is (= expected-attrs (attrs-at s x y))
                     "attrs-at ~D,~D: expected ~D got ~D"
                     x y expected-attrs (attrs-at s x y)))
               (when expected-cx
                 (is (= expected-cx (screen-cursor-x s))
                     "cursor-x: expected ~D got ~D"
                     expected-cx (screen-cursor-x s)))
               (when expected-cy
                 (is (= expected-cy (screen-cursor-y s))
                     "cursor-y: expected ~D got ~D"
                     expected-cy (screen-cursor-y s)))
               (when expected-row
                 (is (string= expected-row
                               (row-string s 0 :end (length expected-row)))
                     "row 0: expected ~S got ~S"
                     expected-row
                     (row-string s 0 :end (length expected-row)))))))))))

;;; ── Layout invariant checker ────────────────────────────────────────────────

(defun check-layout-invariants (slots direction rows cols &key test-name)
  "Assert the three geometric invariants for any divide-window result.

   SLOTS is a list of (X Y W H) rectangles.  DIRECTION is :vertical or
   :horizontal.  ROWS and COLS are the enclosing grid dimensions.
   TEST-NAME is used in failure messages.

   Invariants checked:
     1. Each slot fits within the grid (x>=0, y>=0, w>=1, h>=1,
        x+w<=cols, y+h<=rows).
     2. No two slots overlap along the split axis:
        :vertical   -- adjacent slots do not overlap in X
        :horizontal -- adjacent slots do not overlap in Y"
  (let ((name (or test-name "layout")))
    (dolist (slot slots)
      (destructuring-bind (x y w h) slot
        (is (>= x 0)          "~A: x >= 0 (got ~D)" name x)
        (is (>= y 0)          "~A: y >= 0 (got ~D)" name y)
        (is (>= w 1)          "~A: w >= 1 (got ~D)" name w)
        (is (>= h 1)          "~A: h >= 1 (got ~D)" name h)
        (is (<= (+ x w) cols) "~A: x+w <= cols (~D+~D > ~D)" name x w cols)
        (is (<= (+ y h) rows) "~A: y+h <= rows (~D+~D > ~D)" name y h rows)))
    ;; No pairwise overlap along the split axis.
    (loop for (a . rest) on slots
          do (dolist (b rest)
               (destructuring-bind (ax ay aw ah) a
                 (destructuring-bind (bx by bw bh) b
                   (ecase direction
                     (:vertical
                      (is (or (<= (+ ax aw) bx) (<= (+ bx bw) ax))
                          "~A: vertical overlap between ~A and ~A" name a b))
                     (:horizontal
                      (is (or (<= (+ ay ah) by) (<= (+ by bh) ay))
                          "~A: horizontal overlap between ~A and ~A" name a b)))))))))
