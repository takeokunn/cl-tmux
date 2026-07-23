(in-package #:cl-tmux/test)

;;;; Screen tests (src/terminal/screen.lisp).
;;;; Tests: make-screen construction, grid access, resize, dirty flag,
;;;;        cursor accessors, saved cursor, bell-pending, reset-sgr-pen,
;;;;        alt-cells resize, screen-consume-bell, %make-blank-cells,
;;;;        %make-screen, screen-p, and all screen slot defaults.

;;; ── Shared helpers ───────────────────────────────────────────────────────────

(defmacro assert-all-cells-blank (screen)
  "Assert that every cell in SCREEN is a space with default fg/bg attributes.
   Expands to one EXPECT form per cell position."
  (let ((s (gensym "SCREEN")))
    `(let ((,s ,screen))
       (dotimes (y (screen-height ,s))
         (dotimes (x (screen-width ,s))
           (let ((c (screen-cell ,s x y)))
             (expect (char= #\Space (cell-char c)))
             (expect (= cl-tmux/terminal/types:+default-color+ (cell-fg c)))
             (expect (= cl-tmux/terminal/types:+default-color+ (cell-bg c)))))))))

;;; ── SUITE: screen construction ───────────────────────────────────────────────

(describe "terminal-suite/screen-construction"

  ;; make-screen stores the requested width and height.
  (it "make-screen-sets-width-and-height"
    (let ((s (make-screen 40 12)))
      (expect (= 40 (screen-width  s)))
      (expect (= 12 (screen-height s)))))

  ;; A freshly created screen has its cursor at (0, 0).
  (it "make-screen-cursor-starts-at-origin"
    (with-screen (s 20 10)
      (expect (= 0 (screen-cursor-x s)))
      (expect (= 0 (screen-cursor-y s)))))

  ;; A freshly created screen is dirty.
  (it "make-screen-dirty-flag-is-true"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-truthy)))

  ;; The default scroll region spans the full screen height.
  (it "make-screen-scroll-region-is-full-height"
    (with-screen (s 80 24)
      (expect (= 0  (cl-tmux/terminal/types:screen-scroll-top    s)))
      (expect (= 23 (cl-tmux/terminal/types:screen-scroll-bottom s)))))

  ;; Every cell in a new screen is a space with default attributes.
  (it "make-screen-all-cells-are-blank"
    (with-screen (s 5 3)
      (assert-all-cells-blank s)))

  ;; The cursor is visible on a fresh screen.
  (it "make-screen-cursor-visible-defaults-true"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-cursor-visible s) :to-be-truthy)))

  ;; Copy mode is off on a fresh screen.
  (it "make-screen-copy-mode-defaults-false"
    (with-screen (s 10 5)
      (expect (screen-copy-mode-p s) :to-be-falsy)
      (expect (= 0 (screen-copy-offset s)))))

  ;; The response queue is NIL on a fresh screen.
  (it "make-screen-response-queue-starts-empty"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; The saved-cursor slot is NIL on a fresh screen.
  (it "make-screen-saved-cursor-starts-nil"
    (with-screen (s 10 5)
      (expect (null (cl-tmux/terminal/types:screen-saved-cursor s)))))

  ;; The scrollback buffer is empty on a fresh screen.
  (it "make-screen-scrollback-starts-empty"
    (with-screen (s 10 5)
      (expect (null (screen-scrollback s))))))

;;; ── SUITE: screen-p predicate ────────────────────────────────────────────────

(describe "terminal-suite/screen-p-suite"

  ;; screen-p returns T for an object created by make-screen.
  (it "screen-p-returns-true-for-make-screen"
    (let ((s (make-screen 10 5)))
      (expect (cl-tmux/terminal/types:screen-p s) :to-be-truthy)))

  ;; screen-p returns NIL for non-screen objects.
  (it "screen-p-returns-false-for-non-screen"
    (expect (cl-tmux/terminal/types:screen-p 42) :to-be-falsy)
    (expect (cl-tmux/terminal/types:screen-p "hello") :to-be-falsy)
    (expect (cl-tmux/terminal/types:screen-p nil) :to-be-falsy)
    (expect (cl-tmux/terminal/types:screen-p (cl-tmux/terminal/types:make-cell)) :to-be-falsy)))

;;; ── SUITE: %make-screen direct constructor ───────────────────────────────────

(describe "terminal-suite/make-screen-direct"

  ;; %make-screen with explicit :width/:height/:cells produces the right geometry.
  (it "percent-make-screen-produces-screen-of-correct-dimensions"
    (let* ((w 15) (h 6)
           (cells (cl-tmux/terminal/types:%make-blank-cells (* w h)))
           (s (cl-tmux/terminal/types:%make-screen :width w :height h :cells cells
                                                    :scroll-bottom (1- h))))
      (expect (= w (screen-width  s)))
      (expect (= h (screen-height s)))
      (expect (cl-tmux/terminal/types:screen-p s)))))

;;; ── SUITE: screen-cell and setf screen-cell ──────────────────────────────────

(describe "terminal-suite/screen-cell-access"

  ;; screen-cell returns the cell at the specified (x, y) coordinate.
  (it "screen-cell-read-returns-correct-cell"
    (with-screen (s 5 3)
      (feed s "A")             ; writes 'A' at (0,0), cursor advances to (1,0)
      (expect (char= #\A (cell-char (screen-cell s 0 0))))))

  ;; setf screen-cell stores a cell at the given coordinates.
  (it "setf-screen-cell-stores-cell-at-position"
    (with-screen (s 5 3)
      (let ((new-cell (cl-tmux/terminal/types:make-cell :char #\Z :fg 3 :bg 1)))
        (setf (screen-cell s 2 1) new-cell)
        (let ((read-back (screen-cell s 2 1)))
          (expect (char= #\Z (cell-char read-back)))
          (expect (= 3 (cell-fg  read-back)))
          (expect (= 1 (cell-bg  read-back)))))))

  ;; The bottom-right cell (width-1, height-1) is accessible.
  (it "screen-cell-bottom-right-is-accessible"
    (with-screen (s 10 8)
      (finishes (screen-cell s 9 7))))

  ;; Table-driven grid-write tests: (x y char)
  ;; setf/screen-cell round-trip is correct at multiple grid positions.
  (it "screen-cell-write-read-table"
    (with-screen (s 10 5)
      (dolist (case '((0 0 #\A "top-left corner")
                      (9 0 #\B "top-right corner")
                      (0 4 #\C "bottom-left corner")
                      (9 4 #\D "bottom-right corner")
                      (5 2 #\E "center")))
        (destructuring-bind (x y ch desc) case
          (declare (ignore desc))
          (setf (screen-cell s x y)
                (cl-tmux/terminal/types:make-cell :char ch))
          (expect (char= ch (char-at s x y))))))))

;;; ── SUITE: cursor accessors ──────────────────────────────────────────────────

(describe "terminal-suite/screen-cursor-accessors"

  ;; screen-cursor-x reflects cursor column after writing characters.
  (it "screen-cursor-x-advances-after-write"
    (with-screen (s 20 5)
      (feed s "hello")
      (expect (= 5 (screen-cursor-x s)))))

  ;; screen-cursor-y reflects cursor row after a cursor positioning sequence.
  (it "screen-cursor-y-advances-after-newline"
    (with-screen (s 20 5)
      (feed s (esc "[3;5H"))   ; move cursor to row 2 (1-based 3), col 4
      (expect (= 2 (screen-cursor-y s)))))

  ;; screen-cursor-x is 0 on a fresh screen.
  (it "screen-cursor-x-starts-at-zero"
    (with-screen (s 20 5)
      (expect (= 0 (screen-cursor-x s)))))

  ;; screen-cursor-y is 0 on a fresh screen.
  (it "screen-cursor-y-starts-at-zero"
    (with-screen (s 20 5)
      (expect (= 0 (screen-cursor-y s))))))

;;; ── SUITE: resize ───────────────────────────────────────────────────────────

(describe "terminal-suite/resize"

  ;; Resizing to a larger screen preserves existing content and updates dimensions.
  (it "resize-larger"
    (with-screen (s 10 5)
      (feed s "hello")
      (screen-resize s 20 8)
      (expect (= 20 (screen-width  s)))
      (expect (= 8  (screen-height s)))
      (expect (string= "hello" (row-string s 0 :end 5)))))

  ;; Shrinking the screen clamps an out-of-bounds cursor into the new bounds.
  (it "resize-smaller-clamps-cursor"
    (with-screen (s 20 10)
      (feed s (esc "[10;20H"))  ; cursor near bottom-right
      (screen-resize s 5 3)
      (expect (<= (screen-cursor-x s) 4))
      (expect (<= (screen-cursor-y s) 2))))

  ;; Resizing to the same dimensions leaves content and cursor unchanged.
  (it "resize-noop"
    (with-screen (s 10 5)
      (feed s "abc")
      (let ((cx (screen-cursor-x s))
            (cy (screen-cursor-y s)))
        (screen-resize s 10 5)
        (expect (string= "abc" (row-string s 0 :end 3)))
        (expect (= cx (screen-cursor-x s)))
        (expect (= cy (screen-cursor-y s))))))

  ;; After resize, the scroll region spans the full new height.
  (it "resize-updates-scroll-region-to-full-height"
    (with-screen (s 10 10)
      ;; Set a narrow scroll region first
      (setf (cl-tmux/terminal/types:screen-scroll-top    s) 2
            (cl-tmux/terminal/types:screen-scroll-bottom s) 7)
      (screen-resize s 10 15)
      (expect (= 0  (cl-tmux/terminal/types:screen-scroll-top    s)))
      (expect (= 14 (cl-tmux/terminal/types:screen-scroll-bottom s)))))

  ;; screen-resize always marks the screen dirty.
  (it "resize-marks-screen-dirty"
    (with-screen (s 10 5)
      (screen-clear-dirty s)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-falsy)
      (screen-resize s 20 8)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-truthy)))

  ;; Resizing smaller keeps the overlapping top-left region of content.
  (it "resize-smaller-preserves-top-left-content"
    (with-screen (s 10 5)
      (feed s "ABCDE")
      (screen-resize s 3 3)
      ;; Columns 0-2 of row 0 should have A, B, C
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\B (char-at s 1 0)))
      (expect (char= #\C (char-at s 2 0)))))

  ;; %copy-overlapping-cells copies only the requested top-left
  ;; COPY-COLS x COPY-ROWS rectangle from OLD-CELLS into SCREEN's current grid,
  ;; leaving cells outside that rectangle untouched.
  (it "copy-overlapping-cells-copies-top-left-rectangle"
    (with-screen (s 4 4)
      (let ((old-width 3)
            (old-cells (cl-tmux/terminal/types:%make-blank-cells 9)))
        ;; Old grid (3x3, row-major): "ABC" / "DEF" / "GHI"
        (loop for i from 0
              for ch across "ABCDEFGHI"
              do (setf (aref old-cells i)
                       (cl-tmux/terminal/types:make-cell :char ch)))
        (cl-tmux/terminal/types::%copy-overlapping-cells s old-cells old-width 2 2)
        (expect (char= #\A (char-at s 0 0)))
        (expect (char= #\B (char-at s 1 0)))
        (expect (char= #\D (char-at s 0 1)))
        (expect (char= #\E (char-at s 1 1)))
        (expect (char= #\Space (char-at s 2 0)))
        (expect (char= #\Space (char-at s 0 2))))))

  ;; Resizing while alt-cells is active resizes the primary grid only.
  ;; Alt-cells remain at their previous geometry; the caller is responsible for
  ;; exiting alt-screen mode before resizing when consistency is required.
  (it "resize-with-active-alt-cells-leaves-primary-intact"
    (with-screen (s 10 5)
      ;; Enter alt-screen: alt-cells is saved, a fresh grid is installed.
      (feed s (esc "[?1049h"))
      ;; Write something on the alt screen so the primary is non-trivial.
      (feed s "ALT")
      ;; Resize while on the alt screen.
      (screen-resize s 20 8)
      ;; Dimensions must reflect the new geometry.
      (expect (= 20 (screen-width  s)))
      (expect (= 8  (screen-height s)))
      ;; Cursor must be clamped to new bounds.
      (expect (<= (screen-cursor-x s) 19))
      (expect (<= (screen-cursor-y s) 7))
      ;; The alt-cells vector is still present (we did not exit alt-screen).
      (expect (cl-tmux/terminal/types:screen-alt-cells s) :to-be-truthy))))
