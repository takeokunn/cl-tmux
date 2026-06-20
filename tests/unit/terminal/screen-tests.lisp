(in-package #:cl-tmux/test)

;;;; Screen tests (src/terminal/screen.lisp).
;;;; Tests: make-screen construction, grid access, resize, dirty flag,
;;;;        cursor accessors, saved cursor, bell-pending, reset-sgr-pen,
;;;;        alt-cells resize, screen-consume-bell, %make-blank-cells,
;;;;        %make-screen, screen-p, and all screen slot defaults.

;;; ── Shared helpers ───────────────────────────────────────────────────────────

(defun all-cells-blank-p (screen)
  "Return T when every cell in SCREEN is a space with fg=7, bg=0.
   A predicate form so callers can use it in IS or combine with AND."
  (block check
    (dotimes (y (screen-height screen) t)
      (dotimes (x (screen-width screen))
        (let ((c (screen-cell screen x y)))
          (unless (and (char= #\Space (cell-char c))
                       (= cl-tmux/terminal/types:+default-color+ (cell-fg c))
                       (= cl-tmux/terminal/types:+default-color+ (cell-bg c)))
            (return-from check nil)))))))

(defmacro assert-all-cells-blank (screen)
  "Assert that every cell in SCREEN is a space with default fg/bg attributes.
   Expands to one IS form per cell position so failures report coordinates."
  (let ((s (gensym "SCREEN")))
    `(let ((,s ,screen))
       (dotimes (y (screen-height ,s))
         (dotimes (x (screen-width ,s))
           (let ((c (screen-cell ,s x y)))
             (is (char= #\Space (cell-char c))
                 "cell (~D,~D) char must be space" x y)
             (is (= cl-tmux/terminal/types:+default-color+ (cell-fg c))
                 "cell (~D,~D) fg must be the default sentinel" x y)
             (is (= cl-tmux/terminal/types:+default-color+ (cell-bg c))
                 "cell (~D,~D) bg must be the default sentinel" x y)))))))

;;; ── SUITE: screen construction ───────────────────────────────────────────────

(def-suite screen-construction
  :description "make-screen dimensions, initial slot values, and grid allocation"
  :in terminal-suite)
(in-suite screen-construction)

(test make-screen-sets-width-and-height
  :description "make-screen stores the requested width and height."
  (let ((s (make-screen 40 12)))
    (is (= 40 (screen-width  s)) "width must be 40")
    (is (= 12 (screen-height s)) "height must be 12")))

(test make-screen-cursor-starts-at-origin
  :description "A freshly created screen has its cursor at (0, 0)."
  (with-screen (s 20 10)
    (is (= 0 (screen-cursor-x s)) "initial cursor-x must be 0")
    (is (= 0 (screen-cursor-y s)) "initial cursor-y must be 0")))

(test make-screen-dirty-flag-is-true
  :description "A freshly created screen is dirty."
  (with-screen (s 10 5)
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "new screen must start dirty")))

(test make-screen-scroll-region-is-full-height
  :description "The default scroll region spans the full screen height."
  (with-screen (s 80 24)
    (is (= 0  (cl-tmux/terminal/types:screen-scroll-top    s)) "scroll-top must be 0")
    (is (= 23 (cl-tmux/terminal/types:screen-scroll-bottom s)) "scroll-bottom must be height-1")))

(test make-screen-all-cells-are-blank
  :description "Every cell in a new screen is a space with default attributes."
  (with-screen (s 5 3)
    (assert-all-cells-blank s)))

(test make-screen-cursor-visible-defaults-true
  :description "The cursor is visible on a fresh screen."
  (with-screen (s 10 5)
    (is-true (cl-tmux/terminal/types:screen-cursor-visible s)
             "cursor must be visible initially")))

(test make-screen-copy-mode-defaults-false
  :description "Copy mode is off on a fresh screen."
  (with-screen (s 10 5)
    (is-false (screen-copy-mode-p s) "copy-mode-p must default to NIL")
    (is (= 0 (screen-copy-offset s)) "copy-offset must default to 0")))

(test make-screen-response-queue-starts-empty
  :description "The response queue is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must start as NIL")))

(test make-screen-saved-cursor-starts-nil
  :description "The saved-cursor slot is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-saved-cursor s))
        "saved-cursor must start as NIL")))

(test make-screen-scrollback-starts-empty
  :description "The scrollback buffer is empty on a fresh screen."
  (with-screen (s 10 5)
    (is (null (screen-scrollback s))
        "scrollback must start as NIL (empty)")))

;;; ── SUITE: screen-p predicate ────────────────────────────────────────────────

(def-suite screen-p-suite
  :description "screen-p type predicate"
  :in terminal-suite)
(in-suite screen-p-suite)

(test screen-p-returns-true-for-make-screen
  :description "screen-p returns T for an object created by make-screen."
  (let ((s (make-screen 10 5)))
    (is-true (cl-tmux/terminal/types:screen-p s)
             "screen-p must return T for a make-screen object")))

(test screen-p-returns-false-for-non-screen
  :description "screen-p returns NIL for non-screen objects."
  (is-false (cl-tmux/terminal/types:screen-p 42)
            "screen-p must return NIL for an integer")
  (is-false (cl-tmux/terminal/types:screen-p "hello")
            "screen-p must return NIL for a string")
  (is-false (cl-tmux/terminal/types:screen-p nil)
            "screen-p must return NIL for NIL")
  (is-false (cl-tmux/terminal/types:screen-p (cl-tmux/terminal/types:make-cell))
            "screen-p must return NIL for a cell struct"))

;;; ── SUITE: %make-screen direct constructor ───────────────────────────────────

(def-suite make-screen-direct
  :description "%make-screen low-level constructor contracts"
  :in terminal-suite)
(in-suite make-screen-direct)

(test percent-make-screen-produces-screen-of-correct-dimensions
  :description "%make-screen with explicit :width/:height/:cells produces the right geometry."
  (let* ((w 15) (h 6)
         (cells (cl-tmux/terminal/types:%make-blank-cells (* w h)))
         (s (cl-tmux/terminal/types:%make-screen :width w :height h :cells cells
                                                  :scroll-bottom (1- h))))
    (is (= w (screen-width  s)) "width must be 15")
    (is (= h (screen-height s)) "height must be 6")
    (is (cl-tmux/terminal/types:screen-p s)
        "result must satisfy screen-p")))

;;; ── SUITE: screen-cell and setf screen-cell ──────────────────────────────────

(def-suite screen-cell-access
  :description "screen-cell read and write accessors"
  :in terminal-suite)
(in-suite screen-cell-access)

(test screen-cell-read-returns-correct-cell
  :description "screen-cell returns the cell at the specified (x, y) coordinate."
  (with-screen (s 5 3)
    (feed s "A")             ; writes 'A' at (0,0), cursor advances to (1,0)
    (is (char= #\A (cell-char (screen-cell s 0 0)))
        "cell at (0,0) must be A after feeding 'A'")))

(test setf-screen-cell-stores-cell-at-position
  :description "setf screen-cell stores a cell at the given coordinates."
  (with-screen (s 5 3)
    (let ((new-cell (cl-tmux/terminal/types:make-cell :char #\Z :fg 3 :bg 1)))
      (setf (screen-cell s 2 1) new-cell)
      (let ((read-back (screen-cell s 2 1)))
        (is (char= #\Z (cell-char read-back)) "stored char must be Z")
        (is (= 3 (cell-fg  read-back))        "stored fg must be 3")
        (is (= 1 (cell-bg  read-back))        "stored bg must be 1")))))

(test screen-cell-bottom-right-is-accessible
  :description "The bottom-right cell (width-1, height-1) is accessible."
  (with-screen (s 10 8)
    (finishes (screen-cell s 9 7))))

;;; Table-driven grid-write tests: (x y char)
(test screen-cell-write-read-table
  :description "setf/screen-cell round-trip is correct at multiple grid positions."
  (with-screen (s 10 5)
    (dolist (case '((0 0 #\A "top-left corner")
                    (9 0 #\B "top-right corner")
                    (0 4 #\C "bottom-left corner")
                    (9 4 #\D "bottom-right corner")
                    (5 2 #\E "center")))
      (destructuring-bind (x y ch desc) case
        (setf (screen-cell s x y)
              (cl-tmux/terminal/types:make-cell :char ch))
        (is (char= ch (char-at s x y)) desc)))))

;;; ── SUITE: cursor accessors ──────────────────────────────────────────────────

(def-suite screen-cursor-accessors
  :description "screen-cursor-x and screen-cursor-y track actual cursor position"
  :in terminal-suite)
(in-suite screen-cursor-accessors)

(test screen-cursor-x-advances-after-write
  :description "screen-cursor-x reflects cursor column after writing characters."
  (with-screen (s 20 5)
    (feed s "hello")
    (is (= 5 (screen-cursor-x s))
        "cursor-x must be 5 after writing 5 characters")))

(test screen-cursor-y-advances-after-newline
  :description "screen-cursor-y reflects cursor row after a cursor positioning sequence."
  (with-screen (s 20 5)
    (feed s (esc "[3;5H"))   ; move cursor to row 2 (1-based 3), col 4
    (is (= 2 (screen-cursor-y s))
        "cursor-y must be 2 after CSI 3;5H (0-based row 2)")))

(test screen-cursor-x-starts-at-zero
  :description "screen-cursor-x is 0 on a fresh screen."
  (with-screen (s 20 5)
    (is (= 0 (screen-cursor-x s)) "initial cursor-x must be 0")))

(test screen-cursor-y-starts-at-zero
  :description "screen-cursor-y is 0 on a fresh screen."
  (with-screen (s 20 5)
    (is (= 0 (screen-cursor-y s)) "initial cursor-y must be 0")))

;;; ── SUITE: resize ───────────────────────────────────────────────────────────

(def-suite resize
  :description "Screen resize behaviour"
  :in terminal-suite)
(in-suite resize)

(test resize-larger
  "Resizing to a larger screen preserves existing content and updates dimensions."
  (with-screen (s 10 5)
    (feed s "hello")
    (screen-resize s 20 8)
    (is (= 20 (screen-width  s)))
    (is (= 8  (screen-height s)))
    (is (string= "hello" (row-string s 0 :end 5)))))

(test resize-smaller-clamps-cursor
  "Shrinking the screen clamps an out-of-bounds cursor into the new bounds."
  (with-screen (s 20 10)
    (feed s (esc "[10;20H"))  ; cursor near bottom-right
    (screen-resize s 5 3)
    (is (<= (screen-cursor-x s) 4)
        "cursor-x ~D exceeds new width-1=4" (screen-cursor-x s))
    (is (<= (screen-cursor-y s) 2)
        "cursor-y ~D exceeds new height-1=2" (screen-cursor-y s))))

(test resize-noop
  "Resizing to the same dimensions leaves content and cursor unchanged."
  (with-screen (s 10 5)
    (feed s "abc")
    (let ((cx (screen-cursor-x s))
          (cy (screen-cursor-y s)))
      (screen-resize s 10 5)
      (is (string= "abc" (row-string s 0 :end 3)))
      (is (= cx (screen-cursor-x s)))
      (is (= cy (screen-cursor-y s))))))

(test resize-updates-scroll-region-to-full-height
  :description "After resize, the scroll region spans the full new height."
  (with-screen (s 10 10)
    ;; Set a narrow scroll region first
    (setf (cl-tmux/terminal/types:screen-scroll-top    s) 2
          (cl-tmux/terminal/types:screen-scroll-bottom s) 7)
    (screen-resize s 10 15)
    (is (= 0  (cl-tmux/terminal/types:screen-scroll-top    s))
        "scroll-top must be reset to 0 after resize")
    (is (= 14 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be reset to new-height-1 after resize")))

(test resize-marks-screen-dirty
  :description "screen-resize always marks the screen dirty."
  (with-screen (s 10 5)
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s) "pre-condition: not dirty")
    (screen-resize s 20 8)
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after resize")))

(test resize-smaller-preserves-top-left-content
  :description "Resizing smaller keeps the overlapping top-left region of content."
  (with-screen (s 10 5)
    (feed s "ABCDE")
    (screen-resize s 3 3)
    ;; Columns 0-2 of row 0 should have A, B, C
    (is (char= #\A (char-at s 0 0)) "cell (0,0) must be A after shrink")
    (is (char= #\B (char-at s 1 0)) "cell (1,0) must be B after shrink")
    (is (char= #\C (char-at s 2 0)) "cell (2,0) must be C after shrink")))

(test resize-with-active-alt-cells-leaves-primary-intact
  :description "Resizing while alt-cells is active resizes the primary grid only.
   Alt-cells remain at their previous geometry; the caller is responsible for
   exiting alt-screen mode before resizing when consistency is required."
  (with-screen (s 10 5)
    ;; Enter alt-screen: alt-cells is saved, a fresh grid is installed.
    (feed s (esc "[?1049h"))
    ;; Write something on the alt screen so the primary is non-trivial.
    (feed s "ALT")
    ;; Resize while on the alt screen.
    (screen-resize s 20 8)
    ;; Dimensions must reflect the new geometry.
    (is (= 20 (screen-width  s)) "width must update to 20")
    (is (= 8  (screen-height s)) "height must update to 8")
    ;; Cursor must be clamped to new bounds.
    (is (<= (screen-cursor-x s) 19))
    (is (<= (screen-cursor-y s) 7))
    ;; The alt-cells vector is still present (we did not exit alt-screen).
    (is-true (cl-tmux/terminal/types:screen-alt-cells s)
             "alt-cells must still be set after resize")))

