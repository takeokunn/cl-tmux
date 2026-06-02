(in-package #:cl-tmux/test)

;;;; Screen tests (src/terminal/screen.lisp).
;;;; Tests: make-screen construction, grid access, resize, dirty flag,
;;;;        cursor wrappers, saved cursor, bell-pending, and screen slots.

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
    (dotimes (y 3)
      (dotimes (x 5)
        (let ((c (screen-cell s x y)))
          (is (char= #\Space (cell-char c))
              "cell (~D,~D) char must be space" x y)
          (is (= 7 (cell-fg c))
              "cell (~D,~D) fg must be 7" x y)
          (is (= 0 (cell-bg c))
              "cell (~D,~D) bg must be 0" x y))))))

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

;;; ── SUITE: cursor wrappers ───────────────────────────────────────────────────

(def-suite screen-cursor-wrappers
  :description "screen-cursor-x and screen-cursor-y wrappers over screen-cx/cy"
  :in terminal-suite)
(in-suite screen-cursor-wrappers)

(test screen-cursor-x-reflects-screen-cx
  :description "screen-cursor-x returns the same value as screen-cx."
  (with-screen (s 20 5)
    (feed s "hello")
    (is (= (cl-tmux/terminal/types:screen-cx s) (screen-cursor-x s))
        "screen-cursor-x must equal screen-cx")))

(test screen-cursor-y-reflects-screen-cy
  :description "screen-cursor-y returns the same value as screen-cy."
  (with-screen (s 20 5)
    (feed s (esc "[3;5H"))   ; move cursor to row 2 (1-based 3), col 4
    (is (= (cl-tmux/terminal/types:screen-cy s) (screen-cursor-y s))
        "screen-cursor-y must equal screen-cy")))

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

;;; ── screen-clear-dirty ───────────────────────────────────────────────────────

(test screen-clear-dirty-resets-flag
  "screen-clear-dirty sets screen-dirty-p to NIL."
  (with-screen (s 10 5)
    ;; A freshly created screen starts dirty.
    (is-true (cl-tmux/terminal/types:screen-dirty-p s) "new screen is dirty")
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must be NIL after screen-clear-dirty")))

(test screen-clear-dirty-idempotent
  :description "Calling screen-clear-dirty twice leaves the flag NIL."
  (with-screen (s 10 5)
    (screen-clear-dirty s)
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must remain NIL after second screen-clear-dirty")))

;;; ── bell-pending slot ────────────────────────────────────────────────────────

(def-suite bell-pending-suite
  :description "screen-bell-pending slot: default value, set/clear"
  :in terminal-suite)
(in-suite bell-pending-suite)

(test bell-pending-default-is-nil
  "A fresh screen has bell-pending NIL."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL on a fresh screen")))

(test bell-pending-can-be-set-and-cleared
  "screen-bell-pending can be toggled via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after setf t")
    (setf (cl-tmux/terminal/types:screen-bell-pending s) nil)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after setf nil")))

(test bel-byte-sets-bell-pending
  :description "Feeding a BEL byte (0x07) via screen-process-bytes sets bell-pending."
  (with-screen (s 10 5)
    (screen-clear-dirty s)
    ;; Feed BEL (7) directly
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(7)))
    (is-true (cl-tmux/terminal/types:screen-bell-pending s)
             "bell-pending must be T after feeding BEL byte")))

;;; ── SUITE: miscellaneous screen slots ────────────────────────────────────────

(def-suite screen-slots
  :description "Miscellaneous screen slot defaults and setf contracts"
  :in terminal-suite)
(in-suite screen-slots)

(test screen-last-char-starts-nil
  :description "screen-last-char is NIL until a character is written."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-last-char s))
        "last-char must be NIL on a fresh screen")))

(test screen-last-char-updated-after-write
  :description "screen-last-char holds the most recently written character."
  (with-screen (s 10 5)
    (feed s "Z")
    (is (char= #\Z (cl-tmux/terminal/types:screen-last-char s))
        "last-char must be Z after feeding 'Z'")))

(test screen-charset-defaults-to-ascii
  :description "A fresh screen uses the :ascii character set."
  (with-screen (s 10 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must default to :ascii")))

(test screen-autowrap-defaults-true
  :description "Auto-wrap mode is enabled on a fresh screen."
  (with-screen (s 10 5)
    (is-true (cl-tmux/terminal/types:screen-autowrap s)
             "autowrap must default to T")))

(test screen-cursor-shape-defaults-to-1
  :description "The cursor shape starts at 1 (block blink)."
  (with-screen (s 10 5)
    (is (= 1 (cl-tmux/terminal/types:screen-cursor-shape s))
        "cursor-shape must default to 1")))

(test screen-bracketed-paste-defaults-false
  :description "Bracketed paste mode is off by default."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bracketed-paste s)
              "bracketed-paste must default to NIL")))

(test screen-app-cursor-keys-defaults-false
  :description "Application cursor keys mode is off by default."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-app-cursor-keys s)
              "app-cursor-keys must default to NIL")))

(test screen-title-defaults-empty-string
  :description "The window title slot starts as an empty string."
  (with-screen (s 10 5)
    (is (string= "" (screen-title s))
        "title must default to empty string")))

(test screen-mouse-mode-defaults-zero
  :description "Mouse reporting mode starts at 0 (off)."
  (with-screen (s 10 5)
    (is (= 0 (screen-mouse-mode s))
        "mouse-mode must default to 0")))

(test screen-copy-mark-defaults-nil
  :description "Copy-mode mark starts as NIL (no active selection)."
  (with-screen (s 10 5)
    (is (null (screen-copy-mark s))
        "copy-mark must start as NIL")))
