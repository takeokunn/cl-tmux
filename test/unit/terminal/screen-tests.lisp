(in-package #:cl-tmux/test)

;;;; Screen tests (src/terminal/screen.lisp).
;;;; Tests: make-screen construction, grid access, resize, dirty flag,
;;;;        cursor accessors, saved cursor, bell-pending, reset-sgr-pen,
;;;;        alt-cells resize, screen-consume-bell, %make-blank-cells,
;;;;        %make-screen, screen-p, and all screen slot defaults.

;;; ── Shared helper ────────────────────────────────────────────────────────────

(defun assert-all-cells-blank (screen)
  "Assert that every cell in SCREEN is a space with default fg/bg attributes.
   Used by construction tests; eliminates repeated per-cell IS forms."
  (dotimes (y (screen-height screen))
    (dotimes (x (screen-width screen))
      (let ((c (screen-cell screen x y)))
        (is (char= #\Space (cell-char c))
            "cell (~D,~D) char must be space" x y)
        (is (= 7 (cell-fg c))
            "cell (~D,~D) fg must be 7" x y)
        (is (= 0 (cell-bg c))
            "cell (~D,~D) bg must be 0" x y)))))

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

;;; ── reset-sgr-pen ────────────────────────────────────────────────────────────

(def-suite reset-sgr-pen-suite
  :description "reset-sgr-pen: direct unit tests for all five SGR pen slots"
  :in terminal-suite)
(in-suite reset-sgr-pen-suite)

(test reset-sgr-pen-restores-all-five-slots
  :description "reset-sgr-pen sets all five SGR pen fields to VT100 defaults."
  (with-screen (s 10 5)
    ;; Dirty all five pen slots.
    (setf (cl-tmux/terminal/types:screen-cur-fg       s) 3
          (cl-tmux/terminal/types:screen-cur-bg       s) 4
          (cl-tmux/terminal/types:screen-cur-attrs    s) #b11111111
          (cl-tmux/terminal/types:screen-cur-attrs2   s) #b00000011
          (cl-tmux/terminal/types:screen-cur-ul-color s) 200)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg       s)) "fg must reset to 7")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg       s)) "bg must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs    s)) "attrs must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs2   s)) "attrs2 must reset to 0")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s)) "ul-color must reset to 0")))

(test reset-sgr-pen-idempotent
  :description "Calling reset-sgr-pen twice leaves pen in the default state."
  (with-screen (s 10 5)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg s)) "double-reset fg must be 7")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg s)) "double-reset bg must be 0")))

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

;;; ── screen-consume-bell ──────────────────────────────────────────────────────

(def-suite screen-consume-bell-suite
  :description "screen-consume-bell: consume and clear bell-pending atomically"
  :in terminal-suite)
(in-suite screen-consume-bell-suite)

(test screen-consume-bell-returns-nil-when-no-bell-pending
  :description "screen-consume-bell returns NIL and has no side effect when bell is not pending."
  (with-screen (s 10 5)
    ;; Fresh screen has no bell pending.
    (is-false (cl-tmux/terminal/types:screen-consume-bell s)
              "consume-bell must return NIL when bell is not pending")
    ;; Flag must still be NIL.
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must remain NIL after consume on no-bell screen")))

(test screen-consume-bell-returns-true-and-clears-flag
  :description "screen-consume-bell returns T and clears bell-pending when a bell is pending."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "consume-bell must return T when bell is pending")
    ;; Flag must be cleared now.
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after consume-bell clears it")))

(test screen-consume-bell-idempotent-after-clear
  :description "Calling screen-consume-bell twice returns T then NIL."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    ;; First call: consumes the bell.
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "first consume-bell must return T")
    ;; Second call: no bell pending.
    (is-false (cl-tmux/terminal/types:screen-consume-bell s)
              "second consume-bell must return NIL")))

(test screen-consume-bell-after-bel-byte
  :description "screen-consume-bell clears the flag set by a real BEL byte."
  (with-screen (s 10 5)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(7)))
    ;; Bell should be pending now.
    (is-true (cl-tmux/terminal/types:screen-bell-pending s)
             "pre-condition: bell-pending must be T after BEL byte")
    ;; Consume it.
    (is-true (cl-tmux/terminal/types:screen-consume-bell s)
             "consume-bell must return T")
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after consume")))

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

;;; ── SUITE: copy-mode extra slots ─────────────────────────────────────────────

(def-suite copy-mode-slots
  :description "copy-mode selection, cursor, search-term, and line-selection slots"
  :in terminal-suite)
(in-suite copy-mode-slots)

(test screen-copy-cursor-defaults-nil
  :description "copy-cursor slot is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (screen-copy-cursor s))
        "copy-cursor must start as NIL")))

(test screen-copy-selecting-defaults-false
  :description "copy-selecting flag is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (screen-copy-selecting s)
              "copy-selecting must default to NIL")))

(test screen-copy-search-term-defaults-nil
  :description "copy-search-term slot is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-copy-search-term s))
        "copy-search-term must start as NIL")))

(test screen-copy-line-selection-p-defaults-false
  :description "copy-line-selection-p flag is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-copy-line-selection-p s)
              "copy-line-selection-p must default to NIL")))

(test screen-copy-cursor-can-be-set
  :description "copy-cursor can be set to a (row . col) pair via setf."
  (with-screen (s 10 5)
    (setf (screen-copy-cursor s) (list 2 3))
    (is (equal '(2 3) (screen-copy-cursor s))
        "copy-cursor must hold the value after setf")))

(test screen-copy-selecting-can-be-toggled
  :description "copy-selecting can be set and cleared via setf."
  (with-screen (s 10 5)
    (setf (screen-copy-selecting s) t)
    (is-true (screen-copy-selecting s)
             "copy-selecting must be T after setf T")
    (setf (screen-copy-selecting s) nil)
    (is-false (screen-copy-selecting s)
              "copy-selecting must be NIL after setf NIL")))

(test screen-copy-search-term-can-be-set
  :description "copy-search-term can hold an arbitrary search string."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-copy-search-term s) "hello")
    (is (string= "hello" (cl-tmux/terminal/types:screen-copy-search-term s))
        "copy-search-term must hold the stored string")))

;;; ── SUITE: alt-screen cursor save slots ─────────────────────────────────────

(def-suite alt-screen-slots
  :description "Alt-screen cursor save/restore slot defaults"
  :in terminal-suite)
(in-suite alt-screen-slots)

(test screen-alt-cursor-x-defaults-zero
  :description "alt-cursor-x slot starts at 0 on a fresh screen."
  (with-screen (s 10 5)
    (is (= 0 (cl-tmux/terminal/types:screen-alt-cursor-x s))
        "alt-cursor-x must default to 0")))

(test screen-alt-cursor-y-defaults-zero
  :description "alt-cursor-y slot starts at 0 on a fresh screen."
  (with-screen (s 10 5)
    (is (= 0 (cl-tmux/terminal/types:screen-alt-cursor-y s))
        "alt-cursor-y must default to 0")))

(test screen-alt-cells-defaults-nil
  :description "alt-cells slot is NIL before entering alt-screen mode."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-alt-cells s))
        "alt-cells must be NIL on a fresh screen")))

(test screen-alt-cursor-saved-on-alt-screen-entry
  :description "Entering alt-screen via ESC[?1049h saves cursor position into alt-cursor-x/y."
  (with-screen (s 20 10)
    (feed s (esc "[5;10H"))   ; move cursor to (row=4, col=9)
    (feed s (esc "[?1049h"))  ; enter alt screen
    ;; After entering alt-screen, alt-cursor-x/y should hold the saved cursor.
    (is (= 9 (cl-tmux/terminal/types:screen-alt-cursor-x s))
        "alt-cursor-x must be saved column 9")
    (is (= 4 (cl-tmux/terminal/types:screen-alt-cursor-y s))
        "alt-cursor-y must be saved row 4")))

;;; ── SUITE: mouse-sgr-mode slot ───────────────────────────────────────────────

(def-suite mouse-sgr-mode-suite
  :description "screen-mouse-sgr-mode slot default and toggle"
  :in terminal-suite)
(in-suite mouse-sgr-mode-suite)

(test screen-mouse-sgr-mode-defaults-false
  :description "mouse-sgr-mode is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is-false (screen-mouse-sgr-mode s)
              "mouse-sgr-mode must default to NIL")))

(test screen-mouse-sgr-mode-enabled-by-1006h
  :description "ESC[?1006h enables SGR extended mouse encoding."
  (with-screen (s 10 5)
    (feed s (esc "[?1006h"))
    (is-true (screen-mouse-sgr-mode s)
             "mouse-sgr-mode must be T after ESC[?1006h")))

(test screen-mouse-sgr-mode-disabled-by-1006l
  :description "ESC[?1006l disables SGR extended mouse encoding."
  (with-screen (s 10 5)
    (feed s (esc "[?1006h"))
    (feed s (esc "[?1006l"))
    (is-false (screen-mouse-sgr-mode s)
              "mouse-sgr-mode must be NIL after ESC[?1006l")))

;;; ── SUITE: response-queue ────────────────────────────────────────────────────

(def-suite response-queue-suite
  :description "screen-response-queue: push and drain behaviour"
  :in terminal-suite)
(in-suite response-queue-suite)

(test response-queue-starts-nil
  :description "Response queue is NIL on a fresh screen."
  (with-screen (s 10 5)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must be NIL initially")))

(test response-queue-can-be-pushed-and-drained
  :description "Items pushed onto the response-queue can be nreversed to drain in order."
  (with-screen (s 10 5)
    (push "response-a" (cl-tmux/terminal/types:screen-response-queue s))
    (push "response-b" (cl-tmux/terminal/types:screen-response-queue s))
    (let ((items (nreverse (cl-tmux/terminal/types:screen-response-queue s))))
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (is (equal '("response-a" "response-b") items)
          "drained items must appear in push order"))))

(test response-queue-cleared-after-drain
  :description "Setting response-queue to NIL empties it."
  (with-screen (s 10 5)
    (push "data" (cl-tmux/terminal/types:screen-response-queue s))
    (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must be NIL after explicit clear")))
