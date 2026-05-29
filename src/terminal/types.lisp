(in-package #:cl-tmux/terminal/types)

;;;; Types for the CPS-based VT100 terminal emulator.
;;;;
;;;; This file defines the cell and screen structs, attribute constants,
;;;; and the low-level grid/cursor accessors.  Parser state is held
;;;; entirely inside CPS closures; there are no flat state fields here.

;;; ── Attribute bit constants ────────────────────────────────────────────────

(defconstant +attr-bold+      #b00000001)
(defconstant +attr-dim+       #b00000010)
(defconstant +attr-reverse+   #b00000100)
(defconstant +attr-underline+ #b00001000)
(defconstant +attr-blink+     #b00010000)

;;; ── Cell ───────────────────────────────────────────────────────────────────

(defstruct cell
  "One character position on the virtual screen.

   WIDTH encodes East-Asian double-width handling:
     1 — normal single-column cell
     2 — lead cell of a double-width character
     0 — continuation placeholder occupied by the wide char to its left"
  (char  #\Space :type character)
  (fg    7       :type (unsigned-byte 8))   ; 0-15 ANSI colour; 7 = default fg
  (bg    0       :type (unsigned-byte 8))   ; 0-15 ANSI colour; 0 = default bg
  (attrs 0       :type (unsigned-byte 8))   ; bit-field: bold, dim, reverse, underline, blink
  (width 1       :type (integer 0 2)))      ; 1 normal, 2 wide lead, 0 continuation

(defun blank-cell ()
  "Return a fresh default (space, colour 7/0, no attrs, single-width) cell."
  (make-cell))

(declaim (inline clamp))
(defun clamp (v lo hi)
  "Clamp integer V to the closed interval [LO, HI]."
  (max lo (min hi v)))

(defun safe-code-char (cp)
  "CODE-CHAR guarded against invalid code points; falls back to U+FFFD."
  (or (and (< cp char-code-limit) (code-char cp))
      (code-char #xFFFD)))

(defun char-width (ch)
  "Display column width of CH: 2 for East-Asian Wide / Fullwidth characters
   (CJK, kana, hangul, fullwidth forms, most emoji), 1 otherwise.

   Ambiguous-width ranges (e.g. box drawing) are deliberately treated as 1,
   matching how terminals render them in a non-CJK locale."
  (let ((cp (char-code ch)))
    (if (or (<= #x1100  cp #x115F)    ; Hangul Jamo
            (<= #x2E80  cp #x303E)    ; CJK radicals … Kangxi … CJK symbols
            (<= #x3041  cp #x33FF)    ; Hiragana/Katakana … CJK compat
            (<= #x3400  cp #x4DBF)    ; CJK Extension A
            (<= #x4E00  cp #x9FFF)    ; CJK Unified Ideographs
            (<= #xA000  cp #xA4CF)    ; Yi syllables
            (<= #xAC00  cp #xD7A3)    ; Hangul syllables
            (<= #xF900  cp #xFAFF)    ; CJK Compatibility Ideographs
            (<= #xFE30  cp #xFE4F)    ; CJK Compatibility Forms
            (<= #xFF00  cp #xFF60)    ; Fullwidth ASCII forms
            (<= #xFFE0  cp #xFFE6)    ; Fullwidth signs
            (<= #x1F300 cp #x1FAFF)   ; Emoji & pictographs
            (<= #x20000 cp #x3FFFD))  ; CJK Extension B and beyond
        2
        1)))

;;; ── Screen ─────────────────────────────────────────────────────────────────

(defstruct (screen (:constructor %make-screen))
  "Virtual terminal screen: cursor, cell grid, and CPS parser continuation."
  ;; Geometry
  (width    80 :type fixnum)
  (height   24 :type fixnum)
  ;; Row-major grid: index = y*width + x
  (cells    #() :type simple-vector)
  ;; Cursor position
  (cx 0 :type fixnum)
  (cy 0 :type fixnum)
  ;; Current SGR state stamped on newly written cells
  (cur-fg    7 :type (unsigned-byte 8))
  (cur-bg    0 :type (unsigned-byte 8))
  (cur-attrs 0 :type (unsigned-byte 8))
  ;; Scroll region (inclusive 0-based row indices)
  (scroll-top    0  :type fixnum)
  (scroll-bottom 23 :type fixnum)
  ;; CPS parser: a closure (screen byte) -> function.
  ;; Replaces the old flat fields: state / params / cur-param / intermediate /
  ;; utf8-acc / utf8-left.  Initialised to a wrapper so that the package
  ;; cl-tmux/terminal/parser need not exist at compile time.
  (parser (lambda (screen byte)
            (cl-tmux/terminal/parser:ground-state screen byte))
          :type function)
  ;; Dirty flag: set whenever a cell changes; cleared by renderer after paint
  (dirty-p t :type boolean)
  ;; Lock for thread safety (renderer <-> PTY-reader threads)
  (lock (make-lock "screen"))
  ;; Alt-screen support (?1049h / ?1049l)
  (alt-cells nil)                           ; saved normal-screen cell grid, or nil
  (alt-cx 0 :type fixnum)                   ; cursor column saved on alt-screen entry
  (alt-cy 0 :type fixnum)                   ; cursor row saved on alt-screen entry
  ;; DECSC/DECRC saved cursor: (cx cy fg bg attrs) or NIL when nothing saved
  (saved-cursor nil :type list)
  ;; Copy / scroll-back mode
  (copy-mode-p  nil  :type boolean)
  (copy-offset  0    :type fixnum)          ; lines scrolled back (0 = live view)
  (scrollback   nil  :type list))           ; list of row-vectors, newest first

(defun make-screen (width height)
  "Create a blank screen of given dimensions."
  (let* ((n     (* width height))
         (cells (make-array n :initial-element nil)))
    (dotimes (i n) (setf (aref cells i) (blank-cell)))
    (%make-screen :width         width
                  :height        height
                  :cells         cells
                  :scroll-bottom (1- height))))

;;; ── Cursor wrappers ────────────────────────────────────────────────────────

(defun screen-cursor-x (screen)
  "Return the current cursor column of SCREEN."
  (screen-cx screen))

(defun screen-cursor-y (screen)
  "Return the current cursor row of SCREEN."
  (screen-cy screen))

;;; ── Grid helpers ───────────────────────────────────────────────────────────

(defun screen-cell (screen x y)
  "Return the cell at column X, row Y."
  (aref (screen-cells screen)
        (+ (* y (screen-width screen)) x)))

(defun (setf screen-cell) (cell screen x y)
  (setf (aref (screen-cells screen)
              (+ (* y (screen-width screen)) x))
        cell)
  (setf (screen-dirty-p screen) t))

(defun screen-clear-dirty (screen)
  "Clear the dirty flag on SCREEN."
  (setf (screen-dirty-p screen) nil))

;;; ── Resize ─────────────────────────────────────────────────────────────────

(defun screen-resize (screen new-width new-height)
  "Resize SCREEN to NEW-WIDTH x NEW-HEIGHT in place, preserving the
   overlapping top-left rectangle of content.  Resets the scroll region to
   the full new height and clamps the cursor into bounds.

   Callers that share the screen with a reader thread must hold SCREEN's
   lock; this function does no locking of its own."
  (when (and (= new-width  (screen-width  screen))
             (= new-height (screen-height screen)))
    (return-from screen-resize screen))
  (let* ((old-width  (screen-width  screen))
         (old-height (screen-height screen))
         (old-cells  (screen-cells  screen))
         (n          (* new-width new-height))
         (new-cells  (make-array n :initial-element nil)))
    (dotimes (i n) (setf (aref new-cells i) (blank-cell)))
    (dotimes (y (min old-height new-height))
      (dotimes (x (min old-width new-width))
        (setf (aref new-cells (+ (* y new-width) x))
              (aref old-cells (+ (* y old-width) x)))))
    (setf (screen-cells         screen) new-cells
          (screen-width         screen) new-width
          (screen-height        screen) new-height
          (screen-scroll-top    screen) 0
          (screen-scroll-bottom screen) (1- new-height)
          (screen-cx            screen) (clamp (screen-cx screen) 0 (1- new-width))
          (screen-cy            screen) (clamp (screen-cy screen) 0 (1- new-height))
          (screen-dirty-p       screen) t)
    screen))
