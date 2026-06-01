(in-package #:cl-tmux/terminal/types)

;;;; Mutable screen struct and grid operations.
;;;;
;;;; Depends on cell.lisp (BLANK-CELL, CLAMP) being loaded first.

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
  ;; Current SGR state stamped on newly written cells.
  ;; Color encoding matches cell.fg / cell.bg: 0-255 palette, bit-24 = RGB true-color.
  (cur-fg    7 :type (unsigned-byte 25))
  (cur-bg    0 :type (unsigned-byte 25))
  (cur-attrs 0 :type (unsigned-byte 8))
  ;; Cursor visibility: toggled by DECTCEM (?25h = show, ?25l = hide).
  (cursor-visible t :type boolean)
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

(defun %make-blank-cells (n)
  "Allocate a simple vector of N blank cells (space, default colour, no attrs)."
  (make-array n :initial-contents (loop repeat n collect (blank-cell))))

(defun make-screen (width height)
  "Create a blank screen of given dimensions."
  (%make-screen :width         width
                :height        height
                :cells         (%make-blank-cells (* width height))
                :scroll-bottom (1- height)))

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
        cell
        (screen-dirty-p screen) t))

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
         (new-cells  (%make-blank-cells (* new-width new-height))))
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
