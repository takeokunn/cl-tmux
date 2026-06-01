(in-package #:cl-tmux/terminal/actions)

;;;; DEC private modes, cursor save/restore, hard reset, and display projection.

;;; ── Prolog-like DEC PM rule table macro ─────────────────────────────────────
;;;
;;; dec-pm-set and dec-pm-reset are symmetric: for each mode number there is a
;;; SET action (enter the mode) and a RESET action (leave the mode).  The pair
;;; is expressed as a single Prolog-like fact:
;;;   dec_pm(1049, set,   Screen) :- enter_alt_screen(Screen).
;;;   dec_pm(1049, reset, Screen) :- exit_alt_screen(Screen).
;;;
;;; define-dec-pm-rules builds both functions from one declarative table.
;;; Each SPEC is  (param-number (set-body...) (reset-body...))

(defmacro define-dec-pm-rules (&rest specs)
  "Generate DEC-PM-SET and DEC-PM-RESET from a single Prolog-like rule table.
   Each SPEC is (param (set-body...) (reset-body...)).
   Unknown mode numbers are accepted silently."
  `(progn
     (defun dec-pm-set (screen params)
       "Handle DEC private mode set sequences (?XXXh)."
       (declare (ignorable screen))
       (dolist (param params)
         (case param
           ,@(mapcar (lambda (s) `(,(car s) ,@(cadr  s))) specs))))
     (defun dec-pm-reset (screen params)
       "Handle DEC private mode reset sequences (?XXXl)."
       (declare (ignorable screen))
       (dolist (param params)
         (case param
           ,@(mapcar (lambda (s) `(,(car s) ,@(caddr s))) specs))))))

;;; ── DEC PM rule table (data) ─────────────────────────────────────────────────

(define-dec-pm-rules
  ;; Mode 25 — cursor visibility (DECTCEM)
  (25
   ;; Set (?25h): show the cursor
   ((setf (screen-cursor-visible screen) t))
   ;; Reset (?25l): hide the cursor
   ((setf (screen-cursor-visible screen) nil)))

  ;; Mode 1 — application cursor keys (?1h / ?1l)
  ;; When set, pane expects ESC O A-D instead of ESC [ A-D for arrow keys.
  (1
   ;; Set (?1h): application cursor keys on
   ((setf (screen-app-cursor-keys screen) t))
   ;; Reset (?1l): application cursor keys off
   ((setf (screen-app-cursor-keys screen) nil)))

  ;; Mode 2004 — bracketed paste mode (?2004h / ?2004l)
  ;; Modern shells (bash, zsh, fish) and editors (vim, neovim) toggle this mode.
  (2004
   ;; Set (?2004h): enable bracketed paste
   ((setf (screen-bracketed-paste screen) t))
   ;; Reset (?2004l): disable bracketed paste
   ((setf (screen-bracketed-paste screen) nil)))

  ;; Mode 1000 — basic mouse tracking (X10 button press/release)
  (1000
   ;; Set (?1000h): enable basic mouse tracking
   ((setf (screen-mouse-mode screen) 1))
   ;; Reset (?1000l): disable mouse tracking
   ((setf (screen-mouse-mode screen) 0)))

  ;; Mode 1002 — button-event mouse tracking
  (1002
   ;; Set (?1002h): enable button-event mouse tracking
   ((setf (screen-mouse-mode screen) 2))
   ;; Reset (?1002l): disable mouse tracking
   ((setf (screen-mouse-mode screen) 0)))

  ;; Mode 1003 — all-motion mouse tracking
  (1003
   ;; Set (?1003h): enable all-motion mouse tracking
   ((setf (screen-mouse-mode screen) 3))
   ;; Reset (?1003l): disable mouse tracking
   ((setf (screen-mouse-mode screen) 0)))

  ;; Mode 1006 — SGR extended mouse encoding
  (1006
   ;; Set (?1006h): enable SGR extended mouse encoding
   ((setf (screen-mouse-sgr-mode screen) t))
   ;; Reset (?1006l): disable SGR extended mouse encoding
   ((setf (screen-mouse-sgr-mode screen) nil)))

  ;; Mode 1049 — alternate screen (?1049h enters, ?1049l exits)
  (1049
   ;; Set: save current grid + cursor, replace with a fresh blank grid.
   ((unless (screen-alt-cells screen)
      (setf (screen-alt-cells screen) (copy-seq (screen-cells screen))
            (screen-alt-cx    screen) (screen-cx screen)
            (screen-alt-cy    screen) (screen-cy screen))
      (setf (screen-cells screen)
            (%make-blank-cells (* (screen-width screen) (screen-height screen))))
      (set-cursor screen 0 0)))
   ;; Reset: restore saved grid + cursor, or clear if nothing was saved.
   ((if (screen-alt-cells screen)
        (setf (screen-cells     screen) (screen-alt-cells screen)
              (screen-cx        screen) (screen-alt-cx    screen)
              (screen-cy        screen) (screen-alt-cy    screen)
              (screen-alt-cells screen) nil)
        (erase-display screen 2))
    (setf (screen-dirty-p screen) t))))

;;; ── DECSC / DECRC (cursor save & restore) ──────────────────────────────────

(defun save-cursor (screen)
  "DECSC (ESC 7): save the cursor position and current SGR state."
  (setf (screen-saved-cursor screen)
        (list (screen-cx screen) (screen-cy screen)
              (screen-cur-fg screen) (screen-cur-bg screen)
              (screen-cur-attrs screen))))

(defun restore-cursor (screen)
  "DECRC (ESC 8): restore the cursor position and SGR state saved by DECSC.
   With nothing previously saved, home the cursor and reset SGR (VT100 default)."
  (cond
    ((null (screen-saved-cursor screen))
     (set-cursor screen 0 0)
     (setf (screen-cur-fg screen) 7 (screen-cur-bg screen) 0 (screen-cur-attrs screen) 0))
    (t
     (destructuring-bind (cx cy fg bg attrs) (screen-saved-cursor screen)
       (set-cursor screen cx cy)
       (setf (screen-cur-fg    screen) fg
             (screen-cur-bg    screen) bg
             (screen-cur-attrs screen) attrs)))))

;;; ── Full reset ─────────────────────────────────────────────────────────────

(defun ris-action (screen)
  "RIS — ESC c: hard terminal reset.
   Clears the entire cell grid, homes the cursor, resets all SGR attributes,
   cursor visibility, and restores the scroll region to the full screen height."
  (erase-region screen 0 0
                (1- (screen-width  screen))
                (1- (screen-height screen)))
  (set-cursor screen 0 0)
  (setf (screen-cur-fg      screen) 7
        (screen-cur-bg      screen) 0
        (screen-cur-attrs   screen) 0
        (screen-cursor-visible screen) t
        (screen-scroll-top    screen) 0
        (screen-scroll-bottom screen) (1- (screen-height screen))))

;;; ── Display projection (copy-mode scrollback) ──────────────────────────────

(defparameter *display-blank-cell* (blank-cell)
  "Shared immutable blank cell for out-of-range display lookups.
   Safe to share because cells are never mutated in place.")

(defun screen-display-cell (screen col row)
  "Cell shown at viewport position (COL, ROW) for the current scroll state.
   With copy-offset 0 this is the live grid cell.  When scrolled back by N
   lines the top N rows come from the scrollback buffer and the live grid
   is shifted down by N rows.  Out-of-range reads return *display-blank-cell*."
  (let ((offset (if (screen-copy-mode-p screen) (screen-copy-offset screen) 0)))
    (cond
      ((< row offset)
       (let ((vec (nth (- offset 1 row) (screen-scrollback screen))))
         (if (and vec (< col (length vec)))
             (aref vec col)
             *display-blank-cell*)))
      (t
       (let ((live-row (- row offset)))
         (if (< live-row (screen-height screen))
             (screen-cell screen col live-row)
             *display-blank-cell*))))))
