(in-package #:cl-tmux/terminal/actions)

;;;; DEC private modes, cursor save/restore, hard reset, and display projection.

;;; ── Alt-screen helpers ──────────────────────────────────────────────────────

(defvar *alternate-screen-enabled-function* nil
  "A zero-argument function returning whether the `alternate-screen` option is on
   (non-NIL = the alt screen is allowed).  Installed by the higher layer from the
   option, mirroring *history-limit-function* — keeps this terminal layer free of
   any options dependency.  When NIL (unset), the alt screen is allowed (the
   default), so behaviour is unchanged unless a policy is installed.")

(defun %alternate-screen-allowed-p ()
  "True when entering the alternate screen is permitted: no policy installed
   (default allow) or the installed policy reports the option enabled."
  (or (null *alternate-screen-enabled-function*)
      (funcall *alternate-screen-enabled-function*)))

(defun enter-alt-screen (screen)
  "Save the current grid and cursor to the alt-screen slots, then install a
   fresh blank grid.  No-op when the alt screen is already active, or when the
   `alternate-screen` option is off — full-screen apps then draw on the MAIN
   screen (and their output stays in scrollback), matching tmux."
  (unless (or (not (%alternate-screen-allowed-p))
              (screen-alt-cells screen))
    (setf (screen-alt-cells  screen) (copy-seq (screen-cells screen))
          (screen-alt-cursor-x screen) (screen-cursor-x screen)
          (screen-alt-cursor-y screen) (screen-cursor-y screen))
    (setf (screen-cells screen)
          (%make-blank-cells (* (screen-width screen) (screen-height screen))))
    (set-cursor screen 0 0)
    ;; The displayed grid changes entirely — drop the -J wrap flags (the main
    ;; screen's are not preserved across the alt-screen switch).
    (%clear-all-line-wrapped screen)
    (setf (screen-dirty-p screen) t)))

(defun exit-alt-screen (screen)
  "Restore the saved primary grid and cursor from the alt-screen slots.
   Falls back to erase-display mode 2 when nothing was saved."
  (if (screen-alt-cells screen)
      (setf (screen-cells      screen) (screen-alt-cells    screen)
            (screen-cursor-x   screen) (screen-alt-cursor-x screen)
            (screen-cursor-y   screen) (screen-alt-cursor-y screen)
            (screen-alt-cells  screen) nil)
      (erase-display screen 2))
  (%clear-all-line-wrapped screen)
  (setf (screen-dirty-p screen) t))

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

  ;; Mode 6 — origin mode (DECOM): CUP/HVP rows become relative to the scroll
  ;; region; setting/resetting homes the cursor to the (new) origin.
  (6
   ;; Set (?6h): origin mode on; home the cursor to the scroll-region origin.
   ((setf (screen-origin-mode screen) t)
    (set-cursor screen 0 (screen-scroll-top screen)))
   ;; Reset (?6l): origin mode off (absolute); home the cursor to (0,0).
   ((setf (screen-origin-mode screen) nil)
    (set-cursor screen 0 0)))

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

  ;; Mode 7 — auto-wrap mode (?7h = wrap on, ?7l = wrap off)
  ;; Default is wrap-on (VT100 default).
  (7
   ;; Set (?7h): enable auto-wrap
   ((setf (screen-autowrap screen) t))
   ;; Reset (?7l): disable auto-wrap
   ((setf (screen-autowrap screen) nil)))

  ;; Mode 1004 — focus event reporting (?1004h / ?1004l)
  ;; vim, neovim, and tmux-in-tmux enable this to learn when they gain/lose the
  ;; terminal's focus; the report bytes are sent by focus-event-report below.
  (1004
   ;; Set (?1004h): enable focus event reporting
   ((setf (screen-focus-events screen) t))
   ;; Reset (?1004l): disable focus event reporting
   ((setf (screen-focus-events screen) nil)))

  ;; Mode 1049 — alternate screen (?1049h enters, ?1049l exits)
  (1049
   ;; Set: save current grid + cursor, replace with a fresh blank grid.
   ((enter-alt-screen screen))
   ;; Reset: restore saved grid + cursor, or clear if nothing was saved.
   ((exit-alt-screen screen)))

  ;; Mode 2026 — Synchronized Output (?2026h / ?2026l)
  ;; Applications batch terminal updates between ?2026h and ?2026l.
  ;; We accept and silently ignore this mode — our renderer already
  ;; composites frames atomically, so no special batching is needed.
  (2026
   ((values))  ; no-op set
   ((values))) ; no-op reset

  ;; Mode 2004 - duplicate entry silently overrides (already handled above).
  ;; Mode 47 — alternate screen (older form of 1049, without save/restore)
  (47
   ((enter-alt-screen screen))
   ((exit-alt-screen screen)))

  ;; Mode 2048 — Kitty extended keyboard protocol (?2048h / ?2048l).
  ;; We accept and silently ignore — no extended key reporting is implemented
  ;; (we pass the standard CSI sequences through).  Kitty-aware apps work in
  ;; degraded mode (fall back to legacy encoding) which is correct behaviour.
  (2048
   ((values))  ; no-op set
   ((values))) ; no-op reset

  ;; Mode 1047 — alternate screen buffer (the 1049 component without cursor
  ;; save/restore).  Set switches to the alt screen, reset back to the primary.
  (1047
   ((enter-alt-screen screen))
   ((exit-alt-screen screen)))

  ;; Mode 1048 — save/restore cursor (the other 1049 component): set saves the
  ;; cursor (like DECSC / ESC 7), reset restores it (like DECRC / ESC 8).  Some
  ;; ncurses apps toggle 1047 and 1048 separately instead of the combined 1049.
  (1048
   ((save-cursor screen))
   ((restore-cursor screen)))

  ;; Mode 1 — xterm cursor-key app mode is already handled (line 79-84).
  ;; Mode 12 — local echo mode (accepted silently, not modelled).
  (12
   ((values))
   ((values))))  ; define-dec-pm-rules closes here

;;; ── Focus event reporting (?1004) ──────────────────────────────────────────
;;;
;;; When an application enables focus events, it expects the terminal to deliver
;;; ESC[I when focus is gained and ESC[O when focus is lost.  This pure function
;;; produces those report bytes; the dispatch layer writes them to the pane's PTY
;;; as the active pane changes.  Returns NIL when the screen has not opted in, so
;;; callers can treat "no report" and "focus events off" uniformly.
;;;
;;; defparameter rather than defconstant is used for the report strings because
;;; string identity (EQL) cannot be guaranteed across image reloads — SBCL would
;;; signal a redefinition error for defconstant with a new string object.

(defparameter +focus-gained-report+ (format nil "~C[I" #\Escape)
  "VT sequence delivered to a focused application when it gains terminal focus.")
(defparameter +focus-lost-report+   (format nil "~C[O" #\Escape)
  "VT sequence delivered to a focused application when it loses terminal focus.")

(defun focus-event-report (screen focused-p)
  "Focus-tracking report bytes for SCREEN: ESC[I when FOCUSED-P, ESC[O otherwise.
   Returns NIL unless the screen enabled focus events (?1004h)."
  (when (screen-focus-events screen)
    (if focused-p
        +focus-gained-report+
        +focus-lost-report+)))

;;; ── DECSC / DECRC (cursor save & restore) ──────────────────────────────────

(defun save-cursor (screen)
  "DECSC (ESC 7): save the cursor position and full SGR pen state.
   Saves: cursor-x, cursor-y, cur-fg, cur-bg, cur-attrs, cur-attrs2, cur-ul-color."
  (setf (screen-saved-cursor screen)
        (list (screen-cursor-x     screen)
              (screen-cursor-y     screen)
              (screen-cur-fg       screen)
              (screen-cur-bg       screen)
              (screen-cur-attrs    screen)
              (screen-cur-attrs2   screen)
              (screen-cur-ul-color screen))))

(defun restore-cursor (screen)
  "DECRC (ESC 8): restore the cursor position and full SGR pen state saved by DECSC.
   With nothing previously saved, home the cursor and reset the SGR pen (VT100 default)."
  (cond
    ((null (screen-saved-cursor screen))
     (set-cursor screen 0 0)
     (reset-sgr-pen screen))
    (t
     (destructuring-bind (cursor-x cursor-y fg bg attrs attrs2 ul-color)
         (screen-saved-cursor screen)
       (set-cursor screen cursor-x cursor-y)
       (setf (screen-cur-fg       screen) fg
             (screen-cur-bg       screen) bg
             (screen-cur-attrs    screen) attrs
             (screen-cur-attrs2   screen) attrs2
             (screen-cur-ul-color screen) ul-color)))))

;;; ── Full reset ─────────────────────────────────────────────────────────────

(defun reset-terminal-modes (screen)
  "Reset all terminal mode flags and scroll region to their VT100 defaults.
   Covers: cursor visibility, autowrap, charset, and the scroll region."
  (setf (screen-cursor-visible screen) t
        (screen-scroll-top     screen) 0
        (screen-scroll-bottom  screen) (1- (screen-height screen))
        (screen-charset        screen) :ascii
        (screen-g0-charset     screen) :ascii
        (screen-g1-charset     screen) :ascii
        (screen-active-g       screen) :g0
        (screen-tab-stops      screen) :default
        (screen-origin-mode    screen) nil
        (screen-autowrap       screen) t
        (screen-pending-wrap   screen) nil))

(defun ris-action (screen)
  "RIS — ESC c: hard terminal reset.
   Clears the entire cell grid, homes the cursor, resets all SGR attributes,
   cursor visibility, and restores the scroll region to the full screen height."
  (erase-region screen 0 0
                (1- (screen-width  screen))
                (1- (screen-height screen)))
  (set-cursor screen 0 0)
  (reset-sgr-pen screen)
  (reset-terminal-modes screen))

(defun decaln-action (screen)
  "DECALN — ESC # 8: fill the entire screen with 'E' (the VT100 screen-alignment
   test pattern, used by vttest and terminal conformance suites), then home the
   cursor.  Each cell becomes a default-attribute 'E'."
  (dotimes (y (screen-height screen))
    (dotimes (x (screen-width screen))
      (setf (screen-cell screen x y) (make-cell :char #\E))))
  (set-cursor screen 0 0)
  (setf (screen-dirty-p screen) t))

;;; ── Display projection (copy-mode scrollback) ──────────────────────────────

(defparameter *display-blank-cell* (blank-cell)
  "Shared immutable blank cell for out-of-range display lookups.
   Safe to share because cells are never mutated in place.")

(defun %scrollback-cell (screen col offset-from-top)
  "Return the cell at COLUMN COL in the scrollback row OFFSET-FROM-TOP rows above
   the live grid top (1-based: 1 = newest scrollback row).
   Returns *display-blank-cell* when the row or column is out of range."
  (let ((vec (nth (1- offset-from-top) (screen-scrollback screen))))
    (if (and vec (< col (length vec)))
        (aref vec col)
        *display-blank-cell*)))

(defun %live-grid-cell (screen col live-row)
  "Return the live grid cell at COLUMN COL, ROW LIVE-ROW.
   Returns *display-blank-cell* when LIVE-ROW is beyond the screen height."
  (if (< live-row (screen-height screen))
      (screen-cell screen col live-row)
      *display-blank-cell*))

(defun screen-display-cell (screen col row)
  "Cell shown at viewport position (COL, ROW) for the current scroll state.
   With copy-offset 0 this is the live grid cell.  When scrolled back by N
   lines the top N rows come from the scrollback buffer and the live grid
   is shifted down by N rows.  Out-of-range reads return *display-blank-cell*."
  (let ((offset (if (screen-copy-mode-p screen) (screen-copy-offset screen) 0)))
    (if (< row offset)
        (%scrollback-cell screen col (- offset row))
        (%live-grid-cell  screen col (- row offset)))))

;;; ── DECSCUSR cursor shape ────────────────────────────────────────────────────

(defun set-cursor-shape (screen shape)
  "DECSCUSR: set the cursor shape to SHAPE (0-6, clamped).
   0 = default blinking block, 1 = blinking block, 2 = steady block,
   3 = blinking underline, 4 = steady underline, 5 = blinking bar,
   6 = steady bar."
  (setf (screen-cursor-shape screen) (clamp shape 0 6)))

;;; ── BEL pending ──────────────────────────────────────────────────────────────

(defun set-bell-pending (screen)
  "Mark SCREEN as having a pending BEL (bell event) to be processed by the renderer."
  (setf (screen-bell-pending screen) t))

;;; ── Charset selection ────────────────────────────────────────────────────────
;;;
;;; G0 and G1 designation share the same two-way (:g0/:g1) slot dispatch.
;;; define-charset-slot-rules builds both the read and write helpers from one
;;; declarative table, consistent with the define-dec-pm-rules style.
;;;
;;; Prolog-like facts:
;;;   charset_slot(g0, Screen) :- screen-g0-charset(Screen).
;;;   charset_slot(g1, Screen) :- screen-g1-charset(Screen).

(defmacro define-charset-slot-rules (&rest specs)
  "Build %CHARSET-SLOT-REF and %CHARSET-SLOT-SET from a declarative two-column
   table mapping G designator keywords to screen accessor names.
   Each SPEC is (:gN accessor-name)."
  `(progn
     (defun %charset-slot-ref (screen g)
       "Return the charset designated to G (:g0 or :g1) on SCREEN."
       (ecase g
         ,@(mapcar (lambda (s) `(,(car s) (,(cadr s) screen))) specs)))
     (defun %charset-slot-set (screen g charset)
       "Set the charset designated to G (:g0 or :g1) on SCREEN to CHARSET."
       (ecase g
         ,@(mapcar (lambda (s) `(,(car s) (setf (,(cadr s) screen) charset)))
                   specs)))))

(define-charset-slot-rules
  (:g0 screen-g0-charset)
  (:g1 screen-g1-charset))

(defun set-charset (screen charset)
  "Set the effective character set of SCREEN to CHARSET (:ascii or :dec-graphics).
   Low-level helper retained for tests; the parser uses DESIGNATE-CHARSET /
   INVOKE-CHARSET to model the full VT100 G0/G1 + SO/SI behaviour."
  (setf (screen-charset screen) charset))

(defun screen-invoked-charset (screen g)
  "Return the charset currently designated to G (:g0 or :g1) on SCREEN."
  (%charset-slot-ref screen g))

(defun designate-charset (screen g charset)
  "Designate G (:g0 or :g1) of SCREEN to CHARSET — the effect of ESC ( X (G0)
   or ESC ) X (G1).  Updates the effective charset ONLY when G is the currently
   invoked set, so ESC ) 0 designates G1 without activating line-drawing until a
   SO (0x0E) locking shift selects G1."
  (%charset-slot-set screen g charset)
  (when (eq (screen-active-g screen) g)
    (setf (screen-charset screen) charset)))

(defun invoke-charset (screen g)
  "Invoke G (:g0 or :g1) as the active charset: SO (0x0E) invokes G1, SI (0x0F)
   invokes G0.  Sets the effective charset to G's current designation."
  (setf (screen-active-g screen) g)
  (setf (screen-charset screen) (screen-invoked-charset screen g)))

;;; ── Screen title ─────────────────────────────────────────────────────────────

(defun set-screen-title (screen title)
  "Set the OSC window title of SCREEN to TITLE string."
  (setf (screen-title screen) title))

(defun set-screen-cwd (screen cwd)
  "Set the OSC 7 current working directory of SCREEN to CWD string."
  (setf (screen-cwd screen) cwd))
