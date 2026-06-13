(in-package #:cl-tmux/terminal/actions)

;;;; Terminal modes — part D: focus events, cursor save/restore, resets,
;;;; display projection, DECSCUSR, BEL, ANSI SM/RM, charset selection, screen title.

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
  "DECSC (ESC 7): save the cursor position, full SGR pen, charset state, and origin mode.
   Mirrors tmux's input_save_state, which memcpy's the input cell (attrs/fg/bg) plus the
   charset designation (set/g0set/g1set) and records s->mode (incl. MODE_ORIGIN).
   Saves: cursor-x/y, cur-fg/bg/attrs/attrs2/ul-color, g0/g1/active charset, origin-mode."
  (setf (screen-saved-cursor screen)
        (list (screen-cursor-x     screen)
              (screen-cursor-y     screen)
              (screen-cur-fg       screen)
              (screen-cur-bg       screen)
              (screen-cur-attrs    screen)
              (screen-cur-attrs2   screen)
              (screen-cur-ul-color screen)
              (screen-g0-charset   screen)
              (screen-g1-charset   screen)
              (screen-active-g     screen)
              (screen-charset      screen)
              (screen-origin-mode  screen))))

(defun %restore-cursor-to-defaults (screen)
  "Restore cursor and SGR state to VT100 power-on defaults (no prior DECSC snapshot).
   Homes the cursor, resets the SGR pen, clears origin mode, and resets the G0/G1
   charset designations and the effective charset to :ascii."
  (set-cursor screen 0 0)
  (reset-sgr-pen screen)
  (setf (screen-origin-mode  screen) nil
        (screen-g0-charset   screen) :ascii
        (screen-g1-charset   screen) :ascii
        (screen-active-g     screen) :g0
        (screen-charset      screen) :ascii))

(defun %restore-cursor-from-snapshot (screen snapshot)
  "Restore cursor and SGR state from a DECSC SNAPSHOT (a list produced by SAVE-CURSOR).
   Applies cursor-x/y, SGR pen (fg/bg/attrs/attrs2/ul-color), charset designations
   (G0/G1/active-g/charset), and origin-mode from the snapshot in order."
  (destructuring-bind (cx cy fg bg attrs attrs2 ul-color g0 g1 active-g charset origin-mode)
      snapshot
    (set-cursor screen cx cy)
    (setf (screen-cur-fg       screen) fg
          (screen-cur-bg       screen) bg
          (screen-cur-attrs    screen) attrs
          (screen-cur-attrs2   screen) attrs2
          (screen-cur-ul-color screen) ul-color
          (screen-g0-charset   screen) g0
          (screen-g1-charset   screen) g1
          (screen-active-g     screen) active-g
          (screen-charset      screen) charset
          (screen-origin-mode  screen) origin-mode)))

(defun restore-cursor (screen)
  "DECRC (ESC 8): restore the cursor position, SGR pen, charset state, and origin mode
   saved by DECSC.  Mirrors tmux's input_restore_state.  With nothing previously saved,
   home the cursor and reset the SGR pen, charset, and origin mode to VT100 defaults."
  (if (null (screen-saved-cursor screen))
      (%restore-cursor-to-defaults screen)
      (%restore-cursor-from-snapshot screen (screen-saved-cursor screen))))

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
        (screen-insert-mode    screen) nil
        (screen-newline-mode   screen) nil
        (screen-reverse-screen screen) nil
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

(defun decstr-action (screen)
  "DECSTR — CSI ! p: soft terminal reset.  Restores modes and the SGR pen to their
   power-on defaults but, unlike RIS, does NOT clear the screen or move the cursor.
   Resets the SGR pen, the terminal modes (charset / origin / autowrap / insert /
   scroll region / cursor visibility / pending wrap / tab stops via
   reset-terminal-modes), application cursor keys, bracketed-paste mode, and the
   DECSC saved-cursor (a later DECRC then homes, per xterm)."
  (reset-sgr-pen screen)
  (reset-terminal-modes screen)
  (setf (screen-app-cursor-keys screen) nil
        (screen-bracketed-paste screen) nil
        (screen-saved-cursor    screen) nil))

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

;;; ── ANSI (non-private) Set/Reset Mode — CSI Ps h / CSI Ps l ─────────────────
;;;
;;; The non-private SM/RM modes (no `?` prefix).  IRM (mode 4, insert/replace) is
;;; the one with a visible effect; the rest are accepted and ignored so a stray
;;; `CSI 20 h` etc. does not corrupt the display.  PARAMS is a list of mode ints
;;; (as parsed for dec-pm-set).
;;;
;;; define-ansi-mode-rules mirrors define-dec-pm-rules but generates SET-ANSI-MODE
;;; and RESET-ANSI-MODE from one symmetric declarative table.  Each SPEC is
;;; (param-number slot-accessor) where slot-accessor names the boolean screen slot
;;; that the mode maps to.  Set → T, Reset → NIL.
;;;
;;; Prolog-like facts:
;;;   ansi_mode(4,  screen-insert-mode).
;;;   ansi_mode(20, screen-newline-mode).

(defmacro define-ansi-mode-rules (&rest specs)
  "Generate SET-ANSI-MODE and RESET-ANSI-MODE from a symmetric declarative table.
   Each SPEC is (param-number slot-accessor).
   Set writes T to the slot; Reset writes NIL."
  `(progn
     (defun set-ansi-mode (screen params)
       "ANSI Set Mode (CSI Ps h).  IRM (mode 4) turns on insert mode (printed chars
   shift the rest of the line right); LNM (mode 20) turns on newline mode (LF also
   carriage-returns)."
       (dolist (param params)
         (case param
           ,@(mapcar (lambda (s) `(,(car s) (setf (,(cadr s) screen) t))) specs))))
     (defun reset-ansi-mode (screen params)
       "ANSI Reset Mode (CSI Ps l).  IRM (mode 4) turns off insert mode (replace/
   overwrite); LNM (mode 20) turns off newline mode (LF is a bare line feed)."
       (dolist (param params)
         (case param
           ,@(mapcar (lambda (s) `(,(car s) (setf (,(cadr s) screen) nil))) specs))))))

(define-ansi-mode-rules
  (4  screen-insert-mode)
  (20 screen-newline-mode))

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
  (setf (screen-active-g screen) g
        (screen-charset screen) (screen-invoked-charset screen g)))

;;; ── Screen title ─────────────────────────────────────────────────────────────

(defun set-screen-title (screen title)
  "Set the OSC window title of SCREEN to TITLE string."
  (setf (screen-title screen) title))

(defun set-screen-cwd (screen cwd)
  "Set the OSC 7 current working directory of SCREEN to CWD string."
  (setf (screen-cwd screen) cwd))
