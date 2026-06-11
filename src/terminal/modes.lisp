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

(defun enter-alt-screen (screen &key save-cursor-p)
  "Save the current grid and cursor x/y to the alt-screen slots, then install a
   fresh blank grid.  When SAVE-CURSOR-P is T (mode 1049), also save the FULL
   cursor state (SGR attrs, charset, origin mode) via SAVE-CURSOR — matching
   tmux's ?1049h behaviour which is equivalent to ?1047h + ?1048h (DECSC).
   No-op when the alt screen is already active, or when the `alternate-screen`
   option is off — full-screen apps then draw on the MAIN screen (and their
   output stays in scrollback), matching tmux."
  (unless (or (not (%alternate-screen-allowed-p))
              (screen-alt-cells screen))
    (when save-cursor-p (save-cursor screen))
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

(defun exit-alt-screen (screen &key restore-cursor-p)
  "Restore the saved primary grid and cursor from the alt-screen slots.
   When RESTORE-CURSOR-P is T (mode 1049), also restore the FULL cursor state
   (SGR attrs, charset, origin mode) via RESTORE-CURSOR — equivalent to ?1047l
   + ?1048l (DECRC).  Falls back to erase-display mode 2 when nothing was saved."
  (if (screen-alt-cells screen)
      (setf (screen-cells      screen) (screen-alt-cells    screen)
            (screen-cursor-x   screen) (screen-alt-cursor-x screen)
            (screen-cursor-y   screen) (screen-alt-cursor-y screen)
            (screen-alt-cells  screen) nil)
      (erase-display screen 2))
  (when restore-cursor-p (restore-cursor screen))
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

  ;; Mode 5 — DECSCNM (reverse-video screen): while set, the whole grid renders
  ;; with fg/bg swapped (a global reverse XORed with each cell's own reverse
  ;; attribute).  Apps use it for a screen "flash" or a reverse theme.
  (5
   ;; Set (?5h): reverse-video screen on.
   ((setf (screen-reverse-screen screen) t
          (screen-dirty-p screen) t))
   ;; Reset (?5l): reverse-video screen off.
   ((setf (screen-reverse-screen screen) nil
          (screen-dirty-p screen) t)))

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
  ;; Equivalent to ?1047h + ?1048h (DECSC): saves full cursor state (SGR attrs,
  ;; charset, origin-mode) in addition to the grid swap, matching tmux and xterm.
  (1049
   ;; Set: save full cursor state (DECSC) + grid, replace with a fresh blank grid.
   ((enter-alt-screen screen :save-cursor-p t))
   ;; Reset: restore saved grid + full cursor state (DECRC), or clear if unsaved.
   ((exit-alt-screen screen :restore-cursor-p t)))

  ;; Mode 2026 — Synchronized Output (?2026h / ?2026l)
  ;; Applications batch terminal updates between ?2026h and ?2026l.
  ;; We accept and silently ignore this mode — our renderer already
  ;; composites frames atomically, so no special batching is needed.
  (2026
   ((values))  ; no-op set
   ((values))) ; no-op reset

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

  ;; Mode 12 — local echo mode (accepted silently, not modelled).
  (12
   ((values))
   ((values))))  ; define-dec-pm-rules closes here
