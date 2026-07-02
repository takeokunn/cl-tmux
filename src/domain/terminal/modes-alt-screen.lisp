(in-package #:cl-tmux/terminal/actions)

;;;; Alt-screen enter/exit helpers.

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
