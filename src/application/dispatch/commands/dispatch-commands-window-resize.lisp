(in-package #:cl-tmux)

;;; -- Window resize command -----------------------------------------------------

(defun %cmd-resize-window-arg (session args)
  "resize-window [-aADLRU] [-x cols] [-y rows] [-t target-window] [adjustment]:
   resize a window (tmux args \"DLRUaAt:x:y:\").
   -x cols / -y rows: set the window to exactly COLS x ROWS.
   -L/-R/-U/-D [adjustment]: shrink/grow the window by ADJUSTMENT (default 1)
     columns (-L narrower, -R wider) or rows (-U shorter, -D taller).
   -a: resize to the smallest attached client's size; -A: to the largest.  The
     standalone single-client model has one client, so both use the terminal size.
   Without flags prompts interactively."
  (with-command-input (flags positionals args "xyt"
                             :allowed-flags '(#\a #\A #\D #\L #\R #\U #\x #\y #\t)
                             :max-positionals 1
                             :message "resize-window: unsupported argument")
    (let* ((cols     (%parse-flag-int flags #\x))
           (rows     (%parse-flag-int flags #\y))
           (target   (%flag-value flags #\t))
           (win      (if target
                         (%resolve-window-target session target)
                         (session-active-window session)))
           ;; Optional [adjustment] positional (default 1) for -L/-R/-U/-D.
           (adjust   (or (and (first positionals)
                              (ignore-errors (parse-integer (first positionals)
                                                            :junk-allowed t)))
                         1))
           (term-cols *term-cols*)
           (term-rows (- *term-rows* *status-height*)))
      (when win
        (let ((cur-w (window-width win))
              (cur-h (window-height win)))
          (cond
            ;; Directional adjustment by ADJUSTMENT cells.
            ((%flag-present-p flags #\L) (window-relayout win cur-h (max 1 (- cur-w adjust))))
            ((%flag-present-p flags #\R) (window-relayout win cur-h (+ cur-w adjust)))
            ((%flag-present-p flags #\U) (window-relayout win (max 1 (- cur-h adjust)) cur-w))
            ((%flag-present-p flags #\D) (window-relayout win (+ cur-h adjust) cur-w))
            ;; -a/-A: smallest/largest client size = terminal size (single client).
            ((or (%flag-present-p flags #\a) (%flag-present-p flags #\A))
             (window-relayout win term-rows term-cols))
            ;; -x/-y: absolute size.
            ((and cols rows (> cols 0) (> rows 0))
             (window-relayout win rows cols))))))))
