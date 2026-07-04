(in-package #:cl-tmux/test)

(in-suite server-multi-suite)

;;;; Pure size-selection helpers for multi-client server behavior.

;;; ── %client-fds / %client-size-reduce: pure registry helpers ─────────────────

(test client-fds-returns-fd-of-every-attached-client
  "%client-fds returns the socket fd of every entry in *clients*, in order."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :fd 11)
                (cl-tmux::%make-client-conn :fd 22)
                (cl-tmux::%make-client-conn :fd 33))))
    (is (equal '(11 22 33) (cl-tmux::%client-fds))
        "%client-fds must list the fds in *clients* order")))

(test client-fds-empty-when-no-clients
  "%client-fds returns NIL when no clients are attached."
  (let ((cl-tmux::*clients* nil))
    (is (null (cl-tmux::%client-fds))
        "%client-fds on an empty registry must return NIL")))

(test client-size-reduce-applies-fn-across-rows-and-cols
  "%client-size-reduce applies the given reducing FN independently across every
   attached client's rows and cols."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :rows 50 :cols 80)
                (cl-tmux::%make-client-conn :rows 24 :cols 200)
                (cl-tmux::%make-client-conn :rows 40 :cols 120))))
    (multiple-value-bind (min-rows min-cols) (cl-tmux::%client-size-reduce #'min)
      (check-table (list (list min-rows 24  "min reduce → smallest rows")
                         (list min-cols 80  "min reduce → smallest cols"))))
    (multiple-value-bind (max-rows max-cols) (cl-tmux::%client-size-reduce #'max)
      (check-table (list (list max-rows 50  "max reduce → largest rows")
                         (list max-cols 200 "max reduce → largest cols"))))))

;;; ── %effective-client-size: smallest attached client ─────────────────────────

(test multi-effective-size-is-smallest-client
  "The session renders at the SMALLEST attached client's geometry so every client
   can display the shared broadcast frame."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                (cl-tmux::%make-client-conn :rows 24 :cols 80)
                (cl-tmux::%make-client-conn :rows 40 :cols 120))))
    (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
       (check-table (list (list rows 24 "effective rows = smallest client rows")
                          (list cols 80 "effective cols = smallest client cols"))))))

(test multi-effective-size-no-clients-falls-back
  "With no clients attached, %effective-client-size falls back to *term-rows*/cols."
  (let ((cl-tmux::*clients* nil)
        (cl-tmux::*term-rows* 30)
        (cl-tmux::*term-cols* 100))
    (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
       (check-table (list (list rows 30 "no clients → rows fallback to *term-rows*")
                          (list cols 100 "no clients → cols fallback to *term-cols*"))))))

(test multi-effective-size-largest-mode
  "window-size \"largest\" sizes to the biggest attached client."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "largest")
    (let ((cl-tmux::*clients*
            (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                  (cl-tmux::%make-client-conn :rows 24 :cols 80))))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (check-table (list (list rows 50 "largest rows")
                           (list cols 200 "largest cols")))))))

(test multi-effective-size-latest-mode
  "window-size \"latest\" sizes to the most-recent client (front of *clients*)."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "latest")
    (let ((cl-tmux::*clients*
            (list (cl-tmux::%make-client-conn :rows 40 :cols 120)   ; most recent
                  (cl-tmux::%make-client-conn :rows 24 :cols 80))))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (check-table (list (list rows 40 "latest rows")
                           (list cols 120 "latest cols")))))))

(test multi-effective-size-manual-mode-keeps-current
  "window-size \"manual\" ignores client sizes and keeps *term-rows*/cols."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "manual")
    (let ((cl-tmux::*clients* (list (cl-tmux::%make-client-conn :rows 99 :cols 99)))
          (cl-tmux::*term-rows* 30)
          (cl-tmux::*term-cols* 100))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (check-table (list (list rows 30 "manual keeps current rows")
                           (list cols 100 "manual keeps current cols")))))))
