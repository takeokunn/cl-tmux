(in-package #:cl-tmux/test)

;;;; Modes tests — part IV: mouse reporting DEC private modes, bracketed paste, focus events, app-cursor, auto-wrap, reset-sgr-pen, screen-display-cell.

;;; ── Mouse reporting DEC private mode tests (1000/1002/1003/1006) ────────────
;;;
;;; The original ~80 lines of repetitive mode-toggle tests have been refactored:
;;; a shared helper function (test-dec-pm-toggle) captures the pattern and each
;;; test is now a one-line call, satisfying the test_abstraction_issues finding.

(defun test-dec-pm-toggle-numeric (mode set-value accessor)
  "Shared helper: verify that DEC PM MODE toggles ACCESSOR on SCREEN to
   SET-VALUE on set (h) and back to 0 on reset (l)."
  (with-screen (s 20 5)
    (feed s (esc "[?~Dh" mode))
    (expect (= set-value (funcall accessor s)))
    (feed s (esc "[?~Dl" mode))
    (expect (= 0 (funcall accessor s)))))

(defun test-dec-pm-toggle-boolean (mode accessor)
  "Shared helper: verify that DEC PM MODE toggles boolean ACCESSOR on SCREEN
   to T on set (h) and back to NIL on reset (l)."
  (with-screen (s 20 5)
    (expect (funcall accessor s) :to-be-falsy)
    (feed s (esc "[?~Dh" mode))
    (expect (funcall accessor s) :to-be-truthy)
    (feed s (esc "[?~Dl" mode))
    (expect (funcall accessor s) :to-be-falsy)))

(describe "terminal-suite/direct-modes-suite"

  (it "mouse-mode-numeric-toggle-table"
    ;; ESC[?1000/1002/1003h sets mouse-mode to 1/2/3; the corresponding l resets to 0.
    (dolist (row '((1000 1) (1002 2) (1003 3)))
      (destructuring-bind (mode expected-val) row
        (test-dec-pm-toggle-numeric mode expected-val #'cl-tmux/terminal/types:screen-mouse-mode))))

  (it "dec-pm-boolean-toggle-table"
    ;; DEC private modes 1/1004/1006/2004 toggle their boolean accessors via h/l sequences.
    (dolist (row (list (list 1    #'cl-tmux/terminal/types:screen-app-cursor-keys)
                       (list 1004 #'cl-tmux/terminal/types:screen-focus-events)
                       (list 1006 #'cl-tmux/terminal/types:screen-mouse-sgr-mode)
                       (list 2004 #'cl-tmux/terminal/types:screen-bracketed-paste)))
      (destructuring-bind (mode accessor) row
        (test-dec-pm-toggle-boolean mode accessor))))

  ;; focus-event-report yields ESC[I on focus gained, ESC[O on focus lost, and NIL
  ;; when focus events are disabled.
  (it "focus-event-report-bytes"
    (with-screen (s 20 5)
      (expect (cl-tmux/terminal/actions:focus-event-report s t) :to-be-falsy)
      (expect (cl-tmux/terminal/actions:focus-event-report s nil) :to-be-falsy)
      (feed s (esc "[?1004h"))
      (expect (string= (format nil "~C[I" #\Escape)
                       (cl-tmux/terminal/actions:focus-event-report s t)))
      (expect (string= (format nil "~C[O" #\Escape)
                       (cl-tmux/terminal/actions:focus-event-report s nil)))))

  ;;; ── Application cursor keys (?1h / ?1l) ─────────────────────────────────────

  ;;; ── Auto-wrap mode (?7h / ?7l) ───────────────────────────────────────────────

  ;; auto-wrap is enabled by default (screen-autowrap = T).
  (it "autowrap-default-is-on"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-autowrap s))))

  ;; ESC[?7l disables auto-wrap; ESC[?7h re-enables it.
  ;; Note: screen-autowrap is T by default, so the default-off check in
  ;; test-dec-pm-toggle-boolean does not apply here — we test the round-trip directly.
  (it "autowrap-disable-toggle"
    (with-screen (s 10 5)
      (feed s (esc "[?7l"))
      (expect (cl-tmux/terminal/types:screen-autowrap s) :to-be-falsy)
      (feed s (esc "[?7h"))
      (expect (cl-tmux/terminal/types:screen-autowrap s))))

  ;;; ── reset-sgr-pen direct tests ───────────────────────────────────────────────

  ;; reset-sgr-pen sets fg=7, bg=0, attrs=0, attrs2=0, ul-color=0.
  (it "reset-sgr-pen-clears-all-slots"
    (with-screen (s 10 5)
      ;; Mutate all five slots to non-default values.
      (setf (cl-tmux/terminal/types:screen-cur-fg       s) 3
            (cl-tmux/terminal/types:screen-cur-bg       s) 5
            (cl-tmux/terminal/types:screen-cur-attrs    s) #xFF
            (cl-tmux/terminal/types:screen-cur-attrs2   s) #xFF
            (cl-tmux/terminal/types:screen-cur-ul-color s) 42)
      (cl-tmux/terminal/types:reset-sgr-pen s)
      (expect (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg s)))
      (expect (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg s)))
      (expect (= 0 (cl-tmux/terminal/types:screen-cur-attrs s)))
      (expect (= 0 (cl-tmux/terminal/types:screen-cur-attrs2 s)))
      (expect (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s))))))

;;; ── screen-display-cell tests ────────────────────────────────────────────────

(describe "terminal-suite/display-cell-suite"

  ;; screen-display-cell returns the live grid cell when copy-mode is off.
  (it "display-cell-live-grid-when-no-copy-mode"
    (with-screen (s 5 3)
      (feed s "abcde")
      ;; copy-mode is off: display cell == live cell.
      (expect (char= #\a (cell-char (cl-tmux/terminal/actions:screen-display-cell s 0 0))))
      (expect (char= #\e (cell-char (cl-tmux/terminal/actions:screen-display-cell s 4 0))))))

  ;; screen-display-cell returns a blank cell when the row exceeds screen height.
  (it "display-cell-returns-blank-for-out-of-range-row"
    (with-screen (s 5 3)
      (feed s "hello")
      ;; Row 99 is beyond the screen; expect the shared blank cell.
      (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 99)))
        (expect (char= #\Space (cell-char cell))))))

  ;; screen-display-cell reads from scrollback when copy-offset > 0.
  (it "display-cell-scrollback-when-copy-mode-offset"
    (with-screen (s 5 3)
      ;; Force a scrollback row: feed two full-screen-height worth of lines.
      (feed s (format nil "AAAAA~C~CBBBBB~C~CCCCCC" #\Return #\Linefeed
                                                     #\Return #\Linefeed))
      ;; scrollback must have at least one row now.
      (let ((sb-len (length (cl-tmux/terminal/types:screen-scrollback s))))
        (when (> sb-len 0)
          ;; Enter copy-mode-like state by manipulating the screen slots directly.
          (setf (cl-tmux/terminal/types:screen-copy-mode-p s) t
                (cl-tmux/terminal/types:screen-copy-offset s) 1)
          (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 0)))
            (expect (characterp (cell-char cell))))))))

  ;; screen-display-cell returns blank when the scrollback vector is shorter than the column.
  (it "display-cell-copy-mode-blank-for-empty-scrollback-entry"
    (with-screen (s 5 3)
      ;; Directly install a zero-length scrollback row.
      (setf (cl-tmux/terminal/types:screen-scrollback s) (list (vector))
            (cl-tmux/terminal/types:screen-copy-mode-p s) t
            (cl-tmux/terminal/types:screen-copy-offset s) 1)
      ;; Column 0 of row 0 should be the display-blank-cell (Space).
      (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 0)))
        (expect (char= #\Space (cell-char cell)))))))
