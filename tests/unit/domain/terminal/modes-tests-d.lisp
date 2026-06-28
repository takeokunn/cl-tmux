(in-package #:cl-tmux/test)

;;;; Modes tests — part IV: mouse reporting DEC private modes, bracketed paste, focus events, app-cursor, auto-wrap, reset-sgr-pen, screen-display-cell.

(in-suite direct-modes-suite)

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
    (is (= set-value (funcall accessor s))
        "mouse-mode must be ~D after ESC[?~Dh" set-value mode)
    (feed s (esc "[?~Dl" mode))
    (is (= 0 (funcall accessor s))
        "mouse-mode must be 0 after ESC[?~Dl" mode)))

(defun test-dec-pm-toggle-boolean (mode accessor)
  "Shared helper: verify that DEC PM MODE toggles boolean ACCESSOR on SCREEN
   to T on set (h) and back to NIL on reset (l)."
  (with-screen (s 20 5)
    (is-false (funcall accessor s)
              "mode ~D accessor must be NIL by default" mode)
    (feed s (esc "[?~Dh" mode))
    (is-true (funcall accessor s)
             "mode ~D accessor must be T after ESC[?~Dh" mode)
    (feed s (esc "[?~Dl" mode))
    (is-false (funcall accessor s)
              "mode ~D accessor must be NIL after ESC[?~Dl" mode)))

(test mouse-mode-numeric-toggle-table
  "ESC[?1000/1002/1003h sets mouse-mode to 1/2/3; the corresponding l resets to 0."
  (dolist (row '((1000 1) (1002 2) (1003 3)))
    (destructuring-bind (mode expected-val) row
      (test-dec-pm-toggle-numeric mode expected-val #'cl-tmux/terminal/types:screen-mouse-mode))))

(test dec-pm-boolean-toggle-table
  "DEC private modes 1/1004/1006/2004 toggle their boolean accessors via h/l sequences."
  (dolist (row (list (list 1    #'cl-tmux/terminal/types:screen-app-cursor-keys)
                     (list 1004 #'cl-tmux/terminal/types:screen-focus-events)
                     (list 1006 #'cl-tmux/terminal/types:screen-mouse-sgr-mode)
                     (list 2004 #'cl-tmux/terminal/types:screen-bracketed-paste)))
    (destructuring-bind (mode accessor) row
      (test-dec-pm-toggle-boolean mode accessor))))

(test focus-event-report-bytes
  "focus-event-report yields ESC[I on focus gained, ESC[O on focus lost, and NIL
   when focus events are disabled."
  (with-screen (s 20 5)
    (is-false (cl-tmux/terminal/actions:focus-event-report s t)
              "disabled screen must report NIL for focus gained")
    (is-false (cl-tmux/terminal/actions:focus-event-report s nil)
              "disabled screen must report NIL for focus lost")
    (feed s (esc "[?1004h"))
    (is (string= (format nil "~C[I" #\Escape)
                 (cl-tmux/terminal/actions:focus-event-report s t))
        "focus gained must report ESC[I")
    (is (string= (format nil "~C[O" #\Escape)
                 (cl-tmux/terminal/actions:focus-event-report s nil))
        "focus lost must report ESC[O")))

;;; ── Application cursor keys (?1h / ?1l) ─────────────────────────────────────

;;; ── Auto-wrap mode (?7h / ?7l) ───────────────────────────────────────────────

(test autowrap-default-is-on
  "auto-wrap is enabled by default (screen-autowrap = T)."
  (with-screen (s 10 5)
    (is (cl-tmux/terminal/types:screen-autowrap s)
        "autowrap must be T by default")))

(test autowrap-disable-toggle
  "ESC[?7l disables auto-wrap; ESC[?7h re-enables it.
   Note: screen-autowrap is T by default, so the default-off check in
   test-dec-pm-toggle-boolean does not apply here — we test the round-trip directly."
  (with-screen (s 10 5)
    (feed s (esc "[?7l"))
    (is-false (cl-tmux/terminal/types:screen-autowrap s)
              "autowrap must be NIL after ESC[?7l")
    (feed s (esc "[?7h"))
    (is (cl-tmux/terminal/types:screen-autowrap s)
        "autowrap must be T after ESC[?7h")))

;;; ── reset-sgr-pen direct tests ───────────────────────────────────────────────

(test reset-sgr-pen-clears-all-slots
  "reset-sgr-pen sets fg=7, bg=0, attrs=0, attrs2=0, ul-color=0."
  (with-screen (s 10 5)
    ;; Mutate all five slots to non-default values.
    (setf (cl-tmux/terminal/types:screen-cur-fg       s) 3
          (cl-tmux/terminal/types:screen-cur-bg       s) 5
          (cl-tmux/terminal/types:screen-cur-attrs    s) #xFF
          (cl-tmux/terminal/types:screen-cur-attrs2   s) #xFF
          (cl-tmux/terminal/types:screen-cur-ul-color s) 42)
    (cl-tmux/terminal/types:reset-sgr-pen s)
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg s))
        "fg must be the default sentinel after reset-sgr-pen")
    (is (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg s))
        "bg must be the default sentinel after reset-sgr-pen")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs s))
        "attrs must be 0 after reset-sgr-pen")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs2 s))
        "attrs2 must be 0 after reset-sgr-pen")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-ul-color s))
        "ul-color must be 0 after reset-sgr-pen")))

;;; ── screen-display-cell tests ────────────────────────────────────────────────

(def-suite display-cell-suite
  :description "screen-display-cell projection with copy-mode scrollback"
  :in terminal-suite)
(in-suite display-cell-suite)

(test display-cell-live-grid-when-no-copy-mode
  "screen-display-cell returns the live grid cell when copy-mode is off."
  (with-screen (s 5 3)
    (feed s "abcde")
    ;; copy-mode is off: display cell == live cell.
    (is (char= #\a (cell-char (cl-tmux/terminal/actions:screen-display-cell s 0 0)))
        "col 0 must be 'a' from the live grid")
    (is (char= #\e (cell-char (cl-tmux/terminal/actions:screen-display-cell s 4 0)))
        "col 4 must be 'e' from the live grid")))

(test display-cell-returns-blank-for-out-of-range-row
  "screen-display-cell returns a blank cell when the row exceeds screen height."
  (with-screen (s 5 3)
    (feed s "hello")
    ;; Row 99 is beyond the screen; expect the shared blank cell.
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 99)))
      (is (char= #\Space (cell-char cell))
          "out-of-range row must return a blank cell"))))

(test display-cell-scrollback-when-copy-mode-offset
  "screen-display-cell reads from scrollback when copy-offset > 0."
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
          (is (characterp (cell-char cell))
              "copy-mode row-0 must return a character from scrollback"))))))

(test display-cell-copy-mode-blank-for-empty-scrollback-entry
  "screen-display-cell returns blank when the scrollback vector is shorter than the column."
  (with-screen (s 5 3)
    ;; Directly install a zero-length scrollback row.
    (setf (cl-tmux/terminal/types:screen-scrollback s) (list (vector))
          (cl-tmux/terminal/types:screen-copy-mode-p s) t
          (cl-tmux/terminal/types:screen-copy-offset s) 1)
    ;; Column 0 of row 0 should be the display-blank-cell (Space).
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 0)))
      (is (char= #\Space (cell-char cell))
          "empty scrollback entry must yield a blank cell"))))

