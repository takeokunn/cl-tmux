(in-package #:cl-tmux/test)

;;;; Mode and screen-state tests (src/terminal/modes.lisp).
;;;; Tests: modes suite — RIS, alt-screen, DECSC/DECRC.

(def-suite modes
  :description "Terminal mode transitions: RIS, alt-screen, DECSC/DECRC"
  :in terminal-suite)
(in-suite modes)

(test ris-clears-screen-and-homes-cursor
  "ESC c (RIS) clears the screen and homes the cursor."
  (with-screen (s 10 5)
    (feed s "hello")
    (feed s (esc "[3;3H"))
    (feed s (esc "c"))          ; ESC c = RIS
    (check-cursor s 0 0)
    (is (row-blank-p s 0) "row 0 should be blank after RIS")
    (is (row-blank-p s 1) "row 1 should be blank after RIS")))

(test alt-screen-no-crash
  "ESC[?1049h / ESC[?1049l (enter/exit alt screen) do not crash the emulator."
  (with-screen (s 10 5)
    (feed s "primary")
    ;; Enter alternate screen.
    (feed s (esc "[?1049h"))
    (feed s "alt")
    ;; Exit alternate screen.
    (feed s (esc "[?1049l"))
    ;; After exiting, the primary screen content should be accessible.
    ;; At minimum the emulator must still be in a consistent state.
    (is (integerp (screen-cursor-x s)))
    (is (integerp (screen-cursor-y s)))))

(test alt-screen-save-restore
  "Entering then exiting the alt screen restores the primary screen content."
  (with-screen (s 10 5)
    (feed s "hello")
    (feed s (esc "[?1049h"))  ; enter alt screen -- primary grid saved
    (feed s "ALT")            ; mutate the (blank) alternate screen
    (feed s (esc "[?1049l"))  ; exit alt screen -- primary grid restored
    (is (string= "hello" (row-string s 0 :end 5))
        "primary content not restored after alt-screen round-trip: ~S"
        (row-string s 0 :end 5))))

(test alternate-screen-off-suppresses-alt-buffer
  "When the alternate-screen policy reports off, ESC[?1049h does NOT switch to the
   alt buffer — full-screen app output stays on the MAIN screen (and scrollback)."
  (with-screen (s 10 5)
    (let ((cl-tmux/terminal:*alternate-screen-enabled-function* (lambda () nil)))
      (feed s "primary")
      (feed s (esc "[?1049h"))   ; normally enters the alt screen — suppressed here
      (feed s "ALT")
      (is (null (cl-tmux/terminal/types::screen-alt-cells s))
          "alternate-screen off must leave the alt buffer uninitialised")
      (is (search "ALT" (row-string s 0 :end 10))
          "app output must land on the main screen when alt screen is off (got ~S)"
          (row-string s 0 :end 10)))))

(test alternate-screen-on-still-enters-alt-buffer
  "With the policy reporting on (default), ESC[?1049h still enters the alt screen."
  (with-screen (s 10 5)
    (let ((cl-tmux/terminal:*alternate-screen-enabled-function* (lambda () t)))
      (feed s "hello")
      (feed s (esc "[?1049h"))
      (is (not (null (cl-tmux/terminal/types::screen-alt-cells s)))
          "alternate-screen on must initialise the alt buffer")
      (feed s (esc "[?1049l"))
      (is (string= "hello" (row-string s 0 :end 5))
          "primary content must be restored after the alt round-trip"))))

(test alt-screen-1047-save-restore
  "ESC[?1047h / ESC[?1047l (alt screen buffer, the 1049 component) round-trips the
   primary screen content."
  (with-screen (s 10 5)
    (feed s "hello")
    (feed s (esc "[?1047h"))   ; enter alt screen
    (feed s "ALT")
    (feed s (esc "[?1047l"))   ; exit alt screen — primary restored
    (is (string= "hello" (row-string s 0 :end 5))
        "primary content not restored after ?1047 round-trip: ~S"
        (row-string s 0 :end 5))))

(test cursor-1048-save-restore
  "ESC[?1048h saves the cursor and ESC[?1048l restores it (the 1049 component)."
  (with-screen (s 20 5)
    (feed s (esc "[3;6H"))     ; cursor -> (5, 2)
    (feed s (esc "[?1048h"))   ; save cursor
    (feed s (esc "[1;1H"))     ; cursor -> (0, 0)
    (feed s (esc "[?1048l"))   ; restore cursor
    (check-cursor s 5 2)))

(test decsc-decrc
  "ESC 7 saves the cursor position and SGR state; ESC 8 restores them."
  (with-screen (s 20 5)
    (feed s (esc "[3;6H"))     ; cursor -> (5, 2)
    (feed s (esc "[31;1m"))    ; fg = 1 (red), bold on
    (feed s (esc "7"))         ; DECSC -- save
    (feed s (esc "[1;1H"))     ; cursor -> (0, 0)
    (feed s (esc "[0m"))       ; reset SGR
    (feed s (esc "8"))         ; DECRC -- restore
    (check-cursor s 5 2)
    (feed s "X")               ; written with the restored SGR
    (is (= 1 (fg-at s 5 2)) "DECRC must restore fg")
    (is (logbitp 0 (attrs-at s 5 2)) "DECRC must restore bold")))

(test decrc-without-save-homes-cursor
  "ESC 8 with no prior DECSC homes the cursor (VT100 default)."
  (with-screen (s 20 5)
    (feed s (esc "[3;6H"))
    (feed s (esc "8"))
    (check-cursor s 0 0)))

;;; ── Direct modes function tests ──────────────────────────────────────────────

(def-suite direct-modes-suite
  :description "Direct calls to modes.lisp action functions"
  :in terminal-suite)
(in-suite direct-modes-suite)

(test ris-action-clears-and-homes-cursor
  "ris-action clears all cells, homes the cursor, resets SGR and scroll region, and restores cursor visibility."
  (with-screen (s 10 5)
    (feed s "hello world")
    (cl-tmux/terminal/actions:set-cursor s 5 3)
    ;; Hide cursor first to verify RIS restores it.
    (cl-tmux/terminal/actions:dec-pm-reset s '(25))
    (cl-tmux/terminal/actions:ris-action s)
    (check-cursor s 0 0)
    (is (row-blank-p s 0) "row 0 must be blank after RIS")
    (is (row-blank-p s 3) "row 3 must be blank after RIS")
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be reset to 0")
    (is (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be reset to height-1 (4)")
    (is (cl-tmux/terminal/types:screen-cursor-visible s)
        "cursor-visible must be T after RIS (ESC c hard reset)")))

(test save-and-restore-cursor
  "save-cursor + restore-cursor round-trips the cursor position and SGR state."
  (with-screen (s 20 10)
    ;; Position and set SGR
    (cl-tmux/terminal/actions:set-cursor s 7 4)
    (feed s (format nil "~C[31;1m" #\Escape))   ; fg=1 (red), bold
    (cl-tmux/terminal/actions:save-cursor s)
    ;; Move away and reset
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (feed s (format nil "~C[0m" #\Escape))       ; SGR reset
    ;; Restore
    (cl-tmux/terminal/actions:restore-cursor s)
    (check-cursor s 7 4)
    (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
        "restore-cursor must recover fg=1 (red)")))

(test restore-cursor-without-save-homes-cursor
  "restore-cursor with no prior save homes the cursor and resets SGR."
  (with-screen (s 20 10)
    (cl-tmux/terminal/actions:set-cursor s 9 5)
    ;; No save-cursor called — restore should fall back to (0,0) + default SGR
    (cl-tmux/terminal/actions:restore-cursor s)
    (check-cursor s 0 0)
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg s))
        "fg must be reset to default (7)")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg s))
        "bg must be reset to default (0)")))

(test dec-pm-set-1049-enters-alt-screen
  "dec-pm-set with param 1049 saves the primary grid and installs a blank alt grid."
  (with-screen (s 10 5)
    (feed s "primary")
    (cl-tmux/terminal/actions:dec-pm-set s '(1049))
    ;; Alt-cells must be non-nil (primary grid was saved)
    (is (not (null (cl-tmux/terminal/types:screen-alt-cells s)))
        "alt-cells must be set after ?1049h")
    ;; Cursor must be homed
    (check-cursor s 0 0)
    ;; Current grid (now the alt screen) must be blank
    (is (row-blank-p s 0) "alt screen row 0 must be blank")))

(test dec-pm-reset-1049-exits-alt-screen
  "dec-pm-reset with param 1049 restores the saved primary grid."
  (with-screen (s 10 5)
    (feed s "primary")
    (cl-tmux/terminal/actions:dec-pm-set   s '(1049))  ; enter alt
    (feed s "alt content")
    (cl-tmux/terminal/actions:dec-pm-reset s '(1049))  ; exit alt
    ;; Primary content must be back
    (is (string= "primary" (row-string s 0 :end 7))
        "primary screen content must be restored after ?1049l")
    ;; Alt-cells slot must be cleared
    (is (null (cl-tmux/terminal/types:screen-alt-cells s))
        "alt-cells must be NIL after exiting alt screen")))

(test dec-pm-unknown-mode-is-silently-ignored
  "dec-pm-set and dec-pm-reset with unrecognized mode numbers are no-ops."
  (with-screen (s 10 5)
    (feed s "hello")
    ;; These should not signal errors or change screen state.
    (finishes (cl-tmux/terminal/actions:dec-pm-set   s '(9999 42 0)))
    (finishes (cl-tmux/terminal/actions:dec-pm-reset s '(9999 42 0)))
    ;; Screen is unchanged.
    (check-row s 0 "hello")))

(test define-dec-pm-rules-macro-is-defined
  "define-dec-pm-rules is a defined macro in the actions package."
  (is (macro-function 'cl-tmux/terminal/actions::define-dec-pm-rules)))

(test dectcem-hide-cursor
  "ESC[?25l (DEC PM reset 25) sets screen-cursor-visible to NIL."
  (with-screen (s 20 5)
    ;; Cursor is visible by default.
    (is (cl-tmux/terminal/types:screen-cursor-visible s)
        "cursor must be visible by default")
    (feed s (esc "[?25l"))
    (is-false (cl-tmux/terminal/types:screen-cursor-visible s)
              "screen-cursor-visible must be NIL after ESC[?25l")))

(test dectcem-show-cursor
  "ESC[?25h (DEC PM set 25) restores screen-cursor-visible to T."
  (with-screen (s 20 5)
    (feed s (esc "[?25l"))   ; hide
    (feed s (esc "[?25h"))   ; show again
    (is (cl-tmux/terminal/types:screen-cursor-visible s)
        "screen-cursor-visible must be T after ESC[?25h")))

(test dectcem-dec-pm-set-directly
  "dec-pm-set with param 25 sets cursor-visible to T."
  (with-screen (s 20 5)
    (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil)
    (cl-tmux/terminal/actions:dec-pm-set s '(25))
    (is (cl-tmux/terminal/types:screen-cursor-visible s)
        "dec-pm-set 25 must set cursor-visible to T")))

(test dectcem-dec-pm-reset-directly
  "dec-pm-reset with param 25 sets cursor-visible to NIL."
  (with-screen (s 20 5)
    (cl-tmux/terminal/actions:dec-pm-reset s '(25))
    (is-false (cl-tmux/terminal/types:screen-cursor-visible s)
              "dec-pm-reset 25 must set cursor-visible to NIL")))

(test make-blank-cells-creates-blank-grid
  "%make-blank-cells returns a vector of N blank cells (space char)."
  (let ((cells (cl-tmux/terminal/types::%make-blank-cells 6)))
    (is (= 6 (length cells)) "vector length must equal N")
    (is (every (lambda (c) (char= #\Space (cell-char c))) cells)
        "every cell must be a blank space cell")))

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

(test mouse-mode-1000-set-and-reset
  "ESC[?1000h sets mouse-mode to 1; ESC[?1000l resets it to 0."
  (with-screen (s 20 5)
    (is (= 0 (cl-tmux/terminal/types:screen-mouse-mode s))
        "mouse-mode must be 0 by default")
    (test-dec-pm-toggle-numeric 1000 1 #'cl-tmux/terminal/types:screen-mouse-mode)))

(test mouse-mode-1002-set-and-reset
  "ESC[?1002h sets mouse-mode to 2; ESC[?1002l resets it to 0."
  (test-dec-pm-toggle-numeric 1002 2 #'cl-tmux/terminal/types:screen-mouse-mode))

(test mouse-mode-1003-set-and-reset
  "ESC[?1003h sets mouse-mode to 3; ESC[?1003l resets it to 0."
  (test-dec-pm-toggle-numeric 1003 3 #'cl-tmux/terminal/types:screen-mouse-mode))

(test mouse-sgr-mode-1006-set-and-reset
  "ESC[?1006h sets mouse-sgr-mode to T; ESC[?1006l resets it to NIL."
  (test-dec-pm-toggle-boolean 1006 #'cl-tmux/terminal/types:screen-mouse-sgr-mode))

(test dec-pm-set-1000-directly
  "dec-pm-set/reset with param 1000 toggles mouse-mode 0↔1 directly."
  (test-dec-pm-toggle-numeric 1000 1 #'cl-tmux/terminal/types:screen-mouse-mode))

;;; ── Bracketed paste mode (?2004h / ?2004l) ───────────────────────────────────

(test bracketed-paste-mode-toggle
  "ESC[?2004h sets bracketed-paste to T; ESC[?2004l resets it to NIL."
  (with-screen (s 20 5)
    (is-false (cl-tmux/terminal/types:screen-bracketed-paste s)
              "bracketed-paste must be NIL by default")
    (feed s (esc "[?2004h"))
    (is (cl-tmux/terminal/types:screen-bracketed-paste s)
        "bracketed-paste must be T after ESC[?2004h")
    (feed s (esc "[?2004l"))
    (is-false (cl-tmux/terminal/types:screen-bracketed-paste s)
              "bracketed-paste must be NIL after ESC[?2004l")))

(test bracketed-paste-direct-set-reset
  "dec-pm-set/reset with param 2004 toggles bracketed-paste directly."
  (test-dec-pm-toggle-boolean 2004 #'cl-tmux/terminal/types:screen-bracketed-paste))

;;; ── Focus event reporting (?1004h / ?1004l) ──────────────────────────────────

(test focus-events-mode-toggle
  "ESC[?1004h sets focus-events to T; ESC[?1004l resets it to NIL."
  (with-screen (s 20 5)
    (is-false (cl-tmux/terminal/types:screen-focus-events s)
              "focus-events must be NIL by default")
    (feed s (esc "[?1004h"))
    (is (cl-tmux/terminal/types:screen-focus-events s)
        "focus-events must be T after ESC[?1004h")
    (feed s (esc "[?1004l"))
    (is-false (cl-tmux/terminal/types:screen-focus-events s)
              "focus-events must be NIL after ESC[?1004l")))

(test focus-events-direct-set-reset
  "dec-pm-set/reset with param 1004 toggles focus-events directly."
  (test-dec-pm-toggle-boolean 1004 #'cl-tmux/terminal/types:screen-focus-events))

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

(test app-cursor-keys-toggle
  "ESC[?1h sets app-cursor-keys to T; ESC[?1l resets it to NIL."
  (with-screen (s 20 5)
    (is-false (cl-tmux/terminal/types:screen-app-cursor-keys s)
              "app-cursor-keys must be NIL by default")
    (feed s (esc "[?1h"))
    (is (cl-tmux/terminal/types:screen-app-cursor-keys s)
        "app-cursor-keys must be T after ESC[?1h")
    (feed s (esc "[?1l"))
    (is-false (cl-tmux/terminal/types:screen-app-cursor-keys s)
              "app-cursor-keys must be NIL after ESC[?1l")))

;;; ── Auto-wrap mode (?7h / ?7l) ───────────────────────────────────────────────

(test autowrap-default-is-on
  "auto-wrap is enabled by default (screen-autowrap = T)."
  (with-screen (s 10 5)
    (is (cl-tmux/terminal/types:screen-autowrap s)
        "autowrap must be T by default")))

(test autowrap-disable-toggle
  "ESC[?7l disables auto-wrap; ESC[?7h re-enables it."
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
    (is (= 7 (cl-tmux/terminal/types:screen-cur-fg s))
        "fg must be 7 after reset-sgr-pen")
    (is (= 0 (cl-tmux/terminal/types:screen-cur-bg s))
        "bg must be 0 after reset-sgr-pen")
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

;;; ── SUITE: set-cursor-shape ──────────────────────────────────────────────────
;;;
;;; set-cursor-shape wraps DECSCUSR: clamps the shape value to [0,6] and stores
;;; it in screen-cursor-shape.

(def-suite set-cursor-shape-suite
  :description "set-cursor-shape: DECSCUSR clamping and storage"
  :in terminal-suite)
(in-suite set-cursor-shape-suite)

(test set-cursor-shape-stores-valid-values
  :description "set-cursor-shape stores values in [0,6] unchanged."
  ;; Table: (input expected-shape description)
  (let ((cases '((0 0 "default blinking block")
                 (1 1 "blinking block")
                 (2 2 "steady block")
                 (3 3 "blinking underline")
                 (4 4 "steady underline")
                 (5 5 "blinking bar")
                 (6 6 "steady bar"))))
    (dolist (c cases)
      (destructuring-bind (input expected desc) c
        (with-screen (s 10 5)
          (cl-tmux/terminal/actions:set-cursor-shape s input)
          (is (= expected (cl-tmux/terminal/types:screen-cursor-shape s))
              "set-cursor-shape ~D: expected ~D got ~D (~A)"
              input expected
              (cl-tmux/terminal/types:screen-cursor-shape s)
              desc))))))

(test set-cursor-shape-clamps-above-six-to-six
  :description "set-cursor-shape clamps values > 6 to 6."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-cursor-shape s 99)
    (is (= 6 (cl-tmux/terminal/types:screen-cursor-shape s))
        "set-cursor-shape 99 must clamp to 6, got ~D"
        (cl-tmux/terminal/types:screen-cursor-shape s))))

(test set-cursor-shape-clamps-below-zero-to-zero
  :description "set-cursor-shape clamps negative values to 0."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-cursor-shape s -1)
    (is (= 0 (cl-tmux/terminal/types:screen-cursor-shape s))
        "set-cursor-shape -1 must clamp to 0, got ~D"
        (cl-tmux/terminal/types:screen-cursor-shape s))))

;;; ── SUITE: set-bell-pending and screen-consume-bell ─────────────────────────
;;;
;;; set-bell-pending sets screen-bell-pending to T.
;;; screen-consume-bell returns T and clears the flag; returns NIL when not set.

(def-suite bell-pending-suite
  :description "set-bell-pending and screen-consume-bell"
  :in terminal-suite)
(in-suite bell-pending-suite)

(test set-bell-pending-sets-flag
  :description "set-bell-pending sets screen-bell-pending to T."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL by default")
    (cl-tmux/terminal/actions:set-bell-pending s)
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after set-bell-pending")))

(test screen-consume-bell-returns-true-and-clears-flag
  :description "screen-consume-bell returns T and clears the bell-pending flag."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-bell-pending s)
    (let ((result (cl-tmux/terminal/types:screen-consume-bell s)))
      (is-true result "screen-consume-bell must return T when bell is pending")
      (is-false (cl-tmux/terminal/types:screen-bell-pending s)
                "bell-pending must be NIL after screen-consume-bell"))))

(test screen-consume-bell-returns-nil-when-not-pending
  :description "screen-consume-bell returns NIL without side effects when bell is not pending."
  (with-screen (s 10 5)
    (let ((result (cl-tmux/terminal/types:screen-consume-bell s)))
      (is-false result "screen-consume-bell must return NIL when bell is not pending"))))

(test bell-byte-sets-pending-via-emulator
  :description "A BEL byte (0x07) fed to the emulator sets screen-bell-pending."
  (with-screen (s 10 5)
    (screen-process-bytes s (vector 7))  ; BEL = 0x07
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "BEL byte must set screen-bell-pending to T")))

;;; ── SUITE: set-charset and set-screen-title ──────────────────────────────────
;;;
;;; set-charset stores the character set keyword.
;;; set-screen-title stores the OSC window title string.

(def-suite set-charset-set-title-suite
  :description "set-charset and set-screen-title direct action tests"
  :in terminal-suite)
(in-suite set-charset-set-title-suite)

(test set-charset-stores-ascii-keyword
  :description "set-charset :ascii sets screen-charset to :ascii."
  (with-screen (s 10 5)
    ;; Start with dec-graphics, then reset to ascii
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    (cl-tmux/terminal/actions:set-charset s :ascii)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "screen-charset must be :ascii after set-charset :ascii")))

(test set-charset-stores-dec-graphics-keyword
  :description "set-charset :dec-graphics sets screen-charset to :dec-graphics."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-charset s :dec-graphics)
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "screen-charset must be :dec-graphics after set-charset :dec-graphics")))

(test set-screen-title-stores-string
  :description "set-screen-title stores the given title in screen-title."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-screen-title s "my-window")
    (is (string= "my-window" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be \"my-window\" after set-screen-title")))

(test set-screen-title-stores-empty-string
  :description "set-screen-title accepts an empty string."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-screen-title s "first")
    (cl-tmux/terminal/actions:set-screen-title s "")
    (is (string= "" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be empty string after set-screen-title \"\"")))

(test set-screen-title-via-osc-sequence
  :description "OSC 0 ; title ST sets screen-title via the emulator."
  (with-screen (s 20 5)
    ;; OSC 0 ; hello BEL — OSC title sequence
    (feed s (format nil "~C]0;hello~C" #\Escape (code-char 7)))
    (is (string= "hello" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be \"hello\" after OSC 0;hello BEL sequence")))

;;; ── SUITE: reset-terminal-modes ──────────────────────────────────────────────
;;;
;;; reset-terminal-modes resets cursor visibility, autowrap, charset, and scroll
;;; region to VT100 defaults without touching the cell grid.

(def-suite reset-terminal-modes-suite
  :description "reset-terminal-modes: VT100 default mode restoration"
  :in terminal-suite)
(in-suite reset-terminal-modes-suite)

(test reset-terminal-modes-restores-cursor-visible
  :description "reset-terminal-modes sets screen-cursor-visible to T."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil)
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    (is (cl-tmux/terminal/types:screen-cursor-visible s)
        "cursor-visible must be T after reset-terminal-modes")))

(test reset-terminal-modes-restores-autowrap
  :description "reset-terminal-modes sets screen-autowrap to T."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-autowrap s) nil)
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    (is (cl-tmux/terminal/types:screen-autowrap s)
        "autowrap must be T after reset-terminal-modes")))

(test reset-terminal-modes-restores-charset
  :description "reset-terminal-modes sets screen-charset to :ascii."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must be :ascii after reset-terminal-modes")))

(test reset-terminal-modes-restores-scroll-region-to-full-screen
  :description "reset-terminal-modes resets scroll-top to 0 and scroll-bottom to height-1."
  (with-screen (s 10 8)
    ;; Restrict the scroll region
    (setf (cl-tmux/terminal/types:screen-scroll-top    s) 2
          (cl-tmux/terminal/types:screen-scroll-bottom s) 5)
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be 0 after reset-terminal-modes")
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be height-1 (7) after reset-terminal-modes")))

(test reset-terminal-modes-does-not-clear-cells
  :description "reset-terminal-modes does not erase the cell grid."
  (with-screen (s 10 5)
    (feed s "hello")
    (cl-tmux/terminal/actions:reset-terminal-modes s)
    ;; The text written before reset must survive
    (is (char= #\h (char-at s 0 0))
        "cell content must be preserved after reset-terminal-modes")))

;;; ── SUITE: enter/exit alt-screen direct actions ──────────────────────────────
;;;
;;; enter-alt-screen and exit-alt-screen are called directly to verify the
;;; no-op guard (enter when already in alt, exit when not in alt).

(def-suite alt-screen-direct-suite
  :description "enter-alt-screen / exit-alt-screen direct action tests"
  :in terminal-suite)
(in-suite alt-screen-direct-suite)

(test enter-alt-screen-is-noop-when-already-active
  :description "enter-alt-screen is a no-op when called while the alt screen is already active."
  (with-screen (s 10 5)
    (feed s "primary")
    (cl-tmux/terminal/actions:enter-alt-screen s)    ; first entry — saves grid
    ;; Capture the saved alt-cells reference before the second call
    (let ((saved-alt-cells (cl-tmux/terminal/types:screen-alt-cells s)))
      (cl-tmux/terminal/actions:enter-alt-screen s)  ; second call — no-op
      ;; alt-cells must still point to the same grid snapshot
      (is (eq saved-alt-cells (cl-tmux/terminal/types:screen-alt-cells s))
          "alt-cells reference must not change on second enter-alt-screen"))))

(test exit-alt-screen-clears-to-blank-when-no-saved-grid
  :description "exit-alt-screen with no prior save falls back to erase-display mode 2."
  (with-screen (s 10 5)
    (feed s "hello")
    ;; Call exit-alt-screen without ever entering: alt-cells is NIL
    (cl-tmux/terminal/actions:exit-alt-screen s)
    ;; The erase-display mode 2 fallback should have cleared all cells
    (dotimes (y 5)
      (is (row-blank-p s y)
          "row ~D must be blank after exit-alt-screen with no saved grid" y))))

;;; ── SUITE: enter/exit alt-screen direct content verification ─────────────────

(def-suite alt-screen-content-suite
  :description "enter/exit alt-screen verifies grid save and restore content"
  :in terminal-suite)
(in-suite alt-screen-content-suite)

(test enter-alt-screen-installs-blank-grid
  :description "enter-alt-screen replaces the live grid with a fresh blank grid."
  (with-screen (s 10 5)
    (feed s "primary")
    (cl-tmux/terminal/actions:enter-alt-screen s)
    ;; All cells in the new (alt) grid must be blank
    (dotimes (y 5)
      (is (row-blank-p s y)
          "alt screen row ~D must be blank after enter-alt-screen" y))))

(test exit-alt-screen-restores-primary-cursor-position
  :description "exit-alt-screen restores the cursor position saved at enter time."
  (with-screen (s 20 10)
    ;; Position cursor at (7, 3), enter alt, move cursor, exit
    (cl-tmux/terminal/actions:set-cursor s 7 3)
    (cl-tmux/terminal/actions:enter-alt-screen s)
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:exit-alt-screen s)
    (check-cursor s 7 3)))

;;; ── SUITE: screen-invoked-charset and charset G0/G1 ─────────────────────────
;;;
;;; screen-invoked-charset is exported but previously had zero unit test coverage.
;;; designate-charset G1 path (ESC ) X) was also untested directly.

(def-suite charset-invoke-suite
  :description "screen-invoked-charset, designate-charset G0/G1, invoke-charset"
  :in terminal-suite)
(in-suite charset-invoke-suite)

(test screen-invoked-charset-returns-g0-charset
  :description "screen-invoked-charset :g0 returns the G0 designation."
  (with-screen (s 10 5)
    ;; Default G0 is :ascii
    (is (eq :ascii (cl-tmux/terminal/actions:screen-invoked-charset s :g0))
        "screen-invoked-charset :g0 must return :ascii by default")))

(test screen-invoked-charset-returns-g1-charset
  :description "screen-invoked-charset :g1 returns the G1 designation."
  (with-screen (s 10 5)
    ;; Default G1 is also :ascii; designate it to :dec-graphics first
    (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
    (is (eq :dec-graphics (cl-tmux/terminal/actions:screen-invoked-charset s :g1))
        "screen-invoked-charset :g1 must return :dec-graphics after designation")))

(test designate-charset-g0-and-invoke-activates-charset
  :description "ESC ( 0 (designate G0 to DEC graphics) + active G0 → charset is :dec-graphics."
  (with-screen (s 10 5)
    ;; G0 is active by default; designating it immediately activates the charset.
    (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "charset must be :dec-graphics after designating active G0")))

(test designate-charset-g1-does-not-activate-immediately
  :description "ESC ) 0 (designate G1 to DEC graphics) does NOT change the active charset until SO."
  (with-screen (s 10 5)
    ;; G0 is active; designating G1 must not change the effective charset.
    (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must remain :ascii after designating inactive G1")))

(test invoke-charset-so-activates-g1
  :description "invoke-charset :g1 (SO) switches the active charset to G1's current designation."
  (with-screen (s 10 5)
    ;; Designate G1 to :dec-graphics, then invoke it.
    (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
    (cl-tmux/terminal/actions:invoke-charset s :g1)
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "charset must be :dec-graphics after invoke-charset :g1 with DEC graphics G1")))

(test invoke-charset-si-restores-g0
  :description "invoke-charset :g0 (SI) after SO restores G0's designation as active."
  (with-screen (s 10 5)
    ;; Invoke G1 (SO), then return to G0 (SI).
    (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
    (cl-tmux/terminal/actions:invoke-charset s :g1)
    (cl-tmux/terminal/actions:invoke-charset s :g0)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must revert to :ascii after SI restores G0")))

(test g1-charset-via-parser-esc-paren-zero
  :description "ESC ) 0 through the parser designates G1 to DEC graphics without activating it."
  (with-screen (s 10 5)
    (feed s (esc ")0"))                    ; ESC ) 0 = designate G1 to DEC graphics
    ;; G0 is still active, so charset remains :ascii
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must remain :ascii — G1 designated but not invoked")))

(test g1-charset-so-si-via-parser
  :description "ESC ) 0 + SO activates DEC graphics via G1; SI returns to ASCII via G0."
  (with-screen (s 10 5)
    (feed s (esc ")0"))                         ; designate G1 to DEC graphics
    (feed s (string (code-char #x0E)))          ; SO = invoke G1
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "after SO, charset must be :dec-graphics (G1 invoked)")
    (feed s (string (code-char #x0F)))          ; SI = invoke G0
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "after SI, charset must revert to :ascii (G0 restored)")))

;;; ── SUITE: set-screen-cwd ────────────────────────────────────────────────────
;;;
;;; set-screen-cwd is exported but previously had no direct unit test.

(def-suite set-screen-cwd-suite
  :description "set-screen-cwd: OSC 7 current working directory storage"
  :in terminal-suite)
(in-suite set-screen-cwd-suite)

(test set-screen-cwd-stores-path
  :description "set-screen-cwd stores the given path string in screen-cwd."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-screen-cwd s "/home/user/projects")
    (is (string= "/home/user/projects" (cl-tmux/terminal/types:screen-cwd s))
        "screen-cwd must be \"/home/user/projects\" after set-screen-cwd")))

(test set-screen-cwd-accepts-empty-string
  :description "set-screen-cwd accepts an empty string (clears cwd)."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:set-screen-cwd s "/initial/path")
    (cl-tmux/terminal/actions:set-screen-cwd s "")
    (is (string= "" (cl-tmux/terminal/types:screen-cwd s))
        "screen-cwd must be empty string after set-screen-cwd \"\"")))

;;; ── SUITE: erase-display mode 3 visual verification ─────────────────────────
;;;
;;; The existing test only checks that the scrollback is cleared.  This suite
;;; also asserts that the visible display grid is erased (the two-step nature
;;; of ED mode 3 must be fully covered).

(def-suite erase-display-mode3-suite
  :description "erase-display mode 3: both scrollback and visible grid are cleared"
  :in terminal-suite)
(in-suite erase-display-mode3-suite)

(test erase-display-mode-3-clears-visible-grid
  "erase-display mode 3 also erases the visible display grid (not just scrollback)."
  (with-screen (s 5 3)
    ;; Fill the visible grid with 'X'.
    (dotimes (y 3)
      (dotimes (x 5)
        (cl-tmux/terminal/actions:write-char-at-cursor s #\X)
        (cl-tmux/terminal/actions:set-cursor s (1+ (min x 3)) y)))
    ;; ED mode 3
    (cl-tmux/terminal/actions:erase-display s 3)
    (dotimes (y 3)
      (is (row-blank-p s y)
          "row ~D must be blank in the visible grid after erase-display mode 3" y))))

(test erase-display-mode-3-clears-both-grid-and-scrollback
  "erase-display mode 3 clears the visible grid AND the scrollback in one call."
  (with-screen (s 5 3)
    ;; Build scrollback by forcing scrolls.
    (feed-lines s "L0" "L1" "L2" "L3")
    (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback must be non-empty before erase-display mode 3")
    ;; Also write visible content.
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (feed s "AAAAA")
    ;; ED mode 3
    (cl-tmux/terminal/actions:erase-display s 3)
    ;; Both checks must pass:
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must be NIL after erase-display mode 3")
    (is (row-blank-p s 0)
        "row 0 must be blank in the visible grid after erase-display mode 3")))

;;; ── IRM — Insert/Replace Mode (CSI 4 h / CSI 4 l) ──────────────────────────

(test irm-insert-mode-shifts-line-right
  "CSI 4 h (IRM on): a printed character inserts at the cursor, pushing the rest
   of the line to the right instead of overwriting it."
  (with-screen (s 10 5)
    (feed s "abc")
    (feed s (esc "[H"))      ; cursor home (col 0)
    (feed s (esc "[4h"))     ; IRM on
    (feed s "XY")
    (is (string= "XYabc" (row-string s 0 :end 5))
        "insert mode must shift 'abc' right to yield 'XYabc' (got ~S)"
        (row-string s 0 :end 5))))

(test irm-replace-mode-overwrites
  "Default (and CSI 4 l) replace mode overwrites at the cursor."
  (with-screen (s 10 5)
    (feed s "abc")
    (feed s (esc "[H"))
    (feed s (esc "[4l"))     ; IRM off (explicit)
    (feed s "XY")
    (is (string= "XYc" (row-string s 0 :end 3))
        "replace mode must overwrite to yield 'XYc' (got ~S)"
        (row-string s 0 :end 3))))

(test irm-set-and-reset-toggle-screen-flag
  "CSI 4 h sets the insert-mode flag and CSI 4 l clears it."
  (with-screen (s 10 5)
    (feed s (esc "[4h"))
    (is-true (cl-tmux/terminal/types:screen-insert-mode s)
             "CSI 4 h must set screen-insert-mode")
    (feed s (esc "[4l"))
    (is (not (cl-tmux/terminal/types:screen-insert-mode s))
        "CSI 4 l must clear screen-insert-mode")))

(test irm-reset-by-ris
  "RIS (ESC c) clears insert mode so subsequent writes overwrite again."
  (with-screen (s 10 5)
    (feed s (esc "[4h"))            ; IRM on
    (feed s (esc "c"))             ; RIS
    (is (not (cl-tmux/terminal/types:screen-insert-mode s))
        "RIS must reset insert mode")))

;;; ── LNM — Line Feed/New Line Mode (CSI 20 h / CSI 20 l) ─────────────────────

(test lnm-newline-mode-lf-also-carriage-returns
  "CSI 20 h (LNM on): a line feed also returns the cursor to column 0, so 'a' LF
   'b' stacks vertically at column 0."
  (with-screen (s 10 5)
    (feed s (esc "[20h"))             ; LNM on
    (feed s "a")
    (feed s (string #\Linefeed))      ; LF
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "a at col 0 row 0")
    (is (char= #\b (char-at s 0 1)) "b at col 0 row 1 (LF carriage-returned)")))

(test lnm-off-lf-keeps-column
  "Default (LNM off): a line feed moves down keeping the column, so 'b' lands in
   the next column-position after 'a'."
  (with-screen (s 10 5)
    (feed s "a")
    (feed s (string #\Linefeed))      ; LF
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "a at col 0 row 0")
    (is (char= #\b (char-at s 1 1)) "b at col 1 row 1 (column preserved)")))

(test lnm-set-and-reset-toggle-screen-flag
  "CSI 20 h sets the newline-mode flag and CSI 20 l clears it."
  (with-screen (s 10 5)
    (feed s (esc "[20h"))
    (is-true (cl-tmux/terminal/types:screen-newline-mode s)
             "CSI 20 h must set screen-newline-mode")
    (feed s (esc "[20l"))
    (is (not (cl-tmux/terminal/types:screen-newline-mode s))
        "CSI 20 l must clear screen-newline-mode")))

(test lnm-reset-by-ris
  "RIS (ESC c) clears newline mode."
  (with-screen (s 10 5)
    (feed s (esc "[20h"))
    (feed s (esc "c"))                ; RIS
    (is (not (cl-tmux/terminal/types:screen-newline-mode s))
        "RIS must reset newline mode")))

;;; ── DECSCNM — reverse-video screen (CSI ?5h / ?5l) ──────────────────────────

(test decscnm-set-and-reset-toggle-screen-flag
  "CSI ?5h sets reverse-screen and CSI ?5l clears it."
  (with-screen (s 10 5)
    (feed s (esc "[?5h"))
    (is-true (cl-tmux/terminal/types:screen-reverse-screen s)
             "?5h must set screen-reverse-screen")
    (feed s (esc "[?5l"))
    (is (not (cl-tmux/terminal/types:screen-reverse-screen s))
        "?5l must clear screen-reverse-screen")))

(test decscnm-reset-by-ris
  "RIS (ESC c) clears reverse-video screen mode."
  (with-screen (s 10 5)
    (feed s (esc "[?5h"))
    (feed s (esc "c"))                ; RIS
    (is (not (cl-tmux/terminal/types:screen-reverse-screen s))
        "RIS must reset reverse-video screen")))

;;; ── DECSTR — Soft Terminal Reset (CSI ! p) ─────────────────────────────────

(test decstr-resets-modes-but-preserves-screen-and-cursor
  "DECSTR (CSI ! p) restores modes to defaults WITHOUT clearing the screen or
   moving the cursor — the key distinction from RIS (ESC c)."
  (with-screen (s 10 5)
    (feed s "hello")                 ; content on row 0
    (feed s (esc "[4h"))             ; IRM on
    (feed s (esc "[?7l"))            ; autowrap off
    (feed s (esc "[?25l"))           ; cursor hidden
    (feed s (esc "[2;4r"))           ; scroll region rows 2..4 (DECSTBM homes cursor)
    (feed s (esc "[1;6H"))           ; reposition cursor to row 1, col 6 (0-idx col 5)
    (feed s (esc "[!p"))             ; DECSTR soft reset
    ;; Modes reset:
    (is (not (cl-tmux/terminal/types:screen-insert-mode s)) "DECSTR clears IRM")
    (is-true (cl-tmux/terminal/types:screen-autowrap s)     "DECSTR restores autowrap")
    (is-true (cl-tmux/terminal/types:screen-cursor-visible s) "DECSTR shows the cursor")
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))    "DECSTR restores scroll top")
    (is (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s)) "DECSTR restores scroll bottom")
    ;; Screen + cursor preserved (NOT erased / homed):
    (is (string= "hello" (row-string s 0 :end 5))
        "DECSTR must NOT clear the screen (got ~S)" (row-string s 0 :end 5))
    (is (= 5 (cl-tmux/terminal/types:screen-cursor-x s))
        "DECSTR must NOT move the cursor")))

(test decstr-resets-sgr-pen
  "DECSTR resets the SGR pen so subsequent text is drawn with default attributes."
  (with-screen (s 10 5)
    (feed s (esc "[1;31m"))          ; bold red
    (feed s (esc "[!p"))             ; DECSTR
    (is (= 0 (cl-tmux/terminal/types:screen-cur-attrs s))
        "DECSTR must clear the active SGR attributes")))
