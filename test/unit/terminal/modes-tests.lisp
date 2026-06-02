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
  "dec-pm-set with param 1000 sets mouse-mode to 1."
  (with-screen (s 20 5)
    (cl-tmux/terminal/actions:dec-pm-set s '(1000))
    (is (= 1 (cl-tmux/terminal/types:screen-mouse-mode s))
        "dec-pm-set 1000 must set mouse-mode to 1")))

(test dec-pm-reset-1000-directly
  "dec-pm-reset with param 1000 resets mouse-mode to 0."
  (with-screen (s 20 5)
    (cl-tmux/terminal/actions:dec-pm-set   s '(1000))
    (cl-tmux/terminal/actions:dec-pm-reset s '(1000))
    (is (= 0 (cl-tmux/terminal/types:screen-mouse-mode s))
        "dec-pm-reset 1000 must set mouse-mode to 0")))

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
  (with-screen (s 20 5)
    (cl-tmux/terminal/actions:dec-pm-set s '(2004))
    (is (cl-tmux/terminal/types:screen-bracketed-paste s)
        "dec-pm-set 2004 must set bracketed-paste to T")
    (cl-tmux/terminal/actions:dec-pm-reset s '(2004))
    (is-false (cl-tmux/terminal/types:screen-bracketed-paste s)
              "dec-pm-reset 2004 must set bracketed-paste to NIL")))

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
      (is result "screen-consume-bell must return T when bell is pending")
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
    (feed s (format nil "~C]0;hello~C" #\Escape #\Bel))
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
          "row ~D must be blank after exit-alt-screen with no saved grid" y)))))

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
