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

(test decsc-decrc-preserves-g0-charset
  "ESC 7 / ESC 8 round-trip the G0 charset designation (tmux input_save_state saves
   the charset alongside the cursor, so a DECSC/DECRC pair must restore it)."
  (with-screen (s 20 5)
    (feed s (esc "(0"))                  ; G0 = DEC special graphics (line-drawing)
    (feed s (esc "7"))                   ; DECSC -- save (incl. charset)
    (feed s (esc "(B"))                  ; G0 = ASCII (change it)
    (is (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s))
        "G0 must be ascii before restore")
    (feed s (esc "8"))                   ; DECRC -- restore
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))
        "DECRC must restore G0 = dec-graphics")
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "DECRC must restore the effective charset (G0 active) = dec-graphics")))

(test decsc-decrc-preserves-active-charset
  "ESC 7 / ESC 8 round-trip the ACTIVE charset (which of G0/G1 is invoked via SO/SI)."
  (with-screen (s 20 5)
    (feed s (esc ")0"))                  ; G1 = DEC special graphics
    (feed s (string (code-char #x0E)))   ; SO -- invoke G1 (charset -> graphics)
    (feed s (esc "7"))                   ; DECSC -- save (active-g = g1)
    (feed s (string (code-char #x0F)))   ; SI -- invoke G0 (charset -> ascii)
    (is (eq :g0 (cl-tmux/terminal/types:screen-active-g s))
        "active-g must be g0 before restore")
    (feed s (esc "8"))                   ; DECRC -- restore
    (is (eq :g1 (cl-tmux/terminal/types:screen-active-g s))
        "DECRC must restore active-g = g1")
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "DECRC must restore the effective charset (G1 active) = dec-graphics")))

(test decsc-decrc-preserves-origin-mode
  "ESC 7 / ESC 8 round-trip DECOM origin mode (tmux records s->mode, incl. MODE_ORIGIN)."
  (with-screen (s 20 5)
    (feed s (esc "[?6h"))                ; DECOM origin mode ON
    (feed s (esc "7"))                   ; DECSC -- save (incl. origin mode)
    (feed s (esc "[?6l"))                ; DECOM origin mode OFF
    (is (not (cl-tmux/terminal/types:screen-origin-mode s))
        "origin mode must be off before restore")
    (feed s (esc "8"))                   ; DECRC -- restore
    (is (cl-tmux/terminal/types:screen-origin-mode s)
        "DECRC must restore origin mode = ON")))

(test decrc-without-save-resets-charset-and-origin-mode
  "ESC 8 with no prior DECSC resets charset and origin mode to VT100 defaults."
  (with-screen (s 20 5)
    (feed s (esc "(0"))                  ; G0 = dec-graphics
    (feed s (esc "[?6h"))                ; origin mode ON
    (feed s (esc "8"))                   ; DECRC with no prior save
    (is (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s))
        "G0 must reset to ascii")
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "effective charset must reset to ascii")
    (is (not (cl-tmux/terminal/types:screen-origin-mode s))
        "origin mode must reset to off")))

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

(test dec-pm-mode-6-origin-mode-set-and-reset
  "dec-pm-set/reset with param 6 toggles DECOM origin-mode: set → T, reset → NIL."
  (with-screen (s 20 5)
    (is-false (cl-tmux/terminal/types:screen-origin-mode s)
              "origin-mode must be NIL by default")
    (cl-tmux/terminal/actions:dec-pm-set s '(6))
    (is-true (cl-tmux/terminal/types:screen-origin-mode s)
             "origin-mode must be T after dec-pm-set 6")
    (cl-tmux/terminal/actions:dec-pm-reset s '(6))
    (is-false (cl-tmux/terminal/types:screen-origin-mode s)
              "origin-mode must be NIL after dec-pm-reset 6")))

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

