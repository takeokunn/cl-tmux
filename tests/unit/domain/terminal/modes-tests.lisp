(in-package #:cl-tmux/test)

;;;; Mode and screen-state tests (src/domain/terminal/modes-alt-screen.lisp, modes-dec-pm.lisp).
;;;; Tests: modes suite — RIS, alt-screen, DECSC/DECRC.

(describe "terminal-suite/modes"

  ;; ESC c (RIS) clears the screen and homes the cursor.
  (it "ris-clears-screen-and-homes-cursor"
    (with-screen (s 10 5)
      (feed s "hello")
      (feed s (esc "[3;3H"))
      (feed s (esc "c"))          ; ESC c = RIS
      (check-cursor s 0 0)
      (expect (row-blank-p s 0))
      (expect (row-blank-p s 1))))

  ;; ESC[?1049h / ESC[?1049l (enter/exit alt screen) do not crash the emulator.
  (it "alt-screen-no-crash"
    (with-screen (s 10 5)
      (feed s "primary")
      ;; Enter alternate screen.
      (feed s (esc "[?1049h"))
      (feed s "alt")
      ;; Exit alternate screen.
      (feed s (esc "[?1049l"))
      ;; After exiting, the primary screen content should be accessible.
      ;; At minimum the emulator must still be in a consistent state.
      (expect (integerp (screen-cursor-x s)))
      (expect (integerp (screen-cursor-y s)))))

  ;; Entering then exiting the alt screen restores the primary screen content.
  (it "alt-screen-save-restore"
    (with-screen (s 10 5)
      (feed s "hello")
      (feed s (esc "[?1049h"))  ; enter alt screen -- primary grid saved
      (feed s "ALT")            ; mutate the (blank) alternate screen
      (feed s (esc "[?1049l"))  ; exit alt screen -- primary grid restored
      (expect (string= "hello" (row-string s 0 :end 5)))))

  ;; When the alternate-screen policy reports off, ESC[?1049h does NOT switch to the
  ;; alt buffer — full-screen app output stays on the MAIN screen (and scrollback).
  (it "alternate-screen-off-suppresses-alt-buffer"
    (with-screen (s 10 5)
      (let ((cl-tmux/terminal:*alternate-screen-enabled-function* (lambda () nil)))
        (feed s "primary")
        (feed s (esc "[?1049h"))   ; normally enters the alt screen — suppressed here
        (feed s "ALT")
        (expect (null (cl-tmux/terminal/types::screen-alt-cells s)))
        (expect (search "ALT" (row-string s 0 :end 10))))))

  ;; With the policy reporting on (default), ESC[?1049h still enters the alt screen.
  (it "alternate-screen-on-still-enters-alt-buffer"
    (with-screen (s 10 5)
      (let ((cl-tmux/terminal:*alternate-screen-enabled-function* (lambda () t)))
        (feed s "hello")
        (feed s (esc "[?1049h"))
        (expect (not (null (cl-tmux/terminal/types::screen-alt-cells s))))
        (feed s (esc "[?1049l"))
        (expect (string= "hello" (row-string s 0 :end 5))))))

  ;; ESC[?1047h / ESC[?1047l (alt screen buffer, the 1049 component) round-trips the
  ;; primary screen content.
  (it "alt-screen-1047-save-restore"
    (with-screen (s 10 5)
      (feed s "hello")
      (feed s (esc "[?1047h"))   ; enter alt screen
      (feed s "ALT")
      (feed s (esc "[?1047l"))   ; exit alt screen — primary restored
      (expect (string= "hello" (row-string s 0 :end 5)))))

  ;; ESC[?1048h saves the cursor and ESC[?1048l restores it (the 1049 component).
  (it "cursor-1048-save-restore"
    (with-screen (s 20 5)
      (feed s (esc "[3;6H"))     ; cursor -> (5, 2)
      (feed s (esc "[?1048h"))   ; save cursor
      (feed s (esc "[1;1H"))     ; cursor -> (0, 0)
      (feed s (esc "[?1048l"))   ; restore cursor
      (check-cursor s 5 2)))

  ;; ESC 7 saves the cursor position and SGR state; ESC 8 restores them.
  (it "decsc-decrc"
    (with-screen (s 20 5)
      (feed s (esc "[3;6H"))     ; cursor -> (5, 2)
      (feed s (esc "[31;1m"))    ; fg = 1 (red), bold on
      (feed s (esc "7"))         ; DECSC -- save
      (feed s (esc "[1;1H"))     ; cursor -> (0, 0)
      (feed s (esc "[0m"))       ; reset SGR
      (feed s (esc "8"))         ; DECRC -- restore
      (check-cursor s 5 2)
      (feed s "X")               ; written with the restored SGR
      (expect (= 1 (fg-at s 5 2)))
      (expect (logbitp 0 (attrs-at s 5 2)))))

  ;; ESC 8 with no prior DECSC homes the cursor (VT100 default).
  (it "decrc-without-save-homes-cursor"
    (with-screen (s 20 5)
      (feed s (esc "[3;6H"))
      (feed s (esc "8"))
      (check-cursor s 0 0)))

  ;; ESC 7 / ESC 8 round-trip the G0 charset designation (tmux input_save_state saves
  ;; the charset alongside the cursor, so a DECSC/DECRC pair must restore it).
  (it "decsc-decrc-preserves-g0-charset"
    (with-screen (s 20 5)
      (feed s (esc "(0"))                  ; G0 = DEC special graphics (line-drawing)
      (feed s (esc "7"))                   ; DECSC -- save (incl. charset)
      (feed s (esc "(B"))                  ; G0 = ASCII (change it)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s)))
      (feed s (esc "8"))                   ; DECRC -- restore
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s)))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))))

  ;; ESC 7 / ESC 8 round-trip the ACTIVE charset (which of G0/G1 is invoked via SO/SI).
  (it "decsc-decrc-preserves-active-charset"
    (with-screen (s 20 5)
      (feed s (esc ")0"))                  ; G1 = DEC special graphics
      (feed s (string (code-char #x0E)))   ; SO -- invoke G1 (charset -> graphics)
      (feed s (esc "7"))                   ; DECSC -- save (active-g = g1)
      (feed s (string (code-char #x0F)))   ; SI -- invoke G0 (charset -> ascii)
      (expect (eq :g0 (cl-tmux/terminal/types:screen-active-g s)))
      (feed s (esc "8"))                   ; DECRC -- restore
      (expect (eq :g1 (cl-tmux/terminal/types:screen-active-g s)))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))))

  ;; ESC 7 / ESC 8 round-trip DECOM origin mode (tmux records s->mode, incl. MODE_ORIGIN).
  (it "decsc-decrc-preserves-origin-mode"
    (with-screen (s 20 5)
      (feed s (esc "[?6h"))                ; DECOM origin mode ON
      (feed s (esc "7"))                   ; DECSC -- save (incl. origin mode)
      (feed s (esc "[?6l"))                ; DECOM origin mode OFF
      (expect (not (cl-tmux/terminal/types:screen-origin-mode s)))
      (feed s (esc "8"))                   ; DECRC -- restore
      (expect (cl-tmux/terminal/types:screen-origin-mode s))))

  ;; ESC 8 with no prior DECSC resets charset and origin mode to VT100 defaults.
  (it "decrc-without-save-resets-charset-and-origin-mode"
    (with-screen (s 20 5)
      (feed s (esc "(0"))                  ; G0 = dec-graphics
      (feed s (esc "[?6h"))                ; origin mode ON
      (feed s (esc "8"))                   ; DECRC with no prior save
      (expect (eq :ascii (cl-tmux/terminal/types:screen-g0-charset s)))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))
      (expect (not (cl-tmux/terminal/types:screen-origin-mode s))))))

;;; ── Direct modes function tests ──────────────────────────────────────────────

(describe "terminal-suite/direct-modes-suite"

  ;; ris-action clears all cells, homes the cursor, resets SGR and scroll region, and restores cursor visibility.
  (it "ris-action-clears-and-homes-cursor"
    (with-screen (s 10 5)
      (feed s "hello world")
      (cl-tmux/terminal/actions:set-cursor s 5 3)
      ;; Hide cursor first to verify RIS restores it.
      (cl-tmux/terminal/actions:dec-pm-reset s '(25))
      (cl-tmux/terminal/actions:ris-action s)
      (check-cursor s 0 0)
      (expect (row-blank-p s 0))
      (expect (row-blank-p s 3))
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s)))
      (expect (cl-tmux/terminal/types:screen-cursor-visible s))))

  ;; save-cursor + restore-cursor round-trips the cursor position and SGR state.
  (it "save-and-restore-cursor"
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
      (expect (= 1 (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;; restore-cursor with no prior save homes the cursor and resets SGR.
  (it "restore-cursor-without-save-homes-cursor"
    (with-screen (s 20 10)
      (cl-tmux/terminal/actions:set-cursor s 9 5)
      ;; No save-cursor called — restore should fall back to (0,0) + default SGR
      (cl-tmux/terminal/actions:restore-cursor s)
      (check-cursor s 0 0)
      (expect (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-fg s)))
      (expect (= cl-tmux/terminal/types:+default-color+ (cl-tmux/terminal/types:screen-cur-bg s)))))

  ;; dec-pm-set with param 1049 saves the primary grid and installs a blank alt grid.
  (it "dec-pm-set-1049-enters-alt-screen"
    (with-screen (s 10 5)
      (feed s "primary")
      (cl-tmux/terminal/actions:dec-pm-set s '(1049))
      ;; Alt-cells must be non-nil (primary grid was saved)
      (expect (not (null (cl-tmux/terminal/types:screen-alt-cells s))))
      ;; Cursor must be homed
      (check-cursor s 0 0)
      ;; Current grid (now the alt screen) must be blank
      (expect (row-blank-p s 0))))

  ;; dec-pm-reset with param 1049 restores the saved primary grid.
  (it "dec-pm-reset-1049-exits-alt-screen"
    (with-screen (s 10 5)
      (feed s "primary")
      (cl-tmux/terminal/actions:dec-pm-set   s '(1049))  ; enter alt
      (feed s "alt content")
      (cl-tmux/terminal/actions:dec-pm-reset s '(1049))  ; exit alt
      ;; Primary content must be back
      (expect (string= "primary" (row-string s 0 :end 7)))
      ;; Alt-cells slot must be cleared
      (expect (null (cl-tmux/terminal/types:screen-alt-cells s)))))

  ;; dec-pm-set and dec-pm-reset with unrecognized mode numbers are no-ops.
  (it "dec-pm-unknown-mode-is-silently-ignored"
    (with-screen (s 10 5)
      (feed s "hello")
      ;; These should not signal errors or change screen state.
      (finishes (cl-tmux/terminal/actions:dec-pm-set   s '(9999 42 0)))
      (finishes (cl-tmux/terminal/actions:dec-pm-reset s '(9999 42 0)))
      ;; Screen is unchanged.
      (check-row s 0 "hello")))

  ;; dec-pm-set/reset with param 6 toggles DECOM origin-mode: set → T, reset → NIL.
  (it "dec-pm-mode-6-origin-mode-set-and-reset"
    (with-screen (s 20 5)
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-falsy)
      (cl-tmux/terminal/actions:dec-pm-set s '(6))
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-truthy)
      (cl-tmux/terminal/actions:dec-pm-reset s '(6))
      (expect (cl-tmux/terminal/types:screen-origin-mode s) :to-be-falsy)))

  ;; ESC[?25l hides the cursor (screen-cursor-visible → NIL); ESC[?25h shows it again (→ T).
  (it "dectcem-hide-and-show"
    (with-screen (s 20 5)
      (expect (cl-tmux/terminal/types:screen-cursor-visible s))
      (feed s (esc "[?25l"))
      (expect (cl-tmux/terminal/types:screen-cursor-visible s) :to-be-falsy)
      (feed s (esc "[?25h"))
      (expect (cl-tmux/terminal/types:screen-cursor-visible s))))

  ;; dec-pm-set 25 makes cursor-visible T; dec-pm-reset 25 makes it NIL.
  (it "dectcem-dec-pm-direct"
    (with-screen (s 20 5)
      (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil)
      (cl-tmux/terminal/actions:dec-pm-set s '(25))
      (expect (cl-tmux/terminal/types:screen-cursor-visible s))
      (cl-tmux/terminal/actions:dec-pm-reset s '(25))
      (expect (cl-tmux/terminal/types:screen-cursor-visible s) :to-be-falsy)))

  ;; %make-blank-cells returns a vector of N blank cells (space char).
  (it "make-blank-cells-creates-blank-grid"
    (let ((cells (cl-tmux/terminal/types::%make-blank-cells 6)))
      (expect (= 6 (length cells)))
      (expect (every (lambda (c) (char= #\Space (cell-char c))) cells)))))
