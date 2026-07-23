(in-package #:cl-tmux/test)

;;;; modes tests — part C: screen-invoked-charset / charset G0/G1, set-screen-cwd,
;;;; erase-display mode-3, IRM insert/replace, LNM line-feed, DECSCNM, DECSTR.

;;; ── SUITE: screen-invoked-charset and charset G0/G1 ─────────────────────────
;;;
;;; screen-invoked-charset is exported but previously had zero unit test coverage.
;;; designate-charset G1 path (ESC ) X) was also untested directly.

(describe "terminal-suite/charset-invoke-suite"

  ;; screen-invoked-charset :g0 returns the G0 designation.
  (it "screen-invoked-charset-returns-g0-charset"
    (with-screen (s 10 5)
      ;; Default G0 is :ascii
      (expect (eq :ascii (cl-tmux/terminal/actions:screen-invoked-charset s :g0)))))

  ;; screen-invoked-charset :g1 returns the G1 designation.
  (it "screen-invoked-charset-returns-g1-charset"
    (with-screen (s 10 5)
      ;; Default G1 is also :ascii; designate it to :dec-graphics first
      (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
      (expect (eq :dec-graphics (cl-tmux/terminal/actions:screen-invoked-charset s :g1)))))

  ;; ESC ( 0 (designate G0 to DEC graphics) + active G0 → charset is :dec-graphics.
  (it "designate-charset-g0-and-invoke-activates-charset"
    (with-screen (s 10 5)
      ;; G0 is active by default; designating it immediately activates the charset.
      (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))))

  ;; ESC ) 0 (designate G1 to DEC graphics) does NOT change the active charset until SO.
  (it "designate-charset-g1-does-not-activate-immediately"
    (with-screen (s 10 5)
      ;; G0 is active; designating G1 must not change the effective charset.
      (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; invoke-charset :g1 (SO) switches the active charset to G1's current designation.
  (it "invoke-charset-so-activates-g1"
    (with-screen (s 10 5)
      ;; Designate G1 to :dec-graphics, then invoke it.
      (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
      (cl-tmux/terminal/actions:invoke-charset s :g1)
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))))

  ;; invoke-charset :g0 (SI) after SO restores G0's designation as active.
  (it "invoke-charset-si-restores-g0"
    (with-screen (s 10 5)
      ;; Invoke G1 (SO), then return to G0 (SI).
      (cl-tmux/terminal/actions:designate-charset s :g1 :dec-graphics)
      (cl-tmux/terminal/actions:invoke-charset s :g1)
      (cl-tmux/terminal/actions:invoke-charset s :g0)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; ESC ) 0 through the parser designates G1 to DEC graphics without activating it.
  (it "g1-charset-via-parser-esc-paren-zero"
    (with-screen (s 10 5)
      (feed s (esc ")0"))                    ; ESC ) 0 = designate G1 to DEC graphics
      ;; G0 is still active, so charset remains :ascii
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; ESC ) 0 + SO activates DEC graphics via G1; SI returns to ASCII via G0.
  (it "g1-charset-so-si-via-parser"
    (with-screen (s 10 5)
      (feed s (esc ")0"))                         ; designate G1 to DEC graphics
      (feed s (string (code-char #x0E)))          ; SO = invoke G1
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))
      (feed s (string (code-char #x0F)))          ; SI = invoke G0
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s))))))

;;; ── SUITE: set-screen-cwd ────────────────────────────────────────────────────
;;;
;;; set-screen-cwd is exported but previously had no direct unit test.

(describe "terminal-suite/set-screen-cwd-suite"

  ;; set-screen-cwd stores the given path string in screen-cwd.
  (it "set-screen-cwd-stores-path"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-screen-cwd s "/home/user/projects")
      (expect (string= "/home/user/projects" (cl-tmux/terminal/types:screen-cwd s)))))

  ;; set-screen-cwd accepts an empty string (clears cwd).
  (it "set-screen-cwd-accepts-empty-string"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:set-screen-cwd s "/initial/path")
      (cl-tmux/terminal/actions:set-screen-cwd s "")
      (expect (string= "" (cl-tmux/terminal/types:screen-cwd s))))))

;;; ── SUITE: erase-display mode 3 visual verification ─────────────────────────
;;;
;;; The existing test only checks that the scrollback is cleared.  This suite
;;; also asserts that the visible display grid is erased (the two-step nature
;;; of ED mode 3 must be fully covered).

(describe "terminal-suite/erase-display-mode3-suite"

  ;; erase-display mode 3 also erases the visible display grid (not just scrollback).
  (it "erase-display-mode-3-clears-visible-grid"
    (with-screen (s 5 3)
      ;; Fill the visible grid with 'X'.
      (dotimes (y 3)
        (dotimes (x 5)
          (cl-tmux/terminal/actions:write-char-at-cursor s #\X)
          (cl-tmux/terminal/actions:set-cursor s (1+ (min x 3)) y)))
      ;; ED mode 3
      (cl-tmux/terminal/actions:erase-display s 3)
      (dotimes (y 3)
        (expect (row-blank-p s y)))))

  ;; erase-display mode 3 clears the visible grid AND the scrollback in one call.
  (it "erase-display-mode-3-clears-both-grid-and-scrollback"
    (with-screen (s 5 3)
      ;; Build scrollback by forcing scrolls.
      (feed-lines s "L0" "L1" "L2" "L3")
      (expect (plusp (length (cl-tmux/terminal/types:screen-scrollback s))))
      ;; Also write visible content.
      (cl-tmux/terminal/actions:set-cursor s 0 0)
      (feed s "AAAAA")
      ;; ED mode 3
      (cl-tmux/terminal/actions:erase-display s 3)
      ;; Both checks must pass:
      (expect (null (cl-tmux/terminal/types:screen-scrollback s)))
      (expect (row-blank-p s 0))))

  ;;; ── IRM — Insert/Replace Mode (CSI 4 h / CSI 4 l) ──────────────────────────

  ;; CSI 4 h (IRM on): a printed character inserts at the cursor, pushing the rest
  ;; of the line to the right instead of overwriting it.
  (it "irm-insert-mode-shifts-line-right"
    (with-screen (s 10 5)
      (feed s "abc")
      (feed s (esc "[H"))      ; cursor home (col 0)
      (feed s (esc "[4h"))     ; IRM on
      (feed s "XY")
      (expect (string= "XYabc" (row-string s 0 :end 5)))))

  ;; Default (and CSI 4 l) replace mode overwrites at the cursor.
  (it "irm-replace-mode-overwrites"
    (with-screen (s 10 5)
      (feed s "abc")
      (feed s (esc "[H"))
      (feed s (esc "[4l"))     ; IRM off (explicit)
      (feed s "XY")
      (expect (string= "XYc" (row-string s 0 :end 3)))))

  ;; CSI 4 h sets the insert-mode flag and CSI 4 l clears it.
  (it "irm-set-and-reset-toggle-screen-flag"
    (with-screen (s 10 5)
      (feed s (esc "[4h"))
      (expect (cl-tmux/terminal/types:screen-insert-mode s) :to-be-truthy)
      (feed s (esc "[4l"))
      (expect (not (cl-tmux/terminal/types:screen-insert-mode s)))))

  ;; RIS (ESC c) clears insert-mode, newline-mode, and reverse-screen flags.
  (it "ris-resets-mode-flags-table"
    (dolist (row (list (list (esc "[4h")  #'cl-tmux/terminal/types:screen-insert-mode   "insert mode")
                       (list (esc "[20h") #'cl-tmux/terminal/types:screen-newline-mode   "newline mode")
                       (list (esc "[?5h") #'cl-tmux/terminal/types:screen-reverse-screen "reverse-screen")))
      (destructuring-bind (enable-seq accessor desc) row
        (declare (ignore desc))
        (with-screen (s 10 5)
          (feed s enable-seq)
          (feed s (esc "c"))
          (expect (funcall accessor s) :to-be-falsy)))))

  ;;; ── LNM — Line Feed/New Line Mode (CSI 20 h / CSI 20 l) ─────────────────────

  ;; CSI 20 h (LNM on): a line feed also returns the cursor to column 0, so 'a' LF
  ;; 'b' stacks vertically at column 0.
  (it "lnm-newline-mode-lf-also-carriage-returns"
    (with-screen (s 10 5)
      (feed s (esc "[20h"))             ; LNM on
      (feed s "a")
      (feed s (string #\Linefeed))      ; LF
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 0 1)))))

  ;; Default (LNM off): a line feed moves down keeping the column, so 'b' lands in
  ;; the next column-position after 'a'.
  (it "lnm-off-lf-keeps-column"
    (with-screen (s 10 5)
      (feed s "a")
      (feed s (string #\Linefeed))      ; LF
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 1)))))

  ;; CSI 20 h sets the newline-mode flag and CSI 20 l clears it.
  (it "lnm-set-and-reset-toggle-screen-flag"
    (with-screen (s 10 5)
      (feed s (esc "[20h"))
      (expect (cl-tmux/terminal/types:screen-newline-mode s) :to-be-truthy)
      (feed s (esc "[20l"))
      (expect (not (cl-tmux/terminal/types:screen-newline-mode s)))))

  ;;; ── DECSCNM — reverse-video screen (CSI ?5h / ?5l) ──────────────────────────

  ;; CSI ?5h sets reverse-screen and CSI ?5l clears it.
  (it "decscnm-set-and-reset-toggle-screen-flag"
    (with-screen (s 10 5)
      (feed s (esc "[?5h"))
      (expect (cl-tmux/terminal/types:screen-reverse-screen s) :to-be-truthy)
      (feed s (esc "[?5l"))
      (expect (not (cl-tmux/terminal/types:screen-reverse-screen s)))))

  ;;; ── DECSTR — Soft Terminal Reset (CSI ! p) ─────────────────────────────────

  ;; DECSTR (CSI ! p) restores modes to defaults WITHOUT clearing the screen or
  ;; moving the cursor — the key distinction from RIS (ESC c).
  (it "decstr-resets-modes-but-preserves-screen-and-cursor"
    (with-screen (s 10 5)
      (feed s "hello")                 ; content on row 0
      (feed s (esc "[4h"))             ; IRM on
      (feed s (esc "[?7l"))            ; autowrap off
      (feed s (esc "[?25l"))           ; cursor hidden
      (feed s (esc "[2;4r"))           ; scroll region rows 2..4 (DECSTBM homes cursor)
      (feed s (esc "[1;6H"))           ; reposition cursor to row 1, col 6 (0-idx col 5)
      (feed s (esc "[!p"))             ; DECSTR soft reset
      ;; Modes reset:
      (expect (not (cl-tmux/terminal/types:screen-insert-mode s)))
      (expect (cl-tmux/terminal/types:screen-autowrap s) :to-be-truthy)
      (expect (cl-tmux/terminal/types:screen-cursor-visible s) :to-be-truthy)
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s)))
      ;; Screen + cursor preserved (NOT erased / homed):
      (expect (string= "hello" (row-string s 0 :end 5)))
      (expect (= 5 (cl-tmux/terminal/types:screen-cursor-x s)))))

  ;; DECSTR resets the SGR pen so subsequent text is drawn with default attributes.
  (it "decstr-resets-sgr-pen"
    (with-screen (s 10 5)
      (feed s (esc "[1;31m"))          ; bold red
      (feed s (esc "[!p"))             ; DECSTR
      (expect (= 0 (cl-tmux/terminal/types:screen-cur-attrs s))))))
