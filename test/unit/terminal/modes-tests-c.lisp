(in-package #:cl-tmux/test)

;;;; modes tests — part C: screen-invoked-charset / charset G0/G1, set-screen-cwd,
;;;; erase-display mode-3, IRM insert/replace, LNM line-feed, DECSCNM, DECSTR.

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
