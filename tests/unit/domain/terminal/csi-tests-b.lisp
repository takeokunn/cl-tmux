(in-package #:cl-tmux/test)

;;;; csi tests — part B: ECH, DECRQM, XTWINOPS, CPR, DA table,
;;;; REP count=0, VPR/CNL/HPR, ICH, DCH, ED/EL, SGR in CSI, IL/DL,
;;;; DECFRA, DECCRA, REP in cell-with-attributes suites.

;;; ── SUITE: ech ───────────────────────────────────────────────────────────────

(def-suite ech
  :description "ECH (CSI X) — erase characters without shifting"
  :in terminal-suite)
(in-suite ech)

(test ech-erases-n-chars-in-place
  "CSI 3 X erases 3 characters at the cursor position without moving the cursor."
  (with-screen (s 20 5)
    (feed s "ABCDE")              ; cells 0-4 = A B C D E, cursor at 5
    (feed s (esc "[1;4H"))        ; move cursor to col 3 (1-based)
    (check-cursor s 3 0)
    (feed s (esc "[3X"))          ; ECH 3 — erase cols 3,4,5
    ;; Columns 3,4,5 must now be blank; columns 0,1,2 intact; cursor unchanged.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\B (char-at s 1 0)) "col 1 must be B")
    (is (char= #\C (char-at s 2 0)) "col 2 must be C")
    (is (char= #\Space (char-at s 3 0)) "col 3 must be blank after ECH")
    (is (char= #\Space (char-at s 4 0)) "col 4 must be blank after ECH")
    (is (char= #\Space (char-at s 5 0)) "col 5 must be blank after ECH")
    ;; Cursor must not have moved.
    (check-cursor s 3 0)))

(test ech-default-one-char
  "CSI X with no parameter erases 1 character (default p1* = 1)."
  (with-screen (s 10 5)
    (feed s "ABCD")
    (feed s (esc "[1;3H"))   ; cursor at col 2
    (feed s (esc "[X"))      ; ECH 1 (default)
    (is (char= #\A (char-at s 0 0)) "col 0 intact")
    (is (char= #\B (char-at s 1 0)) "col 1 intact")
    (is (char= #\Space (char-at s 2 0)) "col 2 erased")
    (is (char= #\D (char-at s 3 0)) "col 3 intact (ECH does not shift)")
    (check-cursor s 2 0)))

;;; ── SUITE: dsr ───────────────────────────────────────────────────────────────

(def-suite dsr
  :description "DSR (CSI 5 n) — Device Status Report (replies ESC[0n)"
  :in terminal-suite)
(in-suite dsr)

(test dsr-5n-replies-ok-without-altering-screen
  "CSI 5 n (DSR) queues the ESC[0n status reply without moving the cursor or
   altering screen content."
  (with-screen (s 20 5)
    (feed s "A")
    (feed s (esc "[5n"))   ; DSR — report status (queues ESC[0n)
    (feed s "B")
    ;; Screen content and cursor must be as if the report query were absent.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\B (char-at s 1 0)) "col 1 must be B")
    (check-cursor s 2 0)
    ;; DSR must have queued the terminal-OK status reply.
    (is (some (lambda (r) (search "[0n" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DSR (CSI 5 n) must enqueue the ESC[0n status reply")))

;;; ── SUITE: ich-dch ───────────────────────────────────────────────────────────

(def-suite ich-dch
  :description "ICH (CSI @) insert characters and DCH (CSI P) delete characters"
  :in terminal-suite)
(in-suite ich-dch)

(test ich-inserts-blanks-and-shifts-right
  "CSI 2 @ at column 1 inserts 2 blanks, pushing existing text right."
  (with-screen (s 10 5)
    (feed s "ABCDE")              ; row 0: A B C D E, cursor at 5
    (feed s (esc "[1;2H"))        ; cursor → col 1 (1-based 2)
    (check-cursor s 1 0)
    (feed s (esc "[2@"))          ; ICH 2 — insert 2 blanks at col 1
    ;; A stays at col 0; blanks at 1,2; B→3, C→4; D and E are pushed off.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A (unchanged)")
    (is (char= #\Space (char-at s 1 0)) "col 1 must be blank after ICH")
    (is (char= #\Space (char-at s 2 0)) "col 2 must be blank after ICH")
    (is (char= #\B (char-at s 3 0)) "col 3 must be B (shifted right)")
    (is (char= #\C (char-at s 4 0)) "col 4 must be C (shifted right)")
    ;; Cursor must remain at the insertion point.
    (check-cursor s 1 0)))

(test ich-default-one-char
  "CSI @ with no parameter inserts 1 blank (default p1* = 1)."
  (with-screen (s 10 5)
    (feed s "XY")
    (feed s (esc "[1;1H"))   ; cursor at col 0
    (feed s (esc "[@"))      ; ICH 1 (default)
    (is (char= #\Space (char-at s 0 0)) "col 0 must be blank after ICH default")
    (is (char= #\X (char-at s 1 0))     "col 1 must be X (shifted right)")
    (is (char= #\Y (char-at s 2 0))     "col 2 must be Y (shifted right)")
    (check-cursor s 0 0)))

(test dch-deletes-and-shifts-left
  "CSI 2 P at column 1 deletes 2 characters, pulling remaining chars left."
  (with-screen (s 10 5)
    (feed s "ABCDE")              ; row 0: A B C D E, cursor at 5
    (feed s (esc "[1;2H"))        ; cursor → col 1
    (feed s (esc "[2P"))          ; DCH 2 — delete 2 chars at col 1
    ;; A stays; B and C removed; D→1, E→2; cols 3,4 become blank.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A (unchanged)")
    (is (char= #\D (char-at s 1 0)) "col 1 must be D (shifted left)")
    (is (char= #\E (char-at s 2 0)) "col 2 must be E (shifted left)")
    (is (char= #\Space (char-at s 3 0)) "col 3 must be blank after DCH")
    (is (char= #\Space (char-at s 4 0)) "col 4 must be blank after DCH")
    (check-cursor s 1 0)))

(test dch-default-one-char
  "CSI P with no parameter deletes 1 character (default p1* = 1)."
  (with-screen (s 10 5)
    (feed s "ABCD")
    (feed s (esc "[1;2H"))   ; cursor at col 1
    (feed s (esc "[P"))      ; DCH 1 (default)
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\C (char-at s 1 0)) "col 1 must be C (B deleted, C shifted)")
    (is (char= #\D (char-at s 2 0)) "col 2 must be D (shifted)")
    (is (char= #\Space (char-at s 3 0)) "col 3 must be blank")
    (check-cursor s 1 0)))

;;; ── SUITE: il-dl ─────────────────────────────────────────────────────────────

(def-suite il-dl
  :description "IL (CSI L) insert lines and DL (CSI M) delete lines"
  :in terminal-suite)
(in-suite il-dl)

(test il-inserts-blank-line-at-cursor
  "CSI 1 L at row 1 inserts a blank line, pushing row 1 down to row 2."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2")
    (feed s (esc "[2;1H"))    ; cursor at row 1 (1-based 2)
    (feed s (esc "[L"))       ; IL 1 (default) — insert blank line
    ;; row 0 must be unchanged; row 1 blank; row 2 holds old row 1.
    (check-row s 0 "row0")
    (is (row-blank-p s 1) "row 1 must be blank after IL")
    (check-row s 2 "row1")))

(test il-two-lines
  "CSI 2 L inserts two blank lines, shifting subsequent rows down by 2."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2" "row3")
    (feed s (esc "[2;1H"))   ; cursor at row 1
    (feed s (esc "[2L"))     ; IL 2
    (check-row s 0 "row0")
    (is (row-blank-p s 1) "row 1 must be blank")
    (is (row-blank-p s 2) "row 2 must be blank")
    (check-row s 3 "row1")))

(test dl-deletes-current-line
  "CSI 1 M at row 1 removes that line, pulling row 2 up to row 1."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2")
    (feed s (esc "[2;1H"))    ; cursor at row 1
    (feed s (esc "[M"))       ; DL 1 (default)
    (check-row s 0 "row0")
    (check-row s 1 "row2")    ; row 2 moved up
    (is (row-blank-p s 2) "old row 2 position must be blank after DL")))

(test dl-two-lines
  "CSI 2 M deletes two lines starting at the cursor row."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2" "row3")
    (feed s (esc "[2;1H"))   ; cursor at row 1
    (feed s (esc "[2M"))     ; DL 2
    (check-row s 0 "row0")
    (check-row s 1 "row3")
    (is (row-blank-p s 2) "row 2 must be blank after deleting 2 lines")))

;;; ── SUITE: decstbm-csi ────────────────────────────────────────────────────────

(def-suite decstbm-csi
  :description "DECSTBM (CSI r) — scroll region set via the CSI r sequence"
  :in terminal-suite)
(in-suite decstbm-csi)

(test decstbm-csi-sets-scroll-region
  "ESC[3;8r sets the scroll region to rows 2-7 (0-based) and homes the cursor."
  (with-screen (s 10 10)
    (feed s (esc "[3;8H"))    ; move cursor away from home
    (feed s (esc "[3;8r"))    ; DECSTBM: top=3 (1-based) → 2, bottom=8 → 7
    (is (= 2 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be 2 (0-based) after ESC[3;8r")
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be 7 (0-based) after ESC[3;8r")
    ;; DECSTBM homes the cursor.
    (check-cursor s 0 0)))

(test decstbm-csi-no-params-resets-to-full-screen
  "ESC[r with no parameters resets the scroll region to full screen (rows 0 to height-1)."
  (with-screen (s 10 10)
    ;; First restrict the scroll region.
    (feed s (esc "[3;8r"))
    ;; Then reset with no params (p1=0 → top defaults to 1-1=0; p2=0 → bottom = height-1).
    (feed s (esc "[r"))
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be reset to 0 after ESC[r with no params")
    (is (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be reset to height-1 (9) after ESC[r with no params")))

(test decstbm-csi-scroll-region-constrains-scroll
  "After DECSTBM, scrolling operates within the defined region."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2" "row3")
    ;; Restrict scroll region to rows 1-2 (1-based 2;3r).
    (feed s (esc "[2;3r"))
    ;; From row 1 (inside region), scroll up by 1 line.
    (feed s (esc "[2;1H"))    ; cursor at row 1
    (feed s (esc "[S"))       ; SU 1
    ;; Row 0 must be unaffected (outside the scroll region).
    (check-row s 0 "row0")
    ;; Row 1 (top of region) should have moved to what was row 2.
    (check-row s 1 "row2")))

(test decstbm-csi-invalid-top-greater-than-bottom-resets-to-full-screen
  "DECSTBM with P1 > P2 (invalid margins) resets to full-screen per VT100 spec."
  (with-screen (s 10 10)
    ;; First set a valid region
    (feed s (esc "[3;8r"))
    (is (= 2 (cl-tmux/terminal/types:screen-scroll-top s)))
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)))
    ;; Now send invalid: top=8 (0-based 7) > bottom=3 (0-based 2)
    (feed s (esc "[8;3r"))
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "invalid DECSTBM must reset top to 0")
    (is (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "invalid DECSTBM must reset bottom to height-1")))

;;; ── SUITE: execute-csi-direct ────────────────────────────────────────────────

(def-suite execute-csi-direct
  :description "Direct calls to execute-csi"
  :in terminal-suite)
(in-suite execute-csi-direct)

(test execute-csi-cup-direct
  "execute-csi called directly with final #\\H and params '(3 5) positions cursor."
  (with-screen (s 20 10)
    (cl-tmux/terminal/csi:execute-csi s #\H nil nil '(3 5))
    ;; CUP: row=3 (1-based) → y=2; col=5 (1-based) → x=4
    (check-cursor s 4 2)))

(test execute-csi-sgr-direct
  "execute-csi with final #\\m and params '(31) sets foreground via the SGR path."
  (with-screen (s 20 10)
    (cl-tmux/terminal/csi:execute-csi s #\m nil nil '(31))
    (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
        "execute-csi SGR 31 must set cur-fg to 1 (red)")))

(test execute-csi-unknown-final-is-noop
  "execute-csi with an unrecognized final byte is silently ignored (no error, no state change)."
  (with-screen (s 20 10)
    (finishes (cl-tmux/terminal/csi:execute-csi s #\z nil nil '()))
    ;; Screen state must be at defaults.
    (check-cursor s 0 0)
    (check-sgr-state s :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)))

(test execute-csi-unknown-intermed-is-noop
  "execute-csi with a recognized final but unrecognized intermed byte is silently ignored."
  (with-screen (s 20 10)
    ;; #\! intermed with #\H final is not defined — should be a no-op.
    (finishes (cl-tmux/terminal/csi:execute-csi s #\H #\! nil '(3 5)))
    ;; Cursor must remain at origin (no CUP fired).
    (check-cursor s 0 0)))

;;; ── SUITE: %csi-decstbm-params ───────────────────────────────────────────────

(def-suite csi-decstbm-params
  :description "Direct tests of %csi-decstbm-params helper"
  :in terminal-suite)
(in-suite csi-decstbm-params)

(test %csi-decstbm-params-table
  "%csi-decstbm-params converts 1-based params to 0-based; 0 defaults to top=0 or bottom=height-1."
  (dolist (row '((10 10 3 8  2   7   "normal: p1=3,p2=8 → top=2,bottom=7")
                 (10 10 0 5  0   nil "p1=0 → top=0 (skip bottom)")
                 (10  8 1 0  nil 7   "p2=0 → bottom=height-1=7 (skip top)")))
    (destructuring-bind (sw sh p1 p2 expected-top expected-bottom desc) row
      (with-screen (s sw sh)
        (multiple-value-bind (top bottom)
            (cl-tmux/terminal/csi::%csi-decstbm-params s p1 p2)
          (when expected-top
            (is (= expected-top top) "~A: top" desc))
          (when expected-bottom
            (is (= expected-bottom bottom) "~A: bottom" desc)))))))

;;; ── SUITE: csi-rules-macro-and-helpers ───────────────────────────────────────
;;;
;;; Coverage gap: define-csi-rules (the dispatch-table macro) and %csi-leading-int
;;; (the scalar-param extraction helper) were exercised only indirectly through
;;; execute-csi end-to-end paths, mirroring the direct define-sgr-rules /
;;; %pen-to-sgr-params coverage already present in sgr-tests-b.lisp.

(def-suite csi-rules-macro-and-helpers
  :description "Direct coverage of define-csi-rules and %csi-leading-int"
  :in terminal-suite)
(in-suite csi-rules-macro-and-helpers)

(test define-csi-rules-macro-is-defined
  "define-csi-rules is a defined macro in the csi package."
  (is (macro-function 'cl-tmux/terminal/csi::define-csi-rules)
      "define-csi-rules must be a macro"))

(test execute-csi-has-docstring
  "execute-csi (exported) has a non-empty docstring injected by define-csi-rules."
  (let ((doc (documentation 'cl-tmux/terminal/csi:execute-csi 'function)))
    (is (and (stringp doc) (plusp (length doc)))
        "execute-csi must have a non-empty docstring")))

(test csi-leading-int-table
  "%csi-leading-int returns a plain integer as-is, the head of a colon-group
   list, 0 for an absent (NIL) parameter, and 0 for a NIL-headed colon group."
  (dolist (row (list (list 42       42 "plain integer passes through")
                     (list '(4 3)   4  "colon-group list → its head")
                     (list nil      0  "absent parameter → 0")
                     (list '(nil 3) 0  "colon-group with NIL head → 0")))
    (destructuring-bind (param expected desc) row
      (is (= expected (cl-tmux/terminal/csi::%csi-leading-int param)) "~A" desc))))

;;; ── SUITE: csi-decstr-ansi-mode-dispatch ─────────────────────────────────────
;;;
;;; Coverage gap: DECSTR (CSI ! p) and the ANSI Set/Reset Mode rules (CSI Ps h /
;;; CSI Ps l with no private marker) were tested only via direct calls to
;;; decstr-action / set-ansi-mode / reset-ansi-mode (modes-tests-e.lisp), never
;;; through the execute-csi/parser dispatch path that actually routes to them.

(def-suite csi-decstr-ansi-mode-dispatch
  :description "CSI-level dispatch coverage for DECSTR and ANSI Set/Reset Mode"
  :in terminal-suite)
(in-suite csi-decstr-ansi-mode-dispatch)

(test decstr-via-csi-resets-insert-mode-without-clearing-screen
  "ESC[!p (DECSTR) reached via the parser clears IRM but leaves screen content alone."
  (with-screen (s 10 5)
    (feed s "ABCDE")
    (feed s (esc "[4h"))          ; enable IRM first
    (is-true (cl-tmux/terminal/types:screen-insert-mode s) "IRM must be set before DECSTR")
    (feed s (esc "[!p"))          ; DECSTR — soft reset
    (is-false (cl-tmux/terminal/types:screen-insert-mode s)
              "DECSTR via CSI must clear IRM")
    (is (string= "ABCDE" (row-string s 0 :end 5))
        "DECSTR must not clear existing screen content")))

(test ansi-mode-h-l-via-csi-toggles-insert-mode
  "ESC[4h / ESC[4l (ANSI IRM, no private marker) toggle screen-insert-mode via
   the execute-csi set-ansi-mode/reset-ansi-mode rules."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-insert-mode s) "IRM must be off by default")
    (feed s (esc "[4h"))
    (is-true (cl-tmux/terminal/types:screen-insert-mode s) "ESC[4h must set IRM")
    (feed s (esc "[4l"))
    (is-false (cl-tmux/terminal/types:screen-insert-mode s) "ESC[4l must clear IRM")))

;;; ── SUITE: csi-unknown-sequences ─────────────────────────────────────────────
