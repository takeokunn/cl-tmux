(in-package #:cl-tmux/test)

;;;; csi tests — part B: ECH, DECRQM, XTWINOPS, CPR, DA table,
;;;; REP count=0, VPR/CNL/HPR, ICH, DCH, ED/EL, SGR in CSI, IL/DL,
;;;; DECFRA, DECCRA, REP in cell-with-attributes suites.

;;; ── SUITE: ech ───────────────────────────────────────────────────────────────

(describe "terminal-suite/ech"

  ;; CSI 3 X erases 3 characters at the cursor position without moving the cursor.
  (it "ech-erases-n-chars-in-place"
    (with-screen (s 20 5)
      (feed s "ABCDE")              ; cells 0-4 = A B C D E, cursor at 5
      (feed s (esc "[1;4H"))        ; move cursor to col 3 (1-based)
      (check-cursor s 3 0)
      (feed s (esc "[3X"))          ; ECH 3 — erase cols 3,4,5
      ;; Columns 3,4,5 must now be blank; columns 0,1,2 intact; cursor unchanged.
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\B (char-at s 1 0)))
      (expect (char= #\C (char-at s 2 0)))
      (expect (char= #\Space (char-at s 3 0)))
      (expect (char= #\Space (char-at s 4 0)))
      (expect (char= #\Space (char-at s 5 0)))
      ;; Cursor must not have moved.
      (check-cursor s 3 0)))

  ;; CSI X with no parameter erases 1 character (default p1* = 1).
  (it "ech-default-one-char"
    (with-screen (s 10 5)
      (feed s "ABCD")
      (feed s (esc "[1;3H"))   ; cursor at col 2
      (feed s (esc "[X"))      ; ECH 1 (default)
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\B (char-at s 1 0)))
      (expect (char= #\Space (char-at s 2 0)))
      (expect (char= #\D (char-at s 3 0)))
      (check-cursor s 2 0))))

;;; ── SUITE: dsr ───────────────────────────────────────────────────────────────

(describe "terminal-suite/dsr"

  ;; CSI 5 n (DSR) queues the ESC[0n status reply without moving the cursor or
  ;; altering screen content.
  (it "dsr-5n-replies-ok-without-altering-screen"
    (with-screen (s 20 5)
      (feed s "A")
      (feed s (esc "[5n"))   ; DSR — report status (queues ESC[0n)
      (feed s "B")
      ;; Screen content and cursor must be as if the report query were absent.
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\B (char-at s 1 0)))
      (check-cursor s 2 0)
      ;; DSR must have queued the terminal-OK status reply.
      (expect (some (lambda (r) (search "[0n" r))
                    (cl-tmux/terminal/types:screen-response-queue s))))))

;;; ── SUITE: ich-dch ───────────────────────────────────────────────────────────

(describe "terminal-suite/ich-dch"

  ;; CSI 2 @ at column 1 inserts 2 blanks, pushing existing text right.
  (it "ich-inserts-blanks-and-shifts-right"
    (with-screen (s 10 5)
      (feed s "ABCDE")              ; row 0: A B C D E, cursor at 5
      (feed s (esc "[1;2H"))        ; cursor → col 1 (1-based 2)
      (check-cursor s 1 0)
      (feed s (esc "[2@"))          ; ICH 2 — insert 2 blanks at col 1
      ;; A stays at col 0; blanks at 1,2; B→3, C→4; D and E are pushed off.
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\Space (char-at s 1 0)))
      (expect (char= #\Space (char-at s 2 0)))
      (expect (char= #\B (char-at s 3 0)))
      (expect (char= #\C (char-at s 4 0)))
      ;; Cursor must remain at the insertion point.
      (check-cursor s 1 0)))

  ;; CSI @ with no parameter inserts 1 blank (default p1* = 1).
  (it "ich-default-one-char"
    (with-screen (s 10 5)
      (feed s "XY")
      (feed s (esc "[1;1H"))   ; cursor at col 0
      (feed s (esc "[@"))      ; ICH 1 (default)
      (expect (char= #\Space (char-at s 0 0)))
      (expect (char= #\X (char-at s 1 0)))
      (expect (char= #\Y (char-at s 2 0)))
      (check-cursor s 0 0)))

  ;; CSI 2 P at column 1 deletes 2 characters, pulling remaining chars left.
  (it "dch-deletes-and-shifts-left"
    (with-screen (s 10 5)
      (feed s "ABCDE")              ; row 0: A B C D E, cursor at 5
      (feed s (esc "[1;2H"))        ; cursor → col 1
      (feed s (esc "[2P"))          ; DCH 2 — delete 2 chars at col 1
      ;; A stays; B and C removed; D→1, E→2; cols 3,4 become blank.
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\D (char-at s 1 0)))
      (expect (char= #\E (char-at s 2 0)))
      (expect (char= #\Space (char-at s 3 0)))
      (expect (char= #\Space (char-at s 4 0)))
      (check-cursor s 1 0)))

  ;; CSI P with no parameter deletes 1 character (default p1* = 1).
  (it "dch-default-one-char"
    (with-screen (s 10 5)
      (feed s "ABCD")
      (feed s (esc "[1;2H"))   ; cursor at col 1
      (feed s (esc "[P"))      ; DCH 1 (default)
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\C (char-at s 1 0)))
      (expect (char= #\D (char-at s 2 0)))
      (expect (char= #\Space (char-at s 3 0)))
      (check-cursor s 1 0))))

;;; ── SUITE: il-dl ─────────────────────────────────────────────────────────────

(describe "terminal-suite/il-dl"

  ;; CSI 1 L at row 1 inserts a blank line, pushing row 1 down to row 2.
  (it "il-inserts-blank-line-at-cursor"
    (with-screen (s 10 5)
      (feed-lines s "row0" "row1" "row2")
      (feed s (esc "[2;1H"))    ; cursor at row 1 (1-based 2)
      (feed s (esc "[L"))       ; IL 1 (default) — insert blank line
      ;; row 0 must be unchanged; row 1 blank; row 2 holds old row 1.
      (check-row s 0 "row0")
      (expect (row-blank-p s 1))
      (check-row s 2 "row1")))

  ;; CSI 2 L inserts two blank lines, shifting subsequent rows down by 2.
  (it "il-two-lines"
    (with-screen (s 10 5)
      (feed-lines s "row0" "row1" "row2" "row3")
      (feed s (esc "[2;1H"))   ; cursor at row 1
      (feed s (esc "[2L"))     ; IL 2
      (check-row s 0 "row0")
      (expect (row-blank-p s 1))
      (expect (row-blank-p s 2))
      (check-row s 3 "row1")))

  ;; CSI 1 M at row 1 removes that line, pulling row 2 up to row 1.
  (it "dl-deletes-current-line"
    (with-screen (s 10 5)
      (feed-lines s "row0" "row1" "row2")
      (feed s (esc "[2;1H"))    ; cursor at row 1
      (feed s (esc "[M"))       ; DL 1 (default)
      (check-row s 0 "row0")
      (check-row s 1 "row2")    ; row 2 moved up
      (expect (row-blank-p s 2))))

  ;; CSI 2 M deletes two lines starting at the cursor row.
  (it "dl-two-lines"
    (with-screen (s 10 5)
      (feed-lines s "row0" "row1" "row2" "row3")
      (feed s (esc "[2;1H"))   ; cursor at row 1
      (feed s (esc "[2M"))     ; DL 2
      (check-row s 0 "row0")
      (check-row s 1 "row3")
      (expect (row-blank-p s 2)))))

;;; ── SUITE: decstbm-csi ────────────────────────────────────────────────────────

(describe "terminal-suite/decstbm-csi"

  ;; ESC[3;8r sets the scroll region to rows 2-7 (0-based) and homes the cursor.
  (it "decstbm-csi-sets-scroll-region"
    (with-screen (s 10 10)
      (feed s (esc "[3;8H"))    ; move cursor away from home
      (feed s (esc "[3;8r"))    ; DECSTBM: top=3 (1-based) → 2, bottom=8 → 7
      (expect (= 2 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)))
      ;; DECSTBM homes the cursor.
      (check-cursor s 0 0)))

  ;; ESC[r with no parameters resets the scroll region to full screen (rows 0 to height-1).
  (it "decstbm-csi-no-params-resets-to-full-screen"
    (with-screen (s 10 10)
      ;; First restrict the scroll region.
      (feed s (esc "[3;8r"))
      ;; Then reset with no params (p1=0 → top defaults to 1-1=0; p2=0 → bottom = height-1).
      (feed s (esc "[r"))
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s)))))

  ;; After DECSTBM, scrolling operates within the defined region.
  (it "decstbm-csi-scroll-region-constrains-scroll"
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

  ;; DECSTBM with P1 > P2 (invalid margins) resets to full-screen per VT100 spec.
  (it "decstbm-csi-invalid-top-greater-than-bottom-resets-to-full-screen"
    (with-screen (s 10 10)
      ;; First set a valid region
      (feed s (esc "[3;8r"))
      (expect (= 2 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)))
      ;; Now send invalid: top=8 (0-based 7) > bottom=3 (0-based 2)
      (feed s (esc "[8;3r"))
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top s)))
      (expect (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))))))

;;; ── SUITE: execute-csi-direct ────────────────────────────────────────────────

(describe "terminal-suite/execute-csi-direct"

  ;; execute-csi called directly with final #\H and params '(3 5) positions cursor.
  (it "execute-csi-cup-direct"
    (with-screen (s 20 10)
      (cl-tmux/terminal/csi:execute-csi s #\H nil nil '(3 5))
      ;; CUP: row=3 (1-based) → y=2; col=5 (1-based) → x=4
      (check-cursor s 4 2)))

  ;; execute-csi with final #\m and params '(31) sets foreground via the SGR path.
  (it "execute-csi-sgr-direct"
    (with-screen (s 20 10)
      (cl-tmux/terminal/csi:execute-csi s #\m nil nil '(31))
      (expect (= 1 (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;; execute-csi with an unrecognized final byte is silently ignored (no error, no state change).
  (it "execute-csi-unknown-final-is-noop"
    (with-screen (s 20 10)
      (finishes (cl-tmux/terminal/csi:execute-csi s #\z nil nil '()))
      ;; Screen state must be at defaults.
      (check-cursor s 0 0)
      (check-sgr-state s :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)))

  ;; execute-csi with a recognized final but unrecognized intermed byte is silently ignored.
  (it "execute-csi-unknown-intermed-is-noop"
    (with-screen (s 20 10)
      ;; #\! intermed with #\H final is not defined — should be a no-op.
      (finishes (cl-tmux/terminal/csi:execute-csi s #\H #\! nil '(3 5)))
      ;; Cursor must remain at origin (no CUP fired).
      (check-cursor s 0 0))))

;;; ── SUITE: %csi-decstbm-params ───────────────────────────────────────────────

(describe "terminal-suite/csi-decstbm-params"

  ;; %csi-decstbm-params converts 1-based params to 0-based; 0 defaults to top=0 or bottom=height-1.
  (it "%csi-decstbm-params-table"
    (dolist (row '((10 10 3 8  2   7   "normal: p1=3,p2=8 → top=2,bottom=7")
                   (10 10 0 5  0   nil "p1=0 → top=0 (skip bottom)")
                   (10  8 1 0  nil 7   "p2=0 → bottom=height-1=7 (skip top)")))
      (destructuring-bind (sw sh p1 p2 expected-top expected-bottom desc) row
        (declare (ignore desc))
        (with-screen (s sw sh)
          (multiple-value-bind (top bottom)
              (cl-tmux/terminal/csi::%csi-decstbm-params s p1 p2)
            (when expected-top
              (expect (= expected-top top)))
            (when expected-bottom
              (expect (= expected-bottom bottom)))))))))

;;; ── SUITE: csi-rules-macro-and-helpers ───────────────────────────────────────
;;;
;;; Coverage gap: define-csi-rules (the dispatch-table macro) and %csi-leading-int
;;; (the scalar-param extraction helper) were exercised only indirectly through
;;; execute-csi end-to-end paths, mirroring the direct define-sgr-rules /
;;; %pen-to-sgr-params coverage already present in sgr-tests-b.lisp.

(describe "terminal-suite/csi-rules-macro-and-helpers"

  ;; define-csi-rules is a defined macro in the csi package.
  (it "define-csi-rules-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/csi::define-csi-rules)))

  ;; execute-csi (exported) has a non-empty docstring injected by define-csi-rules.
  (it "execute-csi-has-docstring"
    (let ((doc (documentation 'cl-tmux/terminal/csi:execute-csi 'function)))
      (expect (and (stringp doc) (plusp (length doc))))))

  ;; %csi-leading-int returns a plain integer as-is, the head of a colon-group
  ;; list, 0 for an absent (NIL) parameter, and 0 for a NIL-headed colon group.
  (it "csi-leading-int-table"
    (dolist (row (list (list 42       42 "plain integer passes through")
                       (list '(4 3)   4  "colon-group list → its head")
                       (list nil      0  "absent parameter → 0")
                       (list '(nil 3) 0  "colon-group with NIL head → 0")))
      (destructuring-bind (param expected desc) row
        (declare (ignore desc))
        (expect (= expected (cl-tmux/terminal/csi::%csi-leading-int param)))))))

;;; ── SUITE: csi-decstr-ansi-mode-dispatch ─────────────────────────────────────
;;;
;;; Coverage gap: DECSTR (CSI ! p) and the ANSI Set/Reset Mode rules (CSI Ps h /
;;; CSI Ps l with no private marker) were tested only via direct calls to
;;; decstr-action / set-ansi-mode / reset-ansi-mode (modes-tests-e.lisp), never
;;; through the execute-csi/parser dispatch path that actually routes to them.

(describe "terminal-suite/csi-decstr-ansi-mode-dispatch"

  ;; ESC[!p (DECSTR) reached via the parser clears IRM but leaves screen content alone.
  (it "decstr-via-csi-resets-insert-mode-without-clearing-screen"
    (with-screen (s 10 5)
      (feed s "ABCDE")
      (feed s (esc "[4h"))          ; enable IRM first
      (expect (cl-tmux/terminal/types:screen-insert-mode s) :to-be-truthy)
      (feed s (esc "[!p"))          ; DECSTR — soft reset
      (expect (cl-tmux/terminal/types:screen-insert-mode s) :to-be-falsy)
      (expect (string= "ABCDE" (row-string s 0 :end 5)))))

  ;; ESC[4h / ESC[4l (ANSI IRM, no private marker) toggle screen-insert-mode via
  ;; the execute-csi set-ansi-mode/reset-ansi-mode rules.
  (it "ansi-mode-h-l-via-csi-toggles-insert-mode"
    (with-screen (s 10 5)
      (expect (cl-tmux/terminal/types:screen-insert-mode s) :to-be-falsy)
      (feed s (esc "[4h"))
      (expect (cl-tmux/terminal/types:screen-insert-mode s) :to-be-truthy)
      (feed s (esc "[4l"))
      (expect (cl-tmux/terminal/types:screen-insert-mode s) :to-be-falsy))))

;;; ── SUITE: csi-unknown-sequences ─────────────────────────────────────────────
