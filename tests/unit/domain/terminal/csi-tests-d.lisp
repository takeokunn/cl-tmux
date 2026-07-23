(in-package #:cl-tmux/test)

;;;; csi tests — part D: rep (REP), da-response, DECRQM, XTWINOPS, CPR,
;;;; DA-response table, REP-count-zero.

;;; ── SUITE: rep ───────────────────────────────────────────────────────────────

(describe "terminal-suite/rep"

  ;; CSI 3 b repeats the last printed character 3 times.
  (it "rep-repeats-last-char"
    (with-screen (s 20 5)
      (feed s "A")             ; writes 'A' at col 0, cursor at col 1
      (feed s (esc "[3b"))     ; REP 3: writes 'A' 3 more times
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\A (char-at s 1 0)))
      (expect (char= #\A (char-at s 2 0)))
      (expect (char= #\A (char-at s 3 0)))
      (check-cursor s 4 0)))

  ;; CSI N b is a no-op when no character has been written yet (screen-last-char is NIL).
  (it "rep-noop-when-no-last-char"
    (with-screen (s 20 5)
      ;; No characters written — last-char is NIL.
      (expect (null (cl-tmux/terminal/types:screen-last-char s)))
      (feed s (esc "[3b"))     ; REP 3 — no-op
      ;; Cursor must be at origin and screen must be blank.
      (check-cursor s 0 0)
      (expect (row-blank-p s 0))))

  ;; screen-last-char is updated on each write; REP always uses the most recent.
  (it "rep-uses-last-printed-char"
    (with-screen (s 20 5)
      (feed s "AB")            ; writes A at 0, B at 1; last-char = B
      (expect (char= #\B (cl-tmux/terminal/types:screen-last-char s)))
      (feed s (esc "[2b"))     ; REP 2: writes B twice more
      (expect (char= #\B (char-at s 2 0)))
      (expect (char= #\B (char-at s 3 0))))))

;;; ── SUITE: da-response ───────────────────────────────────────────────────────

(describe "terminal-suite/da-response"

  ;; CSI c (DA1) queues the VT100 response string ESC[?1;2c.
  (it "da1-response"
    (with-screen (s 20 5)
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))
      (feed s (esc "[c"))        ; DA1
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (consp q))
        (expect (some (lambda (r) (search "?1;2c" r)) q)))))

  ;; CSI > c (DA2) queues the secondary device attribute response.
  (it "da2-response"
    (with-screen (s 20 5)
      (feed s (esc "[>c"))       ; DA2
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (consp q))
        (expect (some (lambda (r) (search ">1;" r)) q)))))

  ;; CSI > q (XTVERSION) replies with cl-tmux's own runtime version.
  (it "xtversion-reports-cl-tmux-version"
    (with-screen (s 20 5)
      (feed s (esc "[>q"))       ; XTVERSION
      (expect (string= (format nil "~CP>|cl-tmux ~A~C\\"
                               #\Escape
                               (cl-tmux/version:version-string)
                               #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; CSI = c (DA3 / tertiary device attributes) queues the DECRPTUI reply.
  (it "da3-response"
    (with-screen (s 20 5)
      (feed s (esc "[=c"))       ; DA3
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (consp q))
        (expect (some (lambda (r) (search "!|00000000" r)) q)))))

  ;; ── DECRQM (request DEC private mode, CSI ? Ps $ p) ──────────────────────────

  ;; DECRQM CSI ? 25 $ p reports the cursor-visibility mode as SET (Pm=1) by default.
  (it "decrqm-reports-set-mode"
    (with-screen (s 20 5)
      (feed s (esc "[?25$p"))
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (search (format nil "~C[?25;1$y" #\Escape) r)) q)))))

  ;; After ?25l (hide cursor) DECRQM reports the mode as RESET (Pm=2).
  (it "decrqm-reports-reset-mode"
    (with-screen (s 20 5)
      (feed s (esc "[?25l"))     ; hide cursor
      (feed s (esc "[?25$p"))
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (search (format nil "~C[?25;2$y" #\Escape) r)) q)))))

  ;; DECRQM for an unrecognised mode reports Pm=0 (not recognised).
  (it "decrqm-unknown-mode-reports-zero"
    (with-screen (s 20 5)
      (feed s (esc "[?9999$p"))
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (search (format nil "~C[?9999;0$y" #\Escape) r)) q)))))

  ;; DECRQM ?5 reports DECSCNM (reverse-video screen): reset by default, set after ?5h.
  (it "decrqm-reports-decscnm-mode-5"
    (with-screen (s 20 5)
      (feed s (esc "[?5$p"))
      (expect (some (lambda (r) (search (format nil "~C[?5;2$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))
      (feed s (esc "[?5h"))
      (feed s (esc "[?5$p"))
      (expect (some (lambda (r) (search (format nil "~C[?5;1$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; DECRQM ?7 reports DECAWM (autowrap): set by default, reset after ?7l.
  (it "decrqm-reports-decawm-mode-7"
    (with-screen (s 20 5)
      (feed s (esc "[?7$p"))
      (expect (some (lambda (r) (search (format nil "~C[?7;1$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))
      (feed s (esc "[?7l"))
      (feed s (esc "[?7$p"))
      (expect (some (lambda (r) (search (format nil "~C[?7;2$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; DECRQM ?1006 reports the SGR mouse-encoding state, set after ?1006h.
  (it "decrqm-reports-sgr-mouse-mode-1006"
    (with-screen (s 20 5)
      (feed s (esc "[?1006h"))
      (feed s (esc "[?1006$p"))
      (expect (some (lambda (r) (search (format nil "~C[?1006;1$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; ANSI-mode DECRQM (CSI 4 $ p, no ? marker) reports IRM: reset by default, set
  ;; after CSI 4 h.  Reply has NO ? marker (ESC [ 4 ; Pm $ y).
  (it "decrqm-ansi-reports-irm-mode-4"
    (with-screen (s 20 5)
      (feed s (esc "[4$p"))
      (expect (some (lambda (r) (search (format nil "~C[4;2$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))
      (feed s (esc "[4h"))
      (feed s (esc "[4$p"))
      (expect (some (lambda (r) (search (format nil "~C[4;1$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; ANSI-mode DECRQM (CSI 20 $ p) reports LNM: set after CSI 20 h.
  (it "decrqm-ansi-reports-lnm-mode-20"
    (with-screen (s 20 5)
      (feed s (esc "[20h"))
      (feed s (esc "[20$p"))
      (expect (some (lambda (r) (search (format nil "~C[20;1$y" #\Escape) r))
                    (cl-tmux/terminal/types:screen-response-queue s))))))

;;; ── XTWINOPS size reports (CSI Ps t) ─────────────────────────────────────────

(describe "terminal-suite/xtwinops"

  ;; CSI 18 t reports the text-area size in characters: ESC [ 8 ; rows ; cols t.
  (it "xtwinops-18-reports-text-area-chars"
    (with-screen (s 20 5)
      (feed s (esc "[18t"))
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (string= (format nil "~C[8;5;20t" #\Escape) r)) q)))))

  ;; CSI 19 t reports the screen size in characters: ESC [ 9 ; rows ; cols t.
  (it "xtwinops-19-reports-screen-chars"
    (with-screen (s 20 5)
      (feed s (esc "[19t"))
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (string= (format nil "~C[9;5;20t" #\Escape) r)) q)))))

  ;; A window-manipulation XTWINOPS op (CSI 8 ; 24 ; 80 t resize) enqueues no reply —
  ;; a multiplexer does not resize the outer window.
  (it "xtwinops-resize-op-no-reply"
    (with-screen (s 20 5)
      (feed s (esc "[8;24;80t"))
      (expect (null (cl-tmux/terminal/types:screen-response-queue s)))))

  ;; ── CPR (cursor position report, CSI 6 n) ────────────────────────────────────

  ;; CSI 6 n (CPR) at the home position replies ESC[1;1R (1-based).
  (it "cpr-at-home-replies-1-1"
    (with-screen (s 20 5)
      (feed s (esc "[6n"))       ; CPR — report cursor position
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (consp q))
        (expect (some (lambda (r) (search "[1;1R" r)) q)))))

  ;; After CUP to row 3, col 5, CSI 6 n reports the new 1-based position ESC[3;5R.
  (it "cpr-reports-moved-cursor-position"
    (with-screen (s 20 5)
      (feed s (esc "[3;5H"))     ; CUP → row 3, col 5 (1-based)
      (feed s (esc "[6n"))       ; CPR
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (search "[3;5R" r)) q)))))

  ;; In DECOM origin mode, CPR row is relative to the scroll-top margin (row 1 = margin top).
  ;; Set a 10-row screen, scroll region rows 3..8 (0-based 2..7), enable DECOM,
  ;; place cursor at absolute row 5 (0-based 4) → relative row 3 (4-2+1=3).
  (it "cpr-in-decom-mode-reports-relative-row"
    (with-screen (s 20 10)
      (feed s (esc "[3;8r"))    ; DECSTBM: scroll region rows 3..8 (1-based)
      (feed s (esc "[?6h"))     ; DECOM on — cursor is now relative to margin
      ;; CUP in DECOM mode: row 3 col 1 (1-based relative) → absolute row 4 (0-based)
      (feed s (esc "[3;1H"))
      (feed s (esc "[6n"))      ; CPR
      (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
        (expect (some (lambda (r) (search "[3;1R" r)) q)))))

  ;; ── DA response table: both responses enqueue without error ──────────────────
  ;;
  ;; The two DA variants (DA1/DA2) both follow the same pattern: feed the
  ;; sequence, assert the queue is non-empty, assert a signature string.
  ;; The table condenses the two individual tests into a loop so adding a new
  ;; DA variant only requires a new row.

  ;; DA1 and DA2 both enqueue a response string with the expected signature.
  ;; (parameterized DA1/DA2 response checks)
  (it "da-response-table"
    (dolist (entry '(("[c"  "?1;2c")    ; DA1 signature
                     ("[>c" ">1;")))     ; DA2 signature
      (let ((seq (first entry))
            (sig (second entry)))
        (with-screen (s 20 5)
          (feed s (esc seq))
          (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
            (expect (consp q))
            (expect (some (lambda (r) (search sig r)) q)))))))

  ;; ── REP count=0 is a no-op ───────────────────────────────────────────────────

  ;; CSI 0 b (REP 0) is effectively a no-op: no additional cells written.
  ;; (REP with count=0 writes nothing extra)
  (it "rep-count-zero-is-noop"
    (with-screen (s 20 5)
      (feed s "X")
      (let ((cx (screen-cursor-x s)))
        (feed s (esc "[0b"))
        ;; Cursor should stay at col cx (no writes for count=0).
        (expect (= cx (screen-cursor-x s)))))))

;;; ── Coverage gap: %decrqm-flag-code direct tests ────────────────────────────
;;;
;;; Audit finding: %decrqm-flag-code (formerly %decrqm-boolean) and
;;; %decrqm-ansi-mode-state have no direct unit tests — they are reachable
;;; only through the end-to-end CSI path.

(describe "terminal-suite/decrqm-internal"

  ;; %decrqm-flag-code returns 1 for T (set, wire code) and 2 for NIL (reset).
  (it "decrqm-flag-code-table"
    (expect (= 1 (cl-tmux/terminal/csi::%decrqm-flag-code t)))
    (expect (= 2 (cl-tmux/terminal/csi::%decrqm-flag-code nil))))

  ;; %decrqm-ansi-mode-state reports IRM (mode 4) as 1 when insert-mode T, 2 when NIL.
  (it "decrqm-ansi-mode-state-irm-table"
    (dolist (row '((t   1 "insert-mode T → 1 (set)")
                   (nil 2 "insert-mode NIL → 2 (reset)")))
      (destructuring-bind (insert-mode-val expected desc) row
        (declare (ignore desc))
        (with-screen (s 20 5)
          (setf (cl-tmux/terminal/types:screen-insert-mode s) insert-mode-val)
          (expect (= expected (cl-tmux/terminal/csi::%decrqm-ansi-mode-state s 4)))))))

  ;; %decrqm-ansi-mode-state reports LNM (mode 20) as 1 (set) when newline-mode is T.
  (it "decrqm-ansi-mode-state-lnm-set"
    (with-screen (s 20 5)
      (setf (cl-tmux/terminal/types:screen-newline-mode s) t)
      (expect (= 1 (cl-tmux/terminal/csi::%decrqm-ansi-mode-state s 20)))))

  ;; %decrqm-ansi-mode-state returns 0 for an unrecognised ANSI mode.
  (it "decrqm-ansi-mode-state-unknown-returns-0"
    (with-screen (s 20 5)
      (expect (= 0 (cl-tmux/terminal/csi::%decrqm-ansi-mode-state s 999))))))

;;; ── Coverage gap: enqueue-da3-reply and enqueue-xtversion-reply ─────────────
;;;
;;; Audit finding: DA3 and XTVERSION reply enqueuers were never directly tested.

(describe "terminal-suite/da3-xtversion-direct"

  ;; enqueue-da3-reply contains '!|00000000'; enqueue-xtversion-reply contains 'cl-tmux'.
  (it "enqueue-reply-substring-table"
    (dolist (row (list (list #'cl-tmux/terminal/csi::enqueue-da3-reply      "!|00000000" "DA3 reply")
                       (list #'cl-tmux/terminal/csi::enqueue-xtversion-reply "cl-tmux"    "XTVERSION reply")))
      (destructuring-bind (fn expected desc) row
        (declare (ignore desc))
        (with-screen (s 20 5)
          (funcall fn s)
          (expect (some (lambda (r) (search expected r))
                        (cl-tmux/terminal/types:screen-response-queue s))))))))

;;; ── Coverage gap: enqueue-xtwinops-reply direct tests ───────────────────────
;;;
;;; Audit finding: the enqueue-xtwinops-reply function was only tested end-to-end.

(describe "terminal-suite/xtwinops-direct"

  ;; enqueue-xtwinops-reply op 18 reports text-area ([8;…]) and op 19 reports screen ([9;…]).
  (it "enqueue-xtwinops-reply-size-ops-table"
    (dolist (row '((18 "[8;8;30t" "op 18 text-area report")
                   (19 "[9;8;30t" "op 19 screen report")))
      (destructuring-bind (op expected desc) row
        (declare (ignore desc))
        (with-screen (s 30 8)
          (cl-tmux/terminal/csi::enqueue-xtwinops-reply s op)
          (expect (some (lambda (r) (search expected r))
                        (cl-tmux/terminal/types:screen-response-queue s)))))))

  ;; enqueue-xtwinops-reply with an unsupported op enqueues nothing.
  (it "enqueue-xtwinops-reply-op-99-no-reply"
    (with-screen (s 20 5)
      (cl-tmux/terminal/csi::enqueue-xtwinops-reply s 99)
      (expect (null (cl-tmux/terminal/types:screen-response-queue s))))))
