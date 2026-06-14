(in-package #:cl-tmux/test)

;;;; csi tests — part D: rep (REP), da-response, DECRQM, XTWINOPS, CPR,
;;;; DA-response table, REP-count-zero.

;;; ── SUITE: rep ───────────────────────────────────────────────────────────────

(def-suite rep
  :description "REP (CSI b) — repeat preceding character"
  :in terminal-suite)
(in-suite rep)

(test rep-repeats-last-char
  "CSI 3 b repeats the last printed character 3 times."
  (with-screen (s 20 5)
    (feed s "A")             ; writes 'A' at col 0, cursor at col 1
    (feed s (esc "[3b"))     ; REP 3: writes 'A' 3 more times
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\A (char-at s 1 0)) "col 1 must be A (first REP)")
    (is (char= #\A (char-at s 2 0)) "col 2 must be A (second REP)")
    (is (char= #\A (char-at s 3 0)) "col 3 must be A (third REP)")
    (check-cursor s 4 0)))

(test rep-noop-when-no-last-char
  "CSI N b is a no-op when no character has been written yet (screen-last-char is NIL)."
  (with-screen (s 20 5)
    ;; No characters written — last-char is NIL.
    (is (null (cl-tmux/terminal/types:screen-last-char s))
        "screen-last-char must be NIL on a fresh screen")
    (feed s (esc "[3b"))     ; REP 3 — no-op
    ;; Cursor must be at origin and screen must be blank.
    (check-cursor s 0 0)
    (is (row-blank-p s 0) "row 0 must remain blank after REP with no prior char")))

(test rep-uses-last-printed-char
  "screen-last-char is updated on each write; REP always uses the most recent."
  (with-screen (s 20 5)
    (feed s "AB")            ; writes A at 0, B at 1; last-char = B
    (is (char= #\B (cl-tmux/terminal/types:screen-last-char s))
        "screen-last-char must be B after writing AB")
    (feed s (esc "[2b"))     ; REP 2: writes B twice more
    (is (char= #\B (char-at s 2 0)) "col 2 must be B")
    (is (char= #\B (char-at s 3 0)) "col 3 must be B")))

;;; ── SUITE: da-response ───────────────────────────────────────────────────────

(def-suite da-response
  :description "DA1/DA2 device attribute responses"
  :in terminal-suite)
(in-suite da-response)

(test da1-response
  "CSI c (DA1) queues the VT100 response string ESC[?1;2c."
  (with-screen (s 20 5)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must be empty initially")
    (feed s (esc "[c"))        ; DA1
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI c")
      (is (some (lambda (r) (search "?1;2c" r)) q)
          "DA1 response must contain ?1;2c"))))

(test da2-response
  "CSI > c (DA2) queues the secondary device attribute response."
  (with-screen (s 20 5)
    (feed s (esc "[>c"))       ; DA2
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI >c")
      (is (some (lambda (r) (search ">1;" r)) q)
          "DA2 response must contain >1;"))))

(test xtversion-reports-tmux-version
  "CSI > q (XTVERSION) replies ESC P > | tmux 3.5 ST (cl-tmux's tmux 3.5 identity)."
  (with-screen (s 20 5)
    (feed s (esc "[>q"))       ; XTVERSION
    (is (string= (format nil "~CP>|tmux 3.5~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "XTVERSION must report tmux 3.5")))

(test da3-response
  "CSI = c (DA3 / tertiary device attributes) queues the DECRPTUI reply."
  (with-screen (s 20 5)
    (feed s (esc "[=c"))       ; DA3
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI =c")
      (is (some (lambda (r) (search "!|00000000" r)) q)
          "DA3 reply must contain the unit-id report !|00000000"))))

;;; ── DECRQM (request DEC private mode, CSI ? Ps $ p) ──────────────────────────

(test decrqm-reports-set-mode
  "DECRQM CSI ? 25 $ p reports the cursor-visibility mode as SET (Pm=1) by default."
  (with-screen (s 20 5)
    (feed s (esc "[?25$p"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search (format nil "~C[?25;1$y" #\Escape) r)) q)
          "DECRQM ?25 must report set (Pm=1) when the cursor is visible (got ~S)" q))))

(test decrqm-reports-reset-mode
  "After ?25l (hide cursor) DECRQM reports the mode as RESET (Pm=2)."
  (with-screen (s 20 5)
    (feed s (esc "[?25l"))     ; hide cursor
    (feed s (esc "[?25$p"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search (format nil "~C[?25;2$y" #\Escape) r)) q)
          "DECRQM ?25 must report reset (Pm=2) after ?25l (got ~S)" q))))

(test decrqm-unknown-mode-reports-zero
  "DECRQM for an unrecognised mode reports Pm=0 (not recognised)."
  (with-screen (s 20 5)
    (feed s (esc "[?9999$p"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search (format nil "~C[?9999;0$y" #\Escape) r)) q)
          "DECRQM unknown mode must report Pm=0 (got ~S)" q))))

(test decrqm-reports-decscnm-mode-5
  "DECRQM ?5 reports DECSCNM (reverse-video screen): reset by default, set after ?5h."
  (with-screen (s 20 5)
    (feed s (esc "[?5$p"))
    (is (some (lambda (r) (search (format nil "~C[?5;2$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?5 must report reset (Pm=2) by default")
    (feed s (esc "[?5h"))
    (feed s (esc "[?5$p"))
    (is (some (lambda (r) (search (format nil "~C[?5;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?5 must report set (Pm=1) after ?5h")))

(test decrqm-reports-decawm-mode-7
  "DECRQM ?7 reports DECAWM (autowrap): set by default, reset after ?7l."
  (with-screen (s 20 5)
    (feed s (esc "[?7$p"))
    (is (some (lambda (r) (search (format nil "~C[?7;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?7 must report set (Pm=1, autowrap on) by default")
    (feed s (esc "[?7l"))
    (feed s (esc "[?7$p"))
    (is (some (lambda (r) (search (format nil "~C[?7;2$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?7 must report reset (Pm=2) after ?7l")))

(test decrqm-reports-sgr-mouse-mode-1006
  "DECRQM ?1006 reports the SGR mouse-encoding state, set after ?1006h."
  (with-screen (s 20 5)
    (feed s (esc "[?1006h"))
    (feed s (esc "[?1006$p"))
    (is (some (lambda (r) (search (format nil "~C[?1006;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?1006 must report set (Pm=1) after ?1006h")))

(test decrqm-ansi-reports-irm-mode-4
  "ANSI-mode DECRQM (CSI 4 $ p, no ? marker) reports IRM: reset by default, set
   after CSI 4 h.  Reply has NO ? marker (ESC [ 4 ; Pm $ y)."
  (with-screen (s 20 5)
    (feed s (esc "[4$p"))
    (is (some (lambda (r) (search (format nil "~C[4;2$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "ANSI DECRQM 4 must report reset (Pm=2) by default")
    (feed s (esc "[4h"))
    (feed s (esc "[4$p"))
    (is (some (lambda (r) (search (format nil "~C[4;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "ANSI DECRQM 4 must report set (Pm=1) after CSI 4 h")))

(test decrqm-ansi-reports-lnm-mode-20
  "ANSI-mode DECRQM (CSI 20 $ p) reports LNM: set after CSI 20 h."
  (with-screen (s 20 5)
    (feed s (esc "[20h"))
    (feed s (esc "[20$p"))
    (is (some (lambda (r) (search (format nil "~C[20;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "ANSI DECRQM 20 must report set (Pm=1) after CSI 20 h")))

;;; ── XTWINOPS size reports (CSI Ps t) ─────────────────────────────────────────

(def-suite xtwinops
  :description "XTWINOPS size reports (CSI Ps t)"
  :in terminal-suite)
(in-suite xtwinops)

(test xtwinops-18-reports-text-area-chars
  "CSI 18 t reports the text-area size in characters: ESC [ 8 ; rows ; cols t."
  (with-screen (s 20 5)
    (feed s (esc "[18t"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (string= (format nil "~C[8;5;20t" #\Escape) r)) q)
          "CSI 18 t must report ESC[8;5;20t for a 20x5 screen (got ~S)" q))))

(test xtwinops-19-reports-screen-chars
  "CSI 19 t reports the screen size in characters: ESC [ 9 ; rows ; cols t."
  (with-screen (s 20 5)
    (feed s (esc "[19t"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (string= (format nil "~C[9;5;20t" #\Escape) r)) q)
          "CSI 19 t must report ESC[9;5;20t (got ~S)" q))))

(test xtwinops-resize-op-no-reply
  "A window-manipulation XTWINOPS op (CSI 8 ; 24 ; 80 t resize) enqueues no reply —
   a multiplexer does not resize the outer window."
  (with-screen (s 20 5)
    (feed s (esc "[8;24;80t"))
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "XTWINOPS resize (op 8) must not enqueue a reply")))

;;; ── CPR (cursor position report, CSI 6 n) ────────────────────────────────────

(test cpr-at-home-replies-1-1
  "CSI 6 n (CPR) at the home position replies ESC[1;1R (1-based)."
  (with-screen (s 20 5)
    (feed s (esc "[6n"))       ; CPR — report cursor position
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI 6n")
      (is (some (lambda (r) (search "[1;1R" r)) q)
          "CPR at home must report [1;1R"))))

(test cpr-reports-moved-cursor-position
  "After CUP to row 3, col 5, CSI 6 n reports the new 1-based position ESC[3;5R."
  (with-screen (s 20 5)
    (feed s (esc "[3;5H"))     ; CUP → row 3, col 5 (1-based)
    (feed s (esc "[6n"))       ; CPR
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search "[3;5R" r)) q)
          "CPR after CUP 3;5 must report [3;5R"))))

(test cpr-in-decom-mode-reports-relative-row
  "In DECOM origin mode, CPR row is relative to the scroll-top margin (row 1 = margin top)."
  ;; Set a 10-row screen, scroll region rows 3..8 (0-based 2..7), enable DECOM,
  ;; place cursor at absolute row 5 (0-based 4) → relative row 3 (4-2+1=3).
  (with-screen (s 20 10)
    (feed s (esc "[3;8r"))    ; DECSTBM: scroll region rows 3..8 (1-based)
    (feed s (esc "[?6h"))     ; DECOM on — cursor is now relative to margin
    ;; CUP in DECOM mode: row 3 col 1 (1-based relative) → absolute row 4 (0-based)
    (feed s (esc "[3;1H"))
    (feed s (esc "[6n"))      ; CPR
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search "[3;1R" r)) q)
          "CPR in DECOM mode must report margin-relative row 3, not absolute row 4"))))

;;; ── DA response table: both responses enqueue without error ──────────────────
;;;
;;; The two DA variants (DA1/DA2) both follow the same pattern: feed the
;;; sequence, assert the queue is non-empty, assert a signature string.
;;; The table condenses the two individual tests into a loop so adding a new
;;; DA variant only requires a new row.

(test da-response-table
  "DA1 and DA2 both enqueue a response string with the expected signature."
  :description "parameterized DA1/DA2 response checks"
  (dolist (entry '(("[c"  "?1;2c")    ; DA1 signature
                   ("[>c" ">1;")))     ; DA2 signature
    (let ((seq (first entry))
          (sig (second entry)))
      (with-screen (s 20 5)
        (feed s (esc seq))
        (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
          (is (consp q) "response-queue must be non-empty after ~A" seq)
          (is (some (lambda (r) (search sig r)) q)
              "response must contain ~S" sig))))))

;;; ── REP count=0 is a no-op ───────────────────────────────────────────────────

(test rep-count-zero-is-noop
  "CSI 0 b (REP 0) is effectively a no-op: no additional cells written."
  :description "REP with count=0 writes nothing extra"
  (with-screen (s 20 5)
    (feed s "X")
    (let ((cx (screen-cursor-x s)))
      (feed s (esc "[0b"))
      ;; Cursor should stay at col cx (no writes for count=0).
      (is (= cx (screen-cursor-x s))
          "REP 0 must not advance the cursor"))))

;;; ── Coverage gap: %decrqm-boolean direct tests ───────────────────────────────
;;;
;;; Audit finding: %decrqm-boolean and %decrqm-ansi-mode-state have no direct
;;; unit tests — they are reachable only through the end-to-end CSI path.

(def-suite decrqm-internal
  :description "Direct coverage of DECRQM internal helpers"
  :in terminal-suite)
(in-suite decrqm-internal)

(test decrqm-boolean-table
  "%decrqm-boolean returns 1 for T (set) and 2 for NIL (reset)."
  (is (= 1 (cl-tmux/terminal/csi::%decrqm-boolean t))   "%decrqm-boolean T must return 1")
  (is (= 2 (cl-tmux/terminal/csi::%decrqm-boolean nil)) "%decrqm-boolean NIL must return 2"))

(test decrqm-ansi-mode-state-irm-table
  "%decrqm-ansi-mode-state reports IRM (mode 4) as 1 when insert-mode T, 2 when NIL."
  (dolist (row '((t   1 "insert-mode T → 1 (set)")
                 (nil 2 "insert-mode NIL → 2 (reset)")))
    (destructuring-bind (insert-mode-val expected desc) row
      (with-screen (s 20 5)
        (setf (cl-tmux/terminal/types:screen-insert-mode s) insert-mode-val)
        (is (= expected (cl-tmux/terminal/csi::%decrqm-ansi-mode-state s 4))
            "~A" desc)))))

(test decrqm-ansi-mode-state-lnm-set
  "%decrqm-ansi-mode-state reports LNM (mode 20) as 1 (set) when newline-mode is T."
  (with-screen (s 20 5)
    (setf (cl-tmux/terminal/types:screen-newline-mode s) t)
    (is (= 1 (cl-tmux/terminal/csi::%decrqm-ansi-mode-state s 20))
        "%decrqm-ansi-mode-state mode 20 must be 1 when newline-mode is T")))

(test decrqm-ansi-mode-state-unknown-returns-0
  "%decrqm-ansi-mode-state returns 0 for an unrecognised ANSI mode."
  (with-screen (s 20 5)
    (is (= 0 (cl-tmux/terminal/csi::%decrqm-ansi-mode-state s 999))
        "%decrqm-ansi-mode-state must return 0 for an unknown mode")))

;;; ── Coverage gap: enqueue-da3-reply and enqueue-xtversion-reply ─────────────
;;;
;;; Audit finding: DA3 and XTVERSION reply enqueuers were never directly tested.

(def-suite da3-xtversion-direct
  :description "Direct coverage of DA3 and XTVERSION reply enqueuers"
  :in terminal-suite)
(in-suite da3-xtversion-direct)

(test enqueue-reply-substring-table
  "enqueue-da3-reply contains '!|00000000'; enqueue-xtversion-reply contains 'tmux'."
  (dolist (row (list (list #'cl-tmux/terminal/csi::enqueue-da3-reply      "!|00000000" "DA3 reply")
                     (list #'cl-tmux/terminal/csi::enqueue-xtversion-reply "tmux"       "XTVERSION reply")))
    (destructuring-bind (fn expected desc) row
      (with-screen (s 20 5)
        (funcall fn s)
        (is (some (lambda (r) (search expected r))
                  (cl-tmux/terminal/types:screen-response-queue s))
            "~A must push a string containing ~S" desc expected)))))

;;; ── Coverage gap: enqueue-xtwinops-reply direct tests ───────────────────────
;;;
;;; Audit finding: the enqueue-xtwinops-reply function was only tested end-to-end.

(def-suite xtwinops-direct
  :description "Direct coverage of enqueue-xtwinops-reply"
  :in terminal-suite)
(in-suite xtwinops-direct)

(test enqueue-xtwinops-reply-size-ops-table
  "enqueue-xtwinops-reply op 18 reports text-area ([8;…]) and op 19 reports screen ([9;…])."
  (dolist (row '((18 "[8;8;30t" "op 18 text-area report")
                 (19 "[9;8;30t" "op 19 screen report")))
    (destructuring-bind (op expected desc) row
      (with-screen (s 30 8)
        (cl-tmux/terminal/csi::enqueue-xtwinops-reply s op)
        (is (some (lambda (r) (search expected r))
                  (cl-tmux/terminal/types:screen-response-queue s))
            "~A: must contain ~S" desc expected)))))

(test enqueue-xtwinops-reply-op-99-no-reply
  "enqueue-xtwinops-reply with an unsupported op enqueues nothing."
  (with-screen (s 20 5)
    (cl-tmux/terminal/csi::enqueue-xtwinops-reply s 99)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "unsupported XTWINOPS op must not enqueue a reply")))

