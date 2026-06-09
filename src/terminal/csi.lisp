(in-package #:cl-tmux/terminal/csi)

;;;; CSI (Control Sequence Introducer) macro-driven dispatch.
;;;;
;;;; define-csi-rules builds a COND-based dispatcher keyed on the final
;;;; character and optional intermediate character of the escape sequence.
;;;; execute-csi is the public entry point called by the parser.

;;; ── Macro ──────────────────────────────────────────────────────────────────

(defmacro define-csi-rules (&rest rules)
  "Each RULE is (condition-form &body forms).
   Available bindings in every rule body:
     SCREEN   – the screen struct
     FINAL    – the sequence final character (type character)
     INTERMED – intermediate character (character or nil; e.g. #\\? for DEC)
     PARAMS   – full parameter list (list of fixnum)
     P1       – first  parameter or 0
     P2       – second parameter or 0
     P1*      – (max 1 p1)
     P2*      – (max 1 p2)
   Expands into a DEFUN for EXECUTE-CSI that dispatches via COND.
   Unknown final bytes or unrecognized (INTERMED, FINAL) combinations are
   silently ignored and return (values), matching real-terminal behaviour."
  `(defun execute-csi (screen final intermed private params)
     "Dispatch one complete CSI escape sequence to its terminal action.
      SCREEN is the target screen struct.  FINAL is the sequence's final byte as
      a character.  INTERMED is the optional true intermediate byte (#x20-#x2F,
      e.g. #\\Space for DECSCUSR, #\\$ for DECRQM), or NIL.  PRIVATE is the optional
      private/marker byte (#\\? for DEC private sequences, #\\> for secondary DA),
      or NIL.  PARAMS is the list of integer parameters (possibly empty).
      Unknown sequences are silently ignored; no error is signalled."
     (declare (type screen screen)
              (type character final)
              (ignorable intermed private))
     (let* ((p1  (%csi-leading-int (first  params)))
            (p2  (%csi-leading-int (second params)))
            (p1* (max 1 p1))
            (p2* (max 1 p2)))
       (declare (type fixnum p1 p2 p1* p2*) (ignorable p1 p2 p1* p2*))
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (condition &rest body) rule
                       `(,condition ,@body)))
                   rules)
         (t (values))))))

(declaim (inline %csi-leading-int))
(defun %csi-leading-int (param)
  "The leading integer of a CSI PARAM for the scalar P1/P2 bindings.  A param
   carrying colon sub-parameters arrives as a list (sub0 sub1 …) — non-SGR
   handlers want only its leading value (sub0), matching pre-colon behaviour.
   A plain integer is returned as-is; NIL → 0.  (apply-sgr keeps the raw list
   so it can apply colon-form extended colour.)"
  (cond ((consp param)    (or (first param) 0))
        ((integerp param) param)
        (t 0)))

;;; All grid mutations (insert/delete chars, scroll-region margins, alternate
;;; screen) live in cl-tmux/terminal/actions; the rule table below calls them
;;; directly.  DECSTBM parameters arrive 1-based, so they are converted to the
;;; 0-based inclusive margins that ACTIONS:DECSTBM expects at the call site.

(declaim (inline %csi-decstbm-params))
(defun %csi-decstbm-params (screen p1 p2)
  "Convert 1-based DECSTBM CSI parameters P1 and P2 to the 0-based inclusive
   (top bottom) pair expected by ACTIONS:DECSTBM.
   P2 = 0 means 'full screen': the bottom margin defaults to height-1.
   When top >= bottom (invalid margins), reset to full-screen (VT100 behaviour)."
  (let* ((top    (1- (max 1 p1)))
         (bottom (if (zerop p2) (1- (screen-height screen)) (1- p2))))
    (if (>= top bottom)
        (values 0 (1- (screen-height screen)))
        (values top bottom))))

(defun %cup-row (screen p1)
  "Translate a 1-based CUP/HVP row P1 to a 0-based screen row, honoring DECOM
   origin mode (?6): when set, the row is relative to the scroll-region top and
   clamped to the scroll region; otherwise it is absolute."
  (if (screen-origin-mode screen)
      (min (+ (screen-scroll-top screen) (1- p1)) (screen-scroll-bottom screen))
      (1- p1)))

;;; ── Response-queue action helpers ─────────────────────────────────────────
;;;
;;; DSR, CPR, DA1, and DA2 all push a response string onto the screen's
;;; response-queue so the PTY loop can drain it back to the application.
;;; Extracting named helpers moves the format/push I/O concern out of the
;;; declarative CSI rule table and into named, testable action functions.

(defun enqueue-dsr-reply (screen)
  "Push the Device Status Report OK reply (ESC [ 0 n) onto SCREEN's response queue."
  (push (format nil "~C[0n" #\Escape)
        (screen-response-queue screen)))

(defun enqueue-cpr-reply (screen)
  "Push a Cursor Position Report (ESC [ row ; col R, 1-based) onto SCREEN's
   response queue, reflecting the current cursor position."
  (push (format nil "~C[~D;~DR" #\Escape
                (1+ (screen-cursor-y screen))
                (1+ (screen-cursor-x screen)))
        (screen-response-queue screen)))

(defun enqueue-da1-reply (screen)
  "Push the Primary Device Attributes response (ESC [ ? 1 ; 2 c — VT100 with AVO)
   onto SCREEN's response queue."
  (push (format nil "~C[?1;2c" #\Escape)
        (screen-response-queue screen)))

(defun enqueue-da2-reply (screen)
  "Push the Secondary Device Attributes response (ESC [ > 1 ; 10 ; 0 c)
   onto SCREEN's response queue."
  (push (format nil "~C[>1;10;0c" #\Escape)
        (screen-response-queue screen)))

(defun %decrqm-mode-state (screen mode)
  "DECRQM reply value for DEC private MODE: 1 = set, 2 = reset, 0 = not recognised.
   Reports from the screen's tracked mode flags so an application querying support
   gets an accurate answer; an unknown mode reports 0 (so the app falls back)."
  (flet ((b (x) (if x 1 2)))
    (case mode
      (1    (b (screen-app-cursor-keys screen)))            ; DECCKM
      (6    (b (screen-origin-mode screen)))                ; DECOM
      (25   (b (screen-cursor-visible screen)))             ; DECTCEM
      (1000 (b (= (screen-mouse-mode screen) 1)))           ; X10/normal mouse
      (1002 (b (= (screen-mouse-mode screen) 2)))           ; button-event mouse
      (1003 (b (= (screen-mouse-mode screen) 3)))           ; any-event mouse
      (1004 (b (screen-focus-events screen)))               ; focus reporting
      ((47 1047 1049) (b (and (screen-alt-cells screen) t))) ; alternate screen
      (2004 (b (screen-bracketed-paste screen)))            ; bracketed paste
      (2026 2)   ; synchronized output: accepted but not a persistent mode → reset
      (t    0))))

(defun enqueue-decrqm-reply (screen mode)
  "Push the DECRQM report (ESC [ ? MODE ; Pm $ y) onto SCREEN's response queue,
   where Pm is %decrqm-mode-state for MODE."
  (push (format nil "~C[?~D;~D$y" #\Escape mode (%decrqm-mode-state screen mode))
        (screen-response-queue screen)))

(defun enqueue-xtversion-reply (screen)
  "Push the XTVERSION report (CSI > q) onto SCREEN's response queue:
   ESC P > | tmux 3.5 ST.  cl-tmux presents the tmux 3.5 identity (consistent with
   #{version} and `tmux -V`), so an app querying the terminal version — as real
   tmux 3.5 answers — sees tmux 3.5."
  (push (format nil "~CP>|tmux 3.5~C\\" #\Escape #\Escape)
        (screen-response-queue screen)))

(defun enqueue-xtwinops-reply (screen op)
  "Push the XTWINOPS size REPORT for operation OP onto SCREEN's response queue:
     18 → text-area size in CHARACTERS: ESC [ 8 ; rows ; cols t
     19 → screen size in characters:    ESC [ 9 ; rows ; cols t
   Apps query these to learn the grid size.  Other XTWINOPS operations
   (resize/move/iconify the window, or pixel-size reports cl-tmux cannot answer)
   enqueue nothing — a multiplexer does not manipulate the outer window, and a
   wrong pixel size would mislead callers more than no reply."
  (case op
    (18 (push (format nil "~C[8;~D;~Dt" #\Escape
                      (screen-height screen) (screen-width screen))
              (screen-response-queue screen)))
    (19 (push (format nil "~C[9;~D;~Dt" #\Escape
                      (screen-height screen) (screen-width screen))
              (screen-response-queue screen)))))

;;; ── CSI rule table ─────────────────────────────────────────────────────────

(define-csi-rules

  ;; CUU – Cursor Up
  ((and (null intermed) (char= final #\A))
   (set-cursor screen (screen-cursor-x screen) (- (screen-cursor-y screen) p1*)))

  ;; CUD – Cursor Down
  ((and (null intermed) (char= final #\B))
   (set-cursor screen (screen-cursor-x screen) (+ (screen-cursor-y screen) p1*)))

  ;; CUF – Cursor Forward (right)
  ((and (null intermed) (char= final #\C))
   (set-cursor screen (+ (screen-cursor-x screen) p1*) (screen-cursor-y screen)))

  ;; CUB – Cursor Back (left)
  ((and (null intermed) (char= final #\D))
   (set-cursor screen (- (screen-cursor-x screen) p1*) (screen-cursor-y screen)))

  ;; CNL – Cursor Next Line
  ((and (null intermed) (char= final #\E))
   (set-cursor screen 0 (+ (screen-cursor-y screen) p1*)))

  ;; CPL – Cursor Preceding Line
  ((and (null intermed) (char= final #\F))
   (set-cursor screen 0 (- (screen-cursor-y screen) p1*)))

  ;; CHA – Cursor Horizontal Absolute (1-based column)
  ((and (null intermed) (char= final #\G))
   (set-cursor screen (1- p1*) (screen-cursor-y screen)))

  ;; HPA – Horizontal Position Absolute (CSI `; 1-based column, alias of CHA).
  ((and (null intermed) (char= final #\`))
   (set-cursor screen (1- p1*) (screen-cursor-y screen)))

  ;; HPR – Horizontal Position Relative (CSI a; move right P1, alias of CUF).
  ((and (null intermed) (char= final #\a))
   (set-cursor screen (+ (screen-cursor-x screen) p1*) (screen-cursor-y screen)))

  ;; VPR – Vertical Position Relative (CSI e; move down P1, alias of CUD).
  ((and (null intermed) (char= final #\e))
   (set-cursor screen (screen-cursor-x screen) (+ (screen-cursor-y screen) p1*)))

  ;; SCOSC – Save Cursor Position (CSI s; ANSI.SYS, complements ESC 7 / DECSC).
  ;; This is the OUTPUT (pane → screen) meaning.  On INPUT, CSI <code> u is the
  ;; extended-keys decode handled in events-keystroke.lisp — a separate path, so
  ;; there is no conflict with the CSI u (SCORC) restore below.
  ((and (null intermed) (char= final #\s))
   (save-cursor screen))

  ;; SCORC – Restore Cursor Position (CSI u; ANSI.SYS, complements ESC 8 / DECRC).
  ((and (null intermed) (char= final #\u))
   (restore-cursor screen))

  ;; CUP – Cursor Position (row P1, col P2, 1-based; row is DECOM-aware)
  ((and (null intermed) (char= final #\H))
   (set-cursor screen (1- p2*) (%cup-row screen p1*)))

  ;; ICH – Insert Characters
  ((and (null intermed) (char= final #\@))
   (insert-chars screen p1*))

  ;; ED – Erase in Display
  ((and (null intermed) (char= final #\J))
   (erase-display screen p1))

  ;; EL – Erase in Line
  ((and (null intermed) (char= final #\K))
   (erase-line screen p1))

  ;; IL – Insert Lines at the cursor row
  ((and (null intermed) (char= final #\L))
   (insert-lines screen p1*))

  ;; DL – Delete Lines at the cursor row
  ((and (null intermed) (char= final #\M))
   (delete-lines screen p1*))

  ;; DCH – Delete Characters
  ((and (null intermed) (char= final #\P))
   (delete-chars screen p1*))

  ;; SU – Scroll Up
  ((and (null intermed) (char= final #\S))
   (dotimes (_ p1*) (scroll-up-one screen)))

  ;; SD – Scroll Down
  ((and (null intermed) (char= final #\T))
   (dotimes (_ p1*) (scroll-down-one screen)))

  ;; ECH – Erase Characters (fill with blanks, no shift)
  ((and (null intermed) (char= final #\X))
   (erase-region screen
                 (screen-cursor-x screen) (screen-cursor-y screen)
                 (min (+ (screen-cursor-x screen) p1* -1)
                      (1- (screen-width screen)))
                 (screen-cursor-y screen)))

  ;; REP – Repeat Preceding Character (CSI Ps b)
  ;; Repeats the last printed character P1* times.  The preceding character is
  ;; tracked via the SCREEN-LAST-CHAR slot; if nothing has been written yet
  ;; the sequence is a no-op, which matches xterm behaviour.
  ((and (null intermed) (char= final #\b))
   ;; Count comes from the RAW param: an explicit 0 (CSI 0 b) repeats 0 times
   ;; (a no-op), while an ABSENT param (CSI b) defaults to 1.  p1* = (max 1 p1)
   ;; cannot tell these apart because p1 is 0 in both cases.
   (let ((ch    (screen-last-char screen))
         (count (if params (first params) 1)))
     (when ch
       (dotimes (_ count) (write-char-at-cursor screen ch)))))

  ;; VPA – Vertical Position Absolute (1-based row)
  ((and (null intermed) (char= final #\d))
   (set-cursor screen (screen-cursor-x screen) (1- p1*)))

  ;; HVP – Horizontal and Vertical Position (same as CUP)
  ((and (null intermed) (char= final #\f))
   (set-cursor screen (1- p2*) (%cup-row screen p1*)))

  ;; SGR – Select Graphic Rendition
  ((and (null intermed) (char= final #\m))
   (apply-sgr screen params))

  ;; DSR – Device Status Report.  Replies are queued onto the response-queue,
  ;; which the PTY loop drains back to the application (same path as DA1/DA2).
  ;;   CSI 5 n → device status: ESC [ 0 n  (terminal OK)
  ((and (null intermed) (char= final #\n) (= p1 5))
   (enqueue-dsr-reply screen))
  ;; CPR – Cursor Position Report.
  ;;   CSI 6 n → ESC [ <row> ; <col> R  (1-based; apps like shells, vim, less
  ;;   block waiting for this reply, so an unanswered query hangs them).
  ((and (null intermed) (char= final #\n) (= p1 6))
   (enqueue-cpr-reply screen))

  ;; DECSTBM – Set Top and Bottom Margins.  Params are 1-based; an omitted
  ;; bottom (p2 = 0) means "full screen", matching real-terminal ESC[r reset.
  ((and (null intermed) (char= final #\r))
   (multiple-value-call #'decstbm screen (%csi-decstbm-params screen p1 p2)))

  ;; CHT – Cursor Forward Tabulation (CSI N I)
  ((and (null intermed) (char= final #\I))
   (cursor-cht screen p1*))

  ;; CBT – Cursor Backward Tabulation (CSI N Z)
  ((and (null intermed) (char= final #\Z))
   (cursor-cbt screen p1*))

  ;; TBC – Tab Clear (CSI N g).  p1=0 (or omitted) clears the stop at the cursor
  ;; column; p1=3 clears all tab stops.
  ((and (null intermed) (char= final #\g))
   (clear-tab-stops screen p1))

  ;; DA1 – Primary Device Attributes (CSI c or CSI 0 c)
  ;; Response: ESC [ ? 1 ; 2 c  (VT100 with AVO)
  ;; DA1 (CSI c): require NO private marker so it does not shadow DA2 (CSI > c),
  ;; whose '>' now lives in PRIVATE rather than INTERMED.
  ((and (null intermed) (null private) (char= final #\c))
   (enqueue-da1-reply screen))

  ;; DA2 – Secondary Device Attributes (CSI > c or CSI > 0 c)
  ;; Response: ESC [ > 1 ; 10 ; 0 c
  ((and (eql private #\>) (char= final #\c))
   (enqueue-da2-reply screen))

  ;; XTPUSHTITLE – push window title onto the title stack (CSI > Ps t)
  ;; XTPOPTITLE  – pop window title from the stack (CSI < Ps t)
  ;; These are accepted silently; our title is a single slot so we just no-op.
  ;; Applications use these to save/restore the window title across operations.
  ((and (eql private #\>) (char= final #\t))
   (values))  ; push title — no-op (no title stack implemented)
  ((and (eql private #\<) (char= final #\t))
   (values))  ; pop title — no-op

  ;; XTVERSION — query terminal name/version (CSI > q): reply ESC P > | tmux 3.5 ST
  ((and (eql private #\>) (char= final #\q))
   (enqueue-xtversion-reply screen))

  ;; DEC Private Mode Set (?...h) — e.g. ?1049h enters the alternate screen
  ((and (eql private #\?) (char= final #\h))
   (dec-pm-set screen params))

  ;; DEC Private Mode Reset (?...l) — e.g. ?1049l exits the alternate screen
  ((and (eql private #\?) (char= final #\l))
   (dec-pm-reset screen params))

  ;; DECRQM — Request DEC private Mode (CSI ? Ps $ p): reply with the mode's
  ;; current state (ESC [ ? Ps ; Pm $ y) so apps can detect feature support.
  ((and (eql private #\?) (eql intermed #\$) (char= final #\p))
   (enqueue-decrqm-reply screen p1))

  ;; XTWINOPS — window operations / reports (CSI Ps ; … t, no private marker).
  ;; We answer the size REPORTS (18 = text area in characters, 19 = screen in
  ;; characters) so apps can learn the grid size; window-manipulation operations
  ;; (resize/move/iconify) are no-ops for a multiplexer.
  ((and (null private) (char= final #\t))
   (enqueue-xtwinops-reply screen p1))

  ;; DECSCUSR — cursor shape: CSI N SP q (intermediate = space, final = q)
  ((and (eql intermed #\Space) (char= final #\q))
   (set-cursor-shape screen p1)))
