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

;;; ── CSI rule table ─────────────────────────────────────────────────────────
;;;
;;; The response-queue reply layer (DSR/DA1/DA2/DA3/XTVERSION/CPR fixed and
;;; computed replies, DECRQM mode-state tables, XTWINOPS size-report helpers)
;;; called by the rules below lives in csi-replies.lisp, which loads first.

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

  ;; XTPUSHTITLE – push window title onto the title stack (CSI > Ps t).
  ;; Saves the current title so it can be restored later.  The stack is
  ;; bounded to +title-stack-max-depth+ entries (xterm limit) — the oldest
  ;; entry is discarded when the limit is exceeded.  Used by neovim and other TUIs.
  ((and (eql private #\>) (char= final #\t))
   (push-title-stack screen))

  ;; XTPOPTITLE – pop and restore the most recently pushed title (CSI < Ps t).
  ;; A pop on an empty stack is a no-op, matching xterm.
  ((and (eql private #\<) (char= final #\t))
   (pop-title-stack screen))

  ;; XTVERSION — query terminal name/version (CSI > q)
  ((and (eql private #\>) (char= final #\q))
   (enqueue-xtversion-reply screen))

  ;; DA3 — Tertiary Device Attributes (CSI = c): reply ESC P ! | <unit-id> ST
  ((and (eql private #\=) (char= final #\c))
   (enqueue-da3-reply screen))

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

  ;; DECRQM for ANSI (non-private) modes (CSI Ps $ p): reply ESC [ Ps ; Pm $ y
  ;; (no ? marker).  Covers IRM (4) and LNM (20).
  ((and (null private) (eql intermed #\$) (char= final #\p))
   (enqueue-decrqm-ansi-reply screen p1))

  ;; DECERA — Erase Rectangular Area (CSI Pt ; Pl ; Pb ; Pr $ z).
  ;; Fills the rectangle with BCE (background-colour-erase) blanks.
  ;; Parameters: top left bottom right (all 1-based inclusive).
  ((and (null private) (eql intermed #\$) (char= final #\z))
   (let ((p3 (%csi-leading-int (third  params)))
         (p4 (%csi-leading-int (fourth params))))
     (decera screen p1 p2 p3 p4)))

  ;; DECFRA — Fill Rectangular Area (CSI Pc ; Pt ; Pl ; Pb ; Pr $ x).
  ;; Fills the rectangle with character code Pc, using the current SGR pen.
  ;; Parameters: char-code top left bottom right (all 1-based; Pc is the char).
  ((and (null private) (eql intermed #\$) (char= final #\x))
   (let ((p3 (%csi-leading-int (third  params)))
         (p4 (%csi-leading-int (fourth params)))
         (p5 (%csi-leading-int (fifth  params))))
     (decfra screen p1 p2 p3 p4 p5)))

  ;; DECCRA — Copy Rectangular Area (CSI Pt ; Pl ; Pb ; Pr ; Pp ; Ptp ; Plp ; Ppp $ v).
  ;; Copies source rectangle to target.  Page parameters (Pp, Ppp) are ignored.
  ;; Parameters: src-top src-left src-bottom src-right src-page tgt-top tgt-left tgt-page.
  ((and (null private) (eql intermed #\$) (char= final #\v))
   (let ((p3 (%csi-leading-int (third  params)))
         (p4 (%csi-leading-int (fourth params)))
         (p6 (%csi-leading-int (sixth  params)))
         (p7 (%csi-leading-int (seventh params))))
     (deccra screen p1 p2 p3 p4 p6 p7)))

  ;; ANSI Set/Reset Mode — CSI Ps h / CSI Ps l (NO private marker).  IRM (mode 4)
  ;; toggles insert/replace; other ANSI modes are accepted and ignored.
  ((and (null private) (null intermed) (char= final #\h))
   (set-ansi-mode screen params))
  ((and (null private) (null intermed) (char= final #\l))
   (reset-ansi-mode screen params))

  ;; DECSTR — Soft Terminal Reset (CSI ! p): restore modes/SGR to defaults without
  ;; clearing the screen or moving the cursor (cf. RIS, ESC c, which does both).
  ((and (eql intermed #\!) (char= final #\p))
   (decstr-action screen))

  ;; XTWINOPS — window operations / reports (CSI Ps ; … t, no private marker).
  ;; We answer the size REPORTS (+xtwinops-text-area-query+ = text area in
  ;; characters, +xtwinops-screen-query+ = screen in characters) so apps can
  ;; learn the grid size; window-manipulation operations (resize/move/iconify)
  ;; are no-ops for a multiplexer.
  ((and (null private) (char= final #\t))
   (enqueue-xtwinops-reply screen p1))

  ;; DECSCUSR — cursor shape: CSI N SP q (intermediate = space, final = q)
  ((and (eql intermed #\Space) (char= final #\q))
   (set-cursor-shape screen p1)))
