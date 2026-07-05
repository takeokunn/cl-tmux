(in-package #:cl-tmux/terminal/csi)

;;;; CSI (Control Sequence Introducer) declarative rule table.
;;;;
;;;; define-csi-rules in csi-dispatch.lisp builds EXECUTE-CSI from these facts.
;;;; Parameter interpretation lives in csi-parameters.lisp.
;;;; All grid mutations (insert/delete chars, scroll-region margins, alternate
;;;; screen) live in cl-tmux/terminal/actions; the rule table below calls them
;;;; directly.
;;;;
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
   (let ((preceding-char (screen-last-char screen))
         (count          (if params (first params) 1)))
     (when preceding-char
       (dotimes (_ count) (write-char-at-cursor screen preceding-char)))))

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
