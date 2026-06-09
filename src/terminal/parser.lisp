(in-package #:cl-tmux/terminal/parser)

;;;; CPS-based VT100 parser.
;;;;
;;;; Each parser state is a function: (screen byte) → next-state-function
;;;; The screen's (parser) slot holds the current continuation.
;;;; There is no mutable parser state outside of the CPS closures themselves.
;;;;
;;;; States are defined with DEFINE-STATE, a Prolog-like macro that maps
;;;; byte patterns to actions and their continuation (next state).  Each
;;;; rule reads as a Prolog clause: "given this byte, take this action,
;;;; transition to this state."

;;; ── Inline helpers ─────────────────────────────────────────────────────────

(declaim (inline printable-ascii-p utf8-lead-p utf8-continuation-p))

(defun printable-ascii-p (byte)
  "Return T when BYTE is in the printable ASCII range #x20-#x7E (space through tilde)."
  (and (>= byte #x20) (< byte #x7F)))

(defun utf8-lead-p (byte)
  "Return T when BYTE is a UTF-8 multi-byte lead byte (#xC0-#xFE, excluding #xFF)."
  (and (>= byte #xC0) (/= byte #xFF)))

(defun utf8-continuation-p (byte)
  "Return T when BYTE is a UTF-8 continuation byte (#x80-#xBF, high two bits = 10)."
  (= (logand byte #xC0) #x80))

(defun utf8-lead-decode (byte)
  "Return (values initial-accumulator continuation-bytes-remaining)."
  (cond ((< byte #xE0) (values (logand byte #x1F) 1))
        ((< byte #xF0) (values (logand byte #x0F) 2))
        (t             (values (logand byte #x07) 3))))

;;; ── Prolog-like state definition macro ─────────────────────────────────────
;;;
;;; (define-state NAME (SCREEN BYTE) rule...)
;;; Each rule is (PATTERN &rest BODY) where PATTERN is:
;;;   integer  → exact byte match:   (= BYTE integer)
;;;   symbol   → predicate match:    (symbol BYTE)
;;;   t        → default clause
;;;   list     → verbatim condition
;;; The BODY forms are evaluated in order; the last form is the next state.
;;; Both SCREEN and BYTE are declared ignorable so state functions that
;;; discard their arguments (e.g. osc-state, charset-state) compile cleanly.

(defmacro define-state (name (screen-var byte-var) &rest rules)
  "Prolog-like CPS state definition: one rule per parser state clause."
  `(defun ,name (,screen-var ,byte-var)
     (declare (type screen ,screen-var)
              (type (unsigned-byte 8) ,byte-var)
              (ignorable ,screen-var ,byte-var))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (pattern &rest body) rule
              `(,(cond
                   ((eq pattern 't)    't)
                   ((integerp pattern) `(= ,byte-var ,pattern))
                   ((symbolp pattern)  `(,pattern ,byte-var))
                   (t                   pattern))
                ,@body)))
          rules))))

;;; ── CSI byte-class constants ────────────────────────────────────────────────
;;;
;;; Named constants for the magic hex literals used in make-csi-k.
;;; The ranges follow ECMA-48 § 5.4 table.

(defconstant +csi-digit-low+   #x30 "Lowest decimal digit byte in a CSI sequence (ASCII '0').")
(defconstant +csi-digit-high+  #x39 "Highest decimal digit byte in a CSI sequence (ASCII '9').")
(defconstant +csi-semicolon+   #x3B "CSI parameter separator ';'.")
(defconstant +csi-colon+       #x3A
  "CSI sub-parameter separator ':' (ISO 8613-6).  Introduces colon-delimited
   sub-parameters within one parameter, e.g. SGR 4:3 (undercurl) or
   38:2::R:G:B (true-colour).  A parameter carrying colon sub-parameters is
   collected into a list (sub0 sub1 …) so apply-sgr can apply colon-form
   extended colour, rather than dropping everything after the leading value.")
(defconstant +csi-dec-marker+  #x3F "DEC private-mode marker '?'.")
(defconstant +csi-sec-da+      #x3E "Secondary DA marker '>'.")
(defconstant +csi-intermed-low+  #x20 "Lowest CSI intermediate byte (SPACE).")
(defconstant +csi-intermed-high+ #x2F "Highest CSI intermediate byte.")
(defconstant +csi-final-low+   #x40 "Lowest valid CSI final byte '@'.")
(defconstant +csi-final-high+  #x7E "Highest valid CSI final byte '~'.")

;;; ── Parameterized state constructors ───────────────────────────────────────

(defun %finish-param (param-accumulator subparams)
  "Combine a parameter's leading PARAM-ACCUMULATOR with its colon SUBPARAMS
   (already-flushed sub-values, in reverse order) into the finished parameter:
   a plain integer when no colon appeared, or a list (sub0 sub1 …) when it did.
   An absent leading value defaults to 0 (matching the semicolon-param rule)."
  (if subparams
      (nreverse (cons (or param-accumulator 0) subparams))
      (or param-accumulator 0)))

(defun make-csi-k (&optional (params '()) (param-accumulator nil) (intermed nil)
                             (private nil) (subparams nil))
  "Return a continuation that collects CSI parameters then dispatches.
   Handles the standard VT/ECMA-48 CSI parameter syntax:
     param bytes        +csi-digit-low+ to +csi-digit-high+  (digits 0-9)
     semicolons         +csi-semicolon+                       (parameter separator)
     marker bytes       +csi-dec-marker+ (#\\?) and +csi-sec-da+ (#\\>)
       These are VT convention 'private use' markers that set the intermed slot
       rather than the parameter accumulator.  They are NOT the same as true
       intermediate bytes (#x20-#x2F), even though both affect INTERMED.
     intermediate bytes +csi-intermed-low+ to +csi-intermed-high+  (e.g. SPACE)
       True intermediate bytes such as #x20 (SPACE) select a sub-table of the
       final-byte dispatch (e.g. DECSCUSR uses CSI N SP q).
     final byte         +csi-final-low+  to +csi-final-high+  (dispatch)"
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (cond
      ;; Digit 0-9: accumulate into the current parameter accumulator — unless we
      ;; are skipping a colon sub-parameter, in which case the digit is consumed
      ;; and discarded (we keep only the parameter's leading value).
      ((and (>= byte +csi-digit-low+) (<= byte +csi-digit-high+))
       (make-csi-k params
                   (+ (* (or param-accumulator 0) 10) (- byte +csi-digit-low+))
                   intermed private subparams))
      ;; Colon: ISO 8613-6 sub-parameter separator.  Flush the leading value
      ;; accumulated so far into SUBPARAMS and begin the next sub-parameter; the
      ;; finished parameter becomes a list so apply-sgr parses colon-form
      ;; extended colour (38:2:R:G:B, 38:5:N, 4:3 undercurl) rather than dropping
      ;; everything after the leading value.
      ((= byte +csi-colon+)
       (make-csi-k params nil intermed private
                   (cons (or param-accumulator 0) subparams)))
      ;; Semicolon: flush the current parameter (combining its colon sub-params,
      ;; if any), start fresh.
      ((= byte +csi-semicolon+)
       (make-csi-k (cons (%finish-param param-accumulator subparams) params)
                   nil intermed private nil))
      ;; ? — DEC private-mode marker byte (selects DEC private sequences).
      ;; Recorded in the PRIVATE slot (separate from a true intermediate) so that
      ;; sequences carrying BOTH — e.g. DECRQM "CSI ? Ps $ p" — keep the ? marker
      ;; even when a #x20-#x2F intermediate ($) follows.
      ((= byte +csi-dec-marker+)
       (make-csi-k params param-accumulator intermed #\? subparams))
      ;; > — secondary DA marker byte (selects secondary device attribute queries).
      ((= byte +csi-sec-da+)
       (make-csi-k params param-accumulator intermed #\> subparams))
      ;; < and = — the remaining ECMA-48 private-parameter markers (0x3C / 0x3D):
      ;; e.g. CSI < Ps t (XTPOPTITLE), CSI = c (tertiary DA / DA3).  Recorded in
      ;; PRIVATE like ? and >.  Without these, the byte hit the catch-all and
      ;; ABORTED the sequence, leaving the final byte to print as a stray char.
      ((= byte #x3C)
       (make-csi-k params param-accumulator intermed #\< subparams))
      ((= byte #x3D)
       (make-csi-k params param-accumulator intermed #\= subparams))
      ;; Intermediate bytes (SPACE through 0x2F): record as intermed.
      ;; SPACE (#x20) is the most common (used by DECSCUSR "CSI N SP q");
      ;; $ (#x24) appears in DECRQM.  Does NOT disturb the private marker.
      ((and (>= byte +csi-intermed-low+) (<= byte +csi-intermed-high+))
       (make-csi-k params param-accumulator (code-char byte) private subparams))
      ;; Final byte (0x40-0x7E): flush accumulator, reverse collected params, dispatch.
      ((and (>= byte +csi-final-low+) (<= byte +csi-final-high+))
       (let ((all-params (nreverse (if (or param-accumulator subparams)
                                       (cons (%finish-param param-accumulator subparams)
                                             params)
                                       params))))
         (execute-csi screen (code-char byte) intermed private all-params))
       #'ground-state)
      ;; Anything else: abort CSI (e.g. C0 controls inside a sequence).
      (t #'ground-state))))

(defun make-utf8-k (utf8-acc continuation-bytes-remaining)
  "Return a continuation that collects UTF-8 continuation bytes.
   UTF8-ACC is the accumulator built from the lead byte.
   CONTINUATION-BYTES-REMAINING is the count of continuation bytes still needed.
   On the final continuation byte the assembled code point is written to screen."
  (declare (type fixnum utf8-acc continuation-bytes-remaining))
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (if (utf8-continuation-p byte)
        (let ((new-acc      (logior (ash utf8-acc 6) (logand byte #x3F)))
              (bytes-left   (1- continuation-bytes-remaining)))
          (if (zerop bytes-left)
              (progn (write-codepoint screen new-acc)
                     #'ground-state)
              (make-utf8-k new-acc bytes-left)))
        ;; Malformed: emit U+FFFD, re-process this byte in ground state
        (progn
          (write-codepoint screen #xFFFD)
          (ground-state screen byte)))))

;;; ── Named (non-parameterized) state functions ──────────────────────────────
;;;
;;; Each define-state call reads like Prolog clauses:
;;;   ground-state(Screen, 0x1B) :- next_state(escape_state).
;;;   ground-state(Screen, 0x0D) :- cursor_cr(Screen), next_state(ground_state).

(define-state ground-state (screen byte)
  ;; ── Escape and control ──────────────────────────────────────────────────
  (#x1B  #'escape-state)
  (#x0D  (cursor-cr screen) #'ground-state)
  (#x0A  (cursor-lf screen) #'ground-state)
  (#x0B  (cursor-lf screen) #'ground-state)        ; VT treated as LF
  (#x0C  (cursor-lf screen) #'ground-state)        ; FF treated as LF
  (#x08  (cursor-bs screen) #'ground-state)
  (#x09  (cursor-ht screen) #'ground-state)
  (#x07  (set-bell-pending screen)
         #'ground-state)                           ; BEL — set pending flag
  (#x7F  #'ground-state)                           ; DEL — ignore
  (#x0E  (invoke-charset screen :g1) #'ground-state) ; SO — invoke G1 (locking shift out)
  (#x0F  (invoke-charset screen :g0) #'ground-state) ; SI — invoke G0 (locking shift in)
  ;; ── Printable ASCII ─────────────────────────────────────────────────────
  (printable-ascii-p
   (write-char-at-cursor screen (code-char byte))
   #'ground-state)
  ;; ── Multi-byte UTF-8 ────────────────────────────────────────────────────
  (utf8-lead-p
   (multiple-value-bind (utf8-acc continuation-bytes-remaining) (utf8-lead-decode byte)
     (make-utf8-k utf8-acc continuation-bytes-remaining)))
  ;; ── Invalid / stray continuation byte ───────────────────────────────────
  ((>= byte #x80)
   (write-codepoint screen #xFFFD)
   #'ground-state)
  ;; ── Unhandled C0 control byte ────────────────────────────────────────────
  (t #'ground-state))

(define-state escape-state (screen byte)
  ;; ── Standard ESC sequences ───────────────────────────────────────────────
  (#x5B  (make-csi-k '() nil nil))                ; ESC [ → CSI
  (#x5D  #'osc-state)                              ; ESC ] → OSC
  (#x50  (make-dcs-k))                             ; ESC P → DCS (tmux passthrough or discard)
  (#x4D  (cursor-ri screen)    #'ground-state)    ; ESC M → RI
  (#x63  (ris-action screen)   #'ground-state)    ; ESC c → RIS
  (#x37  (save-cursor screen)    #'ground-state)  ; ESC 7 → DECSC
  (#x38  (restore-cursor screen) #'ground-state)  ; ESC 8 → DECRC
  (#x44  (cursor-lf  screen)     #'ground-state)  ; ESC D → IND (index: down, no CR)
  (#x45  (cursor-nel screen)     #'ground-state)  ; ESC E → NEL (next line: CR+LF)
  (#x48  (set-tab-stop screen)   #'ground-state)  ; ESC H → HTS (set tab stop)
  ;; ── Charset designators: ESC ( designates G0, ESC ) designates G1 ──────────
  (#x28  (make-charset-designator-k :g0))          ; ESC ( → designate G0
  (#x29  (make-charset-designator-k :g1))          ; ESC ) → designate G1
  (#x2A  (make-ignore-designator-k))               ; ESC * → designate G2 (not modeled)
  (#x2B  (make-ignore-designator-k))               ; ESC + → designate G3 (not modeled)
  ;; ESC # — DEC line-size / alignment: the next byte selects (8 = DECALN fill,
  ;; 3/4/5/6 = double-height/width/single-width line attrs, accepted+ignored).
  ;; Without this, ESC # aborted and the selector byte printed as a stray char.
  (#x23  (make-hash-line-size-k))                  ; ESC # → DEC line-size selector
  ;; ── All unrecognized ESC sequences → ground (including DECKPAM #x3D, DECKPNM #x3E)
  (t     #'ground-state))

;;; OSC payload accumulator: captures bytes into a buffer, then dispatches
;;; on BEL (#x07) or ST (ESC \) termination.
;;;
;;; The implementation lives in parser-osc.lisp (loaded after this file):
;;;   *osc52-handler* — clipboard callback variable
;;;   %dispatch-osc   — payload parser and side-effect applier
;;;   make-osc-st-k   — bridge continuation waiting for ESC \ backslash
;;;   make-osc-k      — accumulator continuation for OSC payload bytes

(define-state osc-state (screen byte)
  ;; OSC payload: start accumulating into a fresh buffer.
  ;; Bare BEL and bare ESC with empty payload are handled as no-ops.
  (#x07  #'ground-state)                           ; bare BEL with empty payload
  (#x1B  #'osc-st-state)                           ; possible ST = ESC \ with empty payload
  (t     (let ((buf (make-array 64
                                :element-type '(unsigned-byte 8)
                                :fill-pointer 0
                                :adjustable t)))
           (vector-push-extend byte buf)
           (make-osc-k buf))))

;;; osc-st-state is an internal state used to await the backslash of ESC \
;;; (String Terminator) with an empty OSC payload.  It is not exported because
;;; it is an implementation detail of the OSC accumulator state machine.
;;; Contrast with ground-state and escape-state which are exported since callers
;;; may need to reset or inspect the parser's initial state.
(define-state osc-st-state (screen byte)
  (#x5C  #'ground-state)                           ; \ → ST confirmed (empty payload)
  (t     #'osc-state))

;;; DCS (Device Control String) accumulator.
;;; ESC P introduces a DCS; collect bytes until ESC \ (ST).
;;;
;;; The tmux passthrough sequence is \ePtmux;<payload>\e\\ where every ESC in
;;; the inner <payload> is DOUBLED (\e\e).  When the payload begins with the
;;; bytes "tmux;", we accumulate the rest, un-double the ESCs, and push the
;;; inner sequence onto the screen's passthrough-queue for the renderer to emit
;;; to the OUTER terminal (tmux-in-tmux, iTerm2/kitty inline images).  Any other
;;; DCS (e.g. Sixel) is consumed and discarded as before.
;;;
;;; make-dcs-st-k is the bridge state waiting for the backslash of ESC \ after
;;; an ESC byte seen inside a DCS payload.  This is symmetric with make-osc-st-k.

(defconstant +dcs-max-payload+ 1048576
  "Maximum DCS passthrough payload bytes buffered (1 MiB).  Beyond this the
   payload is truncated — a safety bound against a runaway/malformed stream.")

(defun %dcs-tmux-prefix-p (buffer)
  "T when BUFFER begins with the ASCII bytes for \"tmux;\" (the passthrough tag)."
  (and (>= (fill-pointer buffer) 5)
       (= (aref buffer 0) 116)   ; t
       (= (aref buffer 1) 109)   ; m
       (= (aref buffer 2) 117)   ; u
       (= (aref buffer 3) 120)   ; x
       (= (aref buffer 4) 59)))  ; ;

(defun %dcs-xtgettcap-prefix-p (buffer)
  "T when BUFFER begins with \"+q\" — an XTGETTCAP terminfo-capability request."
  (and (>= (fill-pointer buffer) 2)
       (= (aref buffer 0) 43)     ; +
       (= (aref buffer 1) 113)))  ; q

(defun %hex-decode-string (hex)
  "Decode an even-length hex string to its ASCII characters, or NIL if malformed.
   XTGETTCAP encodes capability names in hex (\"Tc\" → \"5463\")."
  (when (and (plusp (length hex)) (evenp (length hex)))
    (ignore-errors
      (with-output-to-string (out)
        (loop for i from 0 below (length hex) by 2
              do (write-char (code-char (parse-integer hex :start i :end (+ i 2)
                                                        :radix 16))
                             out))))))

(defun %hex-encode-string (string)
  "Hex-encode STRING's characters as lowercase hex (for XTGETTCAP reply values)."
  (with-output-to-string (out)
    (loop for ch across string do (format out "~(~2,'0X~)" (char-code ch)))))

(defun %xtgettcap-value (capname)
  "The XTGETTCAP answer for terminfo capability CAPNAME:
   :BOOLEAN for a present boolean cap, a string for a numeric/string cap, or NIL
   when unknown.  cl-tmux renders 24-bit colour, so it advertises Tc and RGB
   (true-colour) and colors=256 — letting apps that probe via XTGETTCAP enable
   true-colour output."
  (cond
    ((string= capname "Tc")     :boolean)   ; tmux/xterm true-colour flag
    ((string= capname "RGB")    :boolean)   ; direct-colour flag
    ((string= capname "colors") "256")
    (t nil)))

(defun %dcs-split-fields (string)
  "Split STRING on ';' into fields (empty fields preserved)."
  (loop with start = 0
        for pos = (position #\; string :start start)
        collect (subseq string start (or pos (length string)))
        while pos do (setf start (1+ pos))))

(defun %xtgettcap-reply-1 (hex-name)
  "Build one XTGETTCAP DCS reply for the requested HEX-NAME (echoed verbatim):
   known cap → ESC P 1 + r <hexname>[=<hexvalue>] ST; unknown → ESC P 0 + r <hexname> ST."
  (let* ((name (%hex-decode-string hex-name))
         (val  (and name (%xtgettcap-value name))))
    (cond
      ((null val)        (format nil "~CP0+r~A~C\\" #\Escape hex-name #\Escape))
      ((eq val :boolean) (format nil "~CP1+r~A~C\\" #\Escape hex-name #\Escape))
      (t                 (format nil "~CP1+r~A=~A~C\\" #\Escape hex-name
                                 (%hex-encode-string val) #\Escape)))))

(defun %handle-xtgettcap (screen request)
  "Handle an XTGETTCAP request (the payload after \"+q\"): a ';'-separated list of
   hex-encoded capability names.  Enqueue one DCS reply per requested cap onto
   SCREEN's response-queue (drained to the PTY like DA1/DSR)."
  (dolist (hex-name (%dcs-split-fields request))
    (when (plusp (length hex-name))
      (push (%xtgettcap-reply-1 hex-name) (screen-response-queue screen)))))

(defun %dcs-decrqss-prefix-p (buffer)
  "T when BUFFER begins with \"$q\" — a DECRQSS (request status string) query."
  (and (>= (fill-pointer buffer) 2)
       (= (aref buffer 0) 36)     ; $
       (= (aref buffer 1) 113)))  ; q

(defun %decrqss-reply (screen request)
  "Build the DECRQSS reply for REQUEST (the setting queried, after \"$q\").
   Valid → ESC P 1 $ r <value><request> ST; unsupported → ESC P 0 $ r ST.
   Supported settings:
     m    → current SGR pen        (ESC P 1 $ r <params> m ST)
     r    → DECSTBM scroll region  (1-based top;bottom)
     SP q → DECSCUSR cursor style  (the shape number)"
  (cond
    ((string= request "m")
     (format nil "~CP1$r~Am~C\\" #\Escape
             (cl-tmux/terminal/sgr:%pen-to-sgr-params
              (screen-cur-fg screen) (screen-cur-bg screen)
              (screen-cur-attrs screen) (screen-cur-attrs2 screen))
             #\Escape))
    ((string= request "r")
     (format nil "~CP1$r~D;~Dr~C\\" #\Escape
             (1+ (screen-scroll-top screen)) (1+ (screen-scroll-bottom screen))
             #\Escape))
    ((string= request " q")
     (format nil "~CP1$r~D q~C\\" #\Escape (screen-cursor-shape screen) #\Escape))
    (t (format nil "~CP0$r~C\\" #\Escape #\Escape))))

(defun %finish-dcs (screen buffer)
  "Process a completed DCS payload in BUFFER (ESCs already un-doubled).
   - tmux passthrough (\"tmux;<inner>\") → push <inner> onto the passthrough-queue.
   - XTGETTCAP (\"+q<hexcaps>\")         → enqueue capability replies (Tc/RGB/colors).
   - DECRQSS (\"$q<setting>\")           → enqueue a status-string reply (SGR/region/cursor).
   - anything else (e.g. Sixel)          → discard."
  (cond
    ((%dcs-tmux-prefix-p buffer)
     (push (map 'string #'code-char (subseq buffer 5))
           (screen-passthrough-queue screen)))
    ((%dcs-xtgettcap-prefix-p buffer)
     (%handle-xtgettcap screen (map 'string #'code-char (subseq buffer 2))))
    ((%dcs-decrqss-prefix-p buffer)
     (push (%decrqss-reply screen (map 'string #'code-char (subseq buffer 2)))
           (screen-response-queue screen)))))

(defun %dcs-accumulate (buffer byte)
  "Append BYTE to BUFFER unless the payload cap is reached (truncate silently)."
  (when (< (fill-pointer buffer) +dcs-max-payload+)
    (vector-push-extend byte buffer)))

(defun make-dcs-st-k (buffer)
  "Bridge state after an ESC inside a DCS payload (BUFFER accumulated so far).
   On backslash: ST confirmed — finish the DCS and return to ground.
   On ESC: a doubled ESC (\\e\\e) — append ONE literal ESC and keep accumulating.
   On any other byte: lenient — append ESC then re-dispatch the byte."
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (cond
      ((= byte #x5C)               ; backslash = ST confirmed
       (%finish-dcs screen buffer)
       #'ground-state)
      ((= byte #x1B)               ; doubled ESC → one literal ESC in payload
       (%dcs-accumulate buffer #x1B)
       (make-dcs-k buffer))
      (t                           ; malformed: keep the ESC, re-process byte
       (%dcs-accumulate buffer #x1B)
       (funcall (make-dcs-k buffer) screen byte)))))

(defun make-dcs-k (&optional buffer)
  "Return a continuation that accumulates DCS payload bytes into BUFFER until
   ST (ESC \\).  Allocates a fresh adjustable buffer when none is supplied.
   On ESC (#x1B): transition to make-dcs-st-k to await the backslash.
   On all other bytes: accumulate (capped) and continue."
  (let ((buf (or buffer (make-array 64 :element-type '(unsigned-byte 8)
                                       :fill-pointer 0 :adjustable t))))
    (lambda (screen byte)
      (declare (type screen screen) (type (unsigned-byte 8) byte)
               (ignorable screen))
      (if (= byte #x1B)
          ;; Possible ESC \ ST or doubled ESC — hand off to the bridge state.
          (make-dcs-st-k buf)
          ;; Accumulate payload byte (so the tmux; prefix + inner can be parsed).
          (progn (%dcs-accumulate buf byte)
                 (make-dcs-k buf))))))

(defun make-charset-designator-k (g)
  "Return a CPS state that consumes one charset DESIGNATOR byte and designates
   G (:g0 for ESC (, :g1 for ESC )) to the corresponding charset, then returns to
   ground:
     #x30 '0' → DEC special graphics (line-drawing)
     #x42 'B' → US ASCII
     all other designators → ASCII (accepted silently).
   Designating does NOT activate G1 — that requires a SO (0x0E) locking shift."
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (designate-charset screen g (if (= byte #x30) :dec-graphics :ascii))
    #'ground-state))

(defun make-ignore-designator-k ()
  "Return a CPS state that consumes one charset DESIGNATOR byte and returns to
   ground with no effect — for ESC * (designate G2) and ESC + (designate G3),
   which cl-tmux accepts but does not model (only G0/G1 are tracked, via SO/SI).
   Consuming the designator byte avoids it printing as a stray char."
  (lambda (screen byte)
    (declare (ignore screen byte))
    #'ground-state))

(defun make-hash-line-size-k ()
  "Return a CPS state for ESC # — the next byte is a DEC line-size / alignment
   selector:
     #x38 '8' → DECALN: fill the screen with 'E' (the alignment test pattern).
     '3'/'4'  → DECDHL (double-height line top/bottom) — accepted and ignored.
     '5'      → DECSWL (single-width line) — accepted (the default).
     '6'      → DECDWL (double-width line) — accepted and ignored.
   cl-tmux does not model per-line double width/height; the selector is CONSUMED
   either way so it is not printed as a stray char.  Returns to ground."
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (when (= byte #x38)                ; '8' → DECALN
      (decaln-action screen))
    #'ground-state))
