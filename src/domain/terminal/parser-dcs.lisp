;;; DCS helper cluster split out of parser.lisp.
;;;
;;; This file must be loaded before parser.lisp because escape-state calls
;;; make-dcs-k, make-charset-designator-k, make-ignore-final-byte-k, and
;;; make-hash-line-size-k.

(in-package #:cl-tmux/terminal/parser)

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

(defun %buffer-prefix-p (buffer &rest expected)
  "T when BUFFER's first (length EXPECTED) bytes match EXPECTED."
  (and (>= (fill-pointer buffer) (length expected))
       (loop for b in expected
             for i from 0
             always (= (aref buffer i) b))))

(defmacro define-buffer-prefix-checkers (&rest specs)
  "Generate buffer prefix predicate functions from a declarative fact table.
   Each SPEC is (fn-name docstring byte...) — the bytes are matched literally."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name doc &rest bytes) spec
                   `(defun ,name (buffer) ,doc (%buffer-prefix-p buffer ,@bytes))))
               specs)))

(define-buffer-prefix-checkers
  (%dcs-tmux-prefix-p
   "T when BUFFER begins with ASCII \"tmux;\" (DCS passthrough tag)."
   116 109 117 120 59)       ; t m u x ;
  (%dcs-xtgettcap-prefix-p
   "T when BUFFER begins with \"+q\" (XTGETTCAP terminfo capability request)."
   43 113)                   ; + q
  (%dcs-decrqss-prefix-p
   "T when BUFFER begins with \"$q\" (DECRQSS request status string query)."
   36 113))                  ; $ q  ; q

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

(defun %dcs-reply (ok-p body)
  "Build a DCS string-terminator reply: ESC P {1=ok/0=err} BODY ST."
  (format nil "~CP~D~A~C\\" #\Escape (if ok-p 1 0) body #\Escape))

(defun %xtgettcap-reply-1 (hex-name)
  "Build one XTGETTCAP DCS reply for the requested HEX-NAME (echoed verbatim):
   known cap → ESC P 1 + r <hexname>[=<hexvalue>] ST; unknown → ESC P 0 + r <hexname> ST."
  (let* ((name (%hex-decode-string hex-name))
         (val  (and name (%xtgettcap-value name))))
    (cond
      ((null val)        (%dcs-reply nil (format nil "+r~A" hex-name)))
      ((eq val :boolean) (%dcs-reply t   (format nil "+r~A" hex-name)))
      (t                 (%dcs-reply t   (format nil "+r~A=~A" hex-name (%hex-encode-string val)))))))

(defun %handle-xtgettcap (screen request)
  "Handle an XTGETTCAP request (the payload after \"+q\"): a ';'-separated list of
   hex-encoded capability names.  Enqueue one DCS reply per requested cap onto
   SCREEN's response-queue (drained to the PTY like DA1/DSR)."
  (dolist (hex-name (%dcs-split-fields request))
    (when (plusp (length hex-name))
      (push (%xtgettcap-reply-1 hex-name) (screen-response-queue screen)))))

(defun %decrqss-reply (screen request)
  "Build the DECRQSS reply for REQUEST (the setting queried, after \"$q\").
   Valid → ESC P 1 $ r <value><request> ST; unsupported → ESC P 0 $ r ST.
   Supported settings:
     m    → current SGR pen        (ESC P 1 $ r <params> m ST)
     r    → DECSTBM scroll region  (1-based top;bottom)
     SP q → DECSCUSR cursor style  (the shape number)"
  (cond
    ((string= request "m")
     (%dcs-reply t (format nil "$r~Am"
                           (cl-tmux/terminal/sgr:%pen-to-sgr-params
                            (screen-cur-fg screen) (screen-cur-bg screen)
                            (screen-cur-attrs screen) (screen-cur-attrs2 screen)))))
    ((string= request "r")
     (%dcs-reply t (format nil "$r~D;~Dr"
                           (1+ (screen-scroll-top screen)) (1+ (screen-scroll-bottom screen)))))
    ((string= request " q")
     (%dcs-reply t (format nil "$r~D q" (screen-cursor-shape screen))))
    (t (%dcs-reply nil "$r"))))

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

(defun make-ignore-final-byte-k ()
  "Return a CPS state that consumes one trailing byte and returns to ground with
   no effect — for two-byte ESC sequences cl-tmux accepts but does not model:
     ESC SP <final>   S7C1T / S8C1T (7/8-bit C1) and ANSI conformance levels
     ESC %  <final>   charset selection (ESC % G = UTF-8, which cl-tmux already is)
   Consuming the trailing byte avoids it printing as a stray char (the bug when
   the introducer was unhandled and the sequence aborted)."
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
