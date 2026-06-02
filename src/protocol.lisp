(in-package #:cl-tmux/protocol)

;;;; Wire protocol for client/server detach-attach.
;;;;
;;;; A multiplexer server holds the sessions/PTYs; a thin client attaches over a
;;;; Unix socket, forwarding keystrokes and resizes and receiving rendered
;;;; frames.  This file is the pure, transport-agnostic codec — no sockets, no
;;;; global state — so it is fully unit-testable.  The socket transport and the
;;;; server/client loops build on top of it.
;;;;
;;;; Each frame on the wire is:
;;;;
;;;;     [TYPE u8] [LENGTH u32 big-endian] [PAYLOAD ... LENGTH bytes]
;;;;
;;;; encode-* return fresh octet vectors; decode-frame parses ONE frame from a
;;;; buffer and reports how many bytes it consumed, or NIL when the buffer does
;;;; not yet hold a complete frame (so a streaming reader can wait for more).

;;; ── Message type tags ───────────────────────────────────────────────────────

(defconstant +msg-attach+  1 "client→server: attach; payload = rows,cols (u16,u16)")
(defconstant +msg-key+     2 "client→server: raw input bytes for the active pane")
(defconstant +msg-resize+  3 "client→server: terminal resized; payload = rows,cols")
(defconstant +msg-detach+  4 "client→server: detach (empty payload)")
(defconstant +msg-frame+   5 "server→client: a rendered frame (UTF-8 payload)")
(defconstant +msg-bye+     6 "server→client: server is closing (empty payload)")
(defconstant +msg-command+ 7 "client→server: a named command with optional -t target; payload = NUL-delimited [target NUL] command-name NUL [args...]")

(defconstant +header-size+ 5 "1 type byte + 4 length bytes.")

;;; ── Octet helpers (data) ────────────────────────────────────────────────────
;;;
;;; define-uint-encoders is a Prolog-like macro: each spec (name bits doc)
;;; is a fact that generates a big-endian encoder defun.  The byte-extraction
;;; forms are derived mechanically from the bit-width at macro-expansion time.

(defmacro define-uint-encoders (&rest specs)
  "Build big-endian integer encoder functions from a declarative table.
   Each SPEC is (name bits docstring).  Generates one DEFUN per entry:
   (name N) → a fresh vector of BITS/8 bytes, most-significant first."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name bits docstring) spec
            `(defun ,name (n)
               ,docstring
               (vector ,@(loop for shift from (- bits 8) downto 0 by 8
                               collect `(ldb (byte 8 ,shift) n))))))
        specs)))

(define-uint-encoders
  (u16-octets 16 "N (0..65535) as two big-endian octets.")
  (u32-octets 32 "N (0..2^32-1) as four big-endian octets."))

(defun u16-octets-pair (a b)
  "A,B (each 0..65535) as four big-endian octets (two u16s)."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (u16-octets a) (u16-octets b)))

;;; ── Big-endian integer decoders (data) ─────────────────────────────────────
;;;
;;; define-uint-decoders is the symmetric counterpart to define-uint-encoders.
;;; Each spec (name bits doc) generates a DEFUN that reads BITS/8 bytes from
;;; BUFFER at START, assembling them most-significant byte first via LOGIOR/ASH.

(defmacro define-uint-decoders (&rest specs)
  "Build big-endian integer decoder functions from a declarative table.
   Each SPEC is (name bits docstring). Generates one DEFUN per entry:
   (name buffer start) → integer decoded from BITS/8 bytes."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name bits docstring) spec
            (let ((bytes (/ bits 8)))
              `(defun ,name (buffer start)
                 ,docstring
                 (logior ,@(loop for i from 0 below bytes
                                 for shift from (- bits 8) downto 0 by 8
                                 collect (if (zerop shift)
                                             `(aref buffer (+ start ,i))
                                             `(ash (aref buffer (+ start ,i)) ,shift))))))))
        specs)))

(define-uint-decoders
  (read-u16 16 "Decode a big-endian u16 from BUFFER at START.")
  (read-u32 32 "Decode a big-endian u32 from BUFFER at START."))

(defun to-octets (sequence)
  "Coerce SEQUENCE of (unsigned-byte 8) into a simple octet vector."
  (coerce sequence '(simple-array (unsigned-byte 8) (*))))

;;; ── Frame codec (logic) ─────────────────────────────────────────────────────

(defun encode-frame (type payload)
  "Encode one frame of TYPE carrying PAYLOAD (a sequence of octets) into a fresh
   octet vector: [TYPE][LENGTH u32-be][PAYLOAD]."
  (let* ((len   (length payload))
         (frame (make-array (+ +header-size+ len) :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) type)
    (replace frame (u32-octets len) :start1 1)
    (replace frame payload :start1 +header-size+)
    frame))

(defun decode-frame (buffer &optional (start 0) (end (length buffer)))
  "Parse one frame from BUFFER[START..END).
   Returns (values TYPE PAYLOAD NEXT-INDEX) where NEXT-INDEX is the offset just
   past the frame, or (values NIL NIL START) when BUFFER does not yet contain a
   complete frame (header incomplete, or payload not fully arrived)."
  (if (< (- end start) +header-size+)
      (values nil nil start)
      (let* ((type    (aref buffer start))
             (len     (read-u32 buffer (1+ start)))
             (payload-start (+ start +header-size+))
             (next    (+ payload-start len)))
        (if (> next end)
            (values nil nil start)                 ; payload not fully arrived
            (values type
                    (subseq buffer payload-start next)
                    next)))))

;;; ── Wire message definition macro ────────────────────────────────────────────

(defmacro define-wire-messages (&rest specs)
  "Build typed frame constructor functions from a declarative table.
   Each SPEC is (name type-constant lambda-list payload-expr docstring).
   Generates one DEFUN per entry: (name lambda-list) → (encode-frame type payload)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name type-const lambda-list payload-expr docstring) spec
            `(defun ,name ,lambda-list
               ,docstring
               (encode-frame ,type-const ,payload-expr))))
        specs)))

;;; ── Typed message constructors (data) ────────────────────────────────────────

(define-wire-messages
  (msg-attach  +msg-attach+  (rows cols)  (u16-octets-pair rows cols)
   "client→server attach frame carrying the initial terminal size.")
  (msg-key     +msg-key+     (octets)     (to-octets octets)
   "client→server frame carrying raw input OCTETS for the active pane.")
  (msg-resize  +msg-resize+  (rows cols)  (u16-octets-pair rows cols)
   "client→server frame announcing a new terminal size.")
  (msg-detach  +msg-detach+  ()           #()
   "client→server detach frame.")
  (msg-frame   +msg-frame+   (string)     (babel:string-to-octets string :encoding :utf-8)
   "server→client frame carrying a rendered screen STRING (UTF-8 encoded).")
  (msg-bye     +msg-bye+     ()           #()
   "server→client frame announcing the server is closing."))

;;; ── Typed command message constructor ────────────────────────────────────────

(defun msg-command (command-name target args)
  "Build a +msg-command+ frame.
   COMMAND-NAME is a keyword or string.  TARGET is a target string or NIL.
   ARGS is a list of argument strings or NIL."
  (encode-frame +msg-command+
                (encode-command-payload command-name :target target :args args)))

;;; ── Payload decoders (logic) ────────────────────────────────────────────────

(defun decode-size (payload)
  "Decode a rows,cols payload (u16,u16) into (values ROWS COLS)."
  (values (read-u16 payload 0) (read-u16 payload 2)))

(defun decode-text (payload)
  "Decode a UTF-8 frame PAYLOAD into a string."
  (babel:octets-to-string (to-octets payload) :encoding :utf-8))

;;; ── +msg-command+ encoder/decoder ──────────────────────────────────────────
;;;
;;; Payload format: NUL-delimited fields.
;;;   [target NUL] command-keyword-name NUL [arg NUL ...]
;;; When target is NIL the target field is omitted entirely.
;;; The command keyword name is encoded without the leading colon.

(defconstant +nul-byte+ 0
  "ASCII NUL byte — the field delimiter in +msg-command+ payloads.")

(defun target-field-p (field)
  "Return true when FIELD looks like a tmux target rather than a command name.
   A field is a target when it starts with '$' (session sigil), contains ':'
   (session:window syntax), or contains '.' (window.pane syntax).
   This predicate is the sole policy point for target detection; keeping it
   separate from the NUL-field-splitting logic ensures that command names
   containing these characters are never misidentified."
  (and (plusp (length field))
       (or (char= (char field 0) #\$)
           (find #\: field)
           (find #\. field))))

(defun command-name-to-string (command-name)
  "Convert COMMAND-NAME (keyword or string) to a lowercase string for wire encoding."
  (if (keywordp command-name)
      (string-downcase (symbol-name command-name))
      command-name))

(defun assemble-command-fields (name-str target args)
  "Build the ordered list of NUL-delimited field strings for a command payload.
   TARGET is prepended when non-NIL; ARGS are appended after NAME-STR."
  (append (when target (list target))
          (list name-str)
          (or args nil)))

(defun encode-fields-to-buffer (field-octets)
  "Pack FIELD-OCTETS (a list of octet vectors) into a fresh buffer.
   Each field is written followed by a NUL byte; the total length equals
   the sum of all field lengths plus one NUL per field."
  (let* ((nul-count  (length field-octets))
         (data-bytes (reduce #'+ field-octets :key #'length :initial-value 0))
         (total-len  (+ data-bytes nul-count))
         (buffer     (make-array total-len :element-type '(unsigned-byte 8)))
         (position   0))
    (dolist (field-bytes field-octets)
      (replace buffer field-bytes :start1 position)
      (incf position (length field-bytes))
      (setf (aref buffer position) +nul-byte+)
      (incf position))
    buffer))

(defun encode-command-payload (command-name &key target args)
  "Encode a command message payload.
   COMMAND-NAME is a keyword or string naming the command.
   TARGET is an optional -t target string (NIL = current session).
   ARGS is an optional list of argument strings.
   Returns a fresh octet vector of NUL-delimited UTF-8 fields."
  (let* ((name-str     (command-name-to-string command-name))
         (field-strings (assemble-command-fields name-str target args))
         (field-octets  (mapcar (lambda (s)
                                  (babel:string-to-octets s :encoding :utf-8))
                                field-strings)))
    (encode-fields-to-buffer field-octets)))

(defun split-on-nul-bytes (octets)
  "Split OCTETS on NUL bytes and return a list of decoded UTF-8 strings."
  (let ((fields nil)
        (start  0))
    (loop for i from 0 below (length octets)
          when (zerop (aref octets i))
          do (push (babel:octets-to-string octets :start start :end i :encoding :utf-8)
                   fields)
             (setf start (1+ i)))
    (nreverse fields)))

(defun decode-command-payload (payload)
  "Decode a +msg-command+ PAYLOAD into (values command-keyword target args).
   COMMAND-KEYWORD is a keyword symbol of the command name.
   TARGET is a string or NIL when absent.
   ARGS is a list of argument strings (may be nil).
   The first NUL-delimited field is examined by TARGET-FIELD-P to determine
   whether it is a target or the command name; all remaining fields are args."
  (let ((fields (split-on-nul-bytes (to-octets payload))))
    (if (and (>= (length fields) 2)
             (target-field-p (first fields)))
        (values (intern (string-upcase (second fields)) :keyword)
                (first fields)
                (cddr fields))
        (values (intern (string-upcase (first fields)) :keyword)
                nil
                (rest fields)))))
