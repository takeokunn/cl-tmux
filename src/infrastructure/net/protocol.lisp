(in-package #:cl-tmux/protocol)

;;;; Wire protocol for client/server detach-attach.
;;;;
;;;; A multiplexer server holds the sessions/PTYs; a thin client attaches over a
;;;; Unix socket, forwarding keystrokes and resizes and receiving rendered
;;;; frames.  This file is the pure, transport-agnostic frame codec — no sockets,
;;;; no global state — so it is fully unit-testable.  The socket transport and
;;;; the server/client loops build on top of it.
;;;;
;;;; +msg-command+ payload codec lives in protocol-command.lisp (same package).
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
(defconstant +msg-reply+   8 "server→client: a forwarded command's text output (UTF-8 payload), for the CLI command client (e.g. display-message -p).")

(defconstant +attach-flag-read-only+ 1
  "Bit in the optional +msg-attach+ flags byte that marks the attaching client as
   read-only (attach-session -r).  When set the server suppresses pane input,
   paste, and mouse forwarding for that connection (tmux CLIENT_READONLY).")

(defconstant +header-size+ 5 "1 type byte + 4 length bytes.")

;;; ── Frame layout constants ───────────────────────────────────────────────────

(defconstant +payload-length-offset+ 1
  "Byte offset of the u32-big-endian payload-length field inside a frame header.")

(defconstant +attach-size-bytes+ 4
  "Number of bytes occupied by the rows,cols pair in a +msg-attach+ payload.")

(defconstant +attach-flags-offset+ 4
  "Byte offset of the optional flags byte within a +msg-attach+ payload.")

(defconstant +cols-offset-in-size-payload+ 2
  "Byte offset of the cols u16 within a rows,cols size payload.")

;;; ── Octet helpers (data) ────────────────────────────────────────────────────
;;;
;;; define-uint-codec is a Prolog-like macro: each spec (encoder-name
;;; decoder-name bits doc) is a fact that generates a paired big-endian encoder
;;; and decoder defun.  The byte-extraction and shift forms are derived
;;; mechanically from the bit-width at macro-expansion time.

(defmacro define-uint-codec (&rest specs)
  "Build paired big-endian integer encoder and decoder functions from a
   declarative table.  Each SPEC is (encoder-name decoder-name bits docstring)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (encoder-name decoder-name bits docstring) spec
            (let ((bytes (/ bits 8)))
              `(progn
                 (defun ,encoder-name (n)
                   ,(format nil "~A — encoder: N (0..~D) as ~D big-endian octet~:P."
                            docstring (1- (expt 2 bits)) bytes)
                   (vector ,@(loop for shift from (- bits 8) downto 0 by 8
                                   collect `(ldb (byte 8 ,shift) n))))
                 (defun ,decoder-name (buffer start)
                   ,(format nil "~A — decoder: big-endian ~D-bit value from BUFFER at START."
                            docstring bits)
                   (logior ,@(loop for i from 0 below bytes
                                   for shift from (- bits 8) downto 0 by 8
                                   collect (if (zerop shift)
                                               `(aref buffer (+ start ,i))
                                               `(ash (aref buffer (+ start ,i)) ,shift)))))))))
        specs)))

(define-uint-codec
  (u16-octets read-u16 16 "Big-endian unsigned 16-bit integer codec")
  (u32-octets read-u32 32 "Big-endian unsigned 32-bit integer codec"))

(defun u16-octets-pair (a b)
  "A,B (each 0..65535) as four big-endian octets (two u16s)."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (u16-octets a) (u16-octets b)))

(defun to-octets (sequence)
  "Coerce SEQUENCE of (unsigned-byte 8) into a simple octet vector."
  (coerce sequence '(simple-array (unsigned-byte 8) (*))))

;;; ── Frame codec (logic) ─────────────────────────────────────────────────────

(defun encode-frame (type payload)
  "Encode one frame of TYPE carrying PAYLOAD into a fresh octet vector:
   [TYPE][LENGTH u32-be][PAYLOAD].  The vector is assembled declaratively
   via CONCATENATE — no mutable setf/replace calls."
  (let* ((payload-length (length payload))
         (length-bytes   (u32-octets payload-length))
         (payload-vector (to-octets payload)))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (vector type) length-bytes payload-vector)))

(defun decode-frame (buffer &optional (start 0) (end (length buffer)))
  "Parse one frame from BUFFER[START..END).
   Returns (values TYPE PAYLOAD NEXT-INDEX) where NEXT-INDEX is the offset just
   past the frame, or (values NIL NIL START) when BUFFER does not yet contain a
   complete frame (header incomplete, or payload not fully arrived)."
  (if (< (- end start) +header-size+)
      (values nil nil start)
      (let* ((type           (aref buffer start))
             (payload-length (read-u32 buffer (+ start +payload-length-offset+)))
             (payload-start  (+ start +header-size+))
             (next           (+ payload-start payload-length)))
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
  (msg-key     +msg-key+     (octets)     (to-octets octets)
   "client→server frame carrying raw input OCTETS for the active pane.")
  (msg-resize  +msg-resize+  (rows cols)  (u16-octets-pair rows cols)
   "client→server frame announcing a new terminal size.")
  (msg-detach  +msg-detach+  ()           #()
   "client→server detach frame.")
  (msg-frame   +msg-frame+   (string)     (babel:string-to-octets string :encoding :utf-8)
   "server→client frame carrying a rendered screen STRING (UTF-8 encoded).")
  (msg-bye     +msg-bye+     ()           #()
   "server→client frame announcing the server is closing.")
  (msg-reply   +msg-reply+   (string)     (babel:string-to-octets string :encoding :utf-8)
   "server→client frame carrying a forwarded command's text output (UTF-8)."))

;;; ── Typed command message constructor ────────────────────────────────────────

(defun msg-attach (rows cols &optional readonly-p)
  "Build a +msg-attach+ frame carrying the initial terminal size.
   Payload is [rows u16][cols u16] and, when READONLY-P is non-NIL, an extra
   trailing flags byte with +attach-flag-read-only+ set (attach-session -r).
   The trailing byte is omitted when READONLY-P is NIL, so the frame stays
   byte-identical to older clients and decode-size (which reads only bytes 0..3)
   is unaffected."
  (encode-frame +msg-attach+
                (if readonly-p
                    (concatenate '(simple-array (unsigned-byte 8) (*))
                                 (u16-octets-pair rows cols)
                                 (vector +attach-flag-read-only+))
                    (u16-octets-pair rows cols))))

(defun decode-attach-flags (payload)
  "Return the optional flags byte from a +msg-attach+ PAYLOAD, or 0 when absent.
   Older clients send a 4-byte rows,cols payload with no flags byte; this returns
   0 for them so callers can treat the read-only bit as off by default."
  (if (>= (length payload) (1+ +attach-flags-offset+))
      (aref payload +attach-flags-offset+)
      0))

(defun msg-command (command-name target args)
  "Build a +msg-command+ frame.
   COMMAND-NAME is a keyword or string.  TARGET is a target string or NIL.
   ARGS is a list of argument strings or NIL."
  (encode-frame +msg-command+
                (encode-command-payload command-name :target target :args args)))

;;; ── Payload decoders (logic) ────────────────────────────────────────────────

(defun decode-size (payload)
  "Decode a rows,cols payload (u16,u16) into (values ROWS COLS)."
  (values (read-u16 payload 0) (read-u16 payload +cols-offset-in-size-payload+)))

(defun decode-text (payload)
  "Decode a UTF-8 frame PAYLOAD into a string."
  (babel:octets-to-string (to-octets payload) :encoding :utf-8))
