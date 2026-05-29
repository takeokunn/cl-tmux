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

(defconstant +msg-attach+ 1 "client→server: attach; payload = rows,cols (u16,u16)")
(defconstant +msg-key+    2 "client→server: raw input bytes for the active pane")
(defconstant +msg-resize+ 3 "client→server: terminal resized; payload = rows,cols")
(defconstant +msg-detach+ 4 "client→server: detach (empty payload)")
(defconstant +msg-frame+  5 "server→client: a rendered frame (UTF-8 payload)")
(defconstant +msg-bye+    6 "server→client: server is closing (empty payload)")

(defconstant +header-size+ 5 "1 type byte + 4 length bytes.")

;;; ── Octet helpers (data) ────────────────────────────────────────────────────

(defun make-octets (n)
  "A fresh zero-filled octet vector of length N."
  (make-array n :element-type '(unsigned-byte 8)))

(defun u16-octets (n)
  "N (0..65535) as two big-endian octets."
  (vector (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun u32-octets (n)
  "N (0..2^32-1) as four big-endian octets."
  (vector (ldb (byte 8 24) n) (ldb (byte 8 16) n)
          (ldb (byte 8 8) n)  (ldb (byte 8 0) n)))

(defun u16-octets-pair (a b)
  "A,B (each 0..65535) as four big-endian octets (two u16s)."
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (u16-octets a) (u16-octets b)))

(defun read-u16 (buffer start)
  "Decode a big-endian u16 from BUFFER at START."
  (logior (ash (aref buffer start) 8) (aref buffer (1+ start))))

(defun read-u32 (buffer start)
  "Decode a big-endian u32 from BUFFER at START."
  (logior (ash (aref buffer start)        24)
          (ash (aref buffer (+ start 1))  16)
          (ash (aref buffer (+ start 2))   8)
          (aref buffer (+ start 3))))

(defun to-octets (sequence)
  "Coerce SEQUENCE of (unsigned-byte 8) into a simple octet vector."
  (coerce sequence '(simple-array (unsigned-byte 8) (*))))

;;; ── Frame codec (logic) ─────────────────────────────────────────────────────

(defun encode-frame (type payload)
  "Encode one frame of TYPE carrying PAYLOAD (a sequence of octets) into a fresh
   octet vector: [TYPE][LENGTH u32-be][PAYLOAD]."
  (let* ((len   (length payload))
         (frame (make-octets (+ +header-size+ len))))
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

;;; ── Typed message constructors (data) ───────────────────────────────────────

(defun msg-attach (rows cols)
  "client→server attach frame carrying the initial terminal size."
  (encode-frame +msg-attach+ (u16-octets-pair rows cols)))

(defun msg-key (octets)
  "client→server frame carrying raw input OCTETS for the active pane."
  (encode-frame +msg-key+ (to-octets octets)))

(defun msg-resize (rows cols)
  "client→server frame announcing a new terminal size."
  (encode-frame +msg-resize+ (u16-octets-pair rows cols)))

(defun msg-detach ()
  "client→server detach frame."
  (encode-frame +msg-detach+ #()))

(defun msg-frame (string)
  "server→client frame carrying a rendered screen STRING (UTF-8 encoded)."
  (encode-frame +msg-frame+ (babel:string-to-octets string :encoding :utf-8)))

(defun msg-bye ()
  "server→client frame announcing the server is closing."
  (encode-frame +msg-bye+ #()))

;;; ── Payload decoders (logic) ────────────────────────────────────────────────

(defun decode-size (payload)
  "Decode a rows,cols payload (u16,u16) into (values ROWS COLS)."
  (values (read-u16 payload 0) (read-u16 payload 2)))

(defun decode-text (payload)
  "Decode a UTF-8 frame PAYLOAD into a string."
  (babel:octets-to-string (to-octets payload) :encoding :utf-8))
