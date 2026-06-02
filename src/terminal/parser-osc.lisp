(in-package #:cl-tmux/terminal/parser)

;;;; OSC (Operating System Command) accumulator and dispatcher.
;;;;
;;;; This file contains the standalone OSC concern extracted from parser.lisp:
;;;;   - %base64-decode  (private Base64 decoder for OSC 52 clipboard payloads)
;;;;   - %dispatch-osc   (parse the OSC payload and apply side effects to SCREEN)
;;;;   - make-osc-st-k   (CPS continuation waiting for ESC \ ST)
;;;;   - make-osc-k      (CPS accumulator for OSC payload bytes)
;;;;
;;;; Loaded after parser.lisp's state-machine core so that osc-state and
;;;; osc-st-state (defined there) can call make-osc-k and make-osc-st-k.

;;; OSC 52 clipboard callback.  Set by the higher-level code (buffer.lisp or
;;; main) after the terminal module is loaded.  NIL means clipboard writes are
;;; silently dropped (safe default for unit tests that don't need the buffer).
(defvar *osc52-handler* nil
  "A function of one argument (text string) called when OSC 52 clipboard data
   is received.  Install cl-tmux/buffer:add-paste-buffer here at startup.")

;;; ── Base64 decoder (private) ────────────────────────────────────────────────
;;;
;;; Used for OSC 52 clipboard payloads.  We use a simple table-driven approach
;;; rather than depending on an external Base64 library.

(defun %base64-char-index (alphabet ch)
  "Return the 6-bit index of character CH in the Base64 ALPHABET, or NIL."
  (position ch alphabet))

(defun %decode-base64-group (alphabet encoded-string group-start)
  "Decode one 4-character Base64 group starting at GROUP-START in ENCODED-STRING.
   Returns (values byte0-or-nil byte1-or-nil byte2-or-nil).
   A NIL value means the corresponding output byte is absent (padding or truncation)."
  (let* ((input-length (length encoded-string))
         (index0 (%base64-char-index alphabet (char encoded-string group-start)))
         (index1 (and (< (+ group-start 1) input-length)
                      (%base64-char-index alphabet (char encoded-string (+ group-start 1)))))
         (index2 (and (< (+ group-start 2) input-length)
                      (%base64-char-index alphabet (char encoded-string (+ group-start 2)))))
         (index3 (and (< (+ group-start 3) input-length)
                      (%base64-char-index alphabet (char encoded-string (+ group-start 3)))))
         ;; byte0: high 6 bits of index0 | high 2 bits of index1
         (byte0  (and index0 index1
                      (logior (ash index0 2) (ash index1 -4))))
         ;; byte1: low 4 bits of index1 | high 4 bits of index2
         (byte1  (and index1 index2
                      (logand #xFF (logior (ash (logand index1 #xF) 4)
                                           (ash index2 -2)))))
         ;; byte2: low 2 bits of index2 | all 6 bits of index3
         (byte2  (and index2 index3
                      (logand #xFF (logior (ash (logand index2 #x3) 6)
                                           index3)))))
    (values byte0 byte1 byte2)))

(defun %base64-decode (encoded-string)
  "Decode Base64-encoded ENCODED-STRING into a byte vector.
   Processes input in groups of 4 Base64 characters; each group produces
   up to 3 output bytes.  Returns NIL if any decoding error is encountered.

   The output vector is grown with VECTOR-PUSH-EXTEND so padding characters
   ('=') at the end of the string simply produce no output bytes (NIL indices
   stop the WHEN guards)."
  (let* ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (input-length (length encoded-string))
         (output (make-array (ceiling (* 3 input-length) 4)
                             :element-type '(unsigned-byte 8)
                             :fill-pointer 0
                             :adjustable   t)))
    (handler-case
        (loop for group-start from 0 below input-length by 4
              do (multiple-value-bind (byte0 byte1 byte2)
                     (%decode-base64-group alphabet encoded-string group-start)
                   (when byte0 (vector-push-extend byte0 output))
                   (when byte1 (vector-push-extend byte1 output))
                   (when byte2 (vector-push-extend byte2 output)))
              finally (return output))
      (error () nil))))

;;; ── OSC payload dispatcher ──────────────────────────────────────────────────
;;;
;;; The OSC payload has the form:  Ps ; text
;;; where Ps is a small integer command number and text is the payload body.
;;; OSC 0/2 set the window title; OSC 52 writes clipboard data.
;;;
;;; define-osc-rules builds a declarative dispatch table analogous to
;;; define-csi-rules and define-sgr-rules.  Each RULE is:
;;;   (command-or-list body...)
;;; where command-or-list may be a single integer or a list of integers.

(defmacro define-osc-rules (&rest rules)
  "Build %DISPATCH-OSC-COMMAND from a declarative OSC command table.
   Each RULE is (command-designator &body forms) where command-designator
   is an integer or list of integers.  Available bindings: SCREEN and BODY
   (the text after the semicolon in the OSC payload).
   Unknown command numbers are silently ignored."
  `(defun %dispatch-osc-command (screen command body)
     (declare (type screen screen) (ignorable body))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (command-designator &rest forms) rule
              `(,(if (listp command-designator)
                     `(member command ',command-designator)
                     `(eql command ,command-designator))
                ,@forms)))
          rules)
       (t (values)))))

;;; ── OSC command rule table ───────────────────────────────────────────────────
;;;
;;; OSC 0 and 2 set the window title (semantically identical for our purposes).
;;; OSC 52 delivers clipboard data; the Base64 payload is decoded and forwarded
;;; to *osc52-handler* when one has been installed.

(define-osc-rules
  ;; OSC 0 / OSC 2: set window title
  ((0 2)
   (set-screen-title screen body))

  ;; OSC 52: clipboard write — handled by the dedicated helper
  (52
   (%handle-osc-52 body)))

;;; ── OSC payload utilities ────────────────────────────────────────────────────

(defun %parse-osc-command (payload semicolon-position)
  "Parse the OSC command integer from PAYLOAD up to SEMICOLON-POSITION.
   Returns the integer, or NIL if the command field is not a valid integer."
  (handler-case
      (parse-integer (subseq payload 0 semicolon-position))
    (error () nil)))

(defun %handle-osc-52 (text)
  "Handle OSC 52 clipboard write: decode Base64 payload and call *osc52-handler*.
   Format: Pc ; Pd  where Pc is the clipboard target and Pd is Base64-encoded data
   or '?' for a read request (read requests are silently ignored)."
  (let* ((inner-semi   (position #\; text))
         (payload-data (and inner-semi (subseq text (1+ inner-semi)))))
    (when (and payload-data (not (string= payload-data "?")))
      (let* ((decoded-bytes (and payload-data (%base64-decode payload-data)))
             (decoded-text  (and decoded-bytes
                                 (handler-case
                                     (babel:octets-to-string decoded-bytes :encoding :utf-8)
                                   (error () nil)))))
        (when (and decoded-text *osc52-handler*)
          (funcall *osc52-handler* decoded-text))))))

(defun %dispatch-osc (screen payload-buffer)
  "Parse accumulated OSC payload PAYLOAD-BUFFER and apply side effects to SCREEN.
   Handles:
     OSC 0 and OSC 2 — set the window title
     OSC 52          — write clipboard data (Base64-encoded)"
  (let* ((payload  (babel:octets-to-string payload-buffer :encoding :utf-8 :errorp nil))
         (semi-pos (position #\; payload)))
    (when semi-pos
      (let ((command (%parse-osc-command payload semi-pos))
            (body    (subseq payload (1+ semi-pos))))
        (%dispatch-osc-command screen command body)))))

;;; ── CPS OSC accumulator continuations ──────────────────────────────────────
;;;
;;; make-osc-k builds the accumulator closure that collects raw OSC payload
;;; bytes.  make-osc-st-k builds the single-byte "waiting for backslash" bridge
;;; state used after an ESC inside an OSC sequence (potential ESC \ ST).
;;;
;;; Both continuations receive (screen byte) and return the next state function,
;;; matching the CPS state machine contract defined by define-state.

(defun make-osc-st-k (buffer)
  "Return a continuation waiting for the backslash of ESC \\ (String Terminator).
   BUFFER is the accumulated OSC payload so far.
   On backslash: dispatch the payload and return ground-state.
   On any other byte: return ground-state without dispatching (malformed ST)."
  (lambda (screen-arg byte)
    (declare (type screen screen-arg) (type (unsigned-byte 8) byte))
    (when (= byte #x5C)
      (%dispatch-osc screen-arg buffer))
    #'ground-state))

(defun make-osc-k (buffer)
  "Return a continuation that accumulates OSC payload bytes into BUFFER.
   Dispatches to %DISPATCH-OSC on BEL (#x07) or the start of ESC \\ termination."
  (lambda (screen-arg byte)
    (declare (type screen screen-arg) (type (unsigned-byte 8) byte))
    (cond
      ((= byte #x07)
       ;; BEL: OSC terminated — dispatch and return to ground.
       (%dispatch-osc screen-arg buffer)
       #'ground-state)
      ((= byte #x1B)
       ;; Possible ESC \ (ST) — hand off to the bridge state.
       (make-osc-st-k buffer))
      (t
       ;; Continue accumulating payload bytes.
       (vector-push-extend byte buffer)
       (make-osc-k buffer)))))
