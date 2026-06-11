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

(defun %decode-base64-group (alphabet encoded-string group-start)
  "Decode one 4-character Base64 group starting at GROUP-START in ENCODED-STRING.
   Returns (values byte0-or-nil byte1-or-nil byte2-or-nil).
   A NIL value means the corresponding output byte is absent (padding or truncation).

   RFC 4648 bit layout for a group (a b c d) → three bytes:
     byte0 = (a << 2) | (b >> 4)
     byte1 = ((b & 0xF) << 4) | (c >> 2)
     byte2 = ((c & 0x3) << 6) | d"
  (flet (;; Resolve one alphabet-indexed character, returning NIL for out-of-range.
         (alphabet-index (offset)
           (and (< (+ group-start offset) (length encoded-string))
                (position (char encoded-string (+ group-start offset)) alphabet)))
         ;; Assemble the first output byte from Base64 indices a and b.
         (b64-byte0 (index-a index-b)
           (and index-a index-b
                (logior (ash index-a 2) (ash index-b -4))))
         ;; Assemble the second output byte from Base64 indices b and c.
         (b64-byte1 (index-b index-c)
           (and index-b index-c
                (logand #xFF (logior (ash (logand index-b #xF) 4)
                                     (ash index-c -2)))))
         ;; Assemble the third output byte from Base64 indices c and d.
         (b64-byte2 (index-c index-d)
           (and index-c index-d
                (logand #xFF (logior (ash (logand index-c #x3) 6)
                                     index-d)))))
    (let* ((index-a (alphabet-index 0))
           (index-b (alphabet-index 1))
           (index-c (alphabet-index 2))
           (index-d (alphabet-index 3)))
      (values (b64-byte0 index-a index-b)
              (b64-byte1 index-b index-c)
              (b64-byte2 index-c index-d)))))

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

(defun %base64-encode (bytes)
  "Encode a sequence of (unsigned-byte 8) BYTES to a padded Base64 string.
   Inverse of %base64-decode; used to build outbound OSC 52 clipboard sequences."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (n (length bytes)))
    (with-output-to-string (out)
      (loop for i from 0 below n by 3
            do (let* ((b0 (aref bytes i))
                      (b1 (if (< (+ i 1) n) (aref bytes (+ i 1)) 0))
                      (b2 (if (< (+ i 2) n) (aref bytes (+ i 2)) 0))
                      (triple (logior (ash b0 16) (ash b1 8) b2)))
                 (write-char (char alphabet (ldb (byte 6 18) triple)) out)
                 (write-char (char alphabet (ldb (byte 6 12) triple)) out)
                 (write-char (if (< (+ i 1) n)
                                 (char alphabet (ldb (byte 6 6) triple)) #\=)
                             out)
                 (write-char (if (< (+ i 2) n)
                                 (char alphabet (ldb (byte 6 0) triple)) #\=)
                             out))))))

(defun osc52-clipboard-sequence (text)
  "Build the OSC 52 set-clipboard escape sequence (ESC ] 52 ; c ; <base64> ST)
   that copies TEXT to the host system clipboard when written to the OUTER
   terminal.  TEXT is UTF-8 encoded before Base64 encoding.  ST is the 7-bit
   string terminator ESC \\."
  (format nil "~C]52;c;~A~C\\"
          #\Escape
          (%base64-encode (babel:string-to-octets text :encoding :utf-8))
          #\Escape))

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

(defun %percent-decode (encoded-string)
  "Decode %XX percent-escapes in ENCODED-STRING, UTF-8 aware: %20 → space, %E2%9C%93 → ✓.
   A '%' not followed by two hex digits is left literal.  No-op when ENCODED-STRING
   has no '%' (the common case — avoids the byte round-trip)."
  (if (not (find #\% encoded-string))
      encoded-string
      (let ((bytes '())
            (index 0)
            (input-length (length encoded-string)))
        (flet ((hex-digit (char) (digit-char-p char 16)))
          (loop while (< index input-length) do
            (let ((current-char (char encoded-string index)))
              (if (and (char= current-char #\%)
                       (< (+ index 2) input-length)
                       (hex-digit (char encoded-string (1+ index)))
                       (hex-digit (char encoded-string (+ index 2))))
                  (progn
                    (push (+ (* 16 (hex-digit (char encoded-string (1+ index))))
                             (hex-digit (char encoded-string (+ index 2))))
                          bytes)
                    (incf index 3))
                  (progn
                    (loop for byte across (babel:string-to-octets (string current-char)
                                                                  :encoding :utf-8)
                          do (push byte bytes))
                    (incf index))))))
        (babel:octets-to-string (coerce (nreverse bytes) '(vector (unsigned-byte 8)))
                                :encoding :utf-8 :errorp nil))))

(defun %osc7-path (body)
  "Extract the filesystem path from an OSC 7 'file://host/path' URL (the form a
   shell uses to report its cwd) and percent-decode it.
   \"file://host/home/u\" → \"/home/u\"; \"file:///My%20Docs\" → \"/My Docs\".
   Returns BODY unchanged when it is not a file:// URL."
  (let ((prefix "file://"))
    (if (and (>= (length body) (length prefix))
             (string= (subseq body 0 (length prefix)) prefix))
        (let* ((after-scheme (subseq body (length prefix)))   ; "host/path" or "/path"
               (slash        (position #\/ after-scheme)))
          (if slash (%percent-decode (subseq after-scheme slash)) "/"))
        body)))

;;; ── OSC 10/11 dynamic colours (default fg/bg) ────────────────────────────────
;;;
;;; OSC 10 (foreground) and OSC 11 (background) let an application QUERY the
;;; terminal's default colours (body "?") — commonly to detect a light vs dark
;;; theme — or SET them (body a colour spec).  OSC 110/111 reset them.  cl-tmux is
;;; the terminal from the inner app's view, so it must answer queries itself
;;; (real tmux relays to the outer terminal); it reports the stored default,
;;; white-on-black until an app changes it.  Replies go on the response-queue and
;;; are written back to the PTY by the reader loop, exactly like the DA1/DSR path.

(defconstant +osc-default-fg+ #xFFFFFF
  "Default OSC 10 foreground (white); the OSC 110 reset target.")
(defconstant +osc-default-bg+ #x000000
  "Default OSC 11 background (black); the OSC 111 reset target.")

(defun %scale-hex-channel (digits)
  "Scale a 1-4 hex-digit colour channel string to 0-255 (xterm rgb: semantics:
   normalise by the field's maximum 16^n-1).  NIL on a bad / out-of-range field."
  (let ((n (length digits)))
    (when (<= 1 n 4)
      (let ((value (ignore-errors (parse-integer digits :radix 16))))
        (when value
          (round (* value 255) (1- (expt 16 n))))))))

(defun %parse-hash-color (hex)
  "Parse a #RGB or #RRGGBB hex string (without the leading #) to 0xRRGGBB, or NIL."
  (flet ((hx (s) (ignore-errors (parse-integer s :radix 16))))
    (case (length hex)
      (6 (hx hex))
      (3 (let ((r (hx (subseq hex 0 1))) (g (hx (subseq hex 1 2))) (b (hx (subseq hex 2 3))))
           (when (and r g b)
             (logior (ash (* r 17) 16) (ash (* g 17) 8) (* b 17)))))   ; 0xF → 0xFF
      (t nil))))

(defun %parse-rgb-color (channels)
  "Parse an xterm 'R/G/B' channel string (each 1-4 hex digits) to 0xRRGGBB, or NIL."
  (let ((slash1 (position #\/ channels)))
    (when slash1
      (let ((slash2 (position #\/ channels :start (1+ slash1))))
        (when slash2
          (let ((r (%scale-hex-channel (subseq channels 0 slash1)))
                (g (%scale-hex-channel (subseq channels (1+ slash1) slash2)))
                (b (%scale-hex-channel (subseq channels (1+ slash2)))))
            (when (and r g b)
              (logior (ash r 16) (ash g 8) b))))))))

(defun %parse-osc-color (spec)
  "Parse an X11/xterm colour SPEC to a 24-bit 0xRRGGBB integer, or NIL when it is
   not a recognised form.  Handles #RGB, #RRGGBB and rgb:R/G/B (1-4 digits per
   channel).  Named colours and other forms are not recognised."
  (cond
    ((and (plusp (length spec)) (char= (char spec 0) #\#))
     (%parse-hash-color (subseq spec 1)))
    ((and (>= (length spec) 4) (string= (subseq spec 0 4) "rgb:"))
     (%parse-rgb-color (subseq spec 4)))
    (t nil)))

(defun %osc-hex-channel (byte)
  "Format BYTE (0–255) as a 4-char lowercase hex string at the xterm 16-bit channel
   scale (0xFF → \"ffff\") used in OSC colour reports."
  (format nil "~(~4,'0X~)" (* byte #x101)))

(defun %osc-color-reply (command rgb)
  "Build the OSC reply reporting RGB (0xRRGGBB) for an OSC COMMAND query:
   ESC ] <command> ; rgb:RRRR/GGGG/BBBB ST.  Each 8-bit channel is doubled to the
   16-bit form xterm uses (0xFF → ffff), matching what apps expect to parse back."
  (let ((r (ldb (byte 8 16) rgb)) (g (ldb (byte 8 8) rgb)) (b (ldb (byte 8 0) rgb)))
    (format nil "~C]~D;rgb:~A/~A/~A~C\\"
            #\Escape command (%osc-hex-channel r) (%osc-hex-channel g) (%osc-hex-channel b) #\Escape)))

(defun %osc-color-command (screen command body current-rgb set-fn)
  "Handle an OSC 10/11 colour command.  BODY \"?\" → enqueue a reply reporting
   CURRENT-RGB onto SCREEN's response-queue; otherwise parse BODY as a colour and
   apply it via SET-FN.  An unparseable colour is ignored (default left unchanged)."
  (if (string= body "?")
      (push (%osc-color-reply command current-rgb)
            (screen-response-queue screen))
      (let ((rgb (%parse-osc-color body)))
        (when rgb (funcall set-fn rgb)))))

;;; ── OSC 4 palette colour queries ─────────────────────────────────────────────
;;;
;;; OSC 4 ; N ; ? queries palette entry N; some apps use it to read the terminal's
;;; colours (e.g. to compute contrast).  A multiplexer cannot see the OUTER
;;; terminal's actual palette, so — as with OSC 10/11 — cl-tmux answers with the
;;; conventional xterm 256-colour defaults.  SET (OSC 4 ; N ; colour) is accepted
;;; but not applied: cl-tmux renders the standard palette / passes indices through
;;; to the outer terminal, so it always reports the standard palette (no stored,
;;; unrendered override state).

(defparameter +xterm-base16+
  #(#x000000 #xcd0000 #x00cd00 #xcdcd00 #x0000ee #xcd00cd #x00cdcd #xe5e5e5
    #x7f7f7f #xff0000 #x00ff00 #xffff00 #x5c5cff #xff00ff #x00ffff #xffffff)
  "Standard xterm RGB (0xRRGGBB) for palette indices 0-15.")

(defun %xterm-palette-rgb (n)
  "The standard xterm 256-colour palette RGB (0xRRGGBB) for index N, or NIL when
   N is out of 0-255.  0-15: base colours; 16-231: the 6x6x6 colour cube;
   232-255: the 24-step grayscale ramp.  Used to answer OSC 4 queries."
  (cond
    ((not (<= 0 n 255)) nil)
    ((< n 16) (aref +xterm-base16+ n))
    ((< n 232)
     (let* ((i      (- n 16))
            (levels #(0 95 135 175 215 255))
            (r      (aref levels (floor i 36)))
            (g      (aref levels (mod (floor i 6) 6)))
            (b      (aref levels (mod i 6))))
       (logior (ash r 16) (ash g 8) b)))
    (t (let ((v (+ 8 (* (- n 232) 10))))
         (logior (ash v 16) (ash v 8) v)))))

(defun %osc4-reply (index rgb)
  "Build the OSC 4 colour report: ESC ] 4 ; INDEX ; rgb:RRRR/GGGG/BBBB ST."
  (let ((r (ldb (byte 8 16) rgb)) (g (ldb (byte 8 8) rgb)) (b (ldb (byte 8 0) rgb)))
    (format nil "~C]4;~D;rgb:~A/~A/~A~C\\"
            #\Escape index (%osc-hex-channel r) (%osc-hex-channel g) (%osc-hex-channel b) #\Escape)))

(defun %osc-split-fields (string)
  "Split STRING on ';' into a list of fields (empty fields preserved)."
  (loop with start = 0
        for pos = (position #\; string :start start)
        collect (subseq string start (or pos (length string)))
        while pos
        do (setf start (1+ pos))))

(defun %handle-osc-4 (screen body)
  "Handle OSC 4 (set/query palette colours).  BODY is a ';'-separated run of
   INDEX ; SPEC pairs.  For each pair whose SPEC is \"?\", enqueue a reply
   reporting that index's standard palette colour onto SCREEN's response-queue.
   SET specs are accepted but not applied (see the section comment)."
  (loop for (idx-str spec) on (%osc-split-fields body) by #'cddr
        for index = (and idx-str (ignore-errors (parse-integer idx-str)))
        when (and index spec (string= spec "?"))
          do (let ((rgb (%xterm-palette-rgb index)))
               (when rgb
                 (push (%osc4-reply index rgb) (screen-response-queue screen))))))

(define-osc-rules
  ;; OSC 0 / OSC 1 / OSC 2: set the title.  OSC 0 sets icon + window title, OSC 1
  ;; the icon name, OSC 2 the window title; cl-tmux keeps a single title, so all
  ;; three set it (consistent with the existing 0/2 conflation).
  ((0 1 2)
   (set-screen-title screen body))

  ;; OSC 4: palette colour query (reports the standard xterm palette) / set (ignored)
  (4
   (%handle-osc-4 screen body))

  ;; OSC 7: report current working directory (file://host/path) → #{pane_current_path}
  (7
   (set-screen-cwd screen (%osc7-path body)))

  ;; OSC 8: hyperlink — OSC 8 ; params ; URI.  Set the screen's current hyperlink
  ;; to URI (params, e.g. id=…, are ignored); an empty URI (OSC 8 ; ;) clears it.
  ;; Subsequent cells carry the hyperlink; the renderer re-emits OSC 8 around them.
  (8
   (let* ((semi (position #\; body))
          (uri  (if semi (subseq body (1+ semi)) "")))
     (setf (screen-current-hyperlink screen)
           (and (plusp (length uri)) uri))))

  ;; OSC 10: query/set default foreground colour
  (10
   (%osc-color-command screen 10 body (screen-osc-default-fg screen)
                       (lambda (rgb) (setf (screen-osc-default-fg screen) rgb))))

  ;; OSC 11: query/set default background colour (light/dark theme detection)
  (11
   (%osc-color-command screen 11 body (screen-osc-default-bg screen)
                       (lambda (rgb) (setf (screen-osc-default-bg screen) rgb))))

  ;; OSC 110 / 111: reset foreground / background to the built-in defaults
  (110 (setf (screen-osc-default-fg screen) +osc-default-fg+))
  (111 (setf (screen-osc-default-bg screen) +osc-default-bg+))

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
     OSC 0/1/2        — set the window title
     OSC 7            — report current working directory
     OSC 10/11        — query/set default foreground/background colour
     OSC 110/111      — reset default foreground/background colour
     OSC 52           — write clipboard data (Base64-encoded)
   The command field is the integer before the first ';'; a payload with NO ';'
   (e.g. OSC 110) is a parameterless command with an empty body."
  (let* ((payload  (babel:octets-to-string payload-buffer :encoding :utf-8 :errorp nil))
         (semi-pos (position #\; payload))
         (command  (%parse-osc-command payload (or semi-pos (length payload))))
         (body     (if semi-pos (subseq payload (1+ semi-pos)) "")))
    (when command
      (%dispatch-osc-command screen command body))))

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
