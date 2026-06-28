;;;; OSC helper utilities shared by parser-osc and tests.
;;;;
;;;; This file collects the pure helper layer for OSC payload handling:
;;;;   - Base64 encode/decode for OSC 52 clipboard payloads
;;;;   - Hex parsing shared by OSC colour helpers
;;;;   - Percent decoding and OSC 7 path extraction

(in-package #:cl-tmux/terminal/parser)

;;; OSC 52 clipboard callback.  Set by the higher-level code (buffer.lisp or
;;; main) after the terminal module is loaded.  NIL means clipboard writes are
;;; silently dropped (safe default for unit tests that don't need the buffer).
(defvar *osc52-handler* nil
  "A function of one argument (text string) called when OSC 52 clipboard data
   is received.  Install cl-tmux/buffer:add-paste-buffer here at startup.")

(defun %alphabet-index (alphabet char)
  "Return the zero-based index of CHAR in ALPHABET, or NIL if absent."
  (position char alphabet :test #'char=))

(defun %b64-byte0 (index-a index-b)
  (logior (ash index-a 2)
          (ldb (byte 2 4) index-b)))

(defun %b64-byte1 (index-b index-c)
  (logior (ash (ldb (byte 4 0) index-b) 4)
          (ldb (byte 4 2) index-c)))

(defun %b64-byte2 (index-c index-d)
  (logior (ash (ldb (byte 2 0) index-c) 6)
          index-d))

(defun %decode-base64-group (alphabet encoded-string group-start)
  "Decode one 4-character Base64 group starting at GROUP-START in ENCODED-STRING.
   Returns (values byte0-or-nil byte1-or-nil byte2-or-nil)."
  (let* ((index-a (%alphabet-index alphabet (char encoded-string group-start)))
         (index-b (%alphabet-index alphabet (char encoded-string (1+ group-start))))
         (index-c (%alphabet-index alphabet (char encoded-string (+ group-start 2))))
         (index-d (%alphabet-index alphabet (char encoded-string (+ group-start 3)))))
    (when (and index-a index-b)
      (values (%b64-byte0 index-a index-b)
              (when index-c (%b64-byte1 index-b index-c))
              (when index-d (%b64-byte2 index-c index-d))))))

(defun %base64-decode (encoded-string)
  "Decode Base64-encoded ENCODED-STRING into a byte vector.
   Processes input in groups of 4 Base64 characters; each group produces
   up to 3 bytes.  Returns NIL when the input contains non-Base64 characters."
  (handler-case
      (let* ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
             (input-length (length encoded-string))
             (output (make-array 0 :element-type '(unsigned-byte 8)
                                   :fill-pointer 0 :adjustable t)))
        (when (= (mod input-length 4) 0)
          (loop for group-start from 0 below input-length by 4
                do (multiple-value-bind (byte0 byte1 byte2)
                       (%decode-base64-group alphabet encoded-string group-start)
                     (when byte0 (vector-push-extend byte0 output))
                     (when byte1 (vector-push-extend byte1 output))
                     (when byte2 (vector-push-extend byte2 output))))
          output))
    (error () nil)))

(defun %base64-encode (bytes)
  "Encode a sequence of (unsigned-byte 8) BYTES to a padded Base64 string.
   Inverse of %base64-decode; used to build outbound OSC 52 clipboard sequences."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (n (length bytes)))
    (with-output-to-string (out)
      (loop for i from 0 below n by 3
            for b0 = (aref bytes i)
            for b1 = (and (< (1+ i) n) (aref bytes (1+ i)))
            for b2 = (and (< (+ i 2) n) (aref bytes (+ i 2)))
            do (let* ((x (ash b0 -2))
                      (y (logior (ash (ldb (byte 2 0) b0) 4)
                                 (if b1 (ash b1 -4) 0)))
                      (z (if b1
                             (logior (ash (ldb (byte 4 0) b1) 2)
                                     (if b2 (ash b2 -6) 0))
                             64))
                      (w (if b2 (ldb (byte 6 0) b2) 64)))
                 (write-char (char alphabet x) out)
                 (write-char (char alphabet y) out)
                 (write-char (if (< (1+ i) n) (char alphabet z) #\=) out)
                 (write-char (if (< (+ i 2) n) (char alphabet w) #\=) out))))))

(defun %hex-digit-16 (char)
  "Return the numeric value of a hexadecimal digit CHAR, or NIL if invalid."
  (digit-char-p char 16))

(defun osc52-clipboard-sequence (text)
  "Build the OSC 52 set-clipboard escape sequence (ESC ] 52 ; c ; <base64> ST)
   that copies TEXT to the host system clipboard when written to the OUTER
   terminal.  TEXT is UTF-8 encoded before Base64 encoding.  ST is the 7-bit
   string terminator ESC \\."
  (format nil "~C]52;c;~A~C\\"
          #\Escape
          (%base64-encode (babel:string-to-octets text :encoding :utf-8))
          #\Escape))

;;; OSC 0 and 2 set the window title (semantically identical for our purposes).
;;; OSC 52 delivers clipboard data; the Base64 payload is decoded and forwarded
;;; to *osc52-handler* when one has been installed.

;;; define-osc-rules builds a declarative dispatch table analogous to
;;; define-csi-rules and define-sgr-rules.  Each RULE is:
;;;   (command-or-list body...)
;;; where command-or-list may be a single integer or a list of integers.

(defmacro define-osc-rules (&rest rules)
  "Build %DISPATCH-OSC-COMMAND from a declarative OSC command table.
   Each RULE is (command-designator &body forms) where command-designator
   is either an integer command number or a list of command numbers sharing
   the same body.  The special variables SCREEN and BODY are available in the
   rule body.  BODY is the raw OSC payload body (the text after the semicolon in
   the OSC payload).
   Unknown command numbers are silently ignored."
  `(defun %dispatch-osc-command (screen command body)
     (declare (type screen screen) (ignorable body))
     (cond
       ,@(loop for rule in rules
               collect
               (destructuring-bind (command-designator &body body-forms) rule
                 (let ((commands (if (listp command-designator)
                                     command-designator
                                     (list command-designator))))
                   `((member command ',commands)
                     (progn ,@body-forms)))))
       (t nil))))

;;;
;;; OSC 7 embeds the current working directory in a file:// URL; we only care
;;; about the path component.

(defun %flush-utf8-octets (octets out)
  "Write accumulated UTF-8 OCTETS to the string stream OUT and reset OCTETS.
   Attempts babel:octets-to-string with :utf-8; on error, falls back to
   per-byte code-char (Latin-1-like) so no data is silently dropped."
  (when (> (length octets) 0)
    (write-string
     (or (handler-case
             (babel:octets-to-string octets :encoding :utf-8)
           (error () nil))
         (coerce (loop for i below (length octets)
                       collect (code-char (aref octets i)))
                 'string))
     out)
    (setf (fill-pointer octets) 0)))

(defun %percent-decode (encoded-string)
  "Decode %XX percent-escapes in ENCODED-STRING, UTF-8 aware: %20 -> space, %E2%9C%93 -> checkmark.
   A '%' not followed by two hex digits is left literal.  No-op when ENCODED-STRING
   contains no escapes.  Decoded octets are flushed as UTF-8 runs via %FLUSH-UTF8-OCTETS."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                              :fill-pointer 0 :adjustable t))
        (len (length encoded-string)))
    (with-output-to-string (out)
      (loop with i = 0
            while (< i len)
            for ch = (char encoded-string i)
            do (cond
                 ((and (char= ch #\%)
                       (<= (+ i 2) (1- len)))
                  (let ((hi (%hex-digit-16 (char encoded-string (1+ i))))
                        (lo (%hex-digit-16 (char encoded-string (+ i 2)))))
                    (if (and hi lo)
                        (progn
                          (vector-push-extend (+ (* hi 16) lo) octets)
                          (incf i 3))
                        (progn
                          (%flush-utf8-octets octets out)
                          (write-char ch out)
                          (incf i)))))
                 (t
                  (%flush-utf8-octets octets out)
                  (write-char ch out)
                  (incf i))))
      (%flush-utf8-octets octets out))))

(defun %handle-osc-8 (screen body)
  "Handle OSC 8 hyperlink state.  BODY is PARAMS;URI, where URI may be empty to
   clear the active hyperlink.  PARAMS are currently ignored."
  (let ((uri-start (position #\; body)))
    (when uri-start
      (let ((uri (subseq body (1+ uri-start))))
        (setf (screen-current-hyperlink screen)
              (and (> (length uri) 0) uri))))))

(defun %osc7-path (body)
  "Extract the filesystem path from an OSC 7 'file://host/path' URL (the form a
   shell uses to report its cwd) and percent-decode it.
   \"file://host/home/u\" → \"/home/u\"; \"file:///My%20Docs\" → \"/My Docs\".
   Returns BODY unchanged when it is not a file:// URL."
  (let ((prefix "file://"))
    (if (and (>= (length body) (length prefix))
             (string= body prefix :end1 (length prefix) :end2 (length prefix)))
        (let* ((after-scheme (subseq body (length prefix)))   ; "host/path" or "/path"
               (slash        (position #\/ after-scheme)))
          (if slash (%percent-decode (subseq after-scheme slash)) "/"))
        body)))

;;;; OSC colour helpers shared by parser-osc and tests.
;;;
;;; +osc-default-fg+ and +osc-default-bg+ are defconstant in cell.lisp (data layer)
;;; exported from cl-tmux/terminal/types. Access them via the types package.

(defun %scale-hex-channel (channel)
  "Scale a 4-bit or 8-bit hex channel to 8-bit integer."
  (if (< channel 16)
      (* channel 17)
      channel))

(defun %parse-hash-color (hex)
  "Parse a #RGB or #RRGGBB hex string (without the leading #) to 0xRRGGBB, or NIL."
  (case (length hex)
    (6 (cl-tmux::%parse-integer-or-nil hex :radix 16))
    (3 (let ((r (cl-tmux::%parse-integer-or-nil (subseq hex 0 1) :radix 16))
             (g (cl-tmux::%parse-integer-or-nil (subseq hex 1 2) :radix 16))
             (b (cl-tmux::%parse-integer-or-nil (subseq hex 2 3) :radix 16)))
         (when (and r g b)
           (logior (ash (%scale-hex-channel r) 16)
                   (ash (%scale-hex-channel g) 8)
                   (%scale-hex-channel b)))))   ; 0xF → 0xFF
    (otherwise nil)))

(defun %parse-rgb-color (spec)
  "Parse rgb:R/G/B where each channel is 1-4 hex digits.  Channels are scaled to
   8-bit by taking the leading significant hex digits."
  (let* ((parts (cl-ppcre:split "/" spec))
         (valid (= (length parts) 3)))
    (when valid
      (let ((channels
             (mapcar (lambda (s)
                       (and (> (length s) 0)
                            (<= (length s) 4)
                            (cl-tmux::%parse-integer-or-nil s :radix 16)))
                     parts)))
        (when (every #'integerp channels)
          (destructuring-bind (r g b) channels
            (labels ((scale (value digits)
                       (case digits
                         (1 (%scale-hex-channel value))
                         (2 value)
                         (3 (ldb (byte 8 4) value))
                         (4 (ldb (byte 8 8) value)))))
              (let ((r (scale r (length (first parts))))
                    (g (scale g (length (second parts))))
                    (b (scale b (length (third parts)))))
                (logior (ash r 16) (ash g 8) b)))))))))

(defun %parse-osc-color (spec)
  "Parse an X11/xterm colour SPEC to a 24-bit 0xRRGGBB integer, or NIL when it is
   not a recognised form.  Handles #RGB, #RRGGBB and rgb:R/G/B (1-4 digits per
   channel)."
  (when (and (stringp spec) (> (length spec) 0))
    (cond
      ((char= (char spec 0) #\#)
       (%parse-hash-color (subseq spec 1)))
      ((and (>= (length spec) 4)
            (string-equal (subseq spec 0 4) "rgb:"))
       (%parse-rgb-color (subseq spec 4)))
      (t nil))))

(defun %osc-hex-channel (byte)
  "Format an 8-bit BYTE as a four-digit uppercase hex channel for xterm OSC colour replies.
   Multiplies by #x101 (= 257), which is the unique linear scale factor that maps
   0x00 -> 0x0000 and 0xFF -> 0xFFFF: e.g. #x80 -> #x8080, #xFF -> #xFFFF."
  (format nil "~(~4,'0X~)" (* byte #x101)))

(defun %osc-rgb-components (rgb)
  "Return the 8-bit R, G and B components of RGB (0xRRGGBB)."
  (values (ldb (byte 8 16) rgb)
          (ldb (byte 8 8) rgb)
          (ldb (byte 8 0) rgb)))

(defun %osc-rgb-reply (prefix rgb)
  "Build an OSC reply with PREFIX followed by xterm-style rgb:RRRR/GGGG/BBBB data.
   PREFIX should include the leading ']' and the trailing 'rgb:' marker, e.g.
   \"]11;rgb:\" or \"]4;196;rgb:\"."
  (multiple-value-bind (r g b) (%osc-rgb-components rgb)
    (format nil "~C~A~A/~A/~A~C\\"
            #\Escape prefix
            (%osc-hex-channel r)
            (%osc-hex-channel g)
            (%osc-hex-channel b)
            #\Escape)))

(defun %osc-color-reply (command rgb)
  "Build the OSC reply reporting RGB (0xRRGGBB) for an OSC COMMAND query:
   ESC ] <command> ; rgb:RRRR/GGGG/BBBB ST.  Each 8-bit channel is doubled to the
   16-bit hex form expected by xterm-style replies."
  (%osc-rgb-reply (format nil "]~D;rgb:" command) rgb))

(defun %osc-color-command (screen command body current-rgb set-fn)
  "Handle an OSC 10/11 colour command.  BODY \"?\" → enqueue a reply reporting
   CURRENT-RGB onto SCREEN's response-queue; otherwise parse BODY as a colour and
   apply it via SET-FN.  An unparseable colour is ignored (default left unchanged)."
  (if (string= body "?")
      (push (%osc-color-reply command current-rgb)
            (screen-response-queue screen))
      (let ((rgb (%parse-osc-color body)))
        (when rgb (funcall set-fn rgb)))))

(defparameter +xterm-base16+
  #(#x000000 #x800000 #x008000 #x808000 #x000080 #x800080 #x008080 #xC0C0C0
    #x808080 #xFF0000 #x00FF00 #xFFFF00 #x0000FF #xFF00FF #x00FFFF #xFFFFFF))

(defun %xterm-palette-rgb (index)
  "Return the RGB colour for xterm palette INDEX as 0xRRGGBB, or NIL when INDEX
   is outside the standard 0..255 palette range."
  (cond
    ((and (<= 0 index) (< index 16))
     (aref +xterm-base16+ index))
    ((and (<= 16 index) (< index 232))
     (let* ((i (- index 16))
            (r (floor i 36))
            (g (floor (mod i 36) 6))
            (b (mod i 6))
            (levels #(0 95 135 175 215 255)))
       (logior (ash (aref levels r) 16)
               (ash (aref levels g) 8)
               (aref levels b))))
    ((and (<= 232 index) (<= index 255))
     (let ((gray (+ 8 (* (- index 232) 10))))
       (logior (ash gray 16) (ash gray 8) gray)))
    (t nil)))

(defun %osc4-reply (index rgb)
  "Build the OSC 4 palette reply for INDEX with RGB value 0xRRGGBB.
   Returns a string of the form ESC ] 4 ; INDEX ; rgb:RRRR/GGGG/BBBB ST."
  (%osc-rgb-reply (format nil "]4;~D;rgb:" index) rgb))

(defun %osc-split-fields (body)
  "Split OSC 4 BODY into a list of non-empty fields separated by ';'.
   Trailing empty fields are discarded so that '1;?;' and '1;?' are treated
   equivalently."
  (loop with start = 0
        for pos = (position #\; body :start start)
        collect (subseq body start pos)
        while pos
        do (setf start (1+ pos))))

(defun %palette-effective-rgb (screen index)
  "Return the effective 0xRRGGBB colour for palette INDEX: the custom OSC 4 override
   when one is set, otherwise the built-in xterm palette entry (NIL when INDEX is
   outside 0..255)."
  (or (%palette-override-get screen index)
      (%xterm-palette-rgb index)))

(defun %handle-osc-4 (screen body)
  "Handle OSC 4 (set/query palette colours).  BODY is a ';'-separated run of
   INDEX ; SPEC pairs.  For each pair whose SPEC is \"?\", enqueue a reply
   reporting the effective palette entry (custom override if set, else the built-in
   xterm colour).  Otherwise SPEC is parsed as a colour and stored as a custom
   override for INDEX; an unparseable SPEC is ignored."
  (let ((fields (%osc-split-fields body)))
    (loop for (index-spec spec) on fields by #'cddr
          while spec
          for index = (cl-tmux::%parse-integer-or-nil index-spec :junk-allowed t)
          when index
            do (if (string= spec "?")
                   (let ((rgb (%palette-effective-rgb screen index)))
                     (when rgb
                       (push (%osc4-reply index rgb) (screen-response-queue screen))))
                   (let ((rgb (%parse-osc-color spec)))
                     (when rgb
                       (%palette-override-set screen index rgb)))))))

(defun %handle-osc-104 (screen body)
  "Handle OSC 104 (reset palette colours).  An empty BODY resets every custom
   palette override back to the built-in xterm palette; otherwise BODY is a
   ';'-separated list of indices, each of which is reverted individually."
  (let ((fields (%osc-split-fields body)))
    (if (or (null fields)
            (and (= (length fields) 1) (string= (first fields) "")))
        (%palette-override-clear-all screen)
        (dolist (index-spec fields)
          (let ((index (cl-tmux::%parse-integer-or-nil index-spec :junk-allowed t)))
            (when index
              (%palette-override-clear screen index)))))))
