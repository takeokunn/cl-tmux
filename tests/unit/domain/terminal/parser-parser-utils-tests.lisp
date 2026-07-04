(in-package #:cl-tmux/test)

;;;; parser tests - helper utilities and direct parser edge cases.

;;; ── Coverage gap: make-bytes / feed-osc helpers ──────────────────────────────
;;;
;;; Audit finding: the pattern
;;;   (make-array N :element-type '(unsigned-byte 8) :initial-contents '(...))
;;; is repeated 7+ times in parser-tests.lisp.  Centralise it as make-bytes.
;;; The pattern
;;;   (screen-process-bytes s (babel:string-to-octets (format nil "~C]N;...~C" ...) :encoding :utf-8))
;;; is repeated 10+ times.  Centralise it as feed-osc.

(defun make-bytes (&rest byte-values)
  "Return a simple (unsigned-byte 8) vector containing BYTE-VALUES."
  (make-array (length byte-values)
              :element-type '(unsigned-byte 8)
              :initial-contents byte-values))

(defun feed-osc (screen command-number body-string)
  "Feed an OSC sequence with integer COMMAND-NUMBER and BODY-STRING to SCREEN,
   terminated by BEL (ASCII 7).  Uses UTF-8 encoding to match real terminal behaviour."
  (screen-process-bytes screen
    (babel:string-to-octets
      (format nil "~C]~D;~A~C" #\Escape command-number body-string (code-char 7))
      :encoding :utf-8)))

;;; Verify the helpers function correctly before relying on them in later tests.

(test make-bytes-helper
  "make-bytes returns a (unsigned-byte 8) vector with the given byte values."
  (let ((bytes (make-bytes #x1B #x5D #x07)))
    (is (= 3 (length bytes)) "length must be 3")
    (is (= #x1B (aref bytes 0)) "first byte must be ESC")
    (is (= #x5D (aref bytes 1)) "second byte must be ]")
    (is (= #x07 (aref bytes 2)) "third byte must be BEL")))

(test feed-osc-helper
  "feed-osc sends an OSC sequence that causes the expected side-effect."
  (with-screen (s 20 5)
    (feed-osc s 0 "test-title")
    (is (string= "test-title" (cl-tmux/terminal/types:screen-title s))
        "feed-osc for OSC 0 must set screen-title")))

;;; ── Coverage gap: zero-length buffer in screen-process-bytes ─────────────────
;;;
;;; Audit finding: screen-process-bytes with start=0, end=0 on a zero-length
;;; buffer was not tested.

(def-suite parser-suite
  :description "Parser and emulator coverage gap tests"
  :in terminal-suite)
(in-suite parser-suite)

(test screen-process-bytes-zero-length-buffer-is-noop
  "screen-process-bytes on a zero-length buffer (start=end=0) is a no-op."
  (with-screen (s 10 5)
    (let ((buf (make-array 0 :element-type '(unsigned-byte 8))))
      (screen-process-bytes s buf :start 0 :end 0))
    (is (char= #\Space (char-at s 0 0))
        "zero-length buffer must leave screen unchanged")))

;;; ── Coverage gap: %base64-decode edge cases ──────────────────────────────────
;;;
;;; Audit finding: Base64 padding ('='), truncated input, and invalid characters
;;; were not directly asserted.

(def-suite base64-decode-suite
  :description "Direct coverage of %base64-decode edge cases"
  :in terminal-suite)
(in-suite base64-decode-suite)

(test base64-decode-basic-string
  "%base64-decode decodes a standard Base64 string ('hello' = aGVsbG8=)."
  (let ((result (cl-tmux/terminal/parser::%base64-decode "aGVsbG8=")))
    (is (not (null result)) "must return a byte vector, not NIL")
    (is (string= "hello"
                 (babel:octets-to-string result :encoding :utf-8))
        "aGVsbG8= must decode to 'hello'")))

(test base64-decode-empty-string
  "%base64-decode on an empty string returns an empty byte vector."
  (let ((result (cl-tmux/terminal/parser::%base64-decode "")))
    (is (or (null result) (zerop (length result)))
        "empty input must produce empty output or NIL")))

(test base64-decode-truncated-group
  "%base64-decode on input shorter than 4 chars does not crash."
  (finishes (cl-tmux/terminal/parser::%base64-decode "YQ"))
  ;; 'YQ' decodes to 'a' (no padding); should succeed without error.
  (let ((result (cl-tmux/terminal/parser::%base64-decode "YQ==")))
    (is (not (null result)) "padded 2-char group must decode successfully")))

;;; ── Coverage gap: %parse-osc-command error branch ────────────────────────────
;;;
;;; Audit finding: the error-return branch (non-integer command field) was not
;;; directly asserted.

(test parse-osc-command-returns-nil-for-non-integer
  "%parse-osc-command returns NIL when the command field is not a valid integer."
  (let ((result (cl-tmux/terminal/parser::%parse-osc-command "notanumber" 10)))
    (is (null result)
        "%parse-osc-command must return NIL for a non-integer command field")))

(test parse-osc-command-returns-integer-for-valid-input
  "%parse-osc-command returns the integer for a valid command field."
  (let ((result (cl-tmux/terminal/parser::%parse-osc-command "52;data" 2)))
    (is (= 52 result)
        "%parse-osc-command must return 52 for '52' prefix")))

;;; ── Coverage gap: %handle-osc-52 no-inner-semicolon branch ──────────────────
;;;
;;; Audit finding: the branch where the OSC 52 body has no semicolon was not
;;; directly tested.

(test handle-osc-52-no-inner-semicolon-is-noop
  "%handle-osc-52 is a no-op when the body has no semicolon (malformed OSC 52)."
  (let* ((received :not-called)
         (cl-tmux/terminal/parser:*osc52-handler*
           (lambda (text) (setf received text))))
    (finishes (cl-tmux/terminal/parser::%handle-osc-52 "nodatahere"))
    (is (eq :not-called received)
        "%handle-osc-52 with no semicolon must not invoke the handler")))

;;; ── CSI colon sub-parameters (ISO 8613-6) ───────────────────────────────────
;;;
;;; A colon introduces sub-parameters within one CSI parameter (SGR 4:3 undercurl,
;;; 38:2::R:G:B true-colour).  The parser keeps the leading value and skips the
;;; rest, so such a sequence neither aborts (printing stray bytes) nor mis-applies.

(def-suite csi-colon-subparams :description "CSI colon sub-parameter handling"
  :in parser-suite)
(in-suite csi-colon-subparams)

(test csi-colon-undercurl-keeps-leading-underline
  "CSI 4:3 m (undercurl) keeps the leading 4 -> underline; no stray bytes print."
  (with-screen (s 8 2)
    (feed s (esc "[4:3m"))            ; undercurl via colon sub-parameter
    (feed s "X")
    (is (char= #\X (char-at s 0 0))
        "X must be the first cell - the colon sequence printed nothing")
    (is (logbitp 3 (attrs-at s 0 0))
        "the leading 4 must set the underline attribute (bit 3)")))

(test csi-colon-multi-param-mixed
  "CSI 0;4:3;1 m applies reset, underline (from 4:3), bold - colon does not
   bleed into the neighbouring parameters."
  (with-screen (s 8 2)
    (feed s (esc "[0;4:3;1m"))
    (feed s "Y")
    (is (char= #\Y (char-at s 0 0)) "Y is the first cell")
    (is (logbitp 3 (attrs-at s 0 0)) "underline set (from 4:3)")
    (is (logbitp 0 (attrs-at s 0 0)) "bold set (from the trailing ;1)")))

(test csi-colon-truecolor-form-does-not-abort
  "CSI 38:2::255:0:0 m (colon true-colour) must not abort and spew bytes; the
   following text writes cleanly at column 0."
  (with-screen (s 8 2)
    (feed s (esc "[38:2::255:0:0m"))
    (feed s "Z")
    (is (char= #\Z (char-at s 0 0))
        "Z must be the first cell - no stray sub-parameter bytes printed")))
