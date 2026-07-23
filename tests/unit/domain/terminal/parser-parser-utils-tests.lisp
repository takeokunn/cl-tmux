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
;;; FiveAM's *suite* is file-local under ASDF, so these tests need an explicit
;;; suite here — before this file's first in-suite they would otherwise land
;;; in the global suite, which the runner never runs.

(describe "terminal-suite/parser-helper-suite"

  ;; make-bytes returns a (unsigned-byte 8) vector with the given byte values.
  (it "make-bytes-helper"
    (let ((bytes (make-bytes #x1B #x5D #x07)))
      (expect (= 3 (length bytes)))
      (expect (= #x1B (aref bytes 0)))
      (expect (= #x5D (aref bytes 1)))
      (expect (= #x07 (aref bytes 2)))))

  ;; feed-osc sends an OSC sequence that causes the expected side-effect.
  (it "feed-osc-helper"
    (with-screen (s 20 5)
      (feed-osc s 0 "test-title")
      (expect (string= "test-title" (cl-tmux/terminal/types:screen-title s))))))

;;; ── Coverage gap: zero-length buffer in screen-process-bytes ─────────────────
;;;
;;; Audit finding: screen-process-bytes with start=0, end=0 on a zero-length
;;; buffer was not tested.

(describe "terminal-suite/parser-suite"

  ;; screen-process-bytes on a zero-length buffer (start=end=0) is a no-op.
  (it "screen-process-bytes-zero-length-buffer-is-noop"
    (with-screen (s 10 5)
      (let ((buf (make-array 0 :element-type '(unsigned-byte 8))))
        (screen-process-bytes s buf :start 0 :end 0))
      (expect (char= #\Space (char-at s 0 0))))))

;;; ── Coverage gap: %base64-decode edge cases ──────────────────────────────────
;;;
;;; Audit finding: Base64 padding ('='), truncated input, and invalid characters
;;; were not directly asserted.

(describe "terminal-suite/base64-decode-suite"

  ;; %base64-decode decodes a standard Base64 string ('hello' = aGVsbG8=).
  (it "base64-decode-basic-string"
    (let ((result (cl-tmux/terminal/parser::%base64-decode "aGVsbG8=")))
      (expect (not (null result)))
      (expect (string= "hello"
                       (babel:octets-to-string result :encoding :utf-8)))))

  ;; %base64-decode on an empty string returns an empty byte vector.
  (it "base64-decode-empty-string"
    (let ((result (cl-tmux/terminal/parser::%base64-decode "")))
      (expect (or (null result) (zerop (length result))))))

  ;; %base64-decode on input shorter than 4 chars does not crash.
  (it "base64-decode-truncated-group"
    (finishes (cl-tmux/terminal/parser::%base64-decode "YQ"))
    ;; 'YQ' decodes to 'a' (no padding); should succeed without error.
    (let ((result (cl-tmux/terminal/parser::%base64-decode "YQ==")))
      (expect (not (null result)))))

  ;;; ── Coverage gap: %parse-osc-command error branch ────────────────────────────
  ;;;
  ;;; Audit finding: the error-return branch (non-integer command field) was not
  ;;; directly asserted.

  ;; %parse-osc-command returns NIL when the command field is not a valid integer.
  (it "parse-osc-command-returns-nil-for-non-integer"
    (let ((result (cl-tmux/terminal/parser::%parse-osc-command "notanumber" 10)))
      (expect (null result))))

  ;; %parse-osc-command returns the integer for a valid command field.
  (it "parse-osc-command-returns-integer-for-valid-input"
    (let ((result (cl-tmux/terminal/parser::%parse-osc-command "52;data" 2)))
      (expect (= 52 result))))

  ;;; ── Coverage gap: %handle-osc-52 no-inner-semicolon branch ──────────────────
  ;;;
  ;;; Audit finding: the branch where the OSC 52 body has no semicolon was not
  ;;; directly tested.

  ;; %handle-osc-52 is a no-op when the body has no semicolon (malformed OSC 52).
  (it "handle-osc-52-no-inner-semicolon-is-noop"
    (let* ((received :not-called)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      (finishes (cl-tmux/terminal/parser::%handle-osc-52 "nodatahere"))
      (expect (eq :not-called received)))))

;;; ── CSI colon sub-parameters (ISO 8613-6) ───────────────────────────────────
;;;
;;; A colon introduces sub-parameters within one CSI parameter (SGR 4:3 undercurl,
;;; 38:2::R:G:B true-colour).  The parser keeps the leading value and skips the
;;; rest, so such a sequence neither aborts (printing stray bytes) nor mis-applies.

(describe "parser-suite/csi-colon-subparams"

  ;; CSI 4:3 m (undercurl) keeps the leading 4 -> underline; no stray bytes print.
  (it "csi-colon-undercurl-keeps-leading-underline"
    (with-screen (s 8 2)
      (feed s (esc "[4:3m"))            ; undercurl via colon sub-parameter
      (feed s "X")
      (expect (char= #\X (char-at s 0 0)))
      (expect (logbitp 3 (attrs-at s 0 0)))))

  ;; CSI 0;4:3;1 m applies reset, underline (from 4:3), bold - colon does not
  ;; bleed into the neighbouring parameters.
  (it "csi-colon-multi-param-mixed"
    (with-screen (s 8 2)
      (feed s (esc "[0;4:3;1m"))
      (feed s "Y")
      (expect (char= #\Y (char-at s 0 0)))
      (expect (logbitp 3 (attrs-at s 0 0)))
      (expect (logbitp 0 (attrs-at s 0 0)))))

  ;; CSI 38:2::255:0:0 m (colon true-colour) must not abort and spew bytes; the
  ;; following text writes cleanly at column 0.
  (it "csi-colon-truecolor-form-does-not-abort"
    (with-screen (s 8 2)
      (feed s (esc "[38:2::255:0:0m"))
      (feed s "Z")
      (expect (char= #\Z (char-at s 0 0))))))
