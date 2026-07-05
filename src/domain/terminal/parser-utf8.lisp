(in-package #:cl-tmux/terminal/parser)

;;;; UTF-8 parser continuation logic.

(declaim (inline utf8-lead-p utf8-continuation-p))

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

(defun make-utf8-k (code-point-accumulator continuation-bytes-remaining)
  "Return a continuation that collects UTF-8 continuation bytes.
   CODE-POINT-ACCUMULATOR is the accumulator built from the lead byte.
   CONTINUATION-BYTES-REMAINING is the count of continuation bytes still needed.
   On the final continuation byte the assembled code point is written to screen."
  (declare (type fixnum code-point-accumulator continuation-bytes-remaining))
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (if (utf8-continuation-p byte)
        (let ((updated-accumulator (logior (ash code-point-accumulator 6) (logand byte #x3F)))
              (bytes-left          (1- continuation-bytes-remaining)))
          (if (zerop bytes-left)
              (progn (write-codepoint screen updated-accumulator)
                     #'ground-state)
              (make-utf8-k updated-accumulator bytes-left)))
        ;; Malformed: emit U+FFFD, re-process this byte in ground state
        (progn
          (write-codepoint screen #xFFFD)
          (ground-state screen byte)))))
