(in-package #:cl-tmux/test)

;;;; parser tests - OSC 52 clipboard coverage.

(def-suite osc52-coverage
  :description "OSC 52 clipboard handler: callback path and nil handler (silently dropped)"
  :in terminal-suite)
(in-suite osc52-coverage)

(test osc52-handler-invoked-with-decoded-text
  "When *osc52-handler* is set, OSC 52 with a valid Base64 payload invokes it
   with the decoded text string."
  (with-screen (s 20 5)
    ;; Base64-encode \"hello\" -> SGVsbG8=
    (let* ((received nil)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      ;; Base64 of "hello" is aGVsbG8=  (SGVsbG8= would decode to "Hello").
      ;; Feed OSC 52 ; c ; aGVsbG8= BEL  (c = clipboard target, ignored)
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]52;c;aGVsbG8=~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (is (string= "hello" received)
          "osc52-handler must be called with decoded text 'hello'"))))

(test osc52-nil-handler-silently-dropped
  "When *osc52-handler* is NIL, an OSC 52 sequence is consumed without error."
  (with-screen (s 20 5)
    (let ((cl-tmux/terminal/parser:*osc52-handler* nil))
      (finishes
        (screen-process-bytes s
          (babel:string-to-octets
            (format nil "~C]52;c;SGVsbG8=~C" #\Escape (code-char 7))
            :encoding :utf-8))))))

(test osc52-read-request-silently-ignored
  "OSC 52 with payload '?' (clipboard read request) is silently ignored."
  (with-screen (s 20 5)
    (let* ((received :not-called)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]52;c;?~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (is (eq :not-called received)
          "handler must NOT be invoked for a clipboard read request ('?')"))))

;;; ── Coverage gap: osc52-clipboard-sequence (outbound OSC 52 builder) ────────
;;;
;;; osc52-clipboard-sequence is exported from cl-tmux/terminal/parser but was
;;; previously exercised only indirectly (its counterpart, inbound OSC 52
;;; decoding via %handle-osc-52, is covered by the tests above).  This is the
;;; OUTBOUND direction: build the escape sequence cl-tmux writes to the real
;;; terminal to copy TEXT onto the host clipboard.

(test osc52-clipboard-sequence-round-trips-through-base64-decode
  "osc52-clipboard-sequence builds ESC ] 52 ; c ; <base64> ESC \\, and the
   embedded Base64 payload decodes back to the original UTF-8 text."
  (let* ((text   "hello, cl-tmux!")
         (seq    (cl-tmux/terminal/parser:osc52-clipboard-sequence text))
         (prefix (format nil "~C]52;c;" #\Escape)))
    (is (string= prefix (subseq seq 0 (length prefix)))
        "sequence must start with ESC ] 52 ; c ;")
    (is (string= (format nil "~C\\" #\Escape) (subseq seq (- (length seq) 2)))
        "sequence must end with the ST terminator ESC \\")
    (let* ((payload (subseq seq (length prefix) (- (length seq) 2)))
           (decoded (babel:octets-to-string
                     (cl-tmux/terminal/parser::%base64-decode payload)
                     :encoding :utf-8)))
      (is (string= text decoded)
          "the embedded Base64 payload must decode back to the original text"))))
