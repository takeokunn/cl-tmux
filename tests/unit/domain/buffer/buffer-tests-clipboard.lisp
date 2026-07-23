(in-package #:cl-tmux/test)

(describe "buffer-suite"

  ;;; ── Base64 / OSC 52 clipboard helpers ────────────────────────────────────────

  ;; %base64-encode produces standard padded Base64.
  (it "base64-encode-known-value"
    (expect (string= "aGVsbG8="
                 (cl-tmux/terminal/parser::%base64-encode
                  (babel:string-to-octets "hello" :encoding :utf-8))))
    (expect (string= "aGk="
                 (cl-tmux/terminal/parser::%base64-encode
                  (babel:string-to-octets "hi" :encoding :utf-8)))))

  ;; %base64-encode then %base64-decode recovers the original bytes.
  (it "base64-encode-decode-round-trip"
    (let* ((bytes (babel:string-to-octets "Round-trip 123! αβγ" :encoding :utf-8))
           (round (cl-tmux/terminal/parser::%base64-decode
                   (cl-tmux/terminal/parser::%base64-encode bytes))))
      (expect (equalp bytes (coerce round '(vector (unsigned-byte 8)))))))

  ;; osc52-clipboard-sequence wraps Base64 text in ESC ] 52 ; c ; ... ESC backslash —
  ;; the outbound sequence that copies a selection to the host system clipboard.
  (it "osc52-clipboard-sequence-format"
    (let ((seq (cl-tmux/terminal/parser:osc52-clipboard-sequence "hi")))
      (expect (char= #\Escape (char seq 0)))
      (expect (search "]52;c;" seq))
      (expect (search "aGk=" seq))
      (expect (and (char= #\Escape (char seq (- (length seq) 2)))
               (char= #\\ (char seq (1- (length seq)))))))))
