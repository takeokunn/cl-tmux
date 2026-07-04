(in-package #:cl-tmux/test)
(in-suite buffer-suite)

;;; ── Base64 / OSC 52 clipboard helpers ────────────────────────────────────────

(test base64-encode-known-value
  "%base64-encode produces standard padded Base64."
  (is (string= "aGVsbG8="
               (cl-tmux/terminal/parser::%base64-encode
                (babel:string-to-octets "hello" :encoding :utf-8)))
      "base64 of 'hello' must be aGVsbG8=")
  (is (string= "aGk="
               (cl-tmux/terminal/parser::%base64-encode
                (babel:string-to-octets "hi" :encoding :utf-8)))
      "base64 of 'hi' must be aGk= (one pad char)"))

(test base64-encode-decode-round-trip
  "%base64-encode then %base64-decode recovers the original bytes."
  (let* ((bytes (babel:string-to-octets "Round-trip 123! αβγ" :encoding :utf-8))
         (round (cl-tmux/terminal/parser::%base64-decode
                 (cl-tmux/terminal/parser::%base64-encode bytes))))
    (is (equalp bytes (coerce round '(vector (unsigned-byte 8))))
        "encode->decode must be the identity on the original bytes")))

(test osc52-clipboard-sequence-format
  "osc52-clipboard-sequence wraps Base64 text in ESC ] 52 ; c ; ... ESC backslash —
   the outbound sequence that copies a selection to the host system clipboard."
  (let ((seq (cl-tmux/terminal/parser:osc52-clipboard-sequence "hi")))
    (is (char= #\Escape (char seq 0)) "starts with ESC")
    (is (search "]52;c;" seq) "carries the OSC 52 clipboard prefix")
    (is (search "aGk=" seq) "encodes the text as base64")
    (is (and (char= #\Escape (char seq (- (length seq) 2)))
             (char= #\\ (char seq (1- (length seq)))))
        "terminates with ST (ESC backslash)")))
