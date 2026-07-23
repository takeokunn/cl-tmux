(in-package #:cl-tmux/test)

;;;; parser tests - OSC dispatch edge cases.

(describe "terminal-suite/osc-dispatch-edge-cases"

  ;; An OSC payload with no semicolon is silently discarded (no command to dispatch).
  (it "osc-payload-no-semicolon-is-noop"
    (with-screen (s 20 5)
      ;; Feed OSC with no semicolon: just the command number, BEL terminated.
      ;; This should not crash and must not set screen-title.
      (finishes
        (screen-process-bytes s
          (babel:string-to-octets
            (format nil "~C]notanumber~C" #\Escape (code-char 7))
            :encoding :utf-8)))
      ;; screen-title must remain at its default (NIL or empty string).
      (let ((title (cl-tmux/terminal/types:screen-title s)))
        (expect (or (null title) (string= "" title))))))

  ;; An OSC payload with a valid integer command but no matching rule is silently ignored.
  (it "osc-unknown-command-is-silently-ignored"
    (with-screen (s 20 5)
      ;; OSC 99 is not handled - must not crash.
      (finishes
        (screen-process-bytes s
          (babel:string-to-octets
            (format nil "~C]99;some-data~C" #\Escape (code-char 7))
            :encoding :utf-8)))
      ;; screen-title must remain unset (OSC 99 has no handler).
      (let ((title (cl-tmux/terminal/types:screen-title s)))
        (expect (or (null title) (string= "" title))))))

  ;; An OSC terminated immediately by BEL (empty payload) is consumed without error.
  (it "osc-empty-payload-bel-is-noop"
    (with-screen (s 20 5)
      (feed s "A")
      ;; ESC ] BEL - empty payload
      (screen-process-bytes s
        (make-array 3 :element-type '(unsigned-byte 8)
                      :initial-contents (list #x1B #x5D #x07)))
      (feed s "B")
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\B (char-at s 1 0))))))
