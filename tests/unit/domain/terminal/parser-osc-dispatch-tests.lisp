(in-package #:cl-tmux/test)

;;;; parser tests - OSC dispatch edge cases.

(def-suite osc-dispatch-edge-cases
  :description "OSC dispatch edge cases: no-semicolon payload, unknown command"
  :in terminal-suite)
(in-suite osc-dispatch-edge-cases)

(test osc-payload-no-semicolon-is-noop
  "An OSC payload with no semicolon is silently discarded (no command to dispatch)."
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
      (is (or (null title) (string= "" title))
          "screen-title must be unset after invalid OSC payload"))))

(test osc-unknown-command-is-silently-ignored
  "An OSC payload with a valid integer command but no matching rule is silently ignored."
  (with-screen (s 20 5)
    ;; OSC 99 is not handled - must not crash.
    (finishes
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]99;some-data~C" #\Escape (code-char 7))
          :encoding :utf-8)))
    ;; screen-title must remain unset (OSC 99 has no handler).
    (let ((title (cl-tmux/terminal/types:screen-title s)))
      (is (or (null title) (string= "" title))
          "unknown OSC command must not alter screen-title"))))

(test osc-empty-payload-bel-is-noop
  "An OSC terminated immediately by BEL (empty payload) is consumed without error."
  (with-screen (s 20 5)
    (feed s "A")
    ;; ESC ] BEL - empty payload
    (screen-process-bytes s
      (make-array 3 :element-type '(unsigned-byte 8)
                    :initial-contents (list #x1B #x5D #x07)))
    (feed s "B")
    (is (char= #\A (char-at s 0 0)) "char before empty OSC must survive")
    (is (char= #\B (char-at s 1 0)) "char after empty OSC must be written")))
