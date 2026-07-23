(in-package #:cl-tmux/test)

;;;; Runtime message log policy primitives

(describe "runtime-suite"

  ;; %message-log-limit returns the message-limit option value when set.
  (it "message-log-limit-returns-option-when-set"
    (with-isolated-options ("message-limit" 42)
      (expect (= 42 (cl-tmux::%message-log-limit)))))

  ;; %message-log-limit falls back to +max-message-log-entries+ when option is unset.
  ;; with-fresh-options alone is not enough here: message-limit is a KNOWN tmux
  ;; option (registered with a table default of 1000 in *known-option-registry*),
  ;; so get-option still resolves it even with an empty *option-registry* (mirrors
  ;; set-option -u semantics).  Clearing *known-option-registry* too makes the
  ;; option genuinely unknown, exercising %message-log-limit's OR fallback.
  (it "message-log-limit-returns-default-when-unset"
    (with-fresh-options
      (let ((cl-tmux/options::*known-option-registry* (make-hash-table :test #'equal)))
        (expect (= cl-tmux::+max-message-log-entries+
                   (cl-tmux::%message-log-limit))))))

  ;; %append-message-log-entry prepends the entry to the log.
  (it "append-message-log-entry-prepends"
    (with-isolated-options ("message-limit" 100)
      (let* ((log nil)
             (entry (cons (get-universal-time) "hello"))
             (result (cl-tmux::%append-message-log-entry log entry)))
        (expect (= 1 (length result)))
        (expect (eq entry (first result))))))

  ;; %append-message-log-entry caps the log at the effective message-limit.
  (it "append-message-log-entry-caps-at-limit"
    (with-isolated-options ("message-limit" 3)
      (let* ((old-log (list (cons 1 "a") (cons 2 "b") (cons 3 "c")))
             (new-entry (cons 4 "d"))
             (result (cl-tmux::%append-message-log-entry old-log new-entry)))
        (expect (= 3 (length result)))
        (expect (eq new-entry (first result)))))))
