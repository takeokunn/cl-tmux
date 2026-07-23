(in-package #:cl-tmux/test)

;;;; Protocol command payload target and field ordering tests.

(describe "protocol-suite"

  ;;; ── target-field-p edge cases ────────────────────────────────────────────────

  ;; target-field-p recognizes sigil characters ($, :, .) as targets; plain names/numbers are not.
  (it "target-field-p-table"
    (dolist (c '(("$"                        t   "bare '$' is a target")
                 (":"                        t   "bare ':' is a target")
                 ("."                        t   "bare '.' is a target")
                 ("0"                        nil "plain integer is not a target")
                 ("copy-mode-search-forward" nil "hyphenated command name is not a target")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (if expected
            (expect (cl-tmux/protocol:target-field-p input) :to-be-truthy)
            (expect (cl-tmux/protocol:target-field-p input) :to-be-falsy)))))

  ;;; ── encode-command-payload ordering ─────────────────────────────────────────

  ;; encode-command-payload without a target produces a payload whose first
  ;; NUL-terminated field is the command name (not a target).
  (it "encode-command-payload-without-target-starts-with-command-name"
    (let* ((payload (encode-command-payload :list-sessions))
           (fields  (cl-tmux/protocol:split-on-nul-bytes payload)))
      (expect (equal '("list-sessions") fields))))

  ;; encode-command-payload with a target prepends the target before the command name.
  (it "encode-command-payload-with-target-places-target-first"
    (let* ((payload (encode-command-payload :send-keys :target "$0:1.0"))
           (fields  (cl-tmux/protocol:split-on-nul-bytes payload)))
      (expect (equal '("$0:1.0" "send-keys") fields))))

  ;; encode-command-payload with args appends each arg after the command name.
  (it "encode-command-payload-with-args-appends-args-after-command"
    (let* ((payload (encode-command-payload :send-keys :args '("C-c" "q")))
           (fields  (cl-tmux/protocol:split-on-nul-bytes payload)))
      (expect (equal '("send-keys" "C-c" "q") fields)))))
