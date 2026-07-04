(in-package #:cl-tmux/test)

;;;; Protocol command payload target and field ordering tests.

(in-suite protocol-suite)

;;; ── target-field-p edge cases ────────────────────────────────────────────────

(test target-field-p-table
  "target-field-p recognizes sigil characters ($, :, .) as targets; plain names/numbers are not."
  (dolist (c '(("$"                        t   "bare '$' is a target")
               (":"                        t   "bare ':' is a target")
               ("."                        t   "bare '.' is a target")
               ("0"                        nil "plain integer is not a target")
               ("copy-mode-search-forward" nil "hyphenated command name is not a target")))
    (destructuring-bind (input expected desc) c
      (if expected
          (is-true  (cl-tmux/protocol:target-field-p input) "~A" desc)
          (is-false (cl-tmux/protocol:target-field-p input) "~A" desc)))))

;;; ── encode-command-payload ordering ─────────────────────────────────────────

(test encode-command-payload-without-target-starts-with-command-name
  "encode-command-payload without a target produces a payload whose first
   NUL-terminated field is the command name (not a target)."
  (let* ((payload (encode-command-payload :list-sessions))
         (fields  (cl-tmux/protocol:split-on-nul-bytes payload)))
    (is (equal '("list-sessions") fields)
        "no-target payload must contain exactly the command name as one field")))

(test encode-command-payload-with-target-places-target-first
  "encode-command-payload with a target prepends the target before the command name."
  (let* ((payload (encode-command-payload :send-keys :target "$0:1.0"))
         (fields  (cl-tmux/protocol:split-on-nul-bytes payload)))
    (is (equal '("$0:1.0" "send-keys") fields)
        "target must be the first NUL-terminated field")))

(test encode-command-payload-with-args-appends-args-after-command
  "encode-command-payload with args appends each arg after the command name."
  (let* ((payload (encode-command-payload :send-keys :args '("C-c" "q")))
         (fields  (cl-tmux/protocol:split-on-nul-bytes payload)))
    (is (equal '("send-keys" "C-c" "q") fields)
        "args must follow the command name in order")))
