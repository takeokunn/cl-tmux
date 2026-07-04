(in-package #:cl-tmux/config)

;;;; bind directive command-sequence helpers.

;;; ── Semicolon-sequence splitter ──────────────────────────────────────────
;;;
;;; tmux bind directives support ";" (from "\;" in the config line) as a
;;; command separator: bind r source-file ~/.tmux.conf \; display "Reloaded!"
;;; %split-on-semicolons splits a flat token list on ";" tokens,
;;; removing empty segments, yielding a list of per-command token lists.

(defun %strip-brace-block (tokens)
  "When TOKENS form a `{ ... }` block — first token \"{\" and last token \"}\" —
   return the inner tokens; otherwise return TOKENS unchanged.  This lets the
   tmux 3.x brace form `bind r { cmd1 ; cmd2 }` reuse %split-on-semicolons
   exactly like the older `bind r cmd1 \\; cmd2` form.  An empty block `{ }`
   yields NIL (no commands)."
  (if (and (cdr tokens)
           (string= (first tokens) "{")
           (string= (first (last tokens)) "}"))
      (butlast (rest tokens))
      tokens))

(defun %split-on-semicolons (tokens)
  "Split TOKENS on \";\" tokens, returning a list of per-command token lists.
   Empty segments (consecutive semicolons or trailing) are discarded.
   When no semicolons are present, returns (list tokens) unchanged."
  (let ((result  '())
        (current '()))
    (dolist (tok tokens)
      (if (string= tok ";")
          (progn (when current (push (nreverse current) result))
                 (setf current '()))
          (push tok current)))
    (when current (push (nreverse current) result))
    (if result (nreverse result) (list tokens))))
