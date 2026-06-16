(in-package #:cl-tmux)

;;; -- Arg-command dispatch table + command-line runners ------------------------------
;;;
;;; *arg-command-table* maps (list-of-names . #'handler) pairs and is derived
;;; from *dispatch-command-specs* so the arg-taking dispatch table stays aligned
;;; with the shared registry.  Consulted by %run-command-tokens before the
;;; no-arg named-command table.  Defined LAST so all #'%cmd-* function
;;; references are resolved after their definitions in
;;; dispatch-commands{,-buffer,-option,-lifecycle,-pane,-shell,-auto}.lisp.

(defun %make-dispatch-arg-table (specs)
  "Build a list of (list-of-names . #'handler) pairs from dispatch SPECS."
  (let ((table nil))
    (dolist (spec specs (nreverse table))
      (let ((handler (getf spec :arg-handler))
            (names (getf spec :arg-names)))
        (when (and handler names)
          (push (cons names (fdefinition handler)) table))))))

(defparameter *arg-command-table*
  (%make-dispatch-arg-table *dispatch-command-specs*))

(defparameter *arg-command-dispatch*
  (let ((table (make-hash-table :test #'equalp)))
    (dolist (entry *arg-command-table* table)
      (dolist (name (car entry))
        (setf (gethash name table) (cdr entry)))))
  "Fast lookup table for *arg-command-table* keyed by command name string.")

(defun %arg-command-handler (cmd)
  (gethash cmd *arg-command-dispatch*))

(defun %run-command-tokens (session tokens)
  "Run a command line given as an already-tokenised TOKENS list (first = command
   name, rest = arguments).  Dispatch order:
   1. arg-taking commands in *arg-command-table* (consume their arguments)
   2. no-arg named commands via %dispatch-named-command
   Taking pre-split tokens lets arg-bearing key bindings run without lossy
   re-tokenisation.  Returns the handler's return value."
  (let ((cmd  (first tokens))
        (rest (rest tokens)))
    (when cmd
      ;; 1. Arg-taking commands (only when there are arguments to consume).
      (let ((entry (and rest (%arg-command-handler cmd))))
        (if entry
            (funcall entry session rest)
            ;; 2. No-arg named commands (includes arg-cmds invoked with no args).
            (%dispatch-named-command session cmd))))))

(defun %run-command-line (session input)
  "Tokenise INPUT (one command line, shell-style) and run it.
   When the tokenised line contains \";\" tokens, splits into multiple commands
   and runs each in sequence, matching tmux's command-prompt behaviour."
  (let* ((tokens    (cl-tmux/commands:tokenize-command-string input))
         (sequences (cl-tmux/config::%split-on-semicolons tokens)))
    (if (= (length sequences) 1)
        (%run-command-tokens session (first sequences))
        (dolist (subcmd sequences)
          (%run-command-tokens session subcmd)))))
