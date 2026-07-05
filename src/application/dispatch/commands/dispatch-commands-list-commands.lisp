(in-package #:cl-tmux)

;;; -- list-commands and wait-for -------------------------------------------
;;;
;;; %cmd-list-commands-arg renders the command-registry table built in
;;; dispatch-commands-list.lisp/-data.lisp via a hand-rolled -F/positional
;;; parser (list-commands has just one value flag, so it does not need the
;;; clustered-short-flag machinery). wait-for is an unrelated
;;; channel-synchronization command whose parser reuses
;;; %parse-short-flag-bundle from dispatch-commands-list.lisp to walk its
;;; clustered -L/-S/-U flags, while producing its own command-specific
;;; per-flag error messages.

(defun %cmd-list-commands-arg (session args)
  "list-commands [-F format] [command]: list tmux command signatures.
   With no argument, lists all commands one per line.
   With a command name/prefix, shows that command with prefix resolution.
   -F format expands #{command_list_name} and #{command_list_usage} fields."
  (declare (ignore session))
  ;; Manual flag parse to produce list-commands-specific per-error messages.
  (let ((format-string nil)
        (positionals nil)
        (error-message nil))
    (loop with toks = args
          while (and toks (not error-message))
          for tok = (pop toks)
          do (cond
               ((string= tok "-F")
                (if toks
                    (setf format-string (pop toks))
                    (setf error-message
                          "command list-commands: -F expects an argument")))
               ((and (>= (length tok) 2)
                     (char= (char tok 0) #\-)
                     (char/= (char tok 1) #\-))
                (setf error-message
                      (format nil "command list-commands: unknown flag ~A" tok)))
               (t
                (push tok positionals))))
    (when error-message
      (show-overlay error-message)
      (return-from %cmd-list-commands-arg nil))
    (setf positionals (nreverse positionals))
    (when (> (length positionals) 1)
      (show-overlay "command list-commands: too many arguments (need at most 1)")
      (return-from %cmd-list-commands-arg nil))
    (let ((name-input (first positionals)))
      (if name-input
          ;; Single command lookup with exact canonical or unique-prefix resolution.
          (multiple-value-bind (kind result) (%lc-resolve-name name-input)
            (ecase kind
              (:exact     (show-overlay (%lc-render-command result format-string)))
              (:prefix    (show-overlay (%lc-render-command result format-string)))
              (:ambiguous (show-overlay result))
              (:unknown   (show-overlay
                           (format nil "unknown command: ~A" name-input)))))
          ;; All commands: one line each.
          (show-overlay
           (with-output-to-string (s)
             (dolist (name (%lc-all-names))
               (write-string (%lc-render-command name format-string) s)
               (terpri s))))))))

(defun %parse-wait-for-args (args)
  "Parse wait-for arguments using tmux 3.6a option ordering."
  (loop with flags = nil
        with positionals = nil
        with parsing-options-p = t
        with remaining = args
        while remaining
        for token = (pop remaining)
        do (cond
             ((and parsing-options-p (string= token "--"))
              (setf parsing-options-p nil))
             ((and parsing-options-p
                   (>= (length token) 2)
                   (char= (char token 0) #\-)
                   (char/= (char token 1) #\-))
              (multiple-value-bind (bundle-flags new-remaining unknown-flag)
                  (%parse-short-flag-bundle token '(#\L #\S #\U) nil remaining)
                (setf remaining new-remaining)
                (if unknown-flag
                    (return-from %parse-wait-for-args
                      (values nil nil
                              (format nil "command wait-for: unknown flag -~C"
                                      unknown-flag)))
                    (setf flags (append bundle-flags flags)))))
             (t
              (push token positionals)
              (setf parsing-options-p nil)))
        finally
           (let ((parsed-positionals (nreverse positionals)))
             (cond
               ((< (length parsed-positionals) 1)
                (return (values flags
                                parsed-positionals
                                "command wait-for: too few arguments (need at least 1)")))
               ((> (length parsed-positionals) 1)
                (return (values flags
                                parsed-positionals
                                "command wait-for: too many arguments (need at most 1)")))
               (t
                (return (values flags parsed-positionals nil)))))))

(defun %cmd-wait-for-arg (session args)
  "wait-for [-SLU] channel: channel synchronization.
   Bare: block the calling thread until CHANNEL is signaled (or timeout elapses).
   -S: signal (unblock) all threads waiting on CHANNEL.
   -L: lock CHANNEL so subsequent signal calls are suppressed.
  -U: unlock CHANNEL, re-enabling signal-channel."
  (declare (ignore session))
  (multiple-value-bind (flags positionals error-message)
      (%parse-wait-for-args args)
    (if error-message
        (show-overlay error-message)
        (let ((channel (first positionals)))
          (cond
            ((%flag-present-p flags #\S) (signal-channel channel))
            ((%flag-present-p flags #\L) (lock-channel channel))
            ((%flag-present-p flags #\U) (unlock-channel channel))
            (t (wait-for-channel channel)))))))
