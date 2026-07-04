(in-package #:cl-tmux)

;;; -- Pane input and prefix commands ----------------------------------------
;;;

(defun %send-keys-hex-to-string (hex)
  "Convert a send-keys -H argument (a hexadecimal character code like \"1b\" or
   \"41\") to the one-character string it names, or NIL when HEX is not a valid
   in-range code.  Mirrors tmux's send-keys -H (strtol base 16 → key).  Extracted
   as a named helper so the hex→byte logic is unit-testable without a live PTY
   (send-keys-to-pane no-ops on fd -1), matching the send-keys -l test pattern."
  (let ((code (%parse-integer-or-nil hex :radix 16 :junk-allowed t)))
    (when (and code (<= 0 code (1- char-code-limit)))
      (string (code-char code)))))

(defun %send-keys-reset-target-pane-terminal-state (flags target-pane)
  "Apply send-keys -R to TARGET-PANE when requested by FLAGS."
  (when (and (%flag-present-p flags #\R) target-pane (pane-screen target-pane))
    (cl-tmux/terminal/actions:ris-action (pane-screen target-pane))
    (setf *dirty* t)))

(defun %cmd-send-keys-arg (session args)
  "send-keys [-lHMR] [-N count] [-t target-pane] [-X] [key ...]: send keys or a
   copy-mode command.
   -X: the first positional is a named copy-mode command (begin-selection,
       scroll-up, etc.) dispatched to the target pane's copy mode.  -X is a
       BOOLEAN flag — the command is a positional, not -X's value.
   -N count: repeat count.  With -X, the copy-mode command runs COUNT times
       (e.g. `send -X -N 5 scroll-up`); with regular keys, the whole key sequence
       is sent COUNT times.  Default 1.
   -t: target a specific pane by pane-id or 'session:window.pane' syntax.
   -l: send each positional literally (no key-name translation).
   -H: each positional is a hexadecimal character code (e.g. `send -H 1b 5b 41`).
   -M: forward the current mouse event to the target pane.
   -R: reset the target pane's terminal state (RIS) before sending any keys.
   -F: expand #{...} format variables in each key argument before sending.
   Without -X: each positional is a key name or literal string typed into the pane."
  (with-command-input (flags positionals args "tN"
                             :allowed-flags '(#\l #\H #\M #\R #\X #\N #\t #\F)
                             :message "send-keys: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (literal-p  (%flag-present-p flags #\l))
           (hex-p      (%flag-present-p flags #\H))
           (m-p        (%flag-present-p flags #\M))
           (x-p        (%flag-present-p flags #\X))
           (count      (let ((n (%flag-value flags #\N)))
                         (max 1 (or (and n (%parse-integer-or-nil n :junk-allowed t))
                                    1)))))
      (with-target-context (target-session target-win target-pane session target-str)
        (let ((session target-session)
              ;; -F: expand #{...} in each key argument against the target.
              (positionals
                (if (%flag-present-p flags #\F)
                    (let ((ctx (cl-tmux/format:format-context-from-session
                                target-session target-win target-pane)))
                      (mapcar (lambda (k) (cl-tmux/format:expand-format k ctx))
                              positionals))
                    positionals)))
          (%send-keys-reset-target-pane-terminal-state flags target-pane)
          (cond
            (m-p
             (if *current-mouse-event*
                 (%forward-current-mouse-event-to-pane target-pane)
                 (show-overlay "send-keys: no current mouse event")))
            ;; -X: dispatch the copy-mode command (first positional) COUNT times.
            (x-p
             (when (first positionals)
               (dotimes (_ count)
                 (%dispatch-send-keys-X session (first positionals) target-pane
                                        target-win (rest positionals)))))
            ;; Regular keys: send the whole positional sequence COUNT times. With
            ;; -H each positional is a hex code -> the literal character it names.
            ((and positionals target-pane)
             (dotimes (_ count)
               (dolist (key positionals)
                 (if hex-p
                     (let ((str (%send-keys-hex-to-string key)))
                       (when str (send-keys-to-pane target-pane str :literal t)))
                     (send-keys-to-pane target-pane key :literal literal-p)))))))))))

(defun %cmd-send-prefix-arg (session args)
  "send-prefix [-2] [-t target-pane]: send the configured prefix key to a pane.
   -2 sends the secondary prefix key instead of the primary prefix.  -t targets a
   specific pane by pane-id or 'session:window.pane' syntax."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\2 #\t)
                             :max-positionals 0
                             :message "send-prefix: unsupported argument")
    (let* ((target-str (%flag-value flags #\t))
           (target-pane nil)
           (prefix-byte (if (%flag-present-p flags #\2)
                            cl-tmux/config:*prefix2-key-code*
                            cl-tmux/config:*prefix-key-code*)))
      (with-target-context (target-session target-window pane session target-str)
        (declare (ignore target-session target-window))
        (setf target-pane pane))
      (when (and prefix-byte
                 target-pane
                 (not *client-read-only*)
                 (%send-byte-to-pane target-pane prefix-byte))
        t))))
