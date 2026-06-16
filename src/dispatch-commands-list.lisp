(in-package #:cl-tmux)

;;; -- List and command-registry helpers -------------------------------------
;;;
;;; list-sessions/windows/panes/clients/commands plus wait-for argument checks.
;;; The helpers here are shared with main.lisp and the dispatch tests, but the
;;; handlers themselves stay in the arg-command layer.

(declaim (special cl-tmux::*clients*))

(defun %list-command-public-names (&optional name)
  "Return tmux public command names, optionally filtered by NAME."
  (let ((names (copy-list *tmux-public-command-names*)))
    (if (and name (plusp (length name)))
        (remove-if-not (lambda (command)
                         (string-equal command name))
                       names)
        names)))

(defun %format-list-command-entry (format-string command-name)
  "Format one list-commands row with the command_list_* keys cl-tmux knows."
  (if format-string
      (cl-tmux/format:expand-format
       format-string
       (list :command-list-name command-name
             :command-list-alias ""
             :command-list-usage ""))
      command-name))

(defun %list-row-matches-filter-p (filter line)
  "T when FILTER is absent or appears in LINE, case-insensitively."
  (or (null filter)
      (search filter line :test #'char-equal)))

(defun %filtered-overlay-lines-string (lines filter)
  "Return the subset of LINES matching FILTER as one overlay string."
  (%overlay-lines-string
   (loop for line in lines
         when (%list-row-matches-filter-p filter line)
           collect line)))

(defun %show-filtered-overlay-lines (lines filter)
  "Show LINES in an overlay, keeping only rows that match FILTER."
  (show-overlay (%filtered-overlay-lines-string lines filter)))

(defun %show-list-overlay-rows (rows filter &optional raw-text)
  "Show ROWS, using RAW-TEXT when FILTER is absent."
  (if filter
      (%show-filtered-overlay-lines rows filter)
      (show-overlay (or raw-text (%overlay-lines-string rows)))))

(defun %non-empty-overlay-lines (text)
  "Split TEXT into overlay rows and drop empty lines."
  (remove-if (lambda (line) (zerop (length line)))
             (uiop:split-string text :separator '(#\Newline))))

(defun %list-clients-records ()
  "Return (NAME ROWS COLS) records for attached clients, or a local fallback.
   CLIENT-CONN has no tty/session slot yet, so script-visible client names are
   stable synthetic names ordered like *clients* (front = most recent)."
  (if *clients*
      (loop for conn in *clients*
            for i from 0
            collect (list (format nil "client-~D" i)
                          (client-conn-rows conn)
                          (client-conn-cols conn)))
      (list (list "local" *term-rows* *term-cols*))))

(defun %registered-sessions-or-current (session)
  "Return the registered sessions, or SESSION when none are registered."
  (or (mapcar #'cdr *server-sessions*)
      (list session)))

(defun %window-targets-for-session (target-session)
  "Return (SESSION . WINDOW) targets for every window in TARGET-SESSION."
  (mapcar (lambda (win)
            (cons target-session win))
          (session-windows target-session)))

(defun %list-pane-targets (session target-str all-p session-p)
  "Return the target windows for list-panes based on flags and target input."
  (cond
    (all-p
     (loop for target-session in (%registered-sessions-or-current session)
           append (%window-targets-for-session target-session)))
    (session-p
     (with-target-session (target-session target-str session
                           :on-missing :current)
       (%window-targets-for-session target-session)))
    (target-str
     (multiple-value-bind (target-session target-window)
         (%resolve-target-session-window
          session target-str
          (session-active-window session)
          (session-active-pane session))
       (when target-window
         (list (cons target-session target-window)))))
    (t
     (let ((win (session-active-window session)))
       (when win
         (list (cons session win)))))))

(defun %format-list-window-entry (session win fmt)
  "Format one list-windows row using either FMT or the default tmux-style text."
  (if fmt
      (cl-tmux/format:expand-format
       fmt
       (cl-tmux/format:format-context-from-window session win))
      (format nil "~A: ~A (~Dx~D) [~D pane~:P]~A"
              (window-id win) (window-name win)
              (window-width win) (window-height win)
              (length (window-panes win))
              (if (eq win (session-active-window session))
                  " [active]"
                  ""))))

(defun %format-list-pane-entry (session win pane fmt)
  "Format one list-panes row using either FMT or the default tmux-style text."
  (if fmt
      (cl-tmux/format:expand-format
       fmt
       (cl-tmux/format:format-context-from-session session win pane))
      (format nil "~D: [~Dx~D] [~D,~D] pane ~D~A"
              (pane-id pane)
              (pane-width pane) (pane-height pane)
              (pane-x pane) (pane-y pane)
              (pane-id pane)
              (if (eq pane (window-active-pane win))
                  " (active)"
                  ""))))

(defun %format-list-client-entry (session record fmt)
  "Format one list-clients row using either FMT or the default tmux-style text."
  (destructuring-bind (name rows cols) record
    (if fmt
        (cl-tmux/format:expand-format
         fmt
         (cl-tmux/format:format-context-from-session
          session
          (and session (session-active-window session))
          (and session (session-active-pane session))
          :client-width cols
          :client-height rows
          :client-tty name))
        (format nil "~A: ~A [~Ax~A]" name session rows cols))))

(defun %format-list-session-entry (target-session fmt)
  "Format one list-sessions row using FMT."
  (cl-tmux/format:expand-format
   fmt
   (cl-tmux/format:format-context-from-session
    target-session
    (session-active-window target-session)
    nil)))

(defun %list-session-overlay-lines (session fmt)
  "Return list-sessions overlay lines, optional raw text, and a display flag."
  (if fmt
      (values
       (loop for sess in (%registered-sessions-or-current session)
             collect (%format-list-session-entry sess fmt))
       nil
       t)
      (values nil (%format-session-list session) t)))

(defun %list-client-overlay-lines (session fmt)
  "Return list-clients overlay lines and a display flag."
  (values (loop for record in (%list-clients-records)
                collect (%format-list-client-entry session record fmt))
          t))

(defun %list-window-overlay-lines (session fmt target-str all-p)
  "Return list-windows overlay lines and a display flag."
  (let ((sessions (cond
                    (all-p
                     (loop for target-session in (%registered-sessions-or-current session)
                           append (%window-targets-for-session target-session)))
                    (target-str
                     (with-target-session (target-session target-str session)
                       (list target-session)))
                    (t
                     (list session)))))
    (if sessions
        (values
         (loop for sess in sessions
               append (loop for win in (session-windows sess)
                            collect (%format-list-window-entry sess win fmt)))
         t)
        (values nil nil))))

(defun %list-pane-overlay-lines (session fmt target-str all-p session-p)
  "Return list-panes overlay lines and a display flag."
  (let ((targets (%list-pane-targets session target-str all-p session-p)))
    (values
     (loop for target in targets
           append (let ((target-session (car target))
                        (win            (cdr target)))
                    (loop for pane in (window-panes win)
                          collect (%format-list-pane-entry
                                   target-session win pane fmt))))
     (not (null targets)))))

(defun %list-command-overlay-lines (fmt name)
  "Return list-commands overlay lines and a display flag."
  (values (mapcar (lambda (cmd-name)
                    (%format-list-command-entry fmt cmd-name))
                  (%list-command-public-names name))
          t))

(defmacro with-list-overlay-rows ((rows display-p) rows-form &body body)
  "Bind ROWS and DISPLAY-P from ROWS-FORM and run BODY when display is needed."
  `(multiple-value-bind (,rows ,display-p) ,rows-form
     (when ,display-p
       ,@body)))

(defmacro with-list-overlay-rows/raw ((rows raw-text display-p) rows-form &body body)
  "Bind ROWS, RAW-TEXT, and DISPLAY-P from ROWS-FORM and run BODY when needed."
  `(multiple-value-bind (,rows ,raw-text ,display-p) ,rows-form
     (when ,display-p
       ,@body)))

(defmacro define-list-overlay-handler (name (session args) docstring parser-spec
                                       let-bindings rows-form &body body)
  "Define a list command handler with shared parsing and overlay display."
  (let* ((flags-form (first parser-spec))
         (positionals-form (second parser-spec))
         (value-flags-form (third parser-spec))
         (options (cdddr parser-spec))
         (raw-text-p (getf options :raw-text))
         (clean-options (loop for (key val) on options by #'cddr
                              unless (eq key :raw-text)
                                append (list key val))))
    `(define-command-input-handler ,name (,session ,args) ,docstring
         (,flags-form ,positionals-form ,value-flags-form ,@clean-options)
       (let* ,let-bindings
         ,(if raw-text-p
              `(with-list-overlay-rows/raw (rows raw-text display-p)
                   ,rows-form
                 ,@body)
              `(with-list-overlay-rows (rows display-p)
                   ,rows-form
                 ,@body))))))

(define-list-overlay-handler %cmd-list-sessions-arg (session args)
  "list-sessions [-F format] [-f filter]: list sessions.
   -F format: custom format string (default: shows name, windows, attached).
   -f filter keeps expanded rows containing FILTER, case-insensitively.
  Shows overlay in standalone mode."
  (flags positionals "Ff" :allowed-flags (#\F #\f) :max-positionals 0
         :message "list-sessions: unsupported argument" :raw-text t)
  ((fmt    (cdr (assoc #\F flags)))
   (filter (cdr (assoc #\f flags))))
  (%list-session-overlay-lines session fmt)
  (if filter
      (%show-list-overlay-rows
       (or rows (%non-empty-overlay-lines raw-text))
       filter)
      (%show-list-overlay-rows rows nil raw-text)))

(define-list-overlay-handler %cmd-list-clients-arg (session args)
  "list-clients [-F format] [-f filter] [-t target-session]: list attached clients.
   -F format expands tmux client variables such as #{client_name},
   #{client_width}, #{client_height}, and #{client_session}.
   -f filter keeps expanded rows containing FILTER, case-insensitively.
   cl-tmux currently broadcasts one active session to all attached clients, so
   -t selects the session used for format expansion rather than filtering a
   per-client session list."
  (flags positionals "Fft" :allowed-flags (#\F #\f #\t) :max-positionals 0
         :message "list-clients: unsupported argument")
  ((fmt        (or (cdr (assoc #\F flags))
                   "#{client_name}: #{client_session} [#{client_width}x#{client_height}]"))
   (filter     (cdr (assoc #\f flags)))
   (target-str (cdr (assoc #\t flags))))
  (with-target-session (target-session target-str session
                                    :message "list-clients: no such session: ~A"
                                    :on-missing :error)
    (%list-client-overlay-lines target-session fmt))
  (%show-list-overlay-rows rows filter))

(define-list-overlay-handler %cmd-list-windows-arg (session args)
  "list-windows [-F format] [-f filter] [-a] [-t session]: list windows.
   -F format: custom format string.
   -f filter keeps expanded rows containing FILTER, case-insensitively.
   -a: list windows in all sessions.
   -t target-session: list windows in the target session."
  (flags positionals "Fft" :allowed-flags (#\F #\f #\t #\a) :max-positionals 0
         :message "list-windows: unsupported argument")
  ((fmt        (cdr (assoc #\F flags)))
   (filter     (cdr (assoc #\f flags)))
   (target-str (cdr (assoc #\t flags)))
   (all-p      (assoc #\a flags)))
  (%list-window-overlay-lines session fmt target-str all-p)
  (%show-list-overlay-rows rows filter))

(define-list-overlay-handler %cmd-list-panes-arg-full (session args)
  "list-panes [-as] [-F format] [-f filter] [-t target]: list panes.
   -F format: custom format string.
   -f filter keeps expanded rows containing FILTER, case-insensitively.
   -a: list panes in all sessions.
   -s: list panes in all windows of the target/current session."
  (flags positionals "Fft" :allowed-flags (#\F #\f #\t #\a #\s)
         :max-positionals 0
         :message "list-panes: unsupported argument")
  ((fmt        (cdr (assoc #\F flags)))
   (filter     (cdr (assoc #\f flags)))
   (target-str (cdr (assoc #\t flags)))
   (all-p      (assoc #\a flags))
   (session-p  (assoc #\s flags)))
  (%list-pane-overlay-lines session fmt target-str all-p session-p)
  (%show-list-overlay-rows rows filter))

(define-list-overlay-handler %cmd-list-commands-arg (session args)
  "list-commands [-F format] [command]: list tmux public commands one per line;
   with a COMMAND name, show only that command (tmux's `list-commands <name>`)."
  (flags positionals "F" :allowed-flags (#\F) :max-positionals 1
         :message "list-commands: unsupported argument")
  ((fmt  (cdr (assoc #\F flags)))
   (name (first positionals)))
  (%list-command-overlay-lines fmt name)
  (%show-list-overlay-rows rows nil))

(define-command-input-handler %cmd-wait-for-arg (session args)
  "wait-for [-SLU] channel: channel synchronization.
   Bare: block the calling thread until CHANNEL is signaled (or timeout elapses).
   -S: signal (unblock) all threads waiting on CHANNEL.
   -L: lock CHANNEL so subsequent signal calls are suppressed.
  -U: unlock CHANNEL, re-enabling signal-channel."
  (flags positionals "" :allowed-flags (#\L #\S #\U) :min-positionals 1
         :max-positionals 1 :message "wait-for: unsupported argument")
  (let ((channel (first positionals)))
    (when (and channel (plusp (length channel)))
      (cond
        ((assoc #\S flags) (signal-channel channel))
        ((assoc #\L flags) (lock-channel   channel))
        ((assoc #\U flags) (unlock-channel channel))
        (t                 (wait-for-channel channel))))))
