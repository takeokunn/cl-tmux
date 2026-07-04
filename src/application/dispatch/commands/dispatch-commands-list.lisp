(in-package #:cl-tmux)

;;; -- List overlay formatting/handlers ---------------------------------------
;;;
;;; list-sessions/windows/panes/clients formatting helpers, the tmux-compatible
;;; list-* argument parser (%parse-list-command-input and its
;;; %parse-short-flag-bundle helper), and the define-list-overlay-handler
;;; macro plus its four %cmd-list-*-arg handlers.
;;; list-commands and wait-for (their own handlers, plus wait-for's argument
;;; parser) live in dispatch-commands-list-commands.lisp; wait-for's parser
;;; reuses %parse-short-flag-bundle from here.
;;; The helpers here are shared with main.lisp and the dispatch tests, but the
;;; handlers themselves stay in the arg-command layer.

(declaim (special cl-tmux::*clients*))

;;; (*command-usage-table* lives in dispatch-commands-list-data.lisp, loaded before this file)

(defun %lc-usage (canonical-name)
  "Return the usage flags string for CANONICAL-NAME, or empty string when unknown."
  (or (cdr (assoc canonical-name *command-usage-table* :test #'string=))
      ""))

(defun %lc-all-names ()
  "Return all list-commands canonical names in sorted order."
  (sort (mapcar #'car *command-usage-table*) #'string<))

(defun %lc-resolve-name (input)
  "Resolve INPUT for list-commands.
   Returns (values :exact canonical-name) on exact canonical match.
   Returns (values :prefix canonical-name) on unique canonical prefix match.
   Returns (values :ambiguous message-string) on ambiguous prefix.
   Returns (values :unknown nil) when no match found."
  (let ((all (%lc-all-names)))
    (cond
      ;; Exact canonical match
      ((find input all :test #'string=)
       (values :exact input))
      (t
       ;; Prefix search among canonical names
       (let ((matches (remove-if-not
                       (lambda (name)
                         (and (>= (length name) (length input))
                              (string= input name :end2 (length input))))
                       all)))
         (cond
           ((null matches) (values :unknown nil))
           ((= 1 (length matches)) (values :prefix (first matches)))
           (t (values :ambiguous
                      (format nil "ambiguous command: ~A, could be: ~{~A~^, ~}"
                              input (sort (copy-list matches) #'string<))))))))))

(defun %lc-subst-all (string pat replacement)
  "Replace all non-overlapping occurrences of PAT in STRING with REPLACEMENT."
  (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars pat) string replacement))

(defun %lc-render-command (canonical-name format-string)
  "Render one canonical command entry using FORMAT-STRING or default usage output."
  (let ((usage (%lc-usage canonical-name)))
    (if format-string
        (let ((line format-string))
          (setf line (%lc-subst-all line "#{command_list_name}" canonical-name))
          (setf line (%lc-subst-all line "#{command_list_alias}" ""))
          (setf line (%lc-subst-all line "#{command_list_usage}" usage))
          line)
        (format nil "~A ~A" canonical-name usage))))

(defun %list-command-public-names (&optional name)
  "Return sorted tmux public command names, optionally filtered by NAME."
  (let ((names (sort (copy-list *tmux-public-command-names*)
                     #'string<)))
    (if (and name (plusp (length name)))
        (remove-if-not (lambda (command)
                         (string-equal command name))
                       names)
        names)))

(defun %format-list-command-entry (format-string command-name)
  "Format one list-commands row with the public command name."
  (%lc-render-command command-name format-string))

(defun %filtered-overlay-lines-string (lines filter)
  "Return the subset of LINES matching FILTER as one overlay string."
  (%overlay-lines-string
   (loop for line in lines
         when (or (null filter)
                  (search filter line :test #'char-equal))
           collect line)))

(defun %show-list-overlay-rows (rows filter &optional raw-text)
  "Show ROWS, using RAW-TEXT when FILTER is absent."
  (if filter
      (show-overlay (%filtered-overlay-lines-string rows filter))
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
          (cl-tmux/model:session-windows-in-index-order target-session)))

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
        (format nil "~A: ~A [~Ax~A]"
                name
                (or (and session (session-name session)) "")
                cols
                rows))))

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
                     (%registered-sessions-or-current session))
                    (target-str
                     (with-target-session (target-session target-str session)
                       (list target-session)))
                    (t
                     (list session)))))
    (if sessions
        (values
         (loop for sess in sessions
               append (loop for win in (cl-tmux/model:session-windows-in-index-order sess)
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

(defun %list-command-value-flag-p (flag value-flags)
  "Return true when FLAG takes a value in VALUE-FLAGS."
  (and value-flags
       (not (null (position flag value-flags :test #'char=)))))

(defun %list-command-too-many-message (command-name max-positionals)
  "Return tmux-compatible too-many-arguments text for COMMAND-NAME."
  (format nil "command ~A: too many arguments (need at most ~D)"
          command-name max-positionals))

(defun %list-command-unknown-flag-message (command-name flag)
  "Return tmux-compatible unknown short flag text for COMMAND-NAME."
  (format nil "command ~A: unknown flag -~C" command-name flag))

(defun %list-command-invalid-long-flag-message (command-name)
  "Return tmux-compatible invalid long flag text for COMMAND-NAME."
  (format nil "command ~A: invalid flag --" command-name))

(defun %list-command-missing-flag-argument-message (command-name flag)
  "Return tmux-compatible missing flag argument text for COMMAND-NAME."
  (format nil "command ~A: -~C expects an argument" command-name flag))

(defun %parse-short-flag-bundle (token allowed-flags value-flags remaining)
  "Walk clustered short flags in TOKEN (e.g. \"-Ff\"), consuming REMAINING for
   value-flag arguments as needed.
   Returns (values new-flags new-remaining unknown-flag missing-value-flag),
   where NEW-FLAGS is an alist of (char . value-or-t) prepended in walk order,
   and at most one of UNKNOWN-FLAG / MISSING-VALUE-FLAG is non-nil on error.
   Shared by %parse-list-command-input here and %parse-wait-for-args in
   dispatch-commands-list-commands.lisp."
  (loop with flags = nil
        for i from 1 below (length token)
        for flag = (char token i)
        do (cond
             ((not (find flag allowed-flags :test #'char=))
              (return-from %parse-short-flag-bundle
                (values nil remaining flag nil)))
             ((%list-command-value-flag-p flag value-flags)
              (let ((value (if (< (1+ i) (length token))
                               (subseq token (1+ i))
                               (pop remaining))))
                (unless value
                  (return-from %parse-short-flag-bundle
                    (values nil remaining nil flag)))
                (push (cons flag value) flags)
                (loop-finish)))
             (t
              (push (cons flag t) flags)))
        finally (return (values flags remaining nil nil))))

(defun %parse-list-command-input (command-name args value-flags allowed-flags
                                  max-positionals)
  "Parse list-* command ARGS with tmux 3.6a option/error ordering."
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
                   (char= (char token 1) #\-))
              (return-from %parse-list-command-input
                (values nil nil
                        (%list-command-invalid-long-flag-message
                         command-name))))
             ((and parsing-options-p
                   (>= (length token) 2)
                   (char= (char token 0) #\-))
              (multiple-value-bind (bundle-flags new-remaining unknown-flag missing-flag)
                  (%parse-short-flag-bundle token allowed-flags value-flags remaining)
                (setf remaining new-remaining)
                (cond
                  (unknown-flag
                   (return-from %parse-list-command-input
                     (values nil nil
                             (%list-command-unknown-flag-message
                              command-name unknown-flag))))
                  (missing-flag
                   (return-from %parse-list-command-input
                     (values nil nil
                             (%list-command-missing-flag-argument-message
                              command-name missing-flag))))
                  (t
                   (setf flags (append bundle-flags flags))))))
             (t
              (push token positionals)
              (setf parsing-options-p nil)
              (when (> (length positionals) max-positionals)
                (return-from %parse-list-command-input
                  (values flags
                          (nreverse positionals)
                          (%list-command-too-many-message
                           command-name max-positionals))))))
        finally
           (return (values flags (nreverse positionals) nil))))

(defmacro define-list-overlay-handler (name (session args) docstring parser-spec
                                       let-bindings rows-form &body body)
  "Define a list command handler with shared parsing and overlay display."
  (let* ((flags-form (first parser-spec))
         (positionals-form (second parser-spec))
         (value-flags-form (third parser-spec))
         (options (cdddr parser-spec))
         (raw-text-p (getf options :raw-text))
         (command-name (getf options :command))
         (allowed-flags (getf options :allowed-flags))
         (max-positionals (getf options :max-positionals)))
    `(defun ,name (,session ,args)
       ,docstring
       (multiple-value-bind (,flags-form ,positionals-form parse-error)
           (%parse-list-command-input ,command-name ,args ,value-flags-form
                                      ',allowed-flags ,max-positionals)
         (declare (ignorable ,session ,flags-form ,positionals-form))
         (if parse-error
             (progn
               (show-overlay parse-error)
               nil)
             (let* ,let-bindings
               ,(if raw-text-p
                    `(with-list-overlay-rows/raw (rows raw-text display-p)
                         ,rows-form
                       ,@body)
                    `(with-list-overlay-rows (rows display-p)
                         ,rows-form
                       ,@body))))))))

(define-list-overlay-handler %cmd-list-sessions-arg (session args)
  "list-sessions [-F format] [-f filter]: list sessions.
   -F format: custom format string (default: shows name, windows, attached).
   -f filter keeps expanded rows containing FILTER, case-insensitively.
  Shows overlay in standalone mode."
  (flags positionals "Ff" :allowed-flags (#\F #\f) :max-positionals 0
         :command "list-sessions" :raw-text t)
  ((fmt    (%flag-value flags #\F))
   (filter (%flag-value flags #\f)))
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
         :command "list-clients")
  ((fmt        (or (%flag-value flags #\F)
                    "#{client_name}: #{client_session} [#{client_width}x#{client_height}]"))
   (filter     (%flag-value flags #\f))
   (target-str (%flag-value flags #\t)))
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
         :command "list-windows")
  ((fmt        (%flag-value flags #\F))
   (filter     (%flag-value flags #\f))
   (target-str (%flag-value flags #\t))
   (all-p      (%flag-present-p flags #\a)))
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
         :command "list-panes")
  ((fmt        (%flag-value flags #\F))
   (filter     (%flag-value flags #\f))
   (target-str (%flag-value flags #\t))
   (all-p      (%flag-present-p flags #\a))
   (session-p  (%flag-present-p flags #\s)))
  (%list-pane-overlay-lines session fmt target-str all-p session-p)
  (%show-list-overlay-rows rows filter))

;;; %cmd-list-commands-arg, %parse-wait-for-args, and %cmd-wait-for-arg live
;;; in dispatch-commands-list-commands.lisp.
