(in-package #:cl-tmux)

;;; -- List and command-registry helpers -------------------------------------
;;;
;;; list-sessions/windows/panes/clients/commands plus wait-for argument checks.
;;; The helpers here are shared with main.lisp and the dispatch tests, but the
;;; handlers themselves stay in the arg-command layer.

(declaim (special cl-tmux::*clients*))

;;; ── list-commands usage string table ─────────────────────────────────────────
;;;
;;; Maps canonical-name → usage-flags-string matching the real tmux cmd_entry.args
;;; field so `list-commands -F #{command_list_usage}` output is script-compatible.

(defparameter *command-usage-table*
  '(("attach-session"       . "[-dErx] [-c working-directory] [-f flags] [-t target-session]")
    ("bind-key"             . "[-nrN] [-T key-table] [-X] key [note] command [argument ...]")
    ("break-pane"           . "[-abdP] [-F format] [-n window-name] [-s src-pane] [-t dst-window]")
    ("capture-pane"         . "[-aCeJNpPqS] [-b buffer-name] [-E end-line] [-s src-pane] [-S start-line] [-t target-pane]")
    ("choose-buffer"        . "[-GNrZ] [-F format] [-f filter] [-O sort-order] [-t target-pane] [template]")
    ("choose-client"        . "[-GNrZ] [-F format] [-f filter] [-O sort-order] [-t target-pane] [template]")
    ("choose-tree"          . "[-GNrsw] [-F format] [-f filter] [-O sort-order] [-t target-pane] [template]")
    ("clear-history"        . "[-H] [-t target-pane]")
    ("clear-prompt-history" . "[-T prompt-type]")
    ("clock-mode"           . "[-t target-pane]")
    ("command-prompt"       . "[-1bFikN] [-I inputs] [-p prompts] [-t target-client] [-T prompt-type] [template]")
    ("confirm-before"       . "[-b] [-p prompt] [-t target-client] command")
    ("copy-mode"            . "[-eHMuq] [-s src-pane] [-t target-pane]")
    ("customize-mode"       . "[-NZ] [-F format] [-f filter] [-t target-pane]")
    ("delete-buffer"        . "[-b buffer-name]")
    ("detach-client"        . "[-aP] [-E shell-command] [-s target-session] [-t target-client]")
    ("display-menu"         . "[-O] [-b border-lines] [-c target-client] [-C menu-cursor] [-H selected-style] [-s style] [-S separator-style] [-t target-pane] [-T title] [-x position] [-y position] name key command ...")
    ("display-message"      . "[-aINpv] [-c target-client] [-d delay] [-F format] [-l message] [-t target-pane] [message]")
    ("display-panes"        . "[-bN] [-d duration] [-F format] [-t target-client] [template]")
    ("display-popup"        . "[-BCE] [-b border-lines] [-c target-client] [-d start-directory] [-e environment] [-h height] [-s style] [-S border-style] [-t target-pane] [-T title] [-w width] [-x position] [-y position] [shell-command [argument ...]]")
    ("find-window"          . "[-CimnNrTZ] [-F format] [-t target-pane] match-string")
    ("has-session"          . "[-t target-session]")
    ("if-shell"             . "[-bF] [-t target-pane] shell-command command [command]")
    ("join-pane"            . "[-bdfhv] [-l size] [-s src-pane] [-t dst-pane]")
    ("kill-pane"            . "[-a] [-t target-pane]")
    ("kill-server"          . "")
    ("kill-session"         . "[-aC] [-t target-session]")
    ("kill-window"          . "[-a] [-t target-window]")
    ("last-pane"            . "[-deZ] [-t target-window]")
    ("last-window"          . "[-t target-session]")
    ("link-window"          . "[-adbk] [-s src-window] [-t dst-window]")
    ("list-buffers"         . "[-F format] [-f filter]")
    ("list-clients"         . "[-F format] [-f filter] [-t target-session]")
    ("list-commands"        . "[-F format] [command]")
    ("list-keys"            . "[-1aN] [-P prefix-string] [-T key-table] [key]")
    ("list-panes"           . "[-as] [-F format] [-f filter] [-t target]")
    ("list-sessions"        . "[-F format] [-f filter]")
    ("list-windows"         . "[-a] [-F format] [-f filter] [-t target-session]")
    ("load-buffer"          . "[-b buffer-name] [-t target-client] path")
    ("lock-client"          . "[-t target-client]")
    ("lock-server"          . "")
    ("lock-session"         . "[-at] [-t target-session]")
    ("move-pane"            . "[-bdfhv] [-l size] [-s src-pane] [-t dst-pane]")
    ("move-window"          . "[-abrdk] [-s src-window] [-t dst-window]")
    ("new-session"          . "[-AdDEPX] [-c start-directory] [-e environment] [-f flags] [-F format] [-n window-name] [-s session-name] [-t target-session] [-x width] [-y height] [shell-command [argument ...]]")
    ("new-window"           . "[-abdkPS] [-c start-directory] [-e environment] [-F format] [-n window-name] [-t target-window] [shell-command [argument ...]]")
    ("next-layout"          . "[-t target-window]")
    ("next-window"          . "[-a] [-t target-session]")
    ("paste-buffer"         . "[-dpr] [-b buffer-name] [-s separator] [-t target-pane]")
    ("pipe-pane"            . "[-IOo] [-t target-pane] [shell-command]")
    ("previous-layout"      . "[-t target-window]")
    ("previous-window"      . "[-a] [-t target-session]")
    ("refresh-client"       . "[-cDlLRSU] [-A pane:state] [-B name:what:format] [-C size] [-f flags] [-l target] [-t target-client] [adjustment]")
    ("rename-session"       . "[-t target-session] new-name")
    ("rename-window"        . "[-t target-window] new-name")
    ("resize-pane"          . "[-DLMRUZ] [-t target-pane] [-x width] [-y height] [adjustment]")
    ("resize-window"        . "[-aADLRU] [-t target-window] [-x width] [-y height] [adjustment]")
    ("respawn-pane"         . "[-k] [-c start-directory] [-e environment] [-t target-pane] [shell-command [argument ...]]")
    ("respawn-window"       . "[-k] [-c start-directory] [-e environment] [-t target-window] [shell-command [argument ...]]")
    ("rotate-window"        . "[-DUZ] [-t target-window]")
    ("run-shell"            . "[-bC] [-c shell-command] [-d delay] [-t target-pane]")
    ("save-buffer"          . "[-a] [-b buffer-name] path")
    ("select-layout"        . "[-Enop] [-t target-window] [layout-name]")
    ("select-pane"          . "[-DdegGlLlMmRUZ] [-T title] [-t target-pane]")
    ("select-window"        . "[-lnpT] [-t target-window]")
    ("send-keys"            . "[-FHKlMRX] [-N repeat-count] [-t target-pane] key ...")
    ("send-prefix"          . "[-2] [-t target-pane]")
    ("server-access"        . "[-adlrw] [user]")
    ("set-buffer"           . "[-aw] [-b buffer-name] [-n new-buffer-name] [-t target-client] data")
    ("set-environment"      . "[-Fhgru] [-t target-session] name [value]")
    ("set-hook"             . "[-agpRuw] [-t target-pane] hook-name [command]")
    ("set-option"           . "[-aFgopqsuUw] [-t target-pane] option [value]")
    ("set-window-option"    . "[-aFgoqu] [-t target-window] option [value]")
    ("show-buffer"          . "[-b buffer-name]")
    ("show-environment"     . "[-hgs] [-t target-session] [name]")
    ("show-messages"        . "[-JT] [-t target-client]")
    ("show-options"         . "[-AgHpqsvw] [-t target-pane] [option]")
    ("show-window-options"  . "[-gvA] [-t target-window] [option]")
    ("show-session-options" . "[-gvA] [-t target-session] [option]")
    ("show-server-options"  . "[-gvA] [option]")
    ("source-file"          . "[-Fnqv] [path ...]")
    ("split-window"         . "[-bdfhIvPZ] [-c start-directory] [-e environment] [-F format] [-l size] [-t target-pane] [shell-command [argument ...]]")
    ("start-server"         . "")
    ("suspend-client"       . "[-t target-client]")
    ("swap-pane"            . "[-dDUZ] [-s src-pane] [-t dst-pane]")
    ("swap-window"          . "[-d] [-s src-window] [-t dst-window]")
    ("switch-client"        . "[-ElnprZ] [-c target-client] [-f flags] [-t target-session] [-T key-table]")
    ("unbind-key"           . "[-anq] [-T key-table] key")
    ("unlink-window"        . "[-k] [-t target-window]")
    ("wait-for"             . "[-SLU] channel"))
  "Maps canonical command name to tmux usage flags string.")

(defun %lc-alias (canonical-name)
  "Return the tmux short alias for CANONICAL-NAME, or NIL when none exists."
  (loop for (alias . canon) in *tmux-command-aliases*
        when (string= canon canonical-name) return alias))

(defun %lc-usage (canonical-name)
  "Return the usage flags string for CANONICAL-NAME, or empty string when unknown."
  (or (cdr (assoc canonical-name *command-usage-table* :test #'string=))
      ""))

(defun %lc-all-names ()
  "Return all list-commands canonical names in sorted order."
  (sort (mapcar #'car *command-usage-table*) #'string<))

(defun %lc-resolve-name (input)
  "Resolve INPUT for list-commands.
   Returns (values :exact canonical-name) on exact match (name or alias).
   Returns (values :prefix canonical-name) on unique prefix match.
   Returns (values :ambiguous message-string) on ambiguous prefix.
   Returns (values :unknown nil) when no match found."
  (let* ((all (%lc-all-names))
         (alias-canon (%canonical-command-name input)))
    (cond
      ;; Exact canonical match
      ((find input all :test #'string=)
       (values :exact input))
      ;; Exact alias match (alias-canon differs from input = known alias)
      ((not (string= alias-canon input))
       (values :exact alias-canon))
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
  (let ((plen (length pat))
        (result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length string))
          do (let ((pos (search pat string :start2 i)))
               (if pos
                   (progn
                     (loop for c across (subseq string i pos) do
                       (vector-push-extend c result))
                     (loop for c across replacement do
                       (vector-push-extend c result))
                     (setf i (+ pos plen)))
                   (progn
                     (loop for c across (subseq string i) do
                       (vector-push-extend c result))
                     (setf i (length string))))))
    (coerce result 'string)))

(defun %lc-render-command (canonical-name format-string)
  "Render one command entry using FORMAT-STRING or default usage output."
  (let* ((alias (%lc-alias canonical-name))
         (usage (%lc-usage canonical-name)))
    (if format-string
        (let ((line format-string))
          (setf line (%lc-subst-all line "#{command_list_name}" canonical-name))
          (setf line (%lc-subst-all line "#{command_list_alias}" (or alias "")))
          (setf line (%lc-subst-all line "#{command_list_usage}" usage))
          line)
        ;; Default: "name (alias) usage" or "name usage" when no alias.
        (if alias
            (format nil "~A (~A) ~A" canonical-name alias usage)
            (format nil "~A ~A" canonical-name usage)))))

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
         :message "list-clients: unsupported argument")
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
         :message "list-windows: unsupported argument")
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
         :message "list-panes: unsupported argument")
  ((fmt        (%flag-value flags #\F))
   (filter     (%flag-value flags #\f))
   (target-str (%flag-value flags #\t))
   (all-p      (%flag-present-p flags #\a))
   (session-p  (%flag-present-p flags #\s)))
  (%list-pane-overlay-lines session fmt target-str all-p session-p)
  (%show-list-overlay-rows rows filter))

(defun %cmd-list-commands-arg (session args)
  "list-commands [-F format] [command]: list tmux command signatures.
   With no argument, lists all commands one per line.
   With a command name/prefix, shows that command with prefix resolution.
   -F format expands #{command_list_name}, #{command_list_alias},
   #{command_list_usage} fields."
  (declare (ignore session))
  ;; Manual flag parse to produce tmux-compatible per-error messages.
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
          ;; Single command lookup with prefix/alias resolution.
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
        ((%flag-present-p flags #\S) (signal-channel channel))
        ((%flag-present-p flags #\L) (lock-channel   channel))
        ((%flag-present-p flags #\U) (unlock-channel channel))
        (t                 (wait-for-channel channel))))))
