(in-package #:cl-tmux)

;;; -- list-sessions/windows/panes/clients %cmd-* handlers ---------------------

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
