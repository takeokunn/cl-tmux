;;; Startup mode dispatch and CLI entry point.
;;;
;;; Socket discovery/server auto-start helpers live in main-startup-socket.lisp.
;;; Command-client forwarding helpers live in main-startup-forwarding.lisp.
;;; Command handlers live in main-startup-commands.lisp.
;;; This file owns the mode table and binary entry-point dispatch.

(in-package :cl-tmux)

(defun %application-argv ()
  "Return cl-tmux application arguments from SBCL's process argv.
   The Nix wrapper starts the saved core as `sbcl --core ... --no-userinit ...`;
   in that shape SBCL runtime options can appear before the real cl-tmux command."
  (let* ((argv (rest sb-ext:*posix-argv*))
         (marker (or (position "--no-userinit" argv :test #'string= :from-end t)
                     (position "--end-toplevel-options" argv :test #'string= :from-end t))))
    (if marker
        (nthcdr (1+ marker) argv)
        argv)))

(defun %parse-global-cli-argv (argv)
  "Parse ARGV (the application argv, without the argv0 slot) against *cli-app*
   (main-startup-flags.lisp).  Returns the parser invocation, or NIL and
   prints a usage error to *error-output* when ARGV is malformed."
  (handler-case
      (cl-cli:parse-argv *cli-app* (cons "cl-tmux" argv))
    (cl-cli:cli-usage-error (c)
      (format *error-output* "~&cl-tmux: ~A~%" c)
      (write-string (%usage-string) *error-output*)
      nil)))

(defun %apply-global-cli-invocation (invocation)
  "Apply INVOCATION's parsed global options as side effects (socket overrides,
   config-file override, colour-capability downsampling) and return the
   remaining :mode-args rest positional — the mode word plus its own args."
  (let ((socket-name (cl-cli:option-value invocation :socket-name))
        (socket-path (cl-cli:option-value invocation :socket-path))
        (file        (cl-cli:option-value invocation :file)))
    (when socket-name (setf *socket-name-override* socket-name))
    (when socket-path (setf *socket-path-override* socket-path))
    (when file (setf cl-tmux/config:*config-file-override* file))
    (setf cl-tmux/renderer:*color-downsample-fn*
          (when (cl-cli:option-value invocation :force-256) #'cl-tmux/renderer:%rgb-int-to-256)))
  (cl-cli:positional-value invocation :mode-args))

(defun %dispatch-global-cli-flag-actions (invocation mode-args)
  "Run the flag-driven entry points that today double as *startup-modes* mode
   names (-V/-h/-C), so they work regardless of where they appear in argv.
   Returns T when one of them ran (the caller must not also dispatch a mode)."
  (cond
    ((cl-cli:option-value invocation :print-version) (run-version nil) t)
    ((cl-cli:option-value invocation :print-help)    (run-usage nil)   t)
    ((plusp (or (cl-cli:option-value invocation :control) 0))
     (run-control-mode (rest mode-args))
     t)
    (t nil)))

(defun main ()
  "Binary entry point - dispatches on the first argv item via *startup-modes*.
   Global tmux(1)-compatible flags (-2/-C/-D/-L/-N/-S/-T/-V/-c/-f/-h/-l/-u/-v)
   are parsed by *cli-app* (cl-cli, see main-startup-flags.lisp) from anywhere
   in the leading flag run, in any order, before mode dispatch.
   Each entry in *startup-modes* is a plist (handler-symbol &key :raw-args-p).
   :raw-args-p T modes receive the full argv tail; all others receive a single
   session name (defaulting to \"0\").
   Unrecognized or absent modes fall through to run-standalone."
  (let ((invocation (%parse-global-cli-argv (%application-argv))))
    (if (null invocation)
        (sb-ext:exit :code 1)
        (let ((mode-args (%apply-global-cli-invocation invocation)))
          (unless (%dispatch-global-cli-flag-actions invocation mode-args)
            (let* ((mode  (first mode-args))
                   (rest  (rest mode-args))
                   (entry (cdr (assoc mode *startup-modes* :test #'equal))))
              (%dispatch-startup-mode-entry entry mode rest)))))))

(defun %dispatch-unknown-mode (mode rest)
  "Handle an argv whose first item is not a known startup mode.
   A dash flag is a usage error: print the usage summary to stderr and exit 1
   instead of silently starting a standalone session on a typo.
   When MODE names a command and a default-session server is already running
   (its socket exists), forward MODE + REST to it as a command client; otherwise
   run the standalone multiplexer.
   Guarding on an existing socket keeps cl-tmux (no args) and the no-server
   case unchanged; only an explicit subcommand against a live server forwards."
  (labels ((dash-flag-p (name)
             (%dash-flag-p name)))
    (cond
      ((dash-flag-p mode)
       (write-string (%usage-string) *error-output*)
       (sb-ext:exit :code 1))
      ((and mode (probe-file (socket-path "0")))
       (run-command-client "0" (cons mode rest)))
      (t (run-standalone)))))

(defun %dispatch-startup-mode-entry (entry mode rest)
  (if entry
      (%dispatch-startup-mode-handler entry mode rest)
      ;; No recognized mode: forward to a running server as a command client
      ;; (`cl-tmux <command>` against an existing server), else run standalone.
      (%dispatch-unknown-mode mode rest)))

(defun %dispatch-startup-mode-handler (entry mode rest)
  (let ((handler    (first entry))
        (raw-args-p (%startup-mode-raw-args-p mode)))
    ;; Dispatch: :raw-args-p modes receive the full tail; name-only modes
    ;; receive a single session name so their signature stays (name).
    (if raw-args-p
        (funcall (symbol-function handler) rest)
        (funcall (symbol-function handler) (or (first rest) "0")))))

(defun %dash-flag-p (name) (and name (plusp (length name)) (char= (char name 0) #\-)))
