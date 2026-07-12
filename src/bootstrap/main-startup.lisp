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

(defun main ()
  "Binary entry point - dispatches on the first argv item via *startup-modes*.
   tmux's global socket flags (-L socket-name / -S socket-path) are consumed
   from the front of argv before mode dispatch.
   Each entry in *startup-modes* is a plist (handler-symbol &key :raw-args-p).
   :raw-args-p T modes receive the full argv tail; all others receive a single
   session name (defaulting to \"0\").
   Unrecognized or absent modes fall through to run-standalone."
  (let* ((argv    (%consume-global-socket-flags (%application-argv)))
         (mode    (first argv))
         (rest    (rest argv))
         (entry   (cdr (assoc mode *startup-modes* :test #'equal))))
    (%dispatch-startup-mode-entry entry mode rest)))

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
