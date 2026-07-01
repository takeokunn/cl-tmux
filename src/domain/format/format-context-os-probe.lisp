(in-package #:cl-tmux/format)

;;;; OS introspection probes for format-context.
;;;;
;;;; These functions query the operating system (pgrep/ps for
;;;; #{pane_current_command}, /proc or lsof for #{pane_current_path}) and are
;;;; kept separate from the pure plist-building logic in format-context.lisp:
;;;; every function here performs process spawning or filesystem I/O, while
;;;; format-context.lisp only reads already-fetched values.

;;; ── #{pane_current_command} via pgrep/ps ────────────────────────────────────
;;;
;;; The foreground command of a pane's PTY is the youngest child of the shell
;;; process (pane-pid).  pgrep -P <pid> lists children; ps -o comm= formats
;;; the name.  Results are cached per (pid . cache-time) to avoid spawning
;;; two subprocesses on every render cycle.

(defvar *pane-command-cache* (make-hash-table :test #'eql)
  "TTL cache mapping an integer PID to a (universal-time . command-name) cons.
   The CAR is the CL universal-time of the last query; the CDR is the foreground
   command name string returned by pgrep/ps.  Entries older than
   +PANE-COMMAND-CACHE-TTL+ seconds are re-queried on the next access.")

(defconstant +pane-command-cache-ttl+ 2
  "Seconds before #{pane_current_command} is re-queried from the OS.")

(defconstant +pane-command-probe-timeout+ 1
  "Seconds to allow pgrep/ps foreground-command probes to run.")

(defconstant +pane-cwd-proc-timeout+ 1
  "Seconds to allow /proc cwd probes to run.")

(defconstant +pane-cwd-lsof-timeout+ 2
  "Seconds to allow lsof cwd probes to run.")

(defun %run-format-probe (args timeout)
  "Run a short OS probe and return trimmed stdout, or the empty string on failure."
  (handler-case
      (string-trim " \t\n\r"
                   (uiop:run-program args
                                     :output :string
                                     :ignore-error-status t
                                     :timeout timeout))
    (error () "")))

(defun %fetch-pane-command (pid)
  "Query the OS for the foreground command running in PID's terminal.
   Spawns pgrep -P PID to find the youngest child process, then ps -o comm= to
   retrieve its name.  Only the first child PID line is used (pgrep may list
   several).  Returns a command name string on success, or NIL on any failure
   (pgrep/ps not available, no children, process already gone, timeout, etc.)."
  (let ((child-out (%run-format-probe (list "pgrep" "-P" (format nil "~D" pid))
                                      +pane-command-probe-timeout+)))
    (when (plusp (length child-out))
      ;; pgrep returns one PID per line; take the first
      (let ((first-cpid (string-trim " \t\r"
                                     (first (uiop:split-string child-out
                                                               :separator '(#\Newline))))))
        (when (and (plusp (length first-cpid))
                   (every #'digit-char-p first-cpid))
          (let ((name (%run-format-probe (list "ps" "-o" "comm=" "-p" first-cpid)
                                         +pane-command-probe-timeout+)))
            (when (plusp (length name)) name)))))))

(defun %lsof-extract-cwd (lsof-output)
  "Extract the current working directory path from LSOF-OUTPUT (the text returned
   by lsof -Fn).  lsof -Fn prints file-name lines as 'nPATH'; this function returns
   the PATH part of the first such line whose character after 'n' is non-empty.
   Returns NIL when no suitable line is found."
  (dolist (line (uiop:split-string lsof-output :separator '(#\Newline)) nil)
    (when (and (> (length line) 1) (char= (char line 0) #\n))
      (let ((path (subseq line 1)))
        (when (plusp (length path))
          (return path))))))

(defun %pane-cwd-from-os (pane)
  "Query the OS for the current working directory of PANE's shell process.
   On Linux reads /proc/PID/cwd via readlink; on macOS uses lsof -p PID -a -d cwd.
   Returns a non-empty path string, or NIL on failure (no PID, OS error, timeout)."
  (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
    (when (and pid (> pid 0))
      (or
       ;; Linux: /proc/PID/cwd is a symlink to the cwd.
       (let ((proc-path (format nil "/proc/~D/cwd" pid)))
         (when (probe-file proc-path)
           (let ((cwd (%run-format-probe (list "readlink" proc-path)
                                         +pane-cwd-proc-timeout+)))
             (when (plusp (length cwd)) cwd))))
       ;; macOS: lsof reports the cwd as file descriptor 'cwd'.
       ;; Try both full path (/usr/sbin/lsof) and bare name in case PATH varies.
       (let* ((lsof-binary (or (and (probe-file "/usr/sbin/lsof") "/usr/sbin/lsof")
                               "lsof"))
              (lsof-output (%run-format-probe
                            (list lsof-binary "-p" (format nil "~D" pid)
                                  "-a" "-d" "cwd" "-Fn")
                            +pane-cwd-lsof-timeout+))
              (extracted-path (%lsof-extract-cwd lsof-output)))
         (when (and extracted-path (plusp (length extracted-path)))
           extracted-path))))))

(defun %pane-current-command (pane)
  "Return the foreground command name for PANE's PTY process.
   Consults *PANE-COMMAND-CACHE* first; re-queries the OS via %FETCH-PANE-COMMAND
   only when the cached entry is missing or older than +PANE-COMMAND-CACHE-TTL+
   seconds.  Falls back to the shell basename when OS introspection is unavailable
   (no PID, pgrep/ps absent, or PID already gone)."
  (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
    (if (and pid (> pid 0))
        (let* ((cached (gethash pid *pane-command-cache*))
               (now    (get-universal-time))
               (stale  (or (null cached)
                           (> (- now (car cached)) +pane-command-cache-ttl+))))
          (if stale
              (let ((cmd (or (%fetch-pane-command pid)
                             (cl-tmux/model::%shell-basename))))
                (setf (gethash pid *pane-command-cache*) (cons now cmd))
                cmd)
              (cdr cached)))
        (cl-tmux/model::%shell-basename))))
