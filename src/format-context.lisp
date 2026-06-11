(in-package #:cl-tmux/format)

;;;; Context builder for expand-format.
;;;;
;;;; This file is the DATA layer that maps live model objects (session, window,
;;;; pane) to the format-context plist consumed by expand-format in format.lisp.
;;;;
;;;; Data / logic separation:
;;;;   DATA   — format-context-from-session builds the plist from model objects
;;;;   LOGIC  — expand-format (format.lisp) consumes the plist; never touches model
;;;;
;;;; OS introspection (pgrep/ps for pane_current_command, lsof/proc for cwd)
;;;; is intentionally co-located here: both are context-building concerns.

;;; ── Context builder helpers ─────────────────────────────────────────────────

(defun %window-raw-flags (window session-active-window session)
  "Compute the raw window flag string for WINDOW within a SESSION context.
   Returns a string containing zero or more flag characters:
     * = this window is the session-active-window (current)
     - = this window was the previously-active window (last)
     Z = this window has a zoomed pane
   Returns the empty string when no flags apply.  The caller pads to a space
   for #{window_flags}; this function returns the unpadded raw form for
   #{window_raw_flags}."
  (let ((flags ""))
    (when window
      ;; * = current/active window
      (when (and session-active-window (eq window session-active-window))
        (setf flags (concatenate 'string flags "*")))
      ;; - = last window (was previously active and has a positive last-active-time)
      (when (and session
                 (not (eq window session-active-window))
                 ;; Only mark as last if the window has actually been active before
                 (> (cl-tmux/model:window-last-active-time window) 0)
                 (eq window (cl-tmux/model:session-last-window session)))
        (setf flags (concatenate 'string flags "-")))
      ;; Z = zoomed
      (when (cl-tmux/model:window-zoom-p window)
        (setf flags (concatenate 'string flags "Z"))))
    flags))

(defun %current-time-string ()
  "Return HH:MM string from the system clock."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %short-hostname (h)
  "Return the hostname up to the first dot, or the full string if no dot."
  (subseq h 0 (or (position #\. h) (length h))))

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

(defun %fetch-pane-command (pid)
  "Query the OS for the foreground command running in PID's terminal.
   Spawns pgrep -P PID to find the youngest child process, then ps -o comm= to
   retrieve its name.  Only the first child PID line is used (pgrep may list
   several).  Returns a command name string on success, or NIL on any failure
   (pgrep/ps not available, no children, process already gone, timeout, etc.)."
  (handler-case
      (let ((child-out (string-trim " \t\n\r"
                          (uiop:run-program
                           (list "pgrep" "-P" (format nil "~D" pid))
                           :output :string :ignore-error-status t
                           :timeout 1))))
        (when (plusp (length child-out))
          ;; pgrep returns one PID per line; take the first
          (let ((first-cpid (string-trim " \t\r"
                              (first (uiop:split-string child-out
                                                        :separator '(#\Newline))))))
            (when (and (plusp (length first-cpid))
                       (every #'digit-char-p first-cpid))
              (let ((name (string-trim " \t\n\r"
                            (uiop:run-program
                             (list "ps" "-o" "comm=" "-p" first-cpid)
                             :output :string :ignore-error-status t
                             :timeout 1))))
                (when (plusp (length name)) name))))))
    (error () nil)))

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
           (let ((cwd (handler-case
                          (string-trim " \t\n\r"
                                       (uiop:run-program
                                        (list "readlink" proc-path)
                                        :output :string :ignore-error-status t
                                        :timeout 1))
                        (error () ""))))
             (when (plusp (length cwd)) cwd))))
       ;; macOS: lsof reports the cwd as file descriptor 'cwd'.
       ;; Try both full path (/usr/sbin/lsof) and bare name in case PATH varies.
       (handler-case
           (let* ((lsof-binary (or (and (probe-file "/usr/sbin/lsof") "/usr/sbin/lsof")
                                   "lsof"))
                  (lsof-output (string-trim " \t\n\r"
                                 (uiop:run-program
                                  (list lsof-binary "-p" (format nil "~D" pid)
                                        "-a" "-d" "cwd" "-Fn")
                                  :output :string :ignore-error-status t
                                  :timeout 2)))
                  (extracted-path (%lsof-extract-cwd lsof-output)))
             (when (and extracted-path (plusp (length extracted-path)))
               extracted-path))
         (error () nil))))))

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

;;; ── Context plist helpers ───────────────────────────────────────────────────
;;;
;;; These helpers extract sub-computations from format-context-from-session to
;;; keep that function readable.  Each is a focused unit covering one domain.

(defun %window-has-pending-bell-p (window)
  "True when WINDOW has monitor-bell on and at least one pane has a pending bell."
  (and window
       (cl-tmux/options:get-option-for-context "monitor-bell" :window window)
       (some (lambda (p)
               (let ((scr (cl-tmux/model:pane-screen p)))
                 (and scr (cl-tmux/terminal:screen-bell-pending scr))))
             (cl-tmux/model:window-panes window))))

(defun %process-pid-string ()
  "Current process PID as a decimal string, or \"0\" when unavailable.
   Used for both #{client_pid} and #{server_pid} in the single-process model."
  (let ((getpid (ignore-errors (find-symbol "GETPID" "SB-POSIX"))))
    (if getpid (format nil "~D" (ignore-errors (funcall getpid))) "0")))

(defun %server-session-count-string ()
  "Total sessions in *server-sessions* as a decimal string, minimum 1.
   Accesses cl-tmux:*server-sessions* by name to avoid a circular package dependency."
  (format nil "~D"
          (max 1 (ignore-errors
                   (length (symbol-value (find-symbol "*SERVER-SESSIONS*" "CL-TMUX")))))))

;;; ── Context builder ─────────────────────────────────────────────────────────

(defun format-context-from-session (session window pane
                                    &key (client-width 0) (client-height 0)
                                         (client-tty ""))
  "Build a context plist for EXPAND-FORMAT from SESSION, WINDOW, and PANE.
   Any of SESSION, WINDOW, PANE may be NIL; missing slots default to safe
   empty values.

   Optional keyword arguments supply client dimensions and tty path:
     :CLIENT-WIDTH   — terminal width reported to the client (default 0)
     :CLIENT-HEIGHT  — terminal height reported to the client (default 0)
     :CLIENT-TTY     — path to the client tty device (default \"\")

   Returns a plist of context keys.  The authoritative list of all #{...}
   variables is the set of keywords in the returned plist — read the append
   sections below for the current, complete set."
  (let* ((session-wins          (if session (cl-tmux/model:session-windows session) nil))
         (session-active-window (if session (cl-tmux/model:session-active-window session) nil))
         (window-count          (length session-wins))
         (window-raw-flags      (%window-raw-flags window session-active-window session))
         (window-flags          (if (zerop (length window-raw-flags)) " " window-raw-flags))
         (window-panes          (if window (cl-tmux/model:window-panes window) nil))
         (window-layout         (or (and window (cl-tmux/model:layout->string window)) ""))
         ;; pane-title: prefer explicit slot; fall back to OSC 0/2 screen-title.
         (pane-title            (cond
                                  ((null pane) "")
                                  ((plusp (length (cl-tmux/model:pane-title pane)))
                                   (cl-tmux/model:pane-title pane))
                                  ((cl-tmux/model:pane-screen pane)
                                   (cl-tmux/terminal:screen-title (cl-tmux/model:pane-screen pane)))
                                  (t "")))
         ;; pane-current-path: OSC 7 → OS proc query fallback.
         (pane-current-path     (let* ((scr (and pane (cl-tmux/model:pane-screen pane)))
                                       (osc-cwd (and scr (cl-tmux/terminal:screen-cwd scr))))
                                  (if (and osc-cwd (plusp (length osc-cwd)))
                                      osc-cwd (%pane-cwd-from-os pane))))
         (pane-scr              (and pane (cl-tmux/model:pane-screen pane)))
         (cursor-x              (if pane-scr (cl-tmux/terminal:screen-cursor-x pane-scr) 0))
         (cursor-y              (if pane-scr (cl-tmux/terminal:screen-cursor-y pane-scr) 0))
         (pane-synchronized     (if (cl-tmux/options:get-option-for-context
                                     "synchronize-panes" :window window) "1" "0"))
         (hostname              (machine-instance))
         (pid-str               (%process-pid-string)))
    (append
      ;; ── Session-scoped variables ──────────────────────────────────────────
      (list :%session              session
            :session-id            (if session (cl-tmux/model:session-id session) 0)
            :session-name          (if session (cl-tmux/model:session-name session) "")
            :session-windows       window-count
            :session-attached      (if (and session (cl-tmux/model:session-clients session)) "1" "0")
            :session-last-attached (if session
                                       (format nil "~D" (cl-tmux/model:session-last-active session))
                                       "0")
            :session-group         (if (and session (cl-tmux/model:session-group session))
                                       (format nil "~A" (cl-tmux/model:session-group session)) "")
            :session-count         (%server-session-count-string)
            :session-path          (ignore-errors (sb-posix:getcwd))
            :client-session        (if session (cl-tmux/model:session-name session) ""))
      ;; ── Window-scoped variables ───────────────────────────────────────────
      (list :window-index          (if window (cl-tmux/model:window-id window) 0)
            :window-id             (if window (cl-tmux/model:window-id window) 0)
            :window-name           (if window (cl-tmux/model:window-name window) "")
            :window-count          window-count
            :window-active         (if (and window session-active-window
                                            (eq window session-active-window)) "1" "0")
            :window-flags          window-flags
            :window-raw-flags      window-raw-flags
            :window-zoomed-flag    (if (and window (cl-tmux/model:window-zoom-p window)) "Z" " ")
            :window-panes          (length window-panes)
            :window-layout         window-layout
            :window-visible-layout window-layout
            :window-width          (if window (cl-tmux/model:window-width  window) 0)
            :window-height         (if window (cl-tmux/model:window-height window) 0)
            :window-format         (if window "1" "0")
            :window-bell-flag      (if (%window-has-pending-bell-p window) "!" " ")
            :window-activity-flag  (if (and window (cl-tmux/model:window-activity-flag window)) "#" " ")
            :window-silence-flag   (if (and window (cl-tmux/model:window-silence-flag window)) "~" " ")
            :window-start-flag     (if (and window session-wins (eq window (first session-wins))) "1" "0")
            :window-end-flag       (if (and window session-wins
                                            (eq window (car (last session-wins)))) "1" "0")
            :window-last-flag      (if (and window session
                                            (eq window (cl-tmux/model:session-last-window session)))
                                       "1" "0"))
      ;; ── Pane structural variables ─────────────────────────────────────────
      (list :%c-search-pane       pane
            :pane-index           (if pane (cl-tmux/model:pane-id pane) 0)
            :pane-id              (if pane (cl-tmux/model:pane-id pane) 0)
            :pane-title           pane-title
            :pane-tty             (if pane (cl-tmux/model:pane-tty pane) "")
            :pane-current-path    pane-current-path
            :pane-current-command (%pane-current-command pane)
            :pane-format          (if pane "1" "0")
            :pane-active          (if (and pane window
                                           (eq pane (cl-tmux/model:window-active-pane window)))
                                      "1" "0")
            :pane-synchronized    pane-synchronized
            :pane-marked          (if (and pane (cl-tmux/model:pane-marked pane)) "1" "0")
            :pane-input-off       (if (and pane (cl-tmux/model:pane-input-disabled pane)) "1" "0")
            :pane-dead            (if (and pane (<= (cl-tmux/model:pane-fd pane) 0)) "1" "0")
            :pane-pipe            (if (and pane (cl-tmux/model:pane-pipe-fd pane)) "1" "0"))
      ;; ── Pane geometry variables ───────────────────────────────────────────
      (list :pane-width           (if pane (cl-tmux/model:pane-width  pane) 0)
            :pane-height          (if pane (cl-tmux/model:pane-height pane) 0)
            :pane-pid             (if pane (cl-tmux/model:pane-pid    pane) 0)
            :pane-left            (if pane (cl-tmux/model:pane-x      pane) 0)
            :pane-top             (if pane (cl-tmux/model:pane-y      pane) 0)
            :pane-right           (if pane (+ (cl-tmux/model:pane-x pane)
                                              (cl-tmux/model:pane-width pane) -1) 0)
            :pane-bottom          (if pane (+ (cl-tmux/model:pane-y pane)
                                              (cl-tmux/model:pane-height pane) -1) 0)
            :pane-at-top          (if (and pane (= (cl-tmux/model:pane-y pane) 0)) "1" "0")
            :pane-at-left         (if (and pane (= (cl-tmux/model:pane-x pane) 0)) "1" "0")
            :pane-at-bottom       (if (and pane window
                                           (= (+ (cl-tmux/model:pane-y    pane)
                                                 (cl-tmux/model:pane-height pane))
                                              (cl-tmux/model:window-height window)))
                                      "1" "0")
            :pane-at-right        (if (and pane window
                                           (= (+ (cl-tmux/model:pane-x   pane)
                                                 (cl-tmux/model:pane-width pane))
                                              (cl-tmux/model:window-width window)))
                                      "1" "0"))
      ;; ── Screen / copy-mode variables ──────────────────────────────────────
      (list :cursor-x             cursor-x
            :cursor-y             cursor-y
            :cursor-character
            (if (and pane-scr
                     (< -1 cursor-x (cl-tmux/terminal:screen-width  pane-scr))
                     (< -1 cursor-y (cl-tmux/terminal:screen-height pane-scr)))
                (string (cl-tmux/terminal:cell-char
                         (cl-tmux/terminal:screen-cell pane-scr cursor-x cursor-y)))
                "")
            :pane-in-mode         (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr)) "1" "0")
            :pane-mode            (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr)) "copy-mode" "")
            :scroll-position      (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                      (format nil "~D" (cl-tmux/terminal:screen-copy-offset pane-scr))
                                      "")
            :selection-active     (if (and pane-scr
                                           (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                           (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                      "1" "0")
            :selection-present    (if (and pane-scr
                                           (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                           (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                      "1" "0")
            :copy-cursor-x        (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                      (format nil "~D" (cdr (cl-tmux/terminal:screen-copy-cursor pane-scr)))
                                      "")
            :copy-cursor-y        (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                      (format nil "~D" (car (cl-tmux/terminal:screen-copy-cursor pane-scr)))
                                      "")
            :history-size         (format nil "~D"
                                          (if pane-scr
                                              (length (cl-tmux/terminal:screen-scrollback pane-scr))
                                              0)))
      ;; ── Client, server, host, and environment variables ──────────────────
      (list :client-width         client-width
            :client-height        client-height
            :client-tty           client-tty
            :client-name          client-tty
            :client-termname      (or (ignore-errors (sb-ext:posix-getenv "TERM")) "")
            :client-pid           pid-str
            :client-prefix        (if (ignore-errors
                                        (symbol-value (find-symbol "*PREFIX-ACTIVE*" "CL-TMUX")))
                                      "1" "0")
            :client-last-session  ""
            :server-pid           pid-str
            :version              "3.5"
            :hostname             hostname
            :host                 hostname
            :host-short           (%short-hostname hostname)
            :time                 (%current-time-string)
            :term-program         (or (ignore-errors (sb-ext:posix-getenv "TERM_PROGRAM")) "")
            :colorterm            (or (ignore-errors (sb-ext:posix-getenv "COLORTERM")) "")
            :history-limit        (format nil "~D"
                                          (or (cl-tmux/options:get-option "history-limit") 2000))))))

(defun format-context-from-window (session window
                                   &key (client-width 0) (client-height 0)
                                        (client-tty ""))
  "Build a context plist for per-window format strings (e.g. window-status-format).
   Specialised for a single WINDOW: delegates to FORMAT-CONTEXT-FROM-SESSION with
   the window's first pane, so it returns that function's full key set (see its
   body for the authoritative, complete list).  Any argument may be NIL."
  (format-context-from-session session window
                               (when window
                                 (first (cl-tmux/model:window-panes window)))
                               :client-width  client-width
                               :client-height client-height
                               :client-tty    client-tty))
