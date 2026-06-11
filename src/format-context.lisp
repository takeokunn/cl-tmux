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

;;; ── Context builder ─────────────────────────────────────────────────────────

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
   Returns a path string, or empty string on failure (no PID, OS error, timeout)."
  (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
    (unless (and pid (> pid 0)) (return-from %pane-cwd-from-os ""))
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
          (when (plusp (length cwd)) (return-from %pane-cwd-from-os cwd)))))
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
          (or extracted-path ""))
      (error () ""))))

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

   Returns a plist of context keys; the body below is the authoritative list,
   with a per-key ;; comment naming the #{...} variable each one backs (e.g.
   :pane-at-top -> #{pane_at_top}).  An explicit enumeration here repeatedly
   drifted out of date as variables were added, so it is intentionally omitted —
   read the format-context-from-session body (and EXPAND-FORMAT) for the
   current, complete set."
  ;; session-active-window is the session's current window — distinct from
  ;; the WINDOW argument which is the window whose context we are building.
  ;; Naming it explicitly avoids confusion when both appear in the same binding.
  (let* ((session-name    (if session (cl-tmux/model:session-name session) ""))
         (session-wins    (if session (cl-tmux/model:session-windows session) nil))
         (session-active-window (if session (cl-tmux/model:session-active-window session) nil))
         (window-count    (length session-wins))
         ;; #{window_index}: the window's numeric id (respects base-index).
         (window-index    (if window (cl-tmux/model:window-id window) 0))
         (window-name     (if window (cl-tmux/model:window-name window) ""))
         (window-active   (if (and window session-active-window
                                   (eq window session-active-window)) "1" "0"))
         ;; #{window_raw_flags}: composite flag string (*=active, -=last, Z=zoomed),
         ;; "" when no flags apply (no single-space padding fallback).
         (window-raw-flags (%window-raw-flags window session-active-window session))
         ;; #{window_flags}: same as raw flags but padded to a single space when empty.
         (window-flags
          (if (zerop (length window-raw-flags)) " " window-raw-flags))
         ;; #{window_zoomed_flag}: "Z" when the window is zoomed, else " ".
         (window-zoomed-flag (if (and window (cl-tmux/model:window-zoom-p window)) "Z" " "))
         (window-panes    (if window (cl-tmux/model:window-panes window) nil))
         ;; #{pane_index}: the pane's numeric id (respects pane-base-index).
         (pane-index      (if pane (cl-tmux/model:pane-id pane) 0))
         ;; pane-title: prefer the explicit pane-title slot; fall back to the
         ;; screen-title set via OSC 0/2 when the pane has a live screen.
         (pane-title      (cond
                            ((null pane) "")
                            ((and (plusp (length (cl-tmux/model:pane-title pane))))
                             (cl-tmux/model:pane-title pane))
                            ((cl-tmux/model:pane-screen pane)
                             (cl-tmux/terminal:screen-title
                              (cl-tmux/model:pane-screen pane)))
                            (t "")))
         ;; #{pane_current_path}: OSC 7 cwd reported by the shell.
         ;; Falls back to OS proc query (lsof on macOS, /proc on Linux) when
         ;; the shell has not reported its cwd via OSC 7.
         (pane-current-path (let* ((scr (and pane (cl-tmux/model:pane-screen pane)))
                                   (osc-cwd (and scr (cl-tmux/terminal:screen-cwd scr))))
                              (if (and osc-cwd (plusp (length osc-cwd)))
                                  osc-cwd
                                  (%pane-cwd-from-os pane))))
         ;; #{pane_current_command}: foreground process name (via pgrep/ps, TTL-cached).
         (pane-current-command (%pane-current-command pane))
         ;; #{cursor_x} / #{cursor_y}: cursor position in the active pane screen.
         (pane-scr        (and pane (cl-tmux/model:pane-screen pane)))
         (cursor-x        (if pane-scr (cl-tmux/terminal:screen-cursor-x pane-scr) 0))
         (cursor-y        (if pane-scr (cl-tmux/terminal:screen-cursor-y pane-scr) 0))
         ;; #{pane_in_mode}: "1" when pane is in copy mode, else "0".
         (pane-in-mode    (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                              "1" "0"))
         ;; #{window_layout}: tmux layout string (checksum,geometry).
         (window-layout   (or (and window (cl-tmux/model:layout->string window)) ""))
         ;; #{pane_synchronized}: "1" when synchronize-panes option is on, else "0".
         ;; Prefer the window-local override (falls back to global then default);
         ;; fall back to the global read when WINDOW is nil.
         (pane-synchronized (if (cl-tmux/options:get-option-for-context
                                 "synchronize-panes" :window window)
                                "1" "0"))
         ;; #{window_activity_flag}: "#" when the window has unseen activity
         ;; (monitor-activity was triggered).  Cleared when the window is focused.
         (window-activity-flag
          (if (and window (cl-tmux/model:window-activity-flag window)) "#" " "))
         ;; #{window_silence_flag}: "~" when monitor-silence threshold exceeded.
         (window-silence-flag
          (if (and window (cl-tmux/model:window-silence-flag window)) "~" " "))
         ;; #{window_start_flag} / #{window_end_flag}: "1" for first/last window
         ;; in the session list.  Used by themes for list-end decorators.
         (window-start-flag
          (if (and window session-wins (eq window (first session-wins))) "1" "0"))
         (window-end-flag
          (if (and window session-wins (eq window (car (last session-wins)))) "1" "0"))
         ;; #{window_bell_flag}: "!" when any pane in the window has a pending bell
         ;; AND monitor-bell is on for the window (default on).  Used by status
         ;; themes to show an alert indicator; monitor-bell off suppresses it.
         (window-bell-flag
          (if (and window
                   (cl-tmux/options:get-option-for-context "monitor-bell" :window window)
                   (some (lambda (p)
                           (let ((scr (cl-tmux/model:pane-screen p)))
                             (and scr (cl-tmux/terminal:screen-bell-pending scr))))
                         (cl-tmux/model:window-panes window)))
              "!"
              " "))
         (hostname        (machine-instance))
         (time-str        (%current-time-string))
         (host-short      (%short-hostname hostname))
         ;; Environment variables available as format variables.
         ;; These allow theme files to detect the outer terminal (iTerm2, kitty, etc.)
         ;; and adjust rendering accordingly — same set as %if condition context.
         (term-program    (or (ignore-errors (sb-ext:posix-getenv "TERM_PROGRAM")) ""))
         (colorterm       (or (ignore-errors (sb-ext:posix-getenv "COLORTERM")) "")))
    (list ;; Raw SESSION object — carried so the #{W:...} window-iteration
          ;; modifier can walk the session's windows and build a per-window
          ;; context.  Internal (not a #{...} variable); ignored by lookups.
          :%session      session
          :session-name  session-name
          ;; #{session_id}: numeric session identifier.
          :session-id    (if session (cl-tmux/model:session-id session) 0)
          :window-index  window-index
          ;; #{window_id}: numeric window identifier (window-id slot).
          :window-id     (if window (cl-tmux/model:window-id window) 0)
          :window-name   window-name
          :window-count  window-count
          ;; #{session_windows}: tmux's name for the window count.
          :session-windows window-count
          :window-active window-active
          :window-flags  window-flags
          ;; #{window_raw_flags}: same flags but "" (not " ") when empty.
          :window-raw-flags window-raw-flags
          ;; #{window_zoomed_flag}: "Z" when the active pane is zoomed.
          :window-zoomed-flag window-zoomed-flag
          ;; #{window_panes}: number of panes in this window.
          :window-panes  (length window-panes)
          ;; #{window_layout}: layout serialization string.
          :window-layout window-layout
          :pane-index    pane-index
          :pane-title    pane-title
          ;; #{pane_tty}: the pane's slave PTY device path (e.g. /dev/pts/3).
          :pane-tty      (if pane (cl-tmux/model:pane-tty pane) "")
          ;; Internal: the pane object itself, used ONLY by the #{C:term} content
          ;; search to read the visible grid lazily (it is never surfaced as a
          ;; #{...} variable — %variable-to-keyword cannot produce a "%"-prefixed
          ;; keyword, so there is no collision with a user format name).
          :%c-search-pane pane
          ;; #{pane_current_path}: OSC 7 cwd reported by the shell.
          :pane-current-path pane-current-path
          ;; Structural pane variables, all pure functions of the pane struct.
          :pane-id       (if pane (cl-tmux/model:pane-id     pane) 0)
          :pane-width    (if pane (cl-tmux/model:pane-width  pane) 0)
          :pane-height   (if pane (cl-tmux/model:pane-height pane) 0)
          :pane-pid      (if pane (cl-tmux/model:pane-pid    pane) 0)
          :pane-left     (if pane (cl-tmux/model:pane-x      pane) 0)
          :pane-top      (if pane (cl-tmux/model:pane-y      pane) 0)
          ;; #{pane_right}/#{pane_bottom}: the INCLUSIVE far-edge column/row of the
          ;; pane (origin + size - 1), matching tmux's wp->xoff+sx-1 / yoff+sy-1.
          ;; Complements pane_left/pane_top; used by geometry-aware status themes.
          :pane-right    (if pane (+ (cl-tmux/model:pane-x pane)
                                     (cl-tmux/model:pane-width pane) -1) 0)
          :pane-bottom   (if pane (+ (cl-tmux/model:pane-y pane)
                                     (cl-tmux/model:pane-height pane) -1) 0)
          ;; Geometry-derived variables: the window's layout dimensions and the
          ;; pane's adjacency to the window edges, all pure functions of the
          ;; window/pane structs.  pane_at_* are "1"/"0" flag strings (like
          ;; pane_active).  pane_at_bottom/right compare the pane's far edge
          ;; (origin + size) against the window's height/width.
          :window-width   (if window (cl-tmux/model:window-width  window) 0)
          :window-height  (if window (cl-tmux/model:window-height window) 0)
          :pane-at-top    (if (and pane (= (cl-tmux/model:pane-y pane) 0)) "1" "0")
          :pane-at-left   (if (and pane (= (cl-tmux/model:pane-x pane) 0)) "1" "0")
          :pane-at-bottom (if (and pane window
                                   (= (+ (cl-tmux/model:pane-y pane) (cl-tmux/model:pane-height pane))
                                      (cl-tmux/model:window-height window)))
                              "1" "0")
          :pane-at-right  (if (and pane window
                                   (= (+ (cl-tmux/model:pane-x pane) (cl-tmux/model:pane-width pane))
                                      (cl-tmux/model:window-width window)))
                              "1" "0")
          ;; #{pane_active}: "1" when PANE is its window's active pane, else "0".
          :pane-active   (if (and pane window
                                  (eq pane (cl-tmux/model:window-active-pane window)))
                             "1" "0")
          ;; #{cursor_x} / #{cursor_y}: 0-based cursor position.
          :cursor-x      cursor-x
          :cursor-y      cursor-y
          ;; #{cursor_character}: the glyph currently under the cursor, as a
          ;; one-character string; "" when there is no pane or the cursor is out
          ;; of the grid.  Bounds-checked because screen-cell is a raw aref.
          :cursor-character
          (if (and pane-scr
                   (< -1 cursor-x (cl-tmux/terminal:screen-width  pane-scr))
                   (< -1 cursor-y (cl-tmux/terminal:screen-height pane-scr)))
              (string (cl-tmux/terminal:cell-char
                       (cl-tmux/terminal:screen-cell pane-scr cursor-x cursor-y)))
              "")
          ;; #{pane_in_mode}: "1" when copy mode active, else "0".
          :pane-in-mode  pane-in-mode
          ;; #{pane_current_command}: foreground process name (TTL-cached via pgrep/ps).
          :pane-current-command pane-current-command
          :hostname      hostname
          :host          hostname
          :host-short    host-short
          :time          time-str
          :client-width  client-width
          :client-height client-height
          :client-tty    client-tty
          ;; #{client_name}: the client's name.  tmux defaults a client's name to
          ;; its tty path, so we mirror client-tty here (empty when no tty known).
          :client-name   client-tty
          ;; #{client_session}: name of the session this client is viewing (= #S).
          :client-session session-name
          ;; #{client_termname}: the client terminal's TERM (xterm-256color, …).
          :client-termname (or (ignore-errors (sb-ext:posix-getenv "TERM")) "")
          ;; #{client_pid}: PID of the client process.  cl-tmux is single-process,
          ;; so the client and server share a PID (same idiom as #{server_pid}).
          :client-pid    (let ((getpid (ignore-errors (find-symbol "GETPID" "SB-POSIX"))))
                           (if getpid
                               (format nil "~D" (ignore-errors (funcall getpid)))
                               "0"))
          ;; #{version}: cl-tmux version string (matches tmux 3.x format for compat).
          :version       "3.5"
          ;; #{session_attached}: "1" when clients are attached, else "0".
          :session-attached (if (and session
                                     (cl-tmux/model:session-clients session))
                                "1" "0")
          ;; #{server_pid}: PID of the cl-tmux server process (via sb-posix when available).
          :server-pid    (let ((getpid (ignore-errors (find-symbol "GETPID" "SB-POSIX"))))
                           (if getpid
                               (format nil "~D" (ignore-errors (funcall getpid)))
                               "0"))
          ;; #{session_last_attached}: universal-time of last access.
          :session-last-attached (if session
                                     (format nil "~D"
                                             (cl-tmux/model:session-last-active session))
                                     "0")
          ;; #{pane_format}: always "1" in context (we have a pane).
          :pane-format (if pane "1" "0")
          ;; #{window_format}: always "1" in context.
          :window-format (if window "1" "0")
          ;; #{pane_synchronized}: reflects synchronize-panes option.
          :pane-synchronized pane-synchronized
          ;; #{window_bell_flag}: "!" when a pane in the window has a pending bell.
          :window-bell-flag window-bell-flag
          ;; #{window_activity_flag}: "#" when monitor-activity was triggered.
          :window-activity-flag window-activity-flag
          ;; #{window_silence_flag}: "~" when monitor-silence threshold exceeded.
          :window-silence-flag window-silence-flag
          ;; #{window_start_flag} / #{window_end_flag}: first/last in session list.
          :window-start-flag window-start-flag
          :window-end-flag   window-end-flag
          ;; #{scroll_position}: scrollback offset in copy mode, else "".
          :scroll-position (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                               (format nil "~D" (cl-tmux/terminal:screen-copy-offset pane-scr))
                               "")
          ;; #{selection_active}: "1" when copy mode has an active selection.
          :selection-active (if (and pane-scr
                                     (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                     (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                "1" "0")
          ;; #{selection_present}: "1" when a copy-mode selection has been started
          ;; (tmux uses this to gate selection-dependent status text).  Same
          ;; underlying state as selection_active in our single-selection model.
          :selection-present (if (and pane-scr
                                      (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                      (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                 "1" "0")
          ;; #{copy_cursor_x}/#{copy_cursor_y}: copy-mode cursor column/row, "" when
          ;; the pane is not in copy mode.  screen-copy-cursor is a (row . col) cons.
          :copy-cursor-x (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                             (format nil "~D"
                                     (cdr (cl-tmux/terminal:screen-copy-cursor pane-scr)))
                             "")
          :copy-cursor-y (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                             (format nil "~D"
                                     (car (cl-tmux/terminal:screen-copy-cursor pane-scr)))
                             "")
          ;; #{pane_marked}: "1" when the pane is marked, else "0".
          :pane-marked (if (and pane (cl-tmux/model:pane-marked pane)) "1" "0")
          ;; #{pane_input_off}: "1" when pane input is disabled (select-pane -d).
          :pane-input-off (if (and pane (cl-tmux/model:pane-input-disabled pane)) "1" "0")
          ;; #{pane_dead}: "1" when the pane's PTY has closed (remain-on-exit case).
          ;; A pane is dead when its fd is closed (fd <= 0) but it still exists.
          :pane-dead   (if (and pane (<= (cl-tmux/model:pane-fd pane) 0)) "1" "0")
          ;; #{pane_pipe}: "1" when output is being piped (pipe-pane active), else "0".
          :pane-pipe   (if (and pane (cl-tmux/model:pane-pipe-fd pane)) "1" "0")
          ;; #{session_count}: total number of sessions in *server-sessions*.
          ;; Accessed via qualified name because *server-sessions* lives in cl-tmux.
          ;; Falls back to 1 (this session) when the registry is empty or unbound.
          :session-count (format nil "~D"
                                 (max 1 (ignore-errors
                                          (length (symbol-value
                                                   (find-symbol "*SERVER-SESSIONS*"
                                                                "CL-TMUX"))))))
          ;; #{session_group}: session group identifier (empty string when not grouped).
          :session-group (if (and session (cl-tmux/model:session-group session))
                             (format nil "~A" (cl-tmux/model:session-group session))
                             "")
          ;; #{pane_mode}: mode name when the pane is in a special mode.
          ;; "copy-mode" when in copy mode, "" otherwise.
          :pane-mode   (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                           "copy-mode" "")
          ;; Environment variables for terminal detection in themes.
          :term-program term-program
          :colorterm    colorterm
          ;; #{client_prefix}: "1" when the prefix key has been pressed and we're
          ;; waiting for the next key; "0" otherwise.  Used by prefix-highlight plugins.
          ;; Reads *prefix-active* from events-loop.lisp (accessed via qualified name).
          :client-prefix (if (ignore-errors
                               (symbol-value
                                (find-symbol "*PREFIX-ACTIVE*" "CL-TMUX")))
                             "1" "0")
          ;; #{client_last_session}: name of the previously active session.
          ;; Used by some plugins to show a "back" indicator.
          :client-last-session ""
          ;; #{window_visible_layout}: layout string for the visible portion.
          ;; Same as #{window_layout} in our implementation.
          :window-visible-layout (or (and window (cl-tmux/model:layout->string window)) "")
          ;; #{session_path}: initial working directory for the session.
          :session-path (ignore-errors (sb-posix:getcwd))
          ;; #{history_size}: number of lines in the active pane's scrollback.
          :history-size (format nil "~D"
                                (if pane-scr
                                    (length (cl-tmux/terminal:screen-scrollback pane-scr))
                                    0))
          ;; #{history_limit}: configured history limit.
          :history-limit (format nil "~D"
                                 (or (cl-tmux/options:get-option "history-limit") 2000))
          ;; #{window_last_flag}: "1" when this is the last (previously active) window.
          :window-last-flag (if (and window session
                                     (eq window (cl-tmux/model:session-last-window session)))
                                "1" "0"))))

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
