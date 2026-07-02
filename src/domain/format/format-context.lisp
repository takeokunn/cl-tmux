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
;;;; lives in format-context-os-probe.lisp, a distinct I/O-probing concern from
;;;; the pure plist-building logic in this file.
;;;;
;;;; The context plist is assembled from six section builders.  The three
;;;; model-facing builders (session / window / pane-structural) live in this
;;;; file; the three more mechanical getter-table builders (pane-geometry /
;;;; screen / client) live in format-context-screen.lisp.

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
  (with-output-to-string (s)
    (when window
      ;; * = current/active window
      (when (and session-active-window (eq window session-active-window))
        (write-char #\* s))
      ;; - = last window (was previously active and has a positive last-active-time)
      (when (and session
                 (not (eq window session-active-window))
                 ;; Only mark as last if the window has actually been active before
                 (> (cl-tmux/model:window-last-active-time window) 0)
                 (eq window (cl-tmux/model:session-last-window session)))
        (write-char #\- s))
      ;; Z = zoomed
      (when (cl-tmux/model:window-zoom-p window)
        (write-char #\Z s)))))

;;; ── Context plist helpers ───────────────────────────────────────────────────
;;;
;;; These helpers extract sub-computations from format-context-from-session to
;;; keep that function readable.  Each is a focused unit covering one domain.

(defun %window-has-pending-bell-p (window)
  "True when WINDOW has monitor-bell on and its sticky bell flag is set.
   The flag (tmux WINLINK_BELL) is set by the PTY reader when a non-current
   window rings BEL and cleared when the window is selected — so the status
   `!' persists until the window is viewed, matching tmux."
  (and window
       (cl-tmux/options:get-option-for-context "monitor-bell" :window window)
       (cl-tmux/model:window-bell-flag window)))

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

;;; ── Context plist section builders ──────────────────────────────────────────
;;;
;;; Each of these builds one logically-grouped slice of the full context
;;; plist. format-context-from-session appends the six slices together (three
;;; model-facing builders here; the more mechanical pane-geometry / screen /
;;; client getter-table builders live in format-context-screen.lisp).

(defun %session-context-plist (session window-count)
  "Build the session-scoped slice of the format-context plist for SESSION.
   WINDOW-COUNT is the pre-computed number of windows in SESSION."
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
        :client-session        (if session (cl-tmux/model:session-name session) "")))

(defun %window-context-plist (window session session-active-window session-windows
                              window-count window-panes window-flags window-raw-flags
                              window-layout)
  "Build the window-scoped slice of the format-context plist for WINDOW.
   SESSION-ACTIVE-WINDOW, SESSION-WINDOWS, WINDOW-COUNT, WINDOW-PANES,
   WINDOW-FLAGS, WINDOW-RAW-FLAGS, and WINDOW-LAYOUT are pre-computed by the
   caller so they need not be recomputed here."
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
        :window-start-flag     (if (and window session-windows (eq window (first session-windows))) "1" "0")
        :window-end-flag       (if (and window session-windows
                                        (eq window (first (last session-windows)))) "1" "0")
        :window-last-flag      (if (and window session
                                        (eq window (cl-tmux/model:session-last-window session)))
                                   "1" "0")))

(defun %pane-structural-context-plist (pane window pane-title pane-current-path pane-synchronized)
  "Build the pane-structural slice of the format-context plist for PANE.
   PANE-TITLE, PANE-CURRENT-PATH, and PANE-SYNCHRONIZED are pre-computed by
   the caller so they need not be recomputed here."
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
        :pane-pipe            (if (and pane (cl-tmux/model:pane-pipe-active-p pane)) "1" "0")))

;;; ── Context builder ─────────────────────────────────────────────────────────
;;;
;;; %pane-geometry-context-plist, %screen-context-plist, and
;;; %client-context-plist live in format-context-screen.lisp.

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

   Returns a plist of context keys, assembled from six section builders
   (session, window, pane-structural, pane-geometry, screen, client). The
   authoritative list of all #{...} variables is the set of keywords in the
   returned plist — read each section builder for its slice of the keys."
  (let* ((session-windows       (and session (cl-tmux/model:session-windows session)))
         (session-active-window (and session (cl-tmux/model:session-active-window session)))
         (window-count          (length session-windows))
         (window-raw-flags      (%window-raw-flags window session-active-window session))
         (window-flags          (if (zerop (length window-raw-flags)) " " window-raw-flags))
         (window-panes          (and window (cl-tmux/model:window-panes window)))
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
                                  (or (and osc-cwd (plusp (length osc-cwd)) osc-cwd)
                                      (%pane-cwd-from-os pane))))
         (pane-scr              (and pane (cl-tmux/model:pane-screen pane)))
         (cursor-x              (if pane-scr (cl-tmux/terminal:screen-cursor-x pane-scr) 0))
         (cursor-y              (if pane-scr (cl-tmux/terminal:screen-cursor-y pane-scr) 0))
         (pane-synchronized     (if (cl-tmux/options:get-option-for-context
                                     "synchronize-panes" :window window) "1" "0"))
         (hostname              (machine-instance))
         (pid-str               (%process-pid-string)))
    (append
     (%session-context-plist session window-count)
     (%window-context-plist window session session-active-window session-windows
                            window-count window-panes window-flags window-raw-flags
                            window-layout)
     (%pane-structural-context-plist pane window pane-title pane-current-path pane-synchronized)
     (%pane-geometry-context-plist pane window)
     (%screen-context-plist pane-scr cursor-x cursor-y)
     (%client-context-plist client-width client-height client-tty hostname pid-str))))

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
