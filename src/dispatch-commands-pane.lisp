(in-package #:cl-tmux)

;;; -- Window/pane/session structural commands ----------------------------------------
;;;
;;; Named-layout macro, select-layout, list-panes, new-window, split-window,
;;; new-session, switch-client, destroy-session, kill-session, resize-window,
;;; detach-client, and the copy-mode -X command table (for send-keys -X).

;;; -- Layout name → keyword dispatch macro ------------------------------------
;;;
;;; Each row is (aliases... keyword), Prolog-style: one fact per layout name.
;;; The macro generates a flat cond of (member name aliases :test #'string-equal)
;;; checks so adding a new layout requires appending one line here.

(defmacro define-layout-name-table (&rest rows)
  "Build %RESOLVE-LAYOUT-NAME from a declarative aliases→keyword table.
   Each ROW is (keyword alias-string...).  Generates a function that maps a
   layout name string to the corresponding keyword, or NIL for unknown names."
  `(defun %resolve-layout-name (name)
     "Map NAME (a string) to a layout keyword, or NIL when unrecognised."
     (cond
       ,@(mapcar (lambda (row)
                   (destructuring-bind (kw &rest aliases) row
                     `((member name ',aliases :test #'string-equal) ,kw)))
                 rows)
       (t nil))))

(define-layout-name-table
  (:even-horizontal "even-horizontal" "even-h")
  (:even-vertical   "even-vertical"   "even-v")
  (:main-horizontal "main-horizontal" "main-h")
  (:main-vertical   "main-vertical"   "main-v")
  (:tiled           "tiled"))

(defun %cmd-select-layout (session args)
  "select-layout <name>: apply the named layout to the active window.
   Accepted names: even-horizontal (even-h), even-vertical (even-v),
   main-horizontal (main-h), main-vertical (main-v), tiled."
  (let* ((name (first args))
         (kw   (and name (%resolve-layout-name name))))
    (when kw
      (%apply-named-layout-to-session session kw))))

(defun %cmd-list-panes (session args)
  "list-panes: list all panes in the active window (mirrors display-panes)."
  (declare (ignore args))
  (with-active-window (win session)
    (let ((panes (window-panes win)))
      (show-overlay
       (if panes
           (with-output-to-string (stream)
             (dolist (p panes)
               (format stream "~D: ~Dx~D at (~D,~D)~A~%"
                       (pane-id p)
                       (pane-width p) (pane-height p)
                       (pane-x p) (pane-y p)
                       (if (eq p (window-active-pane win)) " [active]" ""))))
           "(no panes)")))))

(defun %cmd-display-panes-arg (session args)
  "display-panes [-d duration]: show pane ids.
   The renderer owns the actual pane-number overlay; this command-line form only
   accepts the duration flag it implements."
  (with-command-input (flags positionals args "d"
                             :allowed-flags '(#\d)
                             :max-positionals 0
                             :message "display-panes: unsupported argument")
    (let* ((duration-str (cdr (assoc #\d flags)))
           (duration (and duration-str
                          (ignore-errors
                            (parse-integer duration-str :junk-allowed t))))
           (saved (cl-tmux/options:get-option "display-panes-time" 1000)))
      (unwind-protect
           (progn
             (when duration
               (cl-tmux/options:set-option "display-panes-time" duration))
             (dispatch-command session :display-panes nil))
        (when duration
          (cl-tmux/options:set-option "display-panes-time" saved))))))

(defun %format-pane-info (session win pane)
  "Return a short pane info string: session:window.pane geometry.
   Used by -P flag in new-window and split-window."
  (format nil "~A:~A.~A: [~Dx~D]"
          (session-name session)
          (if win (window-id win) "?")
          (if pane (pane-id pane) "?")
          (if pane (pane-width pane) 0)
          (if pane (pane-height pane) 0)))

(defun %expand-start-dir (session raw-dir)
  "Expand #{...} format variables in RAW-DIR using the current active pane
   as the format context.  Returns the expanded string, or NIL when RAW-DIR is NIL."
  (when raw-dir
    (multiple-value-bind (win pane) (%active-window-pane session)
      (let ((ctx (cl-tmux/format:format-context-from-session session win pane)))
        (cl-tmux/format:expand-format raw-dir ctx)))))

(defun %show-pane-info-overlay (session win pane print-fmt)
  "Show a transient pane-info overlay for the -P flag.
   Uses PRINT-FMT if given, otherwise the default session:window.pane summary."
  (show-transient-overlay
   (if print-fmt
       (cl-tmux/format:expand-format
        print-fmt
        (cl-tmux/format:format-context-from-session session win pane))
       (%format-pane-info session win pane))))

(defun %cmd-new-window-arg (session args)
  "new-window [-d] [-k] [-P] [-n name] [-t target-window] [-a] [-c start-dir] [-e VAR=val].
   -d: create the window but do not make it active (detached).
   -k: kill any existing window at the target index before creating the new one.
   -P: print the new pane's details (session:window.pane [WxH]) to overlay.
   -F format: with -P, the format string for the printed info (instead of the
     default session:window.pane [WxH]) — e.g. `new-window -dP -F '#{window_id}'`.
   -n name: name the new window.
   -t idx: insert at specific index (assigned as the window id).
   -a: insert after the current window.
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable)."
  (with-command-flags+pos (flags positionals args "ntceF")
    (declare (ignore positionals))
    (let* ((extra-env  (%collect-env-flags flags))
           (name       (cdr (assoc #\n flags)))
           (detach-p   (assoc #\d flags))
           (kill-p     (assoc #\k flags))
           (print-p    (assoc #\P flags))
           (print-fmt  (cdr (assoc #\F flags)))
           (after-p    (assoc #\a flags))
           (raw-dir    (cdr (assoc #\c flags)))
           (start-dir  (%expand-start-dir session raw-dir))
           (at-idx     (%parse-flag-int flags #\t)))
      ;; -k: if a window with the target index already exists, kill it first.
      (when (and kill-p at-idx)
        (let ((existing (find at-idx (session-windows session) :key #'window-id)))
          (when existing
            (%handle-kill-result (kill-window session existing)))))
      ;; Inject -e VAR=val pairs via *pane-extra-env* so %fork-pane picks them up.
      (when extra-env
        (setf *pane-extra-env* extra-env))
      (let ((new-win (%cmd-new-window session
                                      :name name
                                      :start-dir start-dir
                                      :detach (and detach-p t)
                                      :at-index at-idx
                                      :after-current (and after-p t))))
        ;; -P: print new pane details to overlay.
        (when (and print-p new-win)
          (%show-pane-info-overlay session new-win (window-active-pane new-win) print-fmt))
        new-win))))

(defun %parse-split-size (lines-str)
  "Parse a split-window/-l value: \"30\" → 30 (absolute cells, an integer), \"30%\"
   → 0.30 (a real fraction of the parent).  Returns NIL for a missing or
   non-numeric value."
  (when lines-str
    (let ((pct-pos (position #\% lines-str)))
      (if pct-pos
          (let ((n (parse-integer lines-str :end pct-pos :junk-allowed t)))
            (and n (/ n 100.0)))
          (parse-integer lines-str :junk-allowed t)))))

(defun %cmd-split-window (session args)
  "split-window [-h|-v] [-b] [-f] [-d] [-t target] [-l size] [-c start-dir] [-e VAR=val].
   -h: horizontal split (new pane to the right; side-by-side).
   -v: vertical split (new pane below — default).
   -b: insert before the active pane (left of / above) instead of after.
   -f: full-window split — the new pane spans the whole window dimension (the split
       is inserted at the layout root) instead of subdividing the active pane.
   -d: split but do not change focus (detached mode).
   -t target: split the target pane instead of the active pane.
   -l N: size in lines/columns (absolute integer), or -l N% as a percentage
     of the parent pane.
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable).
   -P: print the new pane's details to overlay.
   -F format: with -P, the format string for the printed info (instead of the
     default session:window.pane [WxH]) — e.g. `split-window -dP -F '#{pane_id}'`."
  (with-command-flags+pos (flags positionals args "lcetF")
    (declare (ignore positionals))
    (when (assoc #\p flags)
      (return-from %cmd-split-window nil))
    (let* ((extra-env    (%collect-env-flags flags))
           (horizontal-p (assoc #\h flags))
           (before-p     (assoc #\b flags))
           (full-p       (assoc #\f flags))
           (detach-p     (assoc #\d flags))
           (target-str   (cdr (assoc #\t flags)))
           (lines-str    (cdr (assoc #\l flags)))
           (raw-dir      (cdr (assoc #\c flags)))
           (start-dir    (%expand-start-dir session raw-dir))
           ;; -l N → N cells; -l N% → fraction.
           (size         (%parse-split-size lines-str)))
      ;; -t target: temporarily make the target pane active so %cmd-split
      ;; operates on it.  Restore the previous active pane afterwards if -d.
      (multiple-value-bind (prev-win prev-pane) (%active-window-pane session)
        (when target-str
          (multiple-value-bind (target-win target-pane)
              (%resolve-target-window-pane session target-str prev-win prev-pane)
            (when (and target-win target-pane)
              ;; Switch active window and pane to the target for the split.
              (session-select-window session target-win)
              (window-select-pane target-win target-pane))))
        ;; Inject -e VAR=val pairs via *pane-extra-env* so %fork-pane picks them up.
        (when extra-env
          (setf *pane-extra-env* extra-env))
        (let* ((print-p (assoc #\P flags))
               (result  (%cmd-split session (if horizontal-p :h :v)
                                    :size size :no-focus (and detach-p t)
                                    :start-dir start-dir :before (and before-p t)
                                    :full (and full-p t))))
          ;; Restore original focus when -d (detach).
          (when (and detach-p target-str prev-win)
            (session-select-window session prev-win)
            (when prev-pane (window-select-pane prev-win prev-pane)))
          ;; -P: print the new pane's details.
          (when (and print-p result)
            (%show-pane-info-overlay session (pane-window result) result
                                     (cdr (assoc #\F flags))))
          result)))))

(defun %parse-wxh (str)
  "Parse a \"WxH\" size string (e.g. the default-size option \"80x24\") into
   (values W H), or NIL when STR is not of that form or either dimension
   is not a positive integer."
  (when (stringp str)
    (let* ((x (position #\x str :test #'char-equal))
           (w (and x (parse-integer str :end x :junk-allowed t)))
           (h (and x (parse-integer str :start (1+ x) :junk-allowed t))))
      (when (and w h (plusp w) (plusp h))
        (values w h)))))

(defun %next-free-session-name ()
  "Return the lowest positive-integer string not already in use as a session name."
  (loop for i from 1
        for candidate = (format nil "~D" i)
        unless (server-find-session candidate) return candidate))

(defun %cmd-new-session-arg (session args)
  "new-session [-A] [-d] [-s name] [-n window-name] [-c start-dir] [-x width] [-y height]: create a new session.
   -A: if a session named NAME already exists, attach to it instead of creating a new one.
   -d: create detached (do not switch to the new session).
   -s name: session name.
   -n name: initial window name.
   -c dir: start directory for the initial window's shell.
   -x width: initial columns (default: terminal width, or default-size when -d).
   -y height: initial rows (default: terminal height minus status bar, or
     default-size when -d).
   A DETACHED session (-d) has no client to size it, so — like tmux — it uses the
   default-size option (\"WxH\", default 80x24) when -x/-y are not given."
  (with-command-flags+pos (flags positionals args "sncxyt")
    (declare (ignore positionals))
    (let* ((name            (or (cdr (assoc #\s flags))
                                (format nil "~D" (1+ (length *server-sessions*)))))
           (attach-if-exists (assoc #\A flags))
           (detach-p         (assoc #\d flags))
           (win-name         (cdr (assoc #\n flags)))
           ;; -t <group>: the new session JOINS an existing session's group,
           ;; sharing its window list (tmux "grouped sessions").
           (group-target     (cdr (assoc #\t flags)))
           (start-dir        (cdr (assoc #\c flags)))
           ;; Detached sessions have no client → fall back to default-size, not the
           ;; current terminal size.  NIL for attached sessions (use the terminal).
           (default-wxh      (and detach-p
                                  (cl-tmux/options:get-option "default-size" "80x24")))
           ;; -x/-y override everything when given (junk-allowed).
           (cols             (or (%parse-flag-int flags #\x)
                                 (and default-wxh (nth-value 0 (%parse-wxh default-wxh)))
                                 *term-cols*))
           (rows             (or (%parse-flag-int flags #\y)
                                 (and default-wxh (nth-value 1 (%parse-wxh default-wxh)))
                                 (- *term-rows* *status-height*))))
      ;; -A: attach to existing session if it exists
      (when attach-if-exists
        (let ((existing (server-find-session name)))
          (when existing
            (session-touch existing)
            (unless detach-p (setf *dirty* t))
            (return-from %cmd-new-session-arg existing))))
      ;; Without -A, a name already in use cannot be taken over (server-add-session
      ;; would orphan the existing session): an EXPLICIT -s duplicate is refused
      ;; (tmux's "duplicate session"); an AUTO name bumps to the next free number.
      (when (and (not attach-if-exists) (server-find-session name))
        (if (cdr (assoc #\s flags))
            (progn
              (%overlayf "duplicate session: ~A" name)
              (return-from %cmd-new-session-arg nil))
            (setf name (%next-free-session-name))))
      ;; -t <group>: join an existing session's group instead of spawning a new
      ;; pane.  The grouped session SHARES the target's window list (and thus the
      ;; live PTYs + reader threads already attached to those panes), so it must
      ;; be built with a bare make-session — NOT new-session, which would spawn an
      ;; initial PTY + reader thread that %link-session-to-group then orphans.
      (when group-target
        (let ((target (server-find-session group-target)))
          (unless target
            (%overlayf "can't find session: ~A" group-target)
            (return-from %cmd-new-session-arg nil))
          (let ((grouped (make-session :id (incf *session-id-counter*)
                                       :name name
                                       :last-active (get-universal-time))))
            (server-add-session grouped)
            (server-new-session-in-group grouped target)
            (when (not detach-p)
              (setf *dirty* t)
              (show-transient-overlay
               (format nil "new session: ~A" (session-name grouped))))
            (return-from %cmd-new-session-arg grouped))))
      (let ((new-sess (new-session name rows cols :start-dir start-dir)))
        ;; Apply window name if given
        (when (and win-name new-sess)
          (let ((win (session-active-window new-sess)))
            (when win (rename-window win win-name))))
        ;; Without -d, show an overlay confirming the new session was created.
        ;; With -d, the session is created in background and SESSION (the calling
        ;; session) remains the active display — no dirty flag, no visual switch.
        (when (and new-sess (not detach-p))
          (show-transient-overlay
           (format nil "new session: ~A" (session-name new-sess))))
        new-sess))))

(defvar *key-table* nil
  "The client's active custom key table (a table-name string), or NIL for the
   normal root/prefix flow.  Set by `switch-client -T <table>`; while non-NIL the
   ground input state looks keys up in this table (modal keymaps).  Defined here
   (dispatch-core loads before events-keystroke) so it is declared special before
   either %cmd-switch-client or %ground-input-state references it.")
