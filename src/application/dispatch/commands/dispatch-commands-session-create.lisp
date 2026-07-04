(in-package #:cl-tmux)

;;; -- Session creation command --------------------------------------------------

(defun %parse-wxh (str)
  "Parse a \"WxH\" size string (e.g. the default-size option \"80x24\") into
   (values W H), or NIL when STR is not of that form or either dimension
   is not a positive integer."
  (when (stringp str)
    (let* ((x (position #\x str :test #'char-equal))
           (w (and x (%parse-integer-or-nil str :end x :junk-allowed t)))
           (h (and x (%parse-integer-or-nil str :start (1+ x) :junk-allowed t))))
      (when (and w h (plusp w) (plusp h))
        (values w h)))))

(defun %next-free-session-name ()
  "Return the lowest positive-integer string not already in use as a session name."
  (loop for i from 1
        for candidate = (format nil "~D" i)
        unless (server-find-session candidate) return candidate))

(defun %new-session-name-from-flags (flags)
  "Return the requested session name, or the next auto-generated one."
  (or (%flag-value flags #\s)
      (format nil "~D" (1+ (length *server-sessions*)))))

(defun %default-size-dimensions (detach-p)
  "Return the default detached session dimensions, or NIL when attached."
  (when detach-p
    (multiple-value-bind (cols rows)
        (%parse-wxh (cl-tmux/options:get-option "default-size" "80x24"))
      (values cols rows))))

(defun %new-session-dimensions-from-flags (flags detach-p)
  "Return the initial session dimensions selected by X/Y flags and defaults."
  (multiple-value-bind (default-cols default-rows)
      (%default-size-dimensions detach-p)
    (values (or (%parse-flag-int flags #\x)
                default-cols
                *term-cols*)
            (or (%parse-flag-int flags #\y)
                default-rows
                (- *term-rows* *status-height*)))))

(defun %new-session-return-existing (name detach-p)
  "Return the already-existing session NAME for new-session -A, touching it."
  (let ((existing (server-find-session name)))
    (when existing
      (session-touch existing)
      (unless detach-p
        (setf *dirty* t))
      existing)))

(defun %new-session-attach-existing (name detach-p print-p print-fmt)
  "new-session -A: if a session named NAME already exists, attach to (return) it
   instead of creating a new one, printing its info first when -P was given.
   Returns the existing session, or NIL when NAME is not yet in use (the caller
   then falls through to ordinary session creation)."
  (let ((existing (%new-session-return-existing name detach-p)))
    (when (and existing print-p)
      (%show-session-info-overlay existing print-fmt))
    existing))

(defun %new-session-resolve-name (name attach-if-exists flags)
  "Return the final session name, or NIL when an explicit duplicate is refused."
  (if (and (not attach-if-exists)
           (server-find-session name))
      (if (%flag-present-p flags #\s)
          (progn
            (%overlayf "duplicate session: ~A" name)
            nil)
          (%next-free-session-name))
      name))

(defun %new-session-create-grouped (name group-target detach-p)
  "Create a grouped session that shares windows with GROUP-TARGET."
  (let ((target (server-find-session group-target)))
    (unless target
      (%overlayf "can't find session: ~A" group-target)
      (return-from %new-session-create-grouped nil))
    (let ((grouped (make-session :id (incf *session-id-counter*)
                                 :name name
                                 :last-active (get-universal-time))))
      (server-add-session grouped)
      (server-new-session-in-group grouped target)
      (when (not detach-p)
        (setf *dirty* t)
        (show-transient-overlay
         (format nil "new session: ~A" (session-name grouped))))
      grouped)))

(defun %new-session-finalize (new-sess win-name detach-p)
  "Apply post-creation window naming and overlays to NEW-SESS."
  (when (and win-name new-sess)
    (let ((win (session-active-window new-sess)))
      (when win
        (rename-window win win-name))))
  (when (and new-sess (not detach-p))
    (show-transient-overlay
     (format nil "new session: ~A" (session-name new-sess))))
  new-sess)

(defun %show-session-info-overlay (sess fmt)
  "new-session -P: print info about the created session SESS to an overlay.
   FMT (the -F format) overrides the default #{session_name}: template."
  (when sess
    (let* ((win  (session-active-window sess))
           (pane (and win (window-active-pane win)))
           (template (if (and fmt (plusp (length fmt))) fmt "#{session_name}:")))
      (show-transient-overlay
       (cl-tmux/format:expand-format
        template
        (cl-tmux/format:format-context-from-session sess win pane))))))

(defun %new-session-apply-environment (sess env-pairs)
  "Persist new-session -e VAR=val pairs onto SESS's environment overlay so they
   are inherited by windows created later in the session (the initial pane picks
   them up via *pane-extra-env* at fork time)."
  (when sess
    (dolist (pair env-pairs)
      (cl-tmux/model:session-set-environment sess (car pair) (cdr pair)))))

(defun %cmd-new-session-arg (session args)
  "new-session [-AdEP] [-s name] [-n window-name] [-c start-dir] [-e VAR=val]
   [-F format] [-x width] [-y height]: create a new session.
   -A: if a session named NAME already exists, attach to it instead of creating a new one.
  -d: create detached (do not switch to the new session).
  -s name: session name.
  -n name: initial window name.
  -c dir: start directory for the initial window's shell.
  -e VAR=val: set an environment variable in the new session (repeatable).
  -E: do NOT apply the update-environment option when creating the session.
  -P: print information about the new session after creation.
  -F format: with -P, the format string for the printed info (default
     #{session_name}:).
   -x width: initial columns (default: terminal width, or default-size when -d).
   -y height: initial rows (default: terminal height minus status bar, or
     default-size when -d).
  A DETACHED session (-d) has no client to size it, so -- like tmux -- it uses the
   default-size option (\"WxH\", default 80x24) when -x/-y are not given."
  (with-command-input (flags positionals args "sncxyteF"
                             :allowed-flags '(#\A #\d #\E #\P #\s #\n #\c
                                              #\x #\y #\t #\e #\F)
                             :message "new-session: unsupported argument")
    (declare (ignore positionals))
    (let* ((name            (%new-session-name-from-flags flags))
           (attach-if-exists (%flag-present-p flags #\A))
           (detach-p         (%flag-present-p flags #\d))
           (print-p          (%flag-present-p flags #\P))
           (print-fmt        (%flag-value flags #\F))
           (suppress-env-p   (%flag-present-p flags #\E))
           (env-pairs        (%collect-env-flags flags))
           (win-name         (%flag-value flags #\n))
           ;; -t <group>: the new session JOINS an existing session's group,
           ;; sharing its window list (tmux "grouped sessions").
           (group-target     (%flag-value flags #\t))
           (start-dir        (%flag-value flags #\c)))
      (multiple-value-bind (cols rows)
          (%new-session-dimensions-from-flags flags detach-p)
        (when attach-if-exists
          (return-from %cmd-new-session-arg
            (%new-session-attach-existing name detach-p print-p print-fmt)))
        (setf name (%new-session-resolve-name name attach-if-exists flags))
        (when (null name)
          (return-from %cmd-new-session-arg nil))
        (when group-target
          (let ((grouped (%new-session-create-grouped name group-target detach-p)))
            (%new-session-apply-environment grouped env-pairs)
            (when (and grouped print-p)
              (%show-session-info-overlay grouped print-fmt))
            (return-from %cmd-new-session-arg grouped)))
        ;; -e makes the initial pane inherit VAR=val via *pane-extra-env*; -E
        ;; suppresses update-environment for the whole creation (incl. that pane).
        (let* ((cl-tmux/model:*suppress-update-environment* suppress-env-p)
               (*pane-extra-env* (or env-pairs *pane-extra-env*))
               (new-sess (new-session name rows cols :start-dir start-dir)))
          ;; Persist -c as the session working directory for future windows.
          (when (and new-sess start-dir)
            (setf (session-start-directory new-sess) start-dir))
          (%new-session-apply-environment new-sess env-pairs)
          (let ((result (%new-session-finalize new-sess win-name detach-p)))
            (when (and result print-p)
              (%show-session-info-overlay result print-fmt))
            result))))))
