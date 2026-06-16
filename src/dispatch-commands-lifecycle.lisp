(in-package #:cl-tmux)

(define-command-input-handler %cmd-kill-window (session args)
  "kill-window [-a] [-t target]: kill a window or all windows except the current.
   -a: kill ALL windows in the session EXCEPT the target (or active) window.
   -t target: target window by id or name.
   No flags: kill the active window."
  (flags positionals "t"
         :allowed-flags '(#\a #\t)
         :max-positionals 0
         :message "kill-window: unsupported argument")
  (let* ((target-str  (cdr (assoc #\t flags)))
         (kill-others (assoc #\a flags))
         (ref-win     (%resolve-window-target-or-active session target-str)))
    (if kill-others
        ;; -a: kill all EXCEPT the reference window
        (let ((to-kill (remove ref-win (session-windows session))))
          (dolist (w to-kill)
            (%handle-kill-result (kill-window session w))))
        ;; Normal: kill the target window
        (when ref-win
          (%handle-kill-result (kill-window session ref-win))))))

(defun %window-session-count (window)
  "Number of sessions in *server-sessions* whose window list contains WINDOW.
   Used by unlink-window to avoid orphaning a window that is only in one session."
  (count-if (lambda (entry)
              (member window (session-windows (cdr entry))))
            *server-sessions*))

(define-command-input-handler %cmd-link-window (session args)
  "link-window [-s src] -t dst [-k]: share a window into another session.
   -s src: source window target (session:window); default is the active window.
   -t dst: destination session (session or session:window).
   -k: kill any window already occupying the destination index first.
   The window object is SHARED — it appears in both sessions at the same index
   (cl-tmux stores the index in the window struct, so linked windows share it)."
  (flags positionals "st"
         :allowed-flags '(#\s #\t #\k)
         :max-positionals 0
         :message "link-window: unsupported argument")
  (let* ((src-str (cdr (assoc #\s flags)))
         (dst-str (cdr (assoc #\t flags)))
         (kill-p  (assoc #\k flags))
         ;; Resolve source window (default: active window of current session).
         (src-win (if src-str
                      (nth-value 1 (%resolve-target-session-window session src-str
                                                                    (session-active-window session)
                                                                    nil))
                      (session-active-window session)))
         ;; Resolve destination session.
         (dst-sess nil))
    (with-target-session (target-session dst-str session)
      (setf dst-sess target-session))
    (cond
      ((not (and src-win dst-sess))
       (show-overlay "link-window: source window or destination session not found"))
      ;; Already linked there — nothing to do.
      ((member src-win (session-windows dst-sess))
       (show-overlay "link-window: window already linked in destination"))
      (t
       (let ((collision (find (window-id src-win) (session-windows dst-sess)
                              :key #'window-id)))
         (if (and collision (not kill-p))
             (show-overlay "link-window: target index in use (add -k to replace)")
             (progn
               (when collision (kill-window dst-sess collision))
               (session-insert-window dst-sess src-win)
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-linked+ src-win)
               (show-overlay (if collision
                                 "link-window: linked (replaced existing)"
                                 "link-window: linked")))))))))

(define-command-input-handler %cmd-unlink-window (session args)
  "unlink-window [-t target] [-k]: remove a window's link from its session.
   -t target: window to unlink (default: active window).
   The window is removed from the resolved session only when it is also linked in
   at least one OTHER session (so it is not orphaned).  When it exists in only
   one session, -k is required to actually destroy it (matches tmux)."
  (flags positionals "t"
         :allowed-flags '(#\t #\k)
         :max-positionals 0
         :message "unlink-window: unsupported argument")
  (let* ((target-str (cdr (assoc #\t flags)))
         (kill-p     (assoc #\k flags))
         (win        (%resolve-window-target-or-active session target-str)))
    (cond
      ((null win)
       (show-overlay "unlink-window: window not found"))
      ((> (%window-session-count win) 1)
       ;; Linked elsewhere — safe to drop from this session only.
       (let ((was-active (eq (session-active-window session) win)))
         (setf (session-windows session) (remove win (session-windows session)))
         ;; Reselect a remaining window if we just removed the active one.
         (when (and was-active (session-windows session))
           (session-select-window session (first (session-windows session)))))
       (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-unlinked+ win)
       (show-overlay "unlink-window: unlinked"))
      (kill-p
       ;; Only in this session and -k given — destroy it.
       (%handle-kill-result (kill-window session win))
       (show-overlay "unlink-window: killed (last link)"))
      (t
       (show-overlay "unlink-window: window only in this session (add -k to kill)")))))

(define-command-input-handler %cmd-kill-pane (session args)
  "kill-pane [-a] [-t target]: kill the target pane, or all except target with -a.
   -a: kill all panes in the active window EXCEPT the target (or active) pane.
   -t target: target pane by pane-id.
   No -t: target is the active pane.  A -t that matches nothing is a no-op."
  (flags positionals "t"
         :allowed-flags '(#\a #\t)
         :max-positionals 0
         :message "kill-pane: unsupported argument")
  (let* ((target-str (cdr (assoc #\t flags)))
         (kill-all   (assoc #\a flags))
         (n          (and target-str (parse-integer target-str :junk-allowed t)))
         (win        (session-active-window session))
         ;; Determine the pane to KEEP (when -a) or KILL.
         (ref-pane   (if (and n win)
                         (find n (window-panes win) :key #'pane-id)
                         (session-active-pane session))))
    (cond
      ;; -a: kill all panes EXCEPT the reference pane.
      (kill-all
       (when (and win ref-pane)
         (dolist (p (copy-list (window-panes win)))
           (unless (eq p ref-pane)
             (%handle-kill-result (kill-pane session p))))))
      ;; Normal: kill the target (or active) pane.
      ((or ref-pane (null target-str))
       (%handle-kill-result (kill-pane session ref-pane))))))

(defun %swap-window-ids (session win-a win-b)
  "Exchange the index numbers (window-id) of WIN-A and WIN-B and re-sort the
   session's window list by id — tmux's swap-window, which trades the two windows'
   INDICES (so #{window_index}, the status bar, and select-window -t follow the
   content).  This is distinct from a list-position swap, which would leave the
   indices out of order.  No-op when either window is NIL or they are the same.
   Returns T when a swap occurred."
  (when (and win-a win-b (not (eq win-a win-b)))
    (rotatef (window-id win-a) (window-id win-b))
    (setf (session-windows session)
          (sort (copy-list (session-windows session)) #'< :key #'window-id))
    t))

(define-command-input-handler %cmd-swap-window (session args)
  "swap-window [-s src] -t dst: exchange the index numbers of two windows.  SRC and
   DST are window-id/name targets; with no -s the active window is the source.
   First command to use two value flags (-s and -t) at once."
  (flags positionals "st"
         :allowed-flags '(#\s #\t)
         :max-positionals 0
         :message "swap-window: unsupported argument")
  (let* ((src-str (cdr (assoc #\s flags)))
         (dst-str (cdr (assoc #\t flags)))
         (src (%resolve-window-target-or-active session src-str))
         (dst (and dst-str (%resolve-window-target session dst-str))))
    (%swap-window-ids session src dst)))

(defun %cmd-source-file (session args)
  "source-file [-Fnqv] [-t target-pane] path...: load the tmux config file(s) at the given
   path(s), expanding ~ and shell globs (* ? []).  Enables the canonical reload
   binding (bind r source-file ~/.tmux.conf).  A missing file or parse error never
   crashes the session.  SESSION unused."
  (declare (ignore session))
  (cl-tmux/config:source-files args))

(defun %window-id-occupied-p (session id exclude)
  "T when some window OTHER than EXCLUDE in SESSION already has window-id ID."
  (loop for w in (session-windows session)
        thereis (and (not (eq w exclude)) (= (window-id w) id))))

(defun %shuffle-windows-up (session dst exclude)
  "Make room at index DST by shifting windows up — tmux's winlink_shuffle_up.
   Finds the first free index >= DST (ignoring EXCLUDE) and increments the id of
   every other window in [DST, free) by one, highest-id first so no two windows
   collide mid-shift."
  (let ((free dst))
    (loop while (%window-id-occupied-p session free exclude) do (incf free))
    (dolist (w (sort (remove exclude (copy-list (session-windows session)))
                     #'> :key #'window-id))
      (when (<= dst (window-id w) (1- free))
        (incf (window-id w))))))

(define-command-input-handler %cmd-move-window (session args)
  "move-window [-s src-window] [-t dst-index] [-r] [-a]: move/renumber a window.
   -s src: source window (name or id); default is the active window.
   -t n: destination window-id (numeric index to assign to the window).
   -r: renumber all windows sequentially from base-index (repack gaps).
   -a: insert after the current window (used with -t for relative positioning.
   Without -s/-t: prompts interactively (no-op in arg-command path)."
  (flags positionals "st"
         :allowed-flags '(#\s #\t #\r #\a)
         :max-positionals 0
         :message "move-window: unsupported argument")
  (let* ((src-str (cdr (assoc #\s flags)))
         (repack  (assoc #\r flags))
         (after   (assoc #\a flags))
         (src-win (%resolve-window-target-or-active session src-str))
         (dst-n   (%parse-flag-int flags #\t)))
    (cond
      ;; -r: repack all windows sequentially from base-index
      (repack
       (let* ((base (or (cl-tmux/options:get-option "base-index") 0))
              (sorted (sort (copy-list (session-windows session))
                            #'< :key #'window-id)))
         (loop for win in sorted
               for i from base
               do (setf (window-id win) i))
         (setf (session-windows session) sorted)))
      ;; -t n (with optional -s src / -a): move the window to index n.  -a
      ;; inserts AFTER index n (n+1); the default/-b inserts AT n.  When the
      ;; target index is occupied by ANOTHER window, the windows at and above it
      ;; shift up to make room (tmux's winlink_shuffle_up) rather than the move
      ;; being silently dropped or another window orphaned.
      ((and src-win dst-n)
       (let ((target (if after (1+ dst-n) dst-n)))
         (when (%window-id-occupied-p session target src-win)
           (%shuffle-windows-up session target src-win))
         (setf (window-id src-win) target
               (session-windows session)
               (sort (copy-list (session-windows session)) #'< :key #'window-id)))))))

(define-command-input-handler %cmd-if-shell (session args)
  "if-shell [-bF] [-t target-pane] <cond> <then> [<else>]: when the format CONDITION expands to a
   truthy value (non-empty and not \"0\"), run the THEN command line; otherwise
   run ELSE if given.  Only the -F (format, no shell fork) form is handled; a
   plain shell-condition if-shell is a no-op here (would require a fork)."
  (flags positionals "t"
         :allowed-flags '(#\b #\F #\t)
         :max-positionals 3
         :message "if-shell: unsupported argument")
  (when (assoc #\F flags)
    (let ((cond-str (first  positionals))
          (then     (second positionals))
          (else     (third  positionals)))
      (when cond-str
        (let* ((win  (session-active-window session))
               (pane (session-active-pane session))
               (ctx  (cl-tmux/format:format-context-from-session session win pane))
               (val  (cl-tmux/format:expand-format cond-str ctx)))
          (if (not (member val '("" "0") :test #'string=))
              (when then (%run-command-line session then))
              (when else (%run-command-line session else))))))))
