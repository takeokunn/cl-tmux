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
  (let* ((target-str  (%flag-value flags #\t))
         (kill-others (%flag-present-p flags #\a))
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

(defun %link-window-perform (dst-sess src-win flags dst-index kill-p detach-p)
  "Insert SRC-WIN into DST-SESS at its winlink index, resolving an index
   collision (-k replaces, else refuses) and selecting the linked window
   unless DETACH-P."
  (let* ((desired (cond ((and dst-index (%flag-present-p flags #\a))
                         (1+ dst-index))
                        (dst-index dst-index)
                        (t (window-id src-win))))
         (collision (find desired (session-windows dst-sess)
                          :key (lambda (w)
                                 (cl-tmux/model:session-window-index
                                  dst-sess w))))
         (dst-active (session-active-window dst-sess)))
    (if (and collision (not kill-p))
        (show-overlay "link-window: target index in use (add -k to replace)")
        (progn
          (when collision (kill-window dst-sess collision))
          (session-insert-window dst-sess src-win)
          ;; Record the destination session's winlink index when it
          ;; differs from the window's own id.
          (cl-tmux/model:set-session-window-index dst-sess src-win desired)
          (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-linked+ src-win)
          ;; -k may select a replacement while removing the collision;
          ;; -d means link without changing the destination current window.
          (if detach-p
              (session-select-window dst-sess dst-active)
              (session-select-window dst-sess src-win))
          (show-overlay (if collision
                             "link-window: linked (replaced existing)"
                             "link-window: linked"))))))

(define-command-input-handler %cmd-link-window (session args)
  "link-window [-s src] -t dst [-abdk]: share a window into another session.
   -s src: source window target (session:window); default is the active window.
   -t dst: destination session (session or session:window).
   -k: kill any window already occupying the destination index first.
   -d: do not make the linked window the active window of the destination session
   (default: the newly linked window becomes current, matching tmux).
   -a / -b: insert after / before the destination index.
   The window OBJECT is shared; its index in the destination session is a
   per-session winlink index — '-t sess:N' links it at N there (with -a: N+1)
   while the source session keeps its own index, like tmux winlinks."
  (flags positionals "st"
         :allowed-flags '(#\s #\t #\k #\d #\a #\b)
         :max-positionals 0
         :message "link-window: unsupported argument")
  (let* ((src-str (%flag-value flags #\s))
         (dst-raw (%flag-value flags #\t))
         ;; -t may be "session" or "session:INDEX" — the index becomes the
         ;; window's per-session winlink index in the destination.
         (dst-colon (and dst-raw (position #\: dst-raw)))
         (dst-str (if dst-colon (subseq dst-raw 0 dst-colon) dst-raw))
         (dst-index (and dst-colon
                         (%parse-integer-or-nil (subseq dst-raw (1+ dst-colon)))))
         (kill-p  (%flag-present-p flags #\k))
         (detach-p (%flag-present-p flags #\d))
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
       (%link-window-perform dst-sess src-win flags dst-index kill-p detach-p)))))

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
  (let* ((target-str (%flag-value flags #\t))
         (kill-p     (%flag-present-p flags #\k))
         (win        (%resolve-window-target-or-active session target-str)))
    (cond
      ((null win)
       (show-overlay "unlink-window: window not found"))
      ((> (%window-session-count win) 1)
       ;; Linked elsewhere — safe to drop from this session only.
       (let ((was-active (eq (session-active-window session) win)))
         (setf (session-windows session) (remove win (session-windows session)))
         (session-windows-changed session)
         ;; Reselect a remaining window if we just removed the active one.
         (when (and was-active (session-windows session))
           (session-select-window session (first (session-windows session)))))
       ;; In a session group the removal propagates to every peer, so the
       ;; window may now be referenced by NO session at all — tear down its
       ;; PTYs like tmux destroys a fully-unreferenced window (otherwise the
       ;; shells would leak until kill-server).
       (when (zerop (%window-session-count win))
         (dolist (pane (window-panes win))
           (close-pane-pty pane)))
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
  (let* ((target-str (%flag-value flags #\t))
         (kill-all   (%flag-present-p flags #\a))
         (n          (%parse-integer-or-nil target-str))
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

(define-command-input-handler %cmd-kill-server (session args)
  "kill-server: terminate the server and close every pane in every session."
  (flags positionals ""
         :allowed-flags '()
         :max-positionals 0
         :message "kill-server: unsupported argument")
  (dolist (entry *server-sessions*)
    (let ((sess (cdr entry)))
      (dolist (pane (all-panes sess))
        (close-pane-pty pane))))
  (setf *running* nil)
  :quit)

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
    (session-windows-changed session)
    t))

(define-command-input-handler %cmd-swap-window (session args)
  "swap-window [-s src] -t dst [-d]: exchange the index numbers of two windows.  SRC
   and DST are window-id/name targets; with no -s the active window is the source.
   -d: do not change the active window (default: the swapped window becomes current,
   matching tmux, which without -d makes the moved content current in both sessions).
   First command to use two value flags (-s and -t) at once."
  (flags positionals "st"
         :allowed-flags '(#\s #\t #\d)
         :max-positionals 0
         :message "swap-window: unsupported argument")
  (let* ((src-str (%flag-value flags #\s))
         (dst-str (%flag-value flags #\t))
         (src (%resolve-window-target-or-active session src-str))
         (dst (and dst-str (%resolve-window-target session dst-str))))
    (when (%swap-window-ids session src dst)
      ;; Without -d the swapped window becomes the current window.  tmux selects
      ;; the moved content in both the destination and source sessions; cl-tmux's
      ;; swap is intra-session, so the moved source window becomes current.
      (unless (%flag-present-p flags #\d)
        (session-select-window session src)))))

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

(defun %move-window-to-index (session src-win dst-n after kill-p flags)
  "Move SRC-WIN to index DST-N (or DST-N+1 when AFTER), shuffling windows up
   to make room unless KILL-P replaces the occupant.  Selects SRC-WIN as
   current afterwards unless -d is present in FLAGS."
  (let ((target (if after (1+ dst-n) dst-n)))
    (when (%window-id-occupied-p session target src-win)
      (if kill-p
          ;; -k: kill the window occupying the destination index.
          (let ((occupant (find target (session-windows session)
                                :key #'window-id)))
            (when (and occupant (not (eq occupant src-win)))
              (kill-window session occupant)))
          (%shuffle-windows-up session target src-win)))
    (setf (window-id src-win) target
          (session-windows session)
          (sort (copy-list (session-windows session)) #'< :key #'window-id))
    (session-windows-changed session)
    ;; Without -d the moved window becomes the current window (tmux selects
    ;; the moved window in the destination session unless -d is given).
    (unless (%flag-present-p flags #\d)
      (session-select-window session src-win))))

(define-command-input-handler %cmd-move-window (session args)
  "move-window [-s src-window] [-t dst-index] [-r] [-a] [-b] [-k] [-d]:
   move/renumber a window (tmux args \"abdkrs:t:\").
   -s src: source window (name or id); default is the active window.
   -t n: destination window-id (numeric index to assign to the window).
   -r: renumber all windows sequentially from base-index (repack gaps).
   -a: insert AFTER the destination window (index n+1).
   -b: insert BEFORE the destination window (index n) — cl-tmux's default
       placement, accepted explicitly.
   -k: if the destination index is occupied, KILL the occupying window instead
       of shuffling the windows up to make room.
   -d: do not make the moved window the active window (default: the moved window
   becomes current, matching tmux).
   Without -s/-t: prompts interactively (no-op in arg-command path)."
  (flags positionals "st"
         :allowed-flags '(#\s #\t #\r #\a #\b #\k #\d)
         :max-positionals 0
         :message "move-window: unsupported argument")
  (let* ((src-str (%flag-value flags #\s))
         (repack  (%flag-present-p flags #\r))
         (after   (%flag-present-p flags #\a))
         (kill-p  (%flag-present-p flags #\k))
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
         (setf (session-windows session) sorted)
         (session-windows-changed session)))
      ;; -t n (with optional -s src / -a): move the window to index n.  -a
      ;; inserts AFTER index n (n+1); the default/-b inserts AT n.  When the
      ;; target index is occupied by ANOTHER window, the windows at and above it
      ;; shift up to make room (tmux's winlink_shuffle_up) rather than the move
      ;; being silently dropped or another window orphaned.
      ((and src-win dst-n)
       (%move-window-to-index session src-win dst-n after kill-p flags)))))
