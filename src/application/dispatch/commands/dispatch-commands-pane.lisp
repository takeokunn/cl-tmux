(in-package #:cl-tmux)

;;; -- Window/pane/session structural commands ----------------------------------------
;;;
;;; Select-layout, display-panes, new-window, split-window, and shared pane
;;; helpers.  Canonical layout facts are defined in
;;; dispatch-commands-pane-layout-facts.lisp.

(defun %cmd-select-layout (session args)
  "select-layout [-npoE] [-t target-window] [layout-name]: apply or cycle layouts.
   layout-name: even-horizontal, even-vertical, main-horizontal,
     main-vertical, tiled.
   -n: next preset layout; -p: previous preset layout.
   -E: spread the panes out evenly (mapped to even-vertical).
   -o: undo the last layout change — restores the layout tree saved before
       the previous select-layout/next-layout application (swapping, so a
       second -o redoes).
   -t target-window: the window to lay out (default: the active window)."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\n #\p #\o #\E #\t)
                             :max-positionals 1
                             :message "select-layout: unsupported argument")
    (let* ((target (%flag-value flags #\t))
           (win    (if target
                       (%resolve-window-target session target)
                       (session-active-window session)))
           (name   (first positionals)))
      (when win
        (cond
          ;; -o: swap the current tree with the one saved before the last
          ;; layout change and relayout.
          ((%flag-present-p flags #\o)
           (let ((prev (cl-tmux/model:window-last-layout-tree win)))
             (when prev
               (setf (cl-tmux/model:window-last-layout-tree win)
                     (cl-tmux/model:window-tree win)
                     (cl-tmux/model:window-tree win) prev)
               (window-relayout win (window-height win) (window-width win))
               (setf *dirty* t))))
          ((%flag-present-p flags #\n) (%cycle-layout session win :next))
          ((%flag-present-p flags #\p) (%cycle-layout session win :prev))
          ((%flag-present-p flags #\E) (%apply-named-layout-to-session session :even-vertical))
          (name
           (let ((kw (%resolve-layout-name name)))
             (when kw (%apply-named-layout-to-session session kw)))))))))

(defun %cmd-next-layout (session args)
  "next-layout [-t target-window]: cycle the window to the next preset layout.
   The scriptable form of the :next-layout binding."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "next-layout: unsupported argument")
    (declare (ignore positionals))
    (let* ((target (%flag-value flags #\t))
           (win    (if target
                       (%resolve-window-target session target)
                       (session-active-window session))))
      (when win (%cycle-layout session win :next)))))

(defun %cmd-previous-layout (session args)
  "previous-layout [-t target-window]: cycle the window to the previous preset
   layout.  The scriptable form of the :previous-layout binding."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "previous-layout: unsupported argument")
    (declare (ignore positionals))
    (let* ((target (%flag-value flags #\t))
           (win    (if target
                       (%resolve-window-target session target)
                       (session-active-window session))))
      (when win (%cycle-layout session win :prev)))))

(defun %cmd-display-panes-arg (session args)
  "display-panes [-d duration]: show pane ids.
   -d duration: how long to show the overlay (ms).
   The renderer owns the actual pane-number overlay."
  (with-command-input (flags positionals args "d"
                             :allowed-flags '(#\d)
                             :max-positionals 0
                             :message "display-panes: unsupported argument")
    (declare (ignore positionals))
    (let* ((duration (%display-panes-duration-from-flags flags))
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

(defun %display-panes-duration-from-flags (flags)
  "Parse the -d duration argument for display-panes."
  (let ((duration-str (%flag-value flags #\d)))
    (and duration-str
         (ignore-errors
           (parse-integer duration-str :junk-allowed t)))))

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
   -b: insert before the current window.
  -c dir: start directory for the new pane's shell (format strings expanded).
  -e VAR=val: set environment variable in the new pane (repeatable).
  -S: if a window with the -n name already exists, SELECT it instead of creating
     a new one (with -d, do nothing); matches tmux new-window -S."
  (with-command-flags+pos (flags positionals args "ntceF")
    (declare (ignore positionals))
    (let* ((extra-env  (%collect-env-flags flags))
           (name       (%flag-value flags #\n))
           (select-p   (%flag-present-p flags #\S))
           (detach-p   (%flag-present-p flags #\d))
           (kill-p     (%flag-present-p flags #\k))
           (print-p    (%flag-present-p flags #\P))
           (print-fmt  (%flag-value flags #\F))
           (after-p    (%flag-present-p flags #\a))
           (before-p   (%flag-present-p flags #\b))
           ;; -c overrides; else fall back to the session working directory
           ;; (attach-session/new-session -c), matching tmux's new-window default.
           (raw-dir    (or (%flag-value flags #\c)
                           (session-start-directory session)))
           (start-dir  (%expand-start-dir session raw-dir))
           (at-idx     (%parse-flag-int flags #\t)))
      ;; -S: if a window already has the -n name, select it instead of creating.
      (when (and select-p name)
        (let ((existing (find name (session-windows session)
                              :key #'window-name :test #'string=)))
          (when existing
            (unless detach-p
              (session-select-window session existing)
              (setf *dirty* t))
            (return-from %cmd-new-window-arg existing))))
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
                                      :after-current (and after-p t)
                                      :before-current (and before-p t))))
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

(defun %read-standard-input-octets ()
  "Read command stdin as UTF-8 bytes for split-window -I."
  (babel:string-to-octets
   (with-output-to-string (out)
     (loop for ch = (read-char *standard-input* nil nil)
           while ch
           do (write-char ch out)))
   :encoding :utf-8))

(defparameter *defer-split-window-input* nil
  "When true, split-window -I creates an input pane without reading local stdin.")

(defun %split-window-resolve-target (session target-str)
  "Handle split-window's -t target-str: temporarily make the target pane
   active so %cmd-split operates on it.  Returns (values prev-win prev-pane),
   the window/pane that were active before the swap, for the caller to
   restore afterwards when -d (detach) is given."
  (multiple-value-bind (prev-win prev-pane) (%active-window-pane session)
    (when target-str
      (multiple-value-bind (target-win target-pane)
          (%resolve-target-window-pane session target-str prev-win prev-pane)
        (when (and target-win target-pane)
          ;; Switch active window and pane to the target for the split.
          (session-select-window session target-win)
          (window-select-pane target-win target-pane))))
    (values prev-win prev-pane)))

(defun %split-window-post-actions (session result flags detach-p target-str prev-win prev-pane)
  "Perform split-window's post-split housekeeping: restore the original focus
   when -d (detach) is given, print the new pane's details for -P, and zoom
   the window for -Z (tmux SPAWN_ZOOM).  Returns RESULT unchanged."
  ;; Restore original focus when -d (detach).
  (when (and detach-p target-str prev-win)
    (session-select-window session prev-win)
    (when prev-pane (window-select-pane prev-win prev-pane)))
  ;; -P: print the new pane's details.
  (when (and result (%flag-present-p flags #\P))
    (%show-pane-info-overlay session (pane-window result) result
                             (%flag-value flags #\F)))
  ;; -Z: zoom the window after the split (tmux SPAWN_ZOOM).
  (when (and result (%flag-present-p flags #\Z))
    (let ((rwin (pane-window result)))
      (when (and rwin (not (cl-tmux/model:window-zoom-p rwin)))
        (cl-tmux/model:window-zoom-toggle rwin))))
  result)

(defun %cmd-split-window (session args)
  "split-window [-h|-v] [-b] [-f] [-d] [-I] [-t target] [-l size] [-c start-dir] [-e VAR=val].
   -h: horizontal split (new pane to the right; side-by-side).
   -v: vertical split (new pane below — default).
   -b: insert before the active pane (left of / above) instead of after.
   -f: full-window split — the new pane spans the whole window dimension (the split
       is inserted at the layout root) instead of subdividing the active pane.
   -d: split but do not change focus (detached mode).
   -I: read stdin into the new pane without starting a PTY command.
   -t target: split the target pane instead of the active pane.
   -l N: size in lines/columns (absolute integer), or -l N% as a percentage
     of the parent pane.
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable).
   -P: print the new pane's details to overlay.
   -F format: with -P, the format string for the printed info (instead of the
     default session:window.pane [WxH]) — e.g. `split-window -dP -F '#{pane_id}'`.
   -Z: zoom the window after splitting (tmux SPAWN_ZOOM)."
  (with-command-input (flags positionals args "lcetF"
                             :allowed-flags '(#\h #\v #\b #\f #\d #\I #\t
                                              #\l #\c #\e #\P #\F #\Z))
    (declare (ignore positionals))
    (let* ((extra-env    (%collect-env-flags flags))
           (horizontal-p (%flag-present-p flags #\h))
           (before-p     (%flag-present-p flags #\b))
           (full-p       (%flag-present-p flags #\f))
           (detach-p     (%flag-present-p flags #\d))
           (input-p      (%flag-present-p flags #\I))
           (target-str   (%flag-value flags #\t))
           (lines-str    (%flag-value flags #\l))
           (raw-dir      (%flag-value flags #\c))
           (start-dir    (%expand-start-dir session raw-dir))
           ;; -l N → N cells; -l N% → fraction.
           (size         (%parse-split-size lines-str))
           (input-bytes  (and input-p
                              (not *defer-split-window-input*)
                              (%read-standard-input-octets))))
      ;; -t target: temporarily make the target pane active so %cmd-split
      ;; operates on it.  Restore the previous active pane afterwards if -d.
      (multiple-value-bind (prev-win prev-pane)
          (%split-window-resolve-target session target-str)
        (let* (;; Inject -e VAR=val pairs only for PTY-backed splits; -I does
               ;; not spawn a process and must not leak env to a later pane.
               (*pane-extra-env* (and (not input-p) extra-env))
               (result  (%cmd-split session (if horizontal-p :h :v)
                                    :size size :no-focus (and detach-p t)
                                    :start-dir start-dir :before (and before-p t)
                                    :full (and full-p t)
                                    :input-only (and input-p t)
                                    :input-bytes input-bytes)))
          (%split-window-post-actions session result flags detach-p
                                      target-str prev-win prev-pane))))))

(defvar *key-table* nil
  "The client's active custom key table (a table-name string), or NIL for the
   normal root/prefix flow.  Set by `switch-client -T <table>`; while non-NIL the
   ground input state looks keys up in this table (modal keymaps).  Defined here
   (dispatch-core loads before events-keystroke) so it is declared special before
   either %cmd-switch-client or %ground-input-state references it.")
