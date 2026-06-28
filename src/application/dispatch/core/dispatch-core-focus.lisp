(in-package #:cl-tmux)

;;;; Declarative command dispatch - focus event delivery helpers.

;;; -- Focus event delivery (?1004) -------------------------------------------
;;;
;;; When a pane's application has enabled focus events, switching the active pane
;;; must deliver ESC[O (focus lost) to the pane being left and ESC[I (focus
;;; gained) to the pane being entered. focus-event-report (terminal layer) owns
;;; the byte sequence; here we perform the PTY write. Both are guarded by a live
;;; fd, so panes without a PTY (fd <= 0, e.g. in tests) are a harmless no-op.

(defun %session-of-window (win)
  "The session in *server-sessions* whose window list contains WIN, or NIL.
   Lets chokepoints that only have a window (e.g. %select-pane-with-focus) fire
   .tmux.conf set-hook command hooks, which run-command-hooks dispatches against a
   session."
  (and win (loop for entry in *server-sessions*
                 for sess = (cdr entry)
                 when (member win (session-windows sess)) return sess)))

(defun %session-of-pane (pane)
  "The session in *server-sessions* one of whose windows contains PANE, or NIL.
   Lets %notify-pane-focus fire .tmux.conf set-hook command hooks from a pane."
  (and pane (loop for entry in *server-sessions*
                  for sess = (cdr entry)
                  when (loop for w in (session-windows sess)
                             thereis (member pane (window-panes w)))
                    return sess)))

(defun %notify-pane-focus (pane focused-p)
  "Notify PANE of a focus change: fire the pane-focus-in / pane-focus-out hook
   (independent of ?1004), then send the application its focus-tracking report
   (ESC[I gained / ESC[O lost) when it enabled focus events and PANE has a live
   PTY. A safe no-op when PANE is NIL."
  (when pane
    ;; Hook fires on every focus transition, regardless of whether the app
    ;; enabled ?1004 focus reporting (matches tmux's pane-focus-in/out hooks).
    ;; run-hooks fires both the add-hook and (via the pane's session) set-hook.
    (cl-tmux/hooks:run-hooks (if focused-p
                                 cl-tmux/hooks:+hook-pane-focus-in+
                                 cl-tmux/hooks:+hook-pane-focus-out+)
                             pane))
  (when (cl-tmux/model:pane-live-p pane)
    (let ((seq (cl-tmux/terminal/actions:focus-event-report
                (pane-screen pane) focused-p)))
      (when seq
        (pty-write (pane-fd pane) (babel:string-to-octets seq :encoding :utf-8))))))

(defun %select-pane-with-focus (win new-pane)
  "Make NEW-PANE the active pane of WIN, delivering focus-out to the previously
   active pane and focus-in to NEW-PANE (for panes that enabled ?1004). Used by
   every interactive pane-switch path so focus tracking stays transparent."
  (let ((old (window-active-pane win)))
    (window-select-pane win new-pane)
    (unless (eq old new-pane)
      (%notify-pane-focus old nil)
      (%notify-pane-focus new-pane t)
      ;; tmux's window-pane-changed event hook: WIN's active pane changed.
      ;; (run-hooks fires both registries, deriving the session from WIN.)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-pane-changed+ win))))
