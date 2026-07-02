(in-package #:cl-tmux/renderer)

;;;; Session-frame side effects for the cl-tmux renderer.
;;;;
;;;; This file owns the post-layout output effects: bell emission, cursor
;;;; restoration, background bell relay, and draining passthrough / clipboard
;;;; queues into the final frame stream.

;;; ── Full-session render effects ─────────────────────────────────────────────

(defun %emit-bell (buffer visual-bell)
  "Write the audible BEL character to BUFFER unless VISUAL-BELL is \"on\"
   (visual-only — tmux writes the bell for \"off\" and \"both\").  The visual
   message overlay and the alert-bell hook are handled by the reader-thread
   alert path (%mark-window-bell), decoupled from this relay decision."
  (unless (and (stringp visual-bell) (string-equal visual-bell "on"))
    (write-char (code-char 7) buffer)))

(defun %render-bell-and-cursor (buffer active-pane)
  "Emit a pending BEL from ACTIVE-PANE (if any) and restore cursor visibility.
   bell-action 'none' swallows all BELs; 'other' skips the active pane (handled
   by %render-background-bells instead); 'any'/'current' relay the active pane bell."
  (when active-pane
    (let* ((bell-pending (screen-consume-bell (pane-screen active-pane)))
           (bell-action  (or (cl-tmux/options:get-option "bell-action") "any"))
           (visual-bell  (cl-tmux/options:get-option "visual-bell"))
           (relay-bell   (and bell-pending
                              (not (member bell-action '("none" "other") :test #'string=)))))
      (when relay-bell (%emit-bell buffer visual-bell))))
  (when (or (null active-pane)
            (screen-cursor-visible (pane-screen active-pane)))
    (cursor-visible buffer)
    (when active-pane
      (set-cursor-shape buffer (screen-cursor-shape (pane-screen active-pane))))))

(defun %render-background-bells (buffer session active-window)
  "Drain and relay BEL characters from all non-active windows.
   bell-action 'any': relay bells from every non-active window pane.
   bell-action 'other': same (non-active is by definition 'other').
   bell-action 'current'/'none': no relay from background windows."
  (let* ((bell-action  (or (cl-tmux/options:get-option "bell-action") "any"))
         (visual-bell  (cl-tmux/options:get-option "visual-bell"))
         (relay-p      (member bell-action '("any" "other") :test #'string=)))
    ;; Always consume pending bells (a bell suppressed by bell-action must not
    ;; ring later when its window becomes active); relay only when permitted.
    (dolist (win (session-windows session))
      (unless (eq win active-window)
        (dolist (pane (window-panes win))
          (when (and (pane-screen pane)
                     (screen-consume-bell (pane-screen pane))
                     relay-p)
            (%emit-bell buffer visual-bell)))))))

(defun %drain-screen-queue (buffer panes queue-reader queue-writer option default allowed)
  "Drain queue contents for each pane into BUFFER when OPTION permits emission.
   QUEUE-READER reads the current queue from a screen object.
   QUEUE-WRITER clears the queue on a screen object after draining it.
   The actual read-and-clear is delegated to the terminal LOGIC layer via
   cl-tmux/terminal:screen-drain-queue so this presentation-layer code never
   mutates a screen slot directly."
  (let* ((mode (or (cl-tmux/options:get-option option default) default))
         (emit (member mode allowed :test #'string=)))
    (dolist (pane panes)
      (let ((screen (pane-screen pane)))
        (when screen
          (with-lock-held ((screen-lock screen))
            (let ((queued (screen-drain-queue screen queue-reader queue-writer)))
              (when emit
                (dolist (seq queued)
                  (write-string seq buffer))))))))))

(defun %render-passthrough (buffer panes)
  "Drain each pane's passthrough-queue into BUFFER; gated on allow-passthrough option."
  (%drain-screen-queue buffer panes
                       #'screen-passthrough-queue
                       (lambda (screen value)
                         (setf (screen-passthrough-queue screen) value))
                       "allow-passthrough" "off" '("on" "all")))

(defun %render-clipboard (buffer panes)
  "Drain each pane's clipboard-queue into BUFFER (OSC 52); gated on set-clipboard option."
  (%drain-screen-queue buffer panes
                       #'screen-clipboard-queue
                       (lambda (screen value)
                         (setf (screen-clipboard-queue screen) value))
                       "set-clipboard" "on" '("on" "external")))
