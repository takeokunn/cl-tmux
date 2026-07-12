(in-package #:cl-tmux)

;;;; PTY reader alert helpers.
;;;;
;;;; This file contains the remain-on-exit banner helpers and the shared
;;;; alert-action dispatch helpers used by runtime-reader.lisp.

;;; ANSI SGR sequence displayed on the pane when remain-on-exit is active.
;;; SGR 7 = reverse video; SGR 0 (implicit via reset) restores normal.
;;; Defined as a variable (not defconstant) because SBCL's DEFCONSTANT
;;; requires EQL identity across reloads, which string values fail.
(defvar +remain-on-exit-message+
  (format nil "~C[7m[Process exited]~C[m" #\Escape #\Escape)
  "Fallback reverse-video banner written to the pane screen when remain-on-exit is
   set but remain-on-exit-format is empty or fails to expand.")

(defun %num-or-empty (value)
  "Render VALUE as a decimal string when present, otherwise the empty string."
  (if value (format nil "~D" value) ""))

(defun %pane-death-context (pane)
  "A minimal format context carrying PANE's death record, so
   remain-on-exit-format can reference #{pane_dead_status} /
   #{pane_dead_signal} / #{pane_dead_time} (a full session context is
   intentionally not built on the reader thread)."
  (list :pane-dead        "1"
        :pane-dead-status (%num-or-empty (cl-tmux/model:pane-dead-status pane))
        :pane-dead-signal (%num-or-empty (cl-tmux/model:pane-dead-signal pane))
        :pane-dead-time   (%num-or-empty (cl-tmux/model:pane-dead-time pane))))

(defun %expand-remain-on-exit-format (pane)
  "Expand remain-on-exit-format for PANE, or NIL when the option is empty or
   expansion fails."
  (let ((fmt (ignore-errors
               (cl-tmux/options:get-option-for-context "remain-on-exit-format"
                                                       :pane pane))))
    (when (and fmt (plusp (length fmt)))
      (ignore-errors (cl-tmux/format:expand-format
                      fmt (%pane-death-context pane))))))

(defun %remain-on-exit-banner-text (pane)
  "Return the formatted remain-on-exit text for PANE, or NIL when unavailable."
  (%expand-remain-on-exit-format pane))

(defun %remain-on-exit-banner (pane)
  "The reverse-video banner for a pane kept open by remain-on-exit: the
   remain-on-exit-format option expanded as a format string and wrapped in reverse
   video.  Falls back to +remain-on-exit-message+ on any error or an empty result.
   Expanded against the pane's death-record context so the tmux default's
   #{pane_dead_status}/#{pane_dead_signal}/#{pane_dead_time} references resolve."
  (let ((text (%remain-on-exit-banner-text pane)))
    (if (and text (plusp (length text)))
        (format nil "~C[7m~A~C[m" #\Escape text #\Escape)
        +remain-on-exit-message+)))

(defun %write-remain-on-exit-banner (pane)
  "Write the remain-on-exit banner bytes to PANE's screen.
   This is a side-effectful helper extracted from reader-eof-state so the CPS
   state function itself remains pure (only returns the next state)."
  (let ((screen (pane-screen pane)))
    (when screen
      (let ((banner-bytes (babel:string-to-octets (%remain-on-exit-banner pane)
                                                  :encoding :utf-8)))
        (cl-tmux/terminal/emulator:screen-process-bytes screen banner-bytes)))))

;;; -- Alert-action dispatch table -------------------------------------------
;;;
;;; Maps (action current-p) to a fire decision.
;;; none → never, current → only current window, any → always,
;;; other (default) → only non-current windows.

(defmacro define-alert-action-rules (&rest rules)
  "Define %alert-action-fires-p as a cond dispatch over ACTION/CURRENT-P.
   Each RULE is (action-string result-form) where RESULT-FORM may reference
   the CURRENT-P variable.  A final (t ...) fallback arm handles the 'other'
   default."
  (let ((action-sym  (gensym "ACTION"))
        (current-sym (gensym "CURRENT-P")))
    `(defun %alert-action-fires-p (,action-sym ,current-sym)
       "Whether an activity/silence alert should fire given the ACTION
   (none/current/other/any) and whether the window is the CURRENT (viewed) one:
     none    → never;          current → only the current window;
     any     → always;         other (default) → only non-current windows."
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (action-str result) rule
                       `((string-equal ,action-sym ,action-str)
                         ,(subst current-sym 'current-p result))))
                   rules)))))

(define-alert-action-rules
  ("none"    nil)
  ("current" current-p)
  ("any"     t)
  ;; "other" (default): fires only for non-current windows.
  ("other"   (not current-p)))

(defun %window-is-current-p (win)
  "True when WIN is the active (currently-viewed) window of any registered session.
   Used to honour activity-action/silence-action's current-vs-other distinction."
  (and win
       (some (lambda (entry)
               (eq win (cl-tmux/model:session-active-window (cdr entry))))
             *server-sessions*)))

(defun %visual-alert-message-p (option-name)
  "True when OPTION-NAME (visual-bell / visual-activity / visual-silence — tmux
   off/on/both enums) requests the transient message overlay: on or both."
  (let ((value (cl-tmux/options:get-option option-name)))
    (and (stringp value)
         (member value '("on" "both") :test #'string-equal)
         t)))

(defun %show-window-alert-overlay (label win)
  "Show the shared transient overlay text for LABEL and WIN."
  (show-transient-overlay
   (format nil "~A in window ~A (~A)"
           label
           (cl-tmux/model:window-id win)
           (cl-tmux/model:window-name win))))

(defun %fire-window-alert (hook option-name label win)
  "Run HOOK for WIN and show LABEL when OPTION-NAME asks for a visual overlay."
  (cl-tmux/hooks:run-hooks hook win)
  (when (%visual-alert-message-p option-name)
    (%show-window-alert-overlay label win)))

(defun %mark-window-activity (win)
  "Mark WIN as having activity for monitor-activity: set the activity flag, fire
   the alert-activity hook, and show a visual-activity overlay when that option is
   on.  No-op when WIN is NIL, monitor-activity is off for WIN, the flag is already
   set, or activity-action says not to alert this window (none/current/other/any).
   Extracted from reader-reading-state so the alert-activity firing is
   unit-testable without a live PTY."
  (when (and win
             (cl-tmux/options:get-option-for-context "monitor-activity" :window win)
             (not (cl-tmux/model:window-activity-flag win))
             (%alert-action-fires-p
              (or (cl-tmux/options:get-option "activity-action") "other")
              (%window-is-current-p win)))
    (setf (cl-tmux/model:window-activity-flag win) t)
    (%fire-window-alert cl-tmux/hooks:+hook-alert-activity+
                        "visual-activity"
                        "Activity"
                        win)))

(defun %mark-window-bell (win pane)
  "Bell alert logic for a BEL left pending on PANE's screen (tmux alerts.c):
   entirely gated on monitor-bell.  Sets WIN's sticky bell flag (the status `!',
   cleared when the window is selected), fires the alert-bell hook, and shows the
   visual-bell message overlay (visual-bell on/both).  Like tmux's WINLINK_BELL,
   the alert only applies to a window that is not currently viewed; the audible
   relay for the viewed window is handled by the renderer.  The flag-transition
   guard keeps the hook/overlay firing once per alert, not once per PTY chunk."
  (let ((screen (and pane (cl-tmux/model:pane-screen pane))))
    (when (and win screen
               (cl-tmux/terminal:screen-bell-pending screen)
               (cl-tmux/options:get-option-for-context "monitor-bell" :window win)
               (not (cl-tmux/model:window-bell-flag win))
               (not (%window-is-current-p win)))
      (setf (cl-tmux/model:window-bell-flag win) t)
      ;; bell-action gates the alert itself (hook + visual message) for this
      ;; non-current window: any/other fire, none/current do not.
      (when (%alert-action-fires-p
             (or (cl-tmux/options:get-option "bell-action") "any")
             nil)
        (%fire-window-alert cl-tmux/hooks:+hook-alert-bell+
                            "visual-bell"
                            "Bell"
                            win)))))

(defun %update-window-on-pane-output (win pane)
  "Update window-level state when new bytes arrive on PANE's PTY.
   Stamps last-output-time, clears the silence flag (new output resets the
   silence timer), and fires the activity and bell alert logic.
   Extracted from reader-reading-state to keep the CPS state function focused
   on I/O dispatch."
  (when win
    ;; Always update last-output-time (used by monitor-silence timer).
    (setf (cl-tmux/model:window-last-output-time win) (get-universal-time))
    ;; Clear silence flag: new output resets the silence state.
    (setf (cl-tmux/model:window-silence-flag win) nil)
    ;; Activity flag + alert-activity hook + visual overlay.
    (%mark-window-activity win)
    ;; Sticky bell flag + alert-bell hook + visual-bell overlay.
    (%mark-window-bell win pane)))
