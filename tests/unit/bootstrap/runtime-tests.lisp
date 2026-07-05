(in-package #:cl-tmux/test)

;;;; global variables, pane-reader-loop, reader EOF, and alert actions

(def-suite runtime-suite :description "Runtime state variables and threading utilities")
(in-suite runtime-suite)

;;; ── Test fixture macros ──────────────────────────────────────────────────────

(defmacro with-dead-pane ((pane-var) &body body)
  "Bind PANE-VAR to a standard dead pane (fd=-1, pid=-1, 5×3 screen) for BODY.
   Eliminates the repeated (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))
   boilerplate."
  `(let ((,pane-var (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
     ,@body))

(defmacro with-isolated-state (&body body)
  "Run BODY with both config and hooks isolated (combined with-isolated-config +
   with-isolated-hooks).  Eliminates the double nesting in tests that touch both
   option reads and hook firing."
  `(with-isolated-config
     (with-isolated-hooks
       ,@body)))

;;; ── Global variables exist and have sensible types ───────────────────────────

(test runtime-globals-exist
  :description "*running*, *dirty*, *resize-pending*, *term-rows*, *term-cols* are all boundp."
  (is (boundp 'cl-tmux::*running*)        "*running* must be bound")
  (is (boundp 'cl-tmux::*dirty*)          "*dirty* must be bound")
  (is (boundp 'cl-tmux::*resize-pending*) "*resize-pending* must be bound")
  (is (integerp cl-tmux::*term-rows*)     "*term-rows* must be an integer")
  (is (integerp cl-tmux::*term-cols*)     "*term-cols* must be an integer"))

(test runtime-term-dimensions-positive-table
  "*term-rows* and *term-cols* both default to positive integers."
  (dolist (row (list (list cl-tmux::*term-rows* "*term-rows*")
                     (list cl-tmux::*term-cols* "*term-cols*")))
    (destructuring-bind (val name) row
      (is (plusp val) "~A must be positive, got ~D" name val))))

(test runtime-max-message-log-entries-is-constant
  :description "+max-message-log-entries+ is a positive integer constant."
  (is (constantp 'cl-tmux::+max-message-log-entries+) "+max-message-log-entries+ must be a constant")
  (is (integerp cl-tmux::+max-message-log-entries+) "constant must be an integer")
  (is (plusp cl-tmux::+max-message-log-entries+) "constant must be positive"))

(test runtime-reader-thread-join-timeout-is-constant
  :description "+reader-thread-join-timeout+ is a positive integer constant."
  (is (integerp cl-tmux::+reader-thread-join-timeout+) "join timeout must be an integer")
  (is (plusp cl-tmux::+reader-thread-join-timeout+)    "join timeout must be positive"))

;;; ── %pane-reader-loop ────────────────────────────────────────────────────────

(test pane-reader-loop-is-fbound
  :description "%pane-reader-loop is a defined function (data/logic separation from start-reader-thread)."
  (is (fboundp 'cl-tmux::%pane-reader-loop)
      "%pane-reader-loop must be fbound"))

(test pane-reader-loop-exits-when-running-nil
  :description "%pane-reader-loop exits immediately when *running* is NIL without error."
  (with-dead-pane (pane)
    (let ((cl-tmux::*running* nil)
          (cl-tmux::*dirty*   nil))
      (finishes (cl-tmux::%pane-reader-loop pane)
                "%pane-reader-loop must return cleanly when *running* is NIL")
      (is-false cl-tmux::*dirty* "*dirty* must remain NIL when loop exits immediately"))))

;;; ── CPS reader states ────────────────────────────────────────────────────────

(test reader-eof-state-returns-nil-without-remain-on-exit
  :description "reader-eof-state returns NIL when remain-on-exit is not set."
  (with-dead-pane (pane)
    (with-isolated-options ("remain-on-exit" nil)
      (is (null (cl-tmux::reader-eof-state pane))
          "reader-eof-state must return NIL when remain-on-exit is not set"))))

(test reader-eof-state-returns-remain-state-when-option-set
  :description "reader-eof-state returns #'reader-remain-on-exit-state when remain-on-exit is set."
  (with-dead-pane (pane)
    (with-isolated-options ("remain-on-exit" t)
      (let ((result (cl-tmux::reader-eof-state pane)))
        (is (functionp result)
            "reader-eof-state must return a function when remain-on-exit is set")))))

(test reader-eof-state-honors-pane-local-remain-on-exit
  :description "reader-eof-state honors a PANE-LOCAL remain-on-exit override at
   runtime: with the GLOBAL remain-on-exit NIL but the pane-local value set to
   T, reader-eof-state must return the parking state #'reader-remain-on-exit-state
   (proving runtime.lisp's get-option-for-pane read honors per-pane overrides)."
  (with-isolated-state
    (let* ((sess (make-fake-session))
           (pane (cl-tmux/model:session-active-pane sess))
           (cl-tmux::*dirty* nil))
      (cl-tmux/options:set-option "remain-on-exit" nil)
      (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
      (let ((result (cl-tmux::reader-eof-state pane)))
        (is (eq #'cl-tmux::reader-remain-on-exit-state result)
            "reader-eof-state must return the remain-on-exit parking state when the
             pane-local override is set, even though the global value is NIL")))))

(test remain-on-exit-banner-uses-format-option
  :description "%remain-on-exit-banner expands remain-on-exit-format and wraps it in
   reverse video; an empty format falls back to the built-in message."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 3))))
    (with-isolated-options ("remain-on-exit-format" "DEAD")
      (let ((banner (cl-tmux::%remain-on-exit-banner pane)))
        (is (search "DEAD" banner) "banner must contain the format text (got ~S)" banner)
        (is (search (format nil "~C[7m" #\Escape) banner)
            "banner must be reverse-video (SGR 7)")))
    (with-isolated-options ("remain-on-exit-format" "")
      (is (string= cl-tmux::+remain-on-exit-message+
                   (cl-tmux::%remain-on-exit-banner pane))
          "empty format must fall back to the built-in message"))))

(test reader-eof-state-writes-format-banner-to-screen
  :description "reader-eof-state writes the remain-on-exit-format banner to the pane
   screen when remain-on-exit is set."
  (with-isolated-hooks
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 3))))
      (with-isolated-options ("remain-on-exit" t "remain-on-exit-format" "BYE")
        (cl-tmux::reader-eof-state pane)
        (is (search "BYE" (row-string (pane-screen pane) 0 :end 10))
            "the pane screen must show the custom banner text")))))

(test reader-reading-state-honors-window-local-monitor-activity
  :description "Pins the per-window resolution at the migrated reader-reading-state
   activity-flag site (src/runtime.lisp): that site reads
   (get-option-for-context \"monitor-activity\" :window win) to decide whether to
   set window-activity-flag for a non-active window.  reader-reading-state itself
   needs a live PTY fd (pty-read-blocking; fake panes have fd -1 → immediate EOF,
   not useful), so we directly assert the OBSERVABLE decision the migrated site
   makes: with global monitor-activity NIL, a window whose LOCAL value is on
   resolves T (activity tracked), while a window with no override resolves NIL
   (opted out)."
  (with-isolated-state
    ;; >=2 windows so there is a NON-ACTIVE background window (the activity-flag
    ;; path only fires for non-active windows).
    (let* ((sess        (make-fake-session :nwindows 2))
           (active-win  (cl-tmux/model:session-active-window sess))
           (bg-win      (find-if-not (lambda (w) (eq w active-win))
                                     (cl-tmux/model:session-windows sess))))
      (is (not (null bg-win)) "must have a non-active background window")
      (cl-tmux/options:set-option "monitor-activity" nil)              ; global = NIL
      ;; Window-local "on" on the background window.
      (cl-tmux/options:set-option-for-window "monitor-activity" "on" bg-win)
      (is (eq t (cl-tmux/options:get-option-for-context "monitor-activity" :window bg-win))
          "window-local on must resolve T at the migrated read site (global NIL)")
      ;; The active window has no local override → resolves to global NIL.
      (is (null (cl-tmux/options:get-option-for-context "monitor-activity" :window active-win))
          "a window without the override must resolve NIL (global NIL)"))))

(test reader-reading-state-window-local-monitor-activity-off-over-global-on
  :description "Companion falsey-honoring check at the same migrated site: with
   global monitor-activity on, a window whose LOCAL value is off (NIL) opts out —
   the per-window read returns NIL, proving the present-but-falsey window override
   is honored at the reader-reading-state activity-flag site."
  (with-isolated-state
    (let* ((sess       (make-fake-session :nwindows 2))
           (active-win (cl-tmux/model:session-active-window sess))
           (bg-win     (find-if-not (lambda (w) (eq w active-win))
                                    (cl-tmux/model:session-windows sess))))
      (cl-tmux/options:set-option "monitor-activity" t)               ; global = T
      (cl-tmux/options:set-option-for-window "monitor-activity" "off" bg-win) ; window = NIL
      (is (null (cl-tmux/options:get-option-for-context "monitor-activity" :window bg-win))
          "window-local off (NIL) must win over global on (T) at the migrated site"))))

(test mark-window-activity-fires-alert-activity-hook
  :description "%mark-window-activity sets the activity flag AND fires the
   alert-activity hook (tmux alert hook, previously never fired)."
  (with-isolated-state
    (let* ((sess  (make-fake-session :nwindows 1))
           (win   (cl-tmux/model:session-active-window sess))
           (fired nil))
      (cl-tmux/options:set-option "monitor-activity" "on")
      (setf (cl-tmux/model:window-activity-flag win) nil)
      (cl-tmux/hooks:add-hook "alert-activity"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%mark-window-activity win)
      (is-true (cl-tmux/model:window-activity-flag win) "activity flag must be set")
      (is-true fired "the alert-activity hook must fire"))))

(test mark-window-bell-sets-sticky-flag-and-fires-hook
  :description "%mark-window-bell on a non-current window with a pending BEL sets
   the sticky window bell flag (tmux WINLINK_BELL) and fires the alert-bell hook
   with the window argument."
  (with-isolated-state
    (let* ((sess     (make-fake-session :nwindows 1))
           (win      (cl-tmux/model:session-active-window sess))
           (pane     (first (cl-tmux/model:window-panes win)))
           (hook-win :unset))
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane)) t)
      (cl-tmux/hooks:add-hook "alert-bell"
                              (lambda (&rest args) (setf hook-win (first args))))
      (cl-tmux::%mark-window-bell win pane)
      (is-true (cl-tmux/model:window-bell-flag win)
               "the sticky bell flag must be set")
      (is (eq win hook-win)
          "the alert-bell hook must fire with the window argument"))))

(test mark-window-bell-gating-table
  :description "%mark-window-bell gating: monitor-bell off disables everything;
   bell-action none sets the flag but suppresses the alert hook; no pending BEL
   is a no-op.  Each row: (monitor-bell bell-action pending expect-flag
   expect-hook description)."
  (dolist (row '((nil "any"  t   nil nil "monitor-bell off must disable the bell alert")
                 (t   "none" t   t   nil "bell-action none must set the flag but not fire the hook")
                 (t   "current" t t  nil "bell-action current must not alert a non-current window")
                 (t   "any"  nil nil nil "no pending BEL must be a no-op")))
    (destructuring-bind (monitor action pending expect-flag expect-hook desc) row
      (with-isolated-state
        (let* ((sess  (make-fake-session :nwindows 1))
               (win   (cl-tmux/model:session-active-window sess))
               (pane  (first (cl-tmux/model:window-panes win)))
               (fired nil))
          (cl-tmux/options:set-option "monitor-bell" monitor)
          (cl-tmux/options:set-option "bell-action" action)
          (setf (cl-tmux/terminal/types:screen-bell-pending
                 (cl-tmux/model:pane-screen pane)) pending)
          (cl-tmux/hooks:add-hook "alert-bell"
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%mark-window-bell win pane)
          (is (eq expect-flag (cl-tmux/model:window-bell-flag win)) "~A (flag)" desc)
          (is (eq expect-hook fired) "~A (hook)" desc))))))

(test mark-window-bell-current-window-is-noop
  :description "%mark-window-bell does not flag the currently-viewed window
   (tmux sets WINLINK_BELL only for non-current winlinks)."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (pane (first (cl-tmux/model:window-panes win)))
           (cl-tmux::*server-sessions* (list (cons 1 sess))))
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane)) t)
      (cl-tmux::%mark-window-bell win pane)
      (is-false (cl-tmux/model:window-bell-flag win)
                "the current window must not get the sticky bell flag"))))

(test mark-window-bell-visual-bell-shows-overlay
  :description "visual-bell on/both shows tmux's 'Bell in window N' transient
   overlay from the bell alert path."
  (dolist (row '(("on" t) ("both" t) ("off" nil)))
    (destructuring-bind (visual expect-overlay) row
      (with-isolated-state
        (let* ((sess (make-fake-session :nwindows 1))
               (win  (cl-tmux/model:session-active-window sess))
               (pane (first (cl-tmux/model:window-panes win)))
               (cl-tmux/prompt:*overlay* nil))
          (cl-tmux/options:set-option "visual-bell" visual)
          (setf (cl-tmux/terminal/types:screen-bell-pending
                 (cl-tmux/model:pane-screen pane)) t)
          (cl-tmux::%mark-window-bell win pane)
          (if expect-overlay
              (is (search "Bell in window" (or cl-tmux/prompt:*overlay* ""))
                  "visual-bell ~A must show the bell message overlay" visual)
              (is (null cl-tmux/prompt:*overlay*)
                  "visual-bell off must not show an overlay")))))))

(test session-select-window-clears-bell-flag
  :description "Selecting a window clears its sticky bell flag (tmux clears
   WINLINK_BELL when the window is viewed)."
  (let* ((sess (make-fake-session :nwindows 2))
         (win  (second (cl-tmux/model:session-windows sess))))
    (setf (cl-tmux/model:window-bell-flag win) t)
    (cl-tmux/model:session-select-window sess win)
    (is-false (cl-tmux/model:window-bell-flag win)
              "session-select-window must clear the bell flag")))

(test monitor-silence-fires-alert-silence-hook
  :description "%check-monitor-silence fires the alert-silence hook when a window
   crosses the silence threshold (tmux alert hook, previously never fired)."
  (with-isolated-state
    (let* ((sess  (make-fake-session :nwindows 1))
           (win   (cl-tmux/model:session-active-window sess))
           (fired nil))
      (cl-tmux/options:set-option "monitor-silence" 5)
      ;; silence-action "any" so the alert fires even for the (current) window
      ;; under test (default "other" would suppress the current window).
      (cl-tmux/options:set-option "silence-action" "any")
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux/hooks:add-hook "alert-silence"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-true (cl-tmux/model:window-silence-flag win) "silence flag must be set")
      (is-true fired "the alert-silence hook must fire"))))

(test monitor-silence-default-is-zero-no-op
  :description "With the registered default monitor-silence = 0, %check-monitor-silence
   is a no-op: no window crosses a (disabled) threshold, so no flag is set."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess)))
      (is (eql 0 (cl-tmux/options:get-option "monitor-silence"))
          "monitor-silence must default to 0 (registered)")
      ;; Window has been silent for a long time, but monitoring is off (0).
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-false (cl-tmux/model:window-silence-flag win)
                "monitor-silence 0 must not set the silence flag"))))

(test monitor-silence-visual-silence-shows-overlay
  :description "When visual-silence is on, crossing the silence threshold shows a
   transient overlay naming the quiet window (mirrors visual-activity)."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (cl-tmux/prompt:*overlay* nil))
      (cl-tmux/options:set-option "monitor-silence" 5)
      (cl-tmux/options:set-option "silence-action" "any")  ; fire for the current window
      (cl-tmux/options:set-option "visual-silence" "on")
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (assert-overlay-active
       "visual-silence must show an overlay when silence is detected"))))

(test alert-action-fires-p-policy-matrix
  "%alert-action-fires-p maps an activity/silence action × current-ness to a fire
   decision: none→never, current→only current, any→always, other→only non-current."
  (is-false (cl-tmux::%alert-action-fires-p "none" t))
  (is-false (cl-tmux::%alert-action-fires-p "none" nil))
  (is-true  (cl-tmux::%alert-action-fires-p "current" t))
  (is-false (cl-tmux::%alert-action-fires-p "current" nil))
  (is-true  (cl-tmux::%alert-action-fires-p "any" t))
  (is-true  (cl-tmux::%alert-action-fires-p "any" nil))
  (is-false (cl-tmux::%alert-action-fires-p "other" t))
  (is-true  (cl-tmux::%alert-action-fires-p "other" nil)))

(test silence-action-none-suppresses-alert
  "silence-action none suppresses the silence alert (and flag) even when the
   threshold is crossed."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (fired nil))
      (cl-tmux/options:set-option "monitor-silence" 5)
      (cl-tmux/options:set-option "silence-action" "none")
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux/hooks:add-hook "alert-silence"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-false fired "silence-action none must suppress the alert hook")
      (is-false (cl-tmux/model:window-silence-flag win)
                "silence-action none must not set the silence flag"))))
