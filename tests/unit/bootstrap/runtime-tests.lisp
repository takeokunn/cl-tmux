(in-package #:cl-tmux/test)

;;;; global variables, pane-reader-loop, reader EOF, and alert actions

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

(describe "runtime-suite"

  ;;; ── Global variables exist and have sensible types ───────────────────────────

  ;; *running*, *dirty*, *resize-pending*, *term-rows*, *term-cols* are all boundp.
  (it "runtime-globals-exist"
    (expect (boundp 'cl-tmux::*running*))
    (expect (boundp 'cl-tmux::*dirty*))
    (expect (boundp 'cl-tmux::*resize-pending*))
    (expect (integerp cl-tmux::*term-rows*))
    (expect (integerp cl-tmux::*term-cols*)))

  ;; *term-rows* and *term-cols* both default to positive integers.
  (it "runtime-term-dimensions-positive-table"
    (dolist (row (list (list cl-tmux::*term-rows* "*term-rows*")
                       (list cl-tmux::*term-cols* "*term-cols*")))
      (destructuring-bind (val name) row
        (declare (ignore name))
        (expect (plusp val)))))

  ;; +max-message-log-entries+ is a positive integer constant.
  (it "runtime-max-message-log-entries-is-constant"
    (expect (constantp 'cl-tmux::+max-message-log-entries+))
    (expect (integerp cl-tmux::+max-message-log-entries+))
    (expect (plusp cl-tmux::+max-message-log-entries+)))

  ;; +reader-thread-join-timeout+ is a positive integer constant.
  (it "runtime-reader-thread-join-timeout-is-constant"
    (expect (integerp cl-tmux::+reader-thread-join-timeout+))
    (expect (plusp cl-tmux::+reader-thread-join-timeout+)))

  ;;; ── %pane-reader-loop ────────────────────────────────────────────────────────

  ;; %pane-reader-loop is a defined function (data/logic separation from start-reader-thread).
  (it "pane-reader-loop-is-fbound"
    (expect (fboundp 'cl-tmux::%pane-reader-loop)))

  ;; %pane-reader-loop exits immediately when *running* is NIL without error.
  (it "pane-reader-loop-exits-when-running-nil"
    (with-dead-pane (pane)
      (let ((cl-tmux::*running* nil)
            (cl-tmux::*dirty*   nil))
        (finishes (cl-tmux::%pane-reader-loop pane))
        (expect cl-tmux::*dirty* :to-be-falsy))))

  ;;; ── CPS reader states ────────────────────────────────────────────────────────

  ;; reader-eof-state returns NIL when remain-on-exit is not set.
  (it "reader-eof-state-returns-nil-without-remain-on-exit"
    (with-dead-pane (pane)
      (with-isolated-options ("remain-on-exit" nil)
        (expect (null (cl-tmux::reader-eof-state pane))))))

  ;; reader-eof-state returns #'reader-remain-on-exit-state when remain-on-exit is set.
  (it "reader-eof-state-returns-remain-state-when-option-set"
    (with-dead-pane (pane)
      (with-isolated-options ("remain-on-exit" t)
        (let ((result (cl-tmux::reader-eof-state pane)))
          (expect (functionp result))))))

  ;; reader-eof-state honors a PANE-LOCAL remain-on-exit override at
  ;; runtime: with the GLOBAL remain-on-exit NIL but the pane-local value set to
  ;; T, reader-eof-state must return the parking state #'reader-remain-on-exit-state
  ;; (proving runtime.lisp's get-option-for-pane read honors per-pane overrides).
  (it "reader-eof-state-honors-pane-local-remain-on-exit"
    (with-isolated-state
      (let* ((sess (make-fake-session))
             (pane (cl-tmux/model:session-active-pane sess))
             (cl-tmux::*dirty* nil))
        (cl-tmux/options:set-option "remain-on-exit" nil)
        (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
        (let ((result (cl-tmux::reader-eof-state pane)))
          (expect (eq #'cl-tmux::reader-remain-on-exit-state result))))))

  ;; %remain-on-exit-banner expands remain-on-exit-format and wraps it in
  ;; reverse video; an empty format falls back to the built-in message.
  (it "remain-on-exit-banner-uses-format-option"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 3))))
      (with-isolated-options ("remain-on-exit-format" "DEAD")
        (let ((banner (cl-tmux::%remain-on-exit-banner pane)))
          (expect (search "DEAD" banner))
          (expect (search (format nil "~C[7m" #\Escape) banner))))
      (with-isolated-options ("remain-on-exit-format" "")
        (expect (string= cl-tmux::+remain-on-exit-message+
                         (cl-tmux::%remain-on-exit-banner pane))))))

  ;; reader-eof-state writes the remain-on-exit-format banner to the pane
  ;; screen when remain-on-exit is set.
  (it "reader-eof-state-writes-format-banner-to-screen"
    (with-isolated-hooks
      (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 3))))
        (with-isolated-options ("remain-on-exit" t "remain-on-exit-format" "BYE")
          (cl-tmux::reader-eof-state pane)
          (expect (search "BYE" (row-string (pane-screen pane) 0 :end 10)))))))

  ;; Pins the per-window resolution at the migrated reader-reading-state
  ;; activity-flag site (src/runtime.lisp): that site reads
  ;; (get-option-for-context "monitor-activity" :window win) to decide whether to
  ;; set window-activity-flag for a non-active window.  reader-reading-state itself
  ;; needs a live PTY fd (pty-read-blocking; fake panes have fd -1 → immediate EOF,
  ;; not useful), so we directly assert the OBSERVABLE decision the migrated site
  ;; makes: with global monitor-activity NIL, a window whose LOCAL value is on
  ;; resolves T (activity tracked), while a window with no override resolves NIL
  ;; (opted out).
  (it "reader-reading-state-honors-window-local-monitor-activity"
    (with-isolated-state
      ;; >=2 windows so there is a NON-ACTIVE background window (the activity-flag
      ;; path only fires for non-active windows).
      (let* ((sess        (make-fake-session :nwindows 2))
             (active-win  (cl-tmux/model:session-active-window sess))
             (bg-win      (find-if-not (lambda (w) (eq w active-win))
                                       (cl-tmux/model:session-windows sess))))
        (expect (not (null bg-win)))
        (cl-tmux/options:set-option "monitor-activity" nil)              ; global = NIL
        ;; Window-local "on" on the background window.
        (cl-tmux/options:set-option-for-window "monitor-activity" "on" bg-win)
        (expect (eq t (cl-tmux/options:get-option-for-context "monitor-activity" :window bg-win)))
        ;; The active window has no local override → resolves to global NIL.
        (expect (null (cl-tmux/options:get-option-for-context "monitor-activity" :window active-win))))))

  ;; Companion falsey-honoring check at the same migrated site: with
  ;; global monitor-activity on, a window whose LOCAL value is off (NIL) opts out —
  ;; the per-window read returns NIL, proving the present-but-falsey window override
  ;; is honored at the reader-reading-state activity-flag site.
  (it "reader-reading-state-window-local-monitor-activity-off-over-global-on"
    (with-isolated-state
      (let* ((sess       (make-fake-session :nwindows 2))
             (active-win (cl-tmux/model:session-active-window sess))
             (bg-win     (find-if-not (lambda (w) (eq w active-win))
                                      (cl-tmux/model:session-windows sess))))
        (cl-tmux/options:set-option "monitor-activity" t)               ; global = T
        (cl-tmux/options:set-option-for-window "monitor-activity" "off" bg-win) ; window = NIL
        (expect (null (cl-tmux/options:get-option-for-context "monitor-activity" :window bg-win))))))

  ;; %mark-window-activity sets the activity flag AND fires the
  ;; alert-activity hook (tmux alert hook, previously never fired).
  (it "mark-window-activity-fires-alert-activity-hook"
    (with-isolated-state
      (let* ((sess  (make-fake-session :nwindows 1))
             (win   (cl-tmux/model:session-active-window sess))
             (fired nil))
        (cl-tmux/options:set-option "monitor-activity" "on")
        (setf (cl-tmux/model:window-activity-flag win) nil)
        (cl-tmux/hooks:add-hook "alert-activity"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%mark-window-activity win)
        (expect (cl-tmux/model:window-activity-flag win) :to-be-truthy)
        (expect fired :to-be-truthy))))

  ;; %mark-window-bell on a non-current window with a pending BEL sets
  ;; the sticky window bell flag (tmux WINLINK_BELL) and fires the alert-bell hook
  ;; with the window argument.
  (it "mark-window-bell-sets-sticky-flag-and-fires-hook"
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
        (expect (cl-tmux/model:window-bell-flag win) :to-be-truthy)
        (expect (eq win hook-win)))))

  ;; %mark-window-bell gating: monitor-bell off disables everything;
  ;; bell-action none sets the flag but suppresses the alert hook; no pending BEL
  ;; is a no-op.  Each row: (monitor-bell bell-action pending expect-flag
  ;; expect-hook description).
  (it "mark-window-bell-gating-table"
    (dolist (row '((nil "any"  t   nil nil "monitor-bell off must disable the bell alert")
                   (t   "none" t   t   nil "bell-action none must set the flag but not fire the hook")
                   (t   "current" t t  nil "bell-action current must not alert a non-current window")
                   (t   "any"  nil nil nil "no pending BEL must be a no-op")))
      (destructuring-bind (monitor action pending expect-flag expect-hook desc) row
        (declare (ignore desc))
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
            (expect (eq expect-flag (cl-tmux/model:window-bell-flag win)))
            (expect (eq expect-hook fired)))))))

  ;; %mark-window-bell does not flag the currently-viewed window
  ;; (tmux sets WINLINK_BELL only for non-current winlinks).
  (it "mark-window-bell-current-window-is-noop"
    (with-isolated-state
      (let* ((sess (make-fake-session :nwindows 1))
             (win  (cl-tmux/model:session-active-window sess))
             (pane (first (cl-tmux/model:window-panes win)))
             (cl-tmux::*server-sessions* (list (cons 1 sess))))
        (setf (cl-tmux/terminal/types:screen-bell-pending
               (cl-tmux/model:pane-screen pane)) t)
        (cl-tmux::%mark-window-bell win pane)
        (expect (cl-tmux/model:window-bell-flag win) :to-be-falsy))))

  ;; visual-bell on/both shows tmux's 'Bell in window N' transient
  ;; overlay from the bell alert path.
  (it "mark-window-bell-visual-bell-shows-overlay"
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
                (expect (search "Bell in window" (or cl-tmux/prompt:*overlay* "")))
                (expect (null cl-tmux/prompt:*overlay*))))))))

  ;; Selecting a window clears its sticky bell flag (tmux clears
  ;; WINLINK_BELL when the window is viewed).
  (it "session-select-window-clears-bell-flag"
    (let* ((sess (make-fake-session :nwindows 2))
           (win  (second (cl-tmux/model:session-windows sess))))
      (setf (cl-tmux/model:window-bell-flag win) t)
      (cl-tmux/model:session-select-window sess win)
      (expect (cl-tmux/model:window-bell-flag win) :to-be-falsy)))

  ;; %check-monitor-silence fires the alert-silence hook when a window
  ;; crosses the silence threshold (tmux alert hook, previously never fired).
  (it "monitor-silence-fires-alert-silence-hook"
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
        (expect (cl-tmux/model:window-silence-flag win) :to-be-truthy)
        (expect fired :to-be-truthy))))

  ;; With the registered default monitor-silence = 0, %check-monitor-silence
  ;; is a no-op: no window crosses a (disabled) threshold, so no flag is set.
  (it "monitor-silence-default-is-zero-no-op"
    (with-isolated-state
      (let* ((sess (make-fake-session :nwindows 1))
             (win  (cl-tmux/model:session-active-window sess)))
        (expect (eql 0 (cl-tmux/options:get-option "monitor-silence")))
        ;; Window has been silent for a long time, but monitoring is off (0).
        (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
              (cl-tmux/model:window-silence-flag win) nil)
        (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
        (expect (cl-tmux/model:window-silence-flag win) :to-be-falsy))))

  ;; When visual-silence is on, crossing the silence threshold shows a
  ;; transient overlay naming the quiet window (mirrors visual-activity).
  (it "monitor-silence-visual-silence-shows-overlay"
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

  ;; %alert-action-fires-p maps an activity/silence action × current-ness to a fire
  ;; decision: none→never, current→only current, any→always, other→only non-current.
  (it "alert-action-fires-p-policy-matrix"
    (expect (cl-tmux::%alert-action-fires-p "none" t) :to-be-falsy)
    (expect (cl-tmux::%alert-action-fires-p "none" nil) :to-be-falsy)
    (expect (cl-tmux::%alert-action-fires-p "current" t) :to-be-truthy)
    (expect (cl-tmux::%alert-action-fires-p "current" nil) :to-be-falsy)
    (expect (cl-tmux::%alert-action-fires-p "any" t) :to-be-truthy)
    (expect (cl-tmux::%alert-action-fires-p "any" nil) :to-be-truthy)
    (expect (cl-tmux::%alert-action-fires-p "other" t) :to-be-falsy)
    (expect (cl-tmux::%alert-action-fires-p "other" nil) :to-be-truthy))

  ;; silence-action none suppresses the silence alert (and flag) even when the
  ;; threshold is crossed.
  (it "silence-action-none-suppresses-alert"
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
        (expect fired :to-be-falsy)
        (expect (cl-tmux/model:window-silence-flag win) :to-be-falsy)))))
