(in-package #:cl-tmux/test)

;;;; global variables, reader-thread CPS, stop-reader-threads, wait-for-channel, status timer, remain-on-exit — part I

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
      (cl-tmux/options:set-option "visual-silence" t)
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (assert-overlay-active
       "visual-silence must show an overlay when silence is detected"))))

(test prompt-history-persists-to-history-file
  "add-prompt-history saves to history-file and load-prompt-history restores it
   (newest first)."
  (with-fresh-options
    (let ((path (format nil "~A/cl-tmux-hist-~D.txt"
                        (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                        (get-universal-time))))
      (unwind-protect
           (progn
             (let ((cl-tmux::*prompt-history* nil))
               (cl-tmux/options:set-option "history-file" path)
               (cl-tmux::add-prompt-history "first")
               (cl-tmux::add-prompt-history "second"))
             ;; Fresh in-memory history; loading from the file restores both.
             (let ((cl-tmux::*prompt-history* nil))
               (cl-tmux::load-prompt-history)
               (is (equal '("second" "first") cl-tmux::*prompt-history*)
                   "loaded history must be newest-first")))
        (ignore-errors (delete-file path))))))

(test prompt-history-no-file-is-in-memory-only
  "With history-file unset (default \"\"), history stays in memory and add does not error."
  (with-fresh-options
    (let ((cl-tmux::*prompt-history* nil))
      (is (null (cl-tmux::%prompt-history-path))
          "no history path when history-file is empty")
      (cl-tmux::add-prompt-history "x")
      (is (equal '("x") cl-tmux::*prompt-history*)
          "in-memory history still works without a file"))))

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

(test reader-remain-on-exit-state-returns-nil-when-not-running
  :description "reader-remain-on-exit-state returns NIL immediately when *running* is NIL."
  (with-dead-pane (pane)
    (let ((cl-tmux::*running* nil))
      (is (null (cl-tmux::reader-remain-on-exit-state pane))
          "remain-on-exit state must return NIL when *running* is NIL"))))

;;; Table-driven fbound checks for CPS reader state functions.
(test reader-state-functions-are-all-fbound
  :description "All CPS reader state machine functions are defined."
  (dolist (sym '(cl-tmux::reader-idle-state
                 cl-tmux::reader-reading-state
                 cl-tmux::reader-remain-on-exit-state
                 cl-tmux::reader-eof-state
                 cl-tmux::%run-reader-states
                 cl-tmux::start-reader-thread
                 cl-tmux::install-sigwinch-handler
                 cl-tmux::start-status-timer))
    (is (fboundp sym) "~A must be fbound" sym)))

(test run-reader-states-exits-when-running-nil
  :description "%run-reader-states exits immediately when *running* is NIL, even
given a non-NIL initial state (loop while *running*)."
  (with-dead-pane (pane)
    (let* ((cl-tmux::*running* nil)
           ;; A state function that should never be called.
           (boom (lambda (_p) (declare (ignore _p))
                   (error "state function called despite *running*=NIL"))))
      (finishes (cl-tmux::%run-reader-states pane boom)
                "%run-reader-states must exit immediately when *running* is NIL"))))

;;; ── %cap-list ─────────────────────────────────────────────────────────────────

(test cap-list-returns-list-unchanged-when-under-limit
  "%cap-list returns the list unchanged when its length is <= limit."
  (let ((lst '(1 2 3)))
    (is (equal '(1 2 3) (cl-tmux::%cap-list lst 5))
        "%cap-list must return list unchanged when length <= limit")
    (is (equal '(1 2 3) (cl-tmux::%cap-list lst 3))
        "%cap-list must return list unchanged when length == limit")))

(test cap-list-truncates-when-over-limit
  "%cap-list returns a subseq of at most LIMIT elements when the list is longer."
  (let ((lst '(a b c d e)))
    (is (equal '(a b c) (cl-tmux::%cap-list lst 3))
        "%cap-list must truncate to exactly LIMIT elements")))

(test cap-list-returns-nil-for-nil-input
  "%cap-list returns NIL for NIL input (empty list)."
  (is (null (cl-tmux::%cap-list nil 5))
      "%cap-list of NIL must return NIL"))

(test cap-list-returns-nil-for-zero-limit
  "%cap-list returns NIL when limit is 0."
  (is (null (cl-tmux::%cap-list '(1 2 3) 0))
      "%cap-list with limit 0 must return NIL"))

;;; ── with-channel-plist macro ──────────────────────────────────────────────────

(test with-channel-plist-binds-lock-and-cv
  "with-channel-plist binds LK and CV to the :lock and :cv fields of a channel plist."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch (cl-tmux::%ensure-channel "wplist-test")))
      (cl-tmux::with-channel-plist (lk cv ch)
        (is (eq (getf ch :lock) lk) "LK must be the :lock field")
        (is (eq (getf ch :cv) cv)   "CV must be the :cv field")))))

(test with-channel-plist-is-a-macro
  "with-channel-plist is defined as a macro."
  (is (macro-function 'cl-tmux::with-channel-plist)
      "with-channel-plist must be a macro"))

;;; ── %read-history-lines ──────────────────────────────────────────────────────

(test read-history-lines-returns-lines-reversed
  "%read-history-lines reads non-empty lines from a stream and returns them newest-first."
  (let ((content (format nil "line1~%line2~%line3~%")))
    (with-input-from-string (stream content)
      (let ((result (cl-tmux::%read-history-lines stream)))
        (is (equal '("line3" "line2" "line1") result)
            "%read-history-lines must return lines reversed (newest first)")))))

(test read-history-lines-skips-empty-lines
  "%read-history-lines skips empty lines in the stream."
  (let ((content (format nil "first~%~%second~%")))
    (with-input-from-string (stream content)
      (let ((result (cl-tmux::%read-history-lines stream)))
        (is (= 2 (length result))
            "%read-history-lines must skip empty lines")
        (is (member "first" result :test #'string=) "first must appear")
        (is (member "second" result :test #'string=) "second must appear")))))

(test read-history-lines-returns-nil-for-empty-stream
  "%read-history-lines returns NIL when stream is empty."
  (with-input-from-string (stream "")
    (let ((result (cl-tmux::%read-history-lines stream)))
      (is (null result)
          "%read-history-lines must return NIL for empty stream"))))

;;; ── %message-log-limit ────────────────────────────────────────────────────────

(test message-log-limit-returns-option-when-set
  "%message-log-limit returns the message-limit option value when set."
  (with-isolated-options ("message-limit" 42)
    (is (= 42 (cl-tmux::%message-log-limit))
        "%message-log-limit must return the option value when set")))

(test message-log-limit-returns-default-when-unset
  "%message-log-limit falls back to +max-message-log-entries+ when option is unset.
   with-fresh-options alone is not enough here: message-limit is a KNOWN tmux
   option (registered with a table default of 1000 in *known-option-registry*),
   so get-option still resolves it even with an empty *option-registry* (mirrors
   set-option -u semantics).  Clearing *known-option-registry* too makes the
   option genuinely unknown, exercising %message-log-limit's OR fallback."
  (with-fresh-options
    (let ((cl-tmux/options::*known-option-registry* (make-hash-table :test #'equal)))
      (is (= cl-tmux::+max-message-log-entries+
             (cl-tmux::%message-log-limit))
          "%message-log-limit must fall back to the default constant"))))

;;; ── %append-message-log-entry ─────────────────────────────────────────────────

(test append-message-log-entry-prepends
  "%append-message-log-entry prepends the entry to the log."
  (with-isolated-options ("message-limit" 100)
    (let* ((log nil)
           (entry (cons (get-universal-time) "hello"))
           (result (cl-tmux::%append-message-log-entry log entry)))
      (is (= 1 (length result)) "result must have exactly 1 entry")
      (is (eq entry (first result)) "entry must be first"))))

(test append-message-log-entry-caps-at-limit
  "%append-message-log-entry caps the log at the effective message-limit."
  (with-isolated-options ("message-limit" 3)
    (let* ((old-log (list (cons 1 "a") (cons 2 "b") (cons 3 "c")))
           (new-entry (cons 4 "d"))
           (result (cl-tmux::%append-message-log-entry old-log new-entry)))
      (is (= 3 (length result)) "result must not exceed limit=3")
      (is (eq new-entry (first result)) "newest entry must be first"))))
