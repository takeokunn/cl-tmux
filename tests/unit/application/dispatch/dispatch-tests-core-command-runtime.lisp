(in-package #:cl-tmux/test)

;;;; Command dispatch tests: runtime hooks, command tables, and target context.

(in-suite dispatch-suite)

;;; ── run-command-hooks ────────────────────────────────────────────────────────

(test run-command-hooks-fires-for-session-target
  "run-command-hooks dispatches the registered command hook for a session target.
   run-command-hooks consults cl-tmux/hooks:*command-hooks* (populated by
   set-command-hook / the set-hook config directive), not the lisp-callback
   registry that add-hook populates — that registry is fired by run-hooks."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 1)
      (with-command-test-state (s :overlay t)
        (cl-tmux/hooks:set-command-hook "after-new-window" :list-windows)
        (cl-tmux::run-command-hooks "after-new-window" s)
        (assert-overlay-active
         "run-command-hooks must dispatch the registered command hook")))))

(test run-command-hooks-noop-for-nil-target
  "run-command-hooks is a no-op when TARGET is NIL (no session to dispatch against)."
  (with-isolated-hooks
    (with-fake-session (s :nwindows 1)
      (with-command-test-state (s :overlay t)
        (cl-tmux/hooks:set-command-hook "after-new-window" :list-windows)
        (finishes (cl-tmux::run-command-hooks "after-new-window" nil)
                  "run-command-hooks with NIL target must not signal")
        (is-false (overlay-active-p)
                  "run-command-hooks with NIL target must not dispatch any hook")))))

;;; ── *command-dispatch-table* ─────────────────────────────────────────────────

(test command-dispatch-table-is-hash-table
  "*command-dispatch-table* is a hash-table mapping keywords to handler functions."
  (is (hash-table-p cl-tmux::*command-dispatch-table*)
      "*command-dispatch-table* must be a hash-table")
  (is (functionp (gethash :detach cl-tmux::*command-dispatch-table*))
      ":detach must be a registered handler function")
  (is (functionp (gethash :next-window cl-tmux::*command-dispatch-table*))
      ":next-window must be a registered handler function"))

;;; ── define-command-handlers macro ────────────────────────────────────────────

(test define-command-handlers-registers-into-dispatch-table
  "define-command-handlers populates *command-dispatch-table* for new keywords."
  ;; Use a unique test keyword that won't collide with real handlers.
  (let ((orig (gethash :test-dispatch-sentinel cl-tmux::*command-dispatch-table*)))
    (unwind-protect
         (progn
           (cl-tmux::define-command-handlers
             (:test-dispatch-sentinel (+ 1 2)))
           (is (functionp (gethash :test-dispatch-sentinel
                                   cl-tmux::*command-dispatch-table*))
               "define-command-handlers must register a handler function"))
      (if orig
          (setf (gethash :test-dispatch-sentinel cl-tmux::*command-dispatch-table*) orig)
          (remhash :test-dispatch-sentinel cl-tmux::*command-dispatch-table*)))))

;;; ── define-copy-mode-dispatch-handlers macro ─────────────────────────────────

(test define-copy-mode-dispatch-handlers-macro-is-defined
  "define-copy-mode-dispatch-handlers is a defined macro."
  (is (macro-function 'cl-tmux::define-copy-mode-dispatch-handlers)
      "define-copy-mode-dispatch-handlers must be a macro"))

;;; ── define-directional-handlers macro ────────────────────────────────────────

(test define-directional-handlers-macro-is-defined
  "define-directional-handlers is a defined macro."
  (is (macro-function 'cl-tmux::define-directional-handlers)
      "define-directional-handlers must be a macro"))

(test define-directional-handlers-registers-into-dispatch-table
  "define-directional-handlers registers one handler per (keyword direction)
   entry, each calling (helper-fn session direction)."
  (let ((calls nil))
    (cl-tmux::define-directional-handlers
        (lambda (session direction) (push (cons session direction) calls))
      (:test-directional-sentinel-a :left)
      (:test-directional-sentinel-b :right))
    (unwind-protect
         (progn
           (is (functionp (gethash :test-directional-sentinel-a
                                   cl-tmux::*command-dispatch-table*))
               "define-directional-handlers must register :test-directional-sentinel-a")
           (with-fake-session (s)
             (cl-tmux::dispatch-command s :test-directional-sentinel-a nil)
             (is (equal (cons s :left) (first calls))
                 "the generated handler must call helper-fn with session and direction")
             (cl-tmux::dispatch-command s :test-directional-sentinel-b nil)
             (is (equal (cons s :right) (first calls))
                 "each entry must thread its own direction keyword")))
      (remhash :test-directional-sentinel-a cl-tmux::*command-dispatch-table*)
      (remhash :test-directional-sentinel-b cl-tmux::*command-dispatch-table*))))

;;; ── %resize-active-window-pane ───────────────────────────────────────────────

(test resize-active-window-pane-resizes-active-window
  "%resize-active-window-pane resizes the active pane of SESSION's active
   window via dispatch-command's :resize-* handlers."
  (with-fake-session (s :nwindows 1)
    (finishes (cl-tmux::dispatch-command s :resize-left nil))
    (finishes (cl-tmux::dispatch-command s :resize-right nil))
    (finishes (cl-tmux::dispatch-command s :resize-up nil))
    (finishes (cl-tmux::dispatch-command s :resize-down nil))))

;;; ── %copy-mode-call-with-null-arg ────────────────────────────────────────────

(test copy-mode-call-with-null-arg-calls-fn-with-null
  "%copy-mode-call-with-null-arg calls FN with SESSION and NIL when an active screen exists."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((received-arg :unset))
      (cl-tmux::%copy-mode-call-with-null-arg
       s
       (lambda (screen null-arg)
         (declare (ignore screen))
         (setf received-arg null-arg)))
      (is (null received-arg)
          "%copy-mode-call-with-null-arg must pass NIL as the trailing arg"))))

;;; ── define-show-options-handler macro ────────────────────────────────────────

(test define-show-options-handler-macro-is-defined
  "define-show-options-handler is a defined macro."
  (is (macro-function 'cl-tmux::define-show-options-handler)
      "define-show-options-handler must be a macro"))

(test show-session-options-renders-overlay
  "%show-session-options (generated by define-show-options-handler) shows an overlay
   with '# session options' header."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%show-session-options)
      (is (overlay-active-p) "%show-session-options must open an overlay")
      (is (search "session" *overlay*)
          "%show-session-options overlay must reference 'session' scope"))))

(test show-server-options-renders-overlay
  "%show-server-options (generated by define-show-options-handler with :server scope)
   shows an overlay with 'server' in it."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%show-server-options)
      (is (overlay-active-p) "%show-server-options must open an overlay")
      (is (search "server" *overlay*)
          "%show-server-options overlay must reference 'server' scope"))))

;;; ── %with-window-focus-transition macro ──────────────────────────────────────

(test with-window-focus-transition-fires-hooks-on-window-change
  "%with-window-focus-transition fires the session-window-changed hook when
   the active window changes inside BODY."
  (with-isolated-hooks
    (let* ((s (make-fake-session :nwindows 2))
           (fired nil))
      (with-command-test-state (s)
        (cl-tmux/hooks:add-hook
         cl-tmux/hooks:+hook-session-window-changed+
         (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%with-window-focus-transition (s)
          (session-select-window s (second (session-windows s))))
        (is-true fired
                 "%with-window-focus-transition must fire session-window-changed when window changes")))))

(test with-window-focus-transition-no-hook-when-window-unchanged
  "%with-window-focus-transition does not fire session-window-changed when
   BODY leaves the active window the same."
  (with-isolated-hooks
    (let* ((s (make-fake-session :nwindows 2))
           (fired nil))
      (with-command-test-state (s)
        (cl-tmux/hooks:add-hook
         cl-tmux/hooks:+hook-session-window-changed+
         (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%with-window-focus-transition (s)
          ;; BODY makes no change — same active window.
          (values))
        (is-false fired
                  "%with-window-focus-transition must NOT fire hook when window is unchanged")))))

;;; ── %compute-window-base-index table-driven ──────────────────────────────────

(test compute-window-base-index-table-driven
  "%compute-window-base-index dispatch: :at-index, :after-current, :before-current,
   and the no-flags fallback each return the correct value."
  (with-fake-session (s :nwindows 1)
    (let* ((win (session-active-window s))
           (wid (cl-tmux/model:window-id win)))
      (check-table
       (list
        (list (cl-tmux::%compute-window-base-index win :at-index 7)
              7
              ":at-index 7 must return 7")
        (list (cl-tmux::%compute-window-base-index win :after-current t)
              (1+ wid)
              ":after-current must return wid+1")
        (list (cl-tmux::%compute-window-base-index win :before-current t)
              wid
              ":before-current must return wid")
        (list (cl-tmux::%compute-window-base-index win)
              (or (cl-tmux/options:get-option "base-index") 0)
              "no flags must return base-index option value (default 0)"))
       :test #'=))))

;;; ── next-cyclic / prev-cyclic table-driven ───────────────────────────────────

(test cyclic-navigators-table-driven
  "next-cyclic and prev-cyclic both follow modular-arithmetic stepping, and
   next-cyclic falls back to index 0 when CURRENT is not found in the list."
  (check-table
   (list
    (list (cl-tmux::next-cyclic '(a b c) 'a) 'b "next from a → b")
    (list (cl-tmux::next-cyclic '(a b c) 'c) 'a "next from c wraps to a")
    (list (cl-tmux::next-cyclic '(a b c) 'missing) 'b
          "next from an unknown element falls back to index 0 -> element 1")
    (list (cl-tmux::prev-cyclic '(a b c) 'b) 'a "prev from b → a")
    (list (cl-tmux::prev-cyclic '(a b c) 'a) 'c "prev from a wraps to c"))
   :test #'eql))

;;; ── %resolve-target-window-pane ──────────────────────────────────────────────

(test resolve-target-window-pane-returns-current-when-target-str-nil
  "%resolve-target-window-pane returns CURRENT-WINDOW/CURRENT-PANE unchanged
   when TARGET-STR is absent."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (let* ((win  (first (session-windows s)))
           (pane (window-active-pane win)))
      (multiple-value-bind (rwin rpane)
          (cl-tmux::%resolve-target-window-pane s nil win pane)
        (is (eq win rwin)
            "%resolve-target-window-pane with NIL target-str must return current-window")
        (is (eq pane rpane)
            "%resolve-target-window-pane with NIL target-str must return current-pane")))))

(test resolve-target-window-pane-resolves-window-and-its-active-pane
  "%resolve-target-window-pane, given a target-str naming another window (but
   no pane component), returns that window and its own active pane."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (with-command-test-state (s)
      (let* ((cur-win  (first (session-windows s)))
             (cur-pane (window-active-pane cur-win))
             (tgt-win  (second (session-windows s)))
             (tgt-pane (window-active-pane tgt-win))
             (tgt-str  (format nil "@~A" (cl-tmux/model:window-id tgt-win))))
        (multiple-value-bind (rwin rpane)
            (cl-tmux::%resolve-target-window-pane s tgt-str cur-win cur-pane)
          (is (eq tgt-win rwin)
              "%resolve-target-window-pane must resolve the target window")
          (is (eq tgt-pane rpane)
              "%resolve-target-window-pane must default to the target window's active pane"))))))

(test resolve-target-window-pane-falls-back-to-active-window-for-unresolvable-target
  "%resolve-target-window-pane, given a TARGET-STR that names no existing
   window (e.g. a stale window id), falls back to SESSION's active window
   and pane rather than returning NIL (resolve-target's window clause always
   defaults to session-active-window when the lookup itself fails)."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-command-test-state (s)
      (let* ((cur-win  (session-active-window s))
             (cur-pane (window-active-pane cur-win)))
        (multiple-value-bind (rwin rpane)
            (cl-tmux::%resolve-target-window-pane s "@999" cur-win cur-pane)
          (is (eq cur-win rwin)
              "%resolve-target-window-pane must fall back to the active window for an unresolvable target")
          (is (eq cur-pane rpane)
              "%resolve-target-window-pane must fall back to the active pane for an unresolvable target"))))))

;;; ── %resolve-target-session-window ───────────────────────────────────────────

(test resolve-target-session-window-returns-current-when-target-str-nil
  "%resolve-target-session-window returns SESSION/CURRENT-WINDOW unchanged
   when TARGET-STR is absent."
  (with-fake-session (s :nwindows 1)
    (let ((win (session-active-window s)))
      (multiple-value-bind (rsess rwin)
          (cl-tmux::%resolve-target-session-window s nil win nil)
        (is (eq s rsess)
            "%resolve-target-session-window with NIL target-str must return SESSION")
        (is (eq win rwin)
            "%resolve-target-session-window with NIL target-str must return CURRENT-WINDOW")))))

(test resolve-target-session-window-resolves-window-in-same-session
  "%resolve-target-session-window, given a target-str naming another window in
   the same session, returns that session and window."
  (with-fake-session (s :nwindows 2)
    (with-command-test-state (s)
      (let* ((cur-win (first (session-windows s)))
             (tgt-win (second (session-windows s)))
             (tgt-str (format nil "@~A" (cl-tmux/model:window-id tgt-win))))
        (multiple-value-bind (rsess rwin)
            (cl-tmux::%resolve-target-session-window s tgt-str cur-win nil)
          (is (eq s rsess)
              "%resolve-target-session-window must resolve to the owning session")
          (is (eq tgt-win rwin)
              "%resolve-target-session-window must resolve the target window"))))))

(test resolve-target-session-window-falls-back-to-active-window-for-unresolvable-target
  "%resolve-target-session-window, given a TARGET-STR that names no existing
   window, falls back to SESSION and its active window rather than returning
   NIL (mirrors resolve-target's unconditional session-active-window fallback)."
  (with-fake-session (s :nwindows 1)
    (with-command-test-state (s)
      (let ((cur-win (session-active-window s)))
        (multiple-value-bind (rsess rwin)
            (cl-tmux::%resolve-target-session-window s "@999" cur-win nil)
          (is (eq s rsess)
              "%resolve-target-session-window must fall back to SESSION for an unresolvable target")
          (is (eq cur-win rwin)
              "%resolve-target-session-window must fall back to the active window for an unresolvable target"))))))

;;; ── %resolve-window-target-or-active ─────────────────────────────────────────

(test resolve-window-target-or-active-falls-back-to-active-window
  "%resolve-window-target-or-active returns SESSION's active window when
   TARGET-STR is NIL."
  (with-fake-session (s :nwindows 1)
    (is (eq (session-active-window s)
            (cl-tmux::%resolve-window-target-or-active s nil))
        "%resolve-window-target-or-active with NIL target-str must return the active window")))

(test resolve-window-target-or-active-resolves-named-target
  "%resolve-window-target-or-active resolves TARGET-STR to a non-active window
   when it names one."
  (with-fake-session (s :nwindows 2)
    (let* ((tgt-win (second (session-windows s)))
           (tgt-str (format nil "~A" (cl-tmux/model:window-id tgt-win))))
      (is (eq tgt-win (cl-tmux::%resolve-window-target-or-active s tgt-str))
          "%resolve-window-target-or-active must resolve a valid target-str to that window"))))

;;; ── with-target-session macro ────────────────────────────────────────────────

(test with-target-session-runs-body-with-session-when-target-str-nil
  "with-target-session binds TARGET-SESSION to SESSION and runs BODY when
   TARGET-STR is NIL."
  (with-fake-session (s)
    (is (eq s (cl-tmux::with-target-session (ts nil s) ts))
        "with-target-session with NIL target-str must bind TARGET-SESSION to SESSION")))

(test with-target-session-resolves-named-target
  "with-target-session binds TARGET-SESSION to the resolved session when
   TARGET-STR names one registered in *server-sessions*."
  (let* ((s1 (make-fake-session))
         (s2 (make-fake-session)))
    (setf (cl-tmux::session-name s1) "alpha"
          (cl-tmux::session-name s2) "beta")
    (let ((cl-tmux::*server-sessions* (list (cons "alpha" s1) (cons "beta" s2))))
      (is (eq s2 (cl-tmux::with-target-session (ts "beta" s1) ts))
          "with-target-session must resolve TARGET-STR to the named session"))))

(test with-target-session-on-missing-skip-returns-nil-without-running-body
  "with-target-session with the default :skip ON-MISSING does not run BODY when
   TARGET-STR fails to resolve."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
          (body-ran nil))
      (is (null (cl-tmux::with-target-session (ts "no-such-session" s)
                  (setf body-ran t)
                  ts))
          "with-target-session must return NIL when the target is unresolved and ON-MISSING is :skip")
      (is-false body-ran
                "with-target-session must not run BODY when the target is unresolved and ON-MISSING is :skip"))))

(test with-target-session-on-missing-current-runs-body-with-session
  "with-target-session with ON-MISSING :current runs BODY against SESSION even
   when TARGET-STR fails to resolve."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* (list (cons "0" s))))
      (is (eq s (cl-tmux::with-target-session (ts "no-such-session" s
                                               :on-missing :current)
                  ts))
          "with-target-session with ON-MISSING :current must run BODY with TARGET-SESSION bound to SESSION"))))

(test with-target-session-on-missing-error-shows-message-and-returns-nil
  "with-target-session with ON-MISSING :error shows the MESSAGE overlay
   (formatted with TARGET-STR) and returns NIL without running BODY."
  (with-fake-session (s)
    (with-command-test-state (s :overlay t)
      (let ((body-ran nil))
        (is (null (cl-tmux::with-target-session (ts "no-such-session" s
                                                 :message "no session: ~A"
                                                 :on-missing :error)
                    (setf body-ran t)
                    ts))
            "with-target-session with ON-MISSING :error must return NIL")
        (is-false body-ran
                  "with-target-session with ON-MISSING :error must not run BODY")
        (is (search "no-such-session" *overlay*)
            "with-target-session with ON-MISSING :error must format TARGET-STR into MESSAGE")))))

;;; ── with-target-context macro ────────────────────────────────────────────────

(test with-target-context-defaults-to-current-session-window-pane
  "with-target-context binds TARGET-SESSION/WINDOW/PANE to SESSION's current
   window and pane when TARGET-STR is NIL."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-command-test-state (s)
      (multiple-value-bind (ts tw tp)
          (cl-tmux::with-target-context (ts tw tp s nil)
            (values ts tw tp))
        (is (eq s ts) "with-target-context must default TARGET-SESSION to SESSION")
        (is (eq (session-active-window s) tw)
            "with-target-context must default TARGET-WINDOW to the active window")
        (is (eq (window-active-pane tw) tp)
            "with-target-context must default TARGET-PANE to the active pane")))))

(test with-target-context-resolves-named-window-target
  "with-target-context resolves TARGET-STR naming another window in SESSION to
   that window; since TARGET-STR carries no pane component, TARGET-PANE
   defaults to SESSION's (not the target window's) currently active pane."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (with-command-test-state (s)
      (let* ((cur-pane (session-active-pane s))
             (tgt-win  (second (session-windows s)))
             (tgt-str  (format nil "@~A" (cl-tmux/model:window-id tgt-win))))
        (multiple-value-bind (ts tw tp)
            (cl-tmux::with-target-context (ts tw tp s tgt-str)
              (values ts tw tp))
          (is (eq s ts) "with-target-context must resolve TARGET-SESSION to the owning session")
          (is (eq tgt-win tw) "with-target-context must resolve TARGET-WINDOW to the named window")
          (is (eq cur-pane tp)
              "with-target-context must default TARGET-PANE to SESSION's active pane when TARGET-STR has no pane component"))))))
