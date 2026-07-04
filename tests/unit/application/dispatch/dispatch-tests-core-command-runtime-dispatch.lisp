(in-package #:cl-tmux/test)

;;;; Command dispatch tests: runtime hooks and command-table behavior.

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
