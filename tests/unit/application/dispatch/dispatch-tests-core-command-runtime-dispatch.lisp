(in-package #:cl-tmux/test)

;;;; Command dispatch tests: runtime hooks and command-table behavior.

(describe "dispatch-suite"

  ;;; ── run-command-hooks ────────────────────────────────────────────────────────

  ;; run-command-hooks dispatches the registered command hook for a session target.
  ;; run-command-hooks consults cl-tmux/hooks:*command-hooks* (populated by
  ;; set-command-hook / the set-hook config directive), not the lisp-callback
  ;; registry that add-hook populates — that registry is fired by run-hooks.
  (it "run-command-hooks-fires-for-session-target"
    (with-isolated-hooks
      (with-fake-session (s :nwindows 1)
        (with-command-test-state (s :overlay t)
          (cl-tmux/hooks:set-command-hook "after-new-window" :list-windows)
          (cl-tmux::run-command-hooks "after-new-window" s)
          (assert-overlay-active
           "run-command-hooks must dispatch the registered command hook")))))

  ;; run-command-hooks is a no-op when TARGET is NIL (no session to dispatch against).
  (it "run-command-hooks-noop-for-nil-target"
    (with-isolated-hooks
      (with-fake-session (s :nwindows 1)
        (with-command-test-state (s :overlay t)
          (cl-tmux/hooks:set-command-hook "after-new-window" :list-windows)
          (finishes (cl-tmux::run-command-hooks "after-new-window" nil)
                    "run-command-hooks with NIL target must not signal")
          (expect (overlay-active-p) :to-be-falsy)))))

  ;;; ── *command-dispatch-table* ─────────────────────────────────────────────────

  ;; *command-dispatch-table* is a hash-table mapping keywords to handler functions.
  (it "command-dispatch-table-is-hash-table"
    (expect (hash-table-p cl-tmux::*command-dispatch-table*))
    (expect (functionp (gethash :detach cl-tmux::*command-dispatch-table*)))
    (expect (functionp (gethash :next-window cl-tmux::*command-dispatch-table*))))

  ;;; ── define-command-handlers macro ────────────────────────────────────────────

  ;; define-command-handlers populates *command-dispatch-table* for new keywords.
  (it "define-command-handlers-registers-into-dispatch-table"
    ;; Use a unique test keyword that won't collide with real handlers.
    (let ((orig (gethash :test-dispatch-sentinel cl-tmux::*command-dispatch-table*)))
      (unwind-protect
           (progn
             (cl-tmux::define-command-handlers
               (:test-dispatch-sentinel (+ 1 2)))
             (expect (functionp (gethash :test-dispatch-sentinel
                                        cl-tmux::*command-dispatch-table*))))
        (if orig
            (setf (gethash :test-dispatch-sentinel cl-tmux::*command-dispatch-table*) orig)
            (remhash :test-dispatch-sentinel cl-tmux::*command-dispatch-table*)))))

  ;;; ── define-copy-mode-dispatch-handlers macro ─────────────────────────────────

  ;; define-copy-mode-dispatch-handlers is a defined macro.
  (it "define-copy-mode-dispatch-handlers-macro-is-defined"
    (expect (macro-function 'cl-tmux::define-copy-mode-dispatch-handlers)))

  ;;; ── define-directional-handlers macro ────────────────────────────────────────

  ;; define-directional-handlers is a defined macro.
  (it "define-directional-handlers-macro-is-defined"
    (expect (macro-function 'cl-tmux::define-directional-handlers)))

  ;; define-directional-handlers registers one handler per (keyword direction)
  ;; entry, each calling (helper-fn session direction).
  (it "define-directional-handlers-registers-into-dispatch-table"
    (let ((calls nil))
      (cl-tmux::define-directional-handlers
          (lambda (session direction) (push (cons session direction) calls))
        (:test-directional-sentinel-a :left)
        (:test-directional-sentinel-b :right))
      (unwind-protect
           (progn
             (expect (functionp (gethash :test-directional-sentinel-a
                                        cl-tmux::*command-dispatch-table*)))
             (with-fake-session (s)
               (cl-tmux::dispatch-command s :test-directional-sentinel-a nil)
               (expect (equal (cons s :left) (first calls)))
               (cl-tmux::dispatch-command s :test-directional-sentinel-b nil)
               (expect (equal (cons s :right) (first calls)))))
        (remhash :test-directional-sentinel-a cl-tmux::*command-dispatch-table*)
        (remhash :test-directional-sentinel-b cl-tmux::*command-dispatch-table*))))

  ;;; ── %resize-active-window-pane ───────────────────────────────────────────────

  ;; %resize-active-window-pane resizes the active pane of SESSION's active
  ;; window via dispatch-command's :resize-* handlers.
  (it "resize-active-window-pane-resizes-active-window"
    (with-fake-session (s :nwindows 1)
      (finishes (cl-tmux::dispatch-command s :resize-left nil))
      (finishes (cl-tmux::dispatch-command s :resize-right nil))
      (finishes (cl-tmux::dispatch-command s :resize-up nil))
      (finishes (cl-tmux::dispatch-command s :resize-down nil))))

  ;;; ── %copy-mode-call-with-null-arg ────────────────────────────────────────────

  ;; %copy-mode-call-with-null-arg calls FN with SESSION and NIL when an active screen exists.
  (it "copy-mode-call-with-null-arg-calls-fn-with-null"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (let ((received-arg :unset))
        (cl-tmux::%copy-mode-call-with-null-arg
         s
         (lambda (screen null-arg)
           (declare (ignore screen))
           (setf received-arg null-arg)))
        (expect (null received-arg)))))

  ;;; ── define-show-options-handler macro ────────────────────────────────────────

  ;; define-show-options-handler is a defined macro.
  (it "define-show-options-handler-macro-is-defined"
    (expect (macro-function 'cl-tmux::define-show-options-handler)))

  ;; %show-session-options (generated by define-show-options-handler) shows an overlay
  ;; with '# session options' header.
  (it "show-session-options-renders-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%show-session-options)
        (expect (overlay-active-p))
        (expect (search "session" *overlay*)))))

  ;; %show-server-options (generated by define-show-options-handler with :server scope)
  ;; shows an overlay with 'server' in it.
  (it "show-server-options-renders-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%show-server-options)
        (expect (overlay-active-p))
        (expect (search "server" *overlay*)))))

  ;;; ── %with-window-focus-transition macro ──────────────────────────────────────

  ;; %with-window-focus-transition fires the session-window-changed hook when
  ;; the active window changes inside BODY.
  (it "with-window-focus-transition-fires-hooks-on-window-change"
    (with-isolated-hooks
      (let* ((s (make-fake-session :nwindows 2))
             (fired nil))
        (with-command-test-state (s)
          (cl-tmux/hooks:add-hook
           cl-tmux/hooks:+hook-session-window-changed+
           (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%with-window-focus-transition (s)
            (session-select-window s (second (session-windows s))))
          (expect fired :to-be-truthy)))))

  ;; %with-window-focus-transition does not fire session-window-changed when
  ;; BODY leaves the active window the same.
  (it "with-window-focus-transition-no-hook-when-window-unchanged"
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
          (expect fired :to-be-falsy)))))

  ;;; ── %compute-window-base-index table-driven ──────────────────────────────────

  ;; %compute-window-base-index dispatch: :at-index, :after-current, :before-current,
  ;; and the no-flags fallback each return the correct value.
  (it "compute-window-base-index-table-driven"
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

  ;; next-cyclic and prev-cyclic both follow modular-arithmetic stepping, and
  ;; next-cyclic falls back to index 0 when CURRENT is not found in the list.
  (it "cyclic-navigators-table-driven"
    (check-table
     (list
      (list (cl-tmux::next-cyclic '(a b c) 'a) 'b "next from a → b")
      (list (cl-tmux::next-cyclic '(a b c) 'c) 'a "next from c wraps to a")
      (list (cl-tmux::next-cyclic '(a b c) 'missing) 'b
            "next from an unknown element falls back to index 0 -> element 1")
      (list (cl-tmux::prev-cyclic '(a b c) 'b) 'a "prev from b → a")
      (list (cl-tmux::prev-cyclic '(a b c) 'a) 'c "prev from a wraps to c"))
     :test #'eql)))
