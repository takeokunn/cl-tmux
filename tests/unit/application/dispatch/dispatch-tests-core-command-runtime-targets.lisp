(in-package #:cl-tmux/test)

;;;; Command dispatch tests: target resolution and target context macros.

(in-suite dispatch-suite)

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
