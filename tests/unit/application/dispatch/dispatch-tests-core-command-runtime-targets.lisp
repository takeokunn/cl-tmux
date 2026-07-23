(in-package #:cl-tmux/test)

;;;; Command dispatch tests: target resolution and target context macros.

(describe "dispatch-suite"

  ;;; ── %resolve-target-window-pane ──────────────────────────────────────────────

  ;; %resolve-target-window-pane returns CURRENT-WINDOW/CURRENT-PANE unchanged
  ;; when TARGET-STR is absent.
  (it "resolve-target-window-pane-returns-current-when-target-str-nil"
    (with-fake-session (s :nwindows 2 :npanes 1)
      (let* ((win  (first (session-windows s)))
             (pane (window-active-pane win)))
        (multiple-value-bind (rwin rpane)
            (cl-tmux::%resolve-target-window-pane s nil win pane)
          (expect (eq win rwin))
          (expect (eq pane rpane))))))

  ;; %resolve-target-window-pane, given a target-str naming another window (but
  ;; no pane component), returns that window and its own active pane.
  (it "resolve-target-window-pane-resolves-window-and-its-active-pane"
    (with-fake-session (s :nwindows 2 :npanes 1)
      (with-command-test-state (s)
        (let* ((cur-win  (first (session-windows s)))
               (cur-pane (window-active-pane cur-win))
               (tgt-win  (second (session-windows s)))
               (tgt-pane (window-active-pane tgt-win))
               (tgt-str  (format nil "@~A" (cl-tmux/model:window-id tgt-win))))
          (multiple-value-bind (rwin rpane)
              (cl-tmux::%resolve-target-window-pane s tgt-str cur-win cur-pane)
            (expect (eq tgt-win rwin))
            (expect (eq tgt-pane rpane)))))))

  ;; %resolve-target-window-pane, given a TARGET-STR that names no existing
  ;; window (e.g. a stale window id), falls back to SESSION's active window
  ;; and pane rather than returning NIL (resolve-target's window clause always
  ;; defaults to session-active-window when the lookup itself fails).
  (it "resolve-target-window-pane-falls-back-to-active-window-for-unresolvable-target"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (with-command-test-state (s)
        (let* ((cur-win  (session-active-window s))
               (cur-pane (window-active-pane cur-win)))
          (multiple-value-bind (rwin rpane)
              (cl-tmux::%resolve-target-window-pane s "@999" cur-win cur-pane)
            (expect (eq cur-win rwin))
            (expect (eq cur-pane rpane)))))))

  ;;; ── %resolve-target-session-window ───────────────────────────────────────────

  ;; %resolve-target-session-window returns SESSION/CURRENT-WINDOW unchanged
  ;; when TARGET-STR is absent.
  (it "resolve-target-session-window-returns-current-when-target-str-nil"
    (with-fake-session (s :nwindows 1)
      (let ((win (session-active-window s)))
        (multiple-value-bind (rsess rwin)
            (cl-tmux::%resolve-target-session-window s nil win nil)
          (expect (eq s rsess))
          (expect (eq win rwin))))))

  ;; %resolve-target-session-window, given a target-str naming another window in
  ;; the same session, returns that session and window.
  (it "resolve-target-session-window-resolves-window-in-same-session"
    (with-fake-session (s :nwindows 2)
      (with-command-test-state (s)
        (let* ((cur-win (first (session-windows s)))
               (tgt-win (second (session-windows s)))
               (tgt-str (format nil "@~A" (cl-tmux/model:window-id tgt-win))))
          (multiple-value-bind (rsess rwin)
              (cl-tmux::%resolve-target-session-window s tgt-str cur-win nil)
            (expect (eq s rsess))
            (expect (eq tgt-win rwin)))))))

  ;; %resolve-target-session-window, given a TARGET-STR that names no existing
  ;; window, falls back to SESSION and its active window rather than returning
  ;; NIL (mirrors resolve-target's unconditional session-active-window fallback).
  (it "resolve-target-session-window-falls-back-to-active-window-for-unresolvable-target"
    (with-fake-session (s :nwindows 1)
      (with-command-test-state (s)
        (let ((cur-win (session-active-window s)))
          (multiple-value-bind (rsess rwin)
              (cl-tmux::%resolve-target-session-window s "@999" cur-win nil)
            (expect (eq s rsess))
            (expect (eq cur-win rwin)))))))

  ;;; ── %resolve-window-target-or-active ─────────────────────────────────────────

  ;; %resolve-window-target-or-active returns SESSION's active window when
  ;; TARGET-STR is NIL.
  (it "resolve-window-target-or-active-falls-back-to-active-window"
    (with-fake-session (s :nwindows 1)
      (expect (eq (session-active-window s)
              (cl-tmux::%resolve-window-target-or-active s nil)))))

  ;; %resolve-window-target-or-active resolves TARGET-STR to a non-active window
  ;; when it names one.
  (it "resolve-window-target-or-active-resolves-named-target"
    (with-fake-session (s :nwindows 2)
      (let* ((tgt-win (second (session-windows s)))
             (tgt-str (format nil "~A" (cl-tmux/model:window-id tgt-win))))
        (expect (eq tgt-win (cl-tmux::%resolve-window-target-or-active s tgt-str))))))

  ;;; ── with-target-session macro ────────────────────────────────────────────────

  ;; with-target-session binds TARGET-SESSION to SESSION and runs BODY when
  ;; TARGET-STR is NIL.
  (it "with-target-session-runs-body-with-session-when-target-str-nil"
    (with-fake-session (s)
      (expect (eq s (cl-tmux::with-target-session (ts nil s) ts)))))

  ;; with-target-session binds TARGET-SESSION to the resolved session when
  ;; TARGET-STR names one registered in *server-sessions*.
  (it "with-target-session-resolves-named-target"
    (let* ((s1 (make-fake-session))
           (s2 (make-fake-session)))
      (setf (cl-tmux::session-name s1) "alpha"
            (cl-tmux::session-name s2) "beta")
      (let ((cl-tmux::*server-sessions* (list (cons "alpha" s1) (cons "beta" s2))))
        (expect (eq s2 (cl-tmux::with-target-session (ts "beta" s1) ts))))))

  ;; with-target-session with the default :skip ON-MISSING does not run BODY when
  ;; TARGET-STR fails to resolve.
  (it "with-target-session-on-missing-skip-returns-nil-without-running-body"
    (with-fake-session (s)
      (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
            (body-ran nil))
        (expect (null (cl-tmux::with-target-session (ts "no-such-session" s)
                    (setf body-ran t)
                    ts)))
        (expect body-ran :to-be-falsy))))

  ;; with-target-session with ON-MISSING :current runs BODY against SESSION even
  ;; when TARGET-STR fails to resolve.
  (it "with-target-session-on-missing-current-runs-body-with-session"
    (with-fake-session (s)
      (let ((cl-tmux::*server-sessions* (list (cons "0" s))))
        (expect (eq s (cl-tmux::with-target-session (ts "no-such-session" s
                                                 :on-missing :current)
                    ts))))))

  ;; with-target-session with ON-MISSING :error shows the MESSAGE overlay
  ;; (formatted with TARGET-STR) and returns NIL without running BODY.
  (it "with-target-session-on-missing-error-shows-message-and-returns-nil"
    (with-fake-session (s)
      (with-command-test-state (s :overlay t)
        (let ((body-ran nil))
          (expect (null (cl-tmux::with-target-session (ts "no-such-session" s
                                                   :message "no session: ~A"
                                                   :on-missing :error)
                      (setf body-ran t)
                      ts)))
          (expect body-ran :to-be-falsy)
          (expect (search "no-such-session" *overlay*))))))

  ;;; ── with-target-context macro ────────────────────────────────────────────────

  ;; with-target-context binds TARGET-SESSION/WINDOW/PANE to SESSION's current
  ;; window and pane when TARGET-STR is NIL.
  (it "with-target-context-defaults-to-current-session-window-pane"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (with-command-test-state (s)
        (multiple-value-bind (ts tw tp)
            (cl-tmux::with-target-context (ts tw tp s nil)
              (values ts tw tp))
          (expect (eq s ts))
          (expect (eq (session-active-window s) tw))
          (expect (eq (window-active-pane tw) tp))))))

  ;; with-target-context resolves TARGET-STR naming another window in SESSION to
  ;; that window; since TARGET-STR carries no pane component, TARGET-PANE
  ;; defaults to SESSION's (not the target window's) currently active pane.
  (it "with-target-context-resolves-named-window-target"
    (with-fake-session (s :nwindows 2 :npanes 1)
      (with-command-test-state (s)
        (let* ((cur-pane (session-active-pane s))
               (tgt-win  (second (session-windows s)))
               (tgt-str  (format nil "@~A" (cl-tmux/model:window-id tgt-win))))
          (multiple-value-bind (ts tw tp)
              (cl-tmux::with-target-context (ts tw tp s tgt-str)
                (values ts tw tp))
            (expect (eq s ts))
            (expect (eq tgt-win tw))
            (expect (eq cur-pane tp))))))))
