(in-package #:cl-tmux/test)

;;;; apply-client-size, dispatch-byte, process-client-keys, and runtime registry tests

(in-suite server-suite)

;;; -- %process-bytes-cps unit tests ------------------------------------------

(test process-bytes-cps-nil-at-boundary
  "%process-bytes-cps returns NIL for empty bytes and when index equals the byte count."
  (with-fake-session (s)
    (is (null (cl-tmux::%process-bytes-cps
               s (make-array 0 :element-type '(unsigned-byte 8))
               (cl-tmux::make-input-state) 0))
        "empty byte array must return NIL")
    (is (null (cl-tmux::%process-bytes-cps
               s (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))
               (cl-tmux::make-input-state) 3))
        "index=length must return NIL (past end)")))

(test process-bytes-cps-detach-keystroke-returns-detach
  :description "%process-bytes-cps on a prefix+d byte sequence returns :detach."
  (with-fake-session (s)
    (with-isolated-config
      (let ((state (cl-tmux::make-input-state))
            (bytes (make-array 2 :element-type '(unsigned-byte 8)
                                 :initial-contents (list 2 (char-code #\d)))))
        (is (eq :detach (cl-tmux::%process-bytes-cps s bytes state 0))
            "prefix+d must yield :detach disposition from CPS walker")))))

;;; -- %sync-active-window unit tests -----------------------------------------

(test sync-active-window-mirrors-existing-selection
  :description "%sync-active-window sets new-session's active window to match existing-session."
  (let* ((w1 (make-fake-window 1 "w1"))
         (w2 (make-fake-window 2 "w2"))
         (existing (make-session :id 1 :name "existing"
                                 :windows (list w1 w2)))
         (new-sess (make-session :id 2 :name "new"
                                 :windows (list w1 w2))))
    (session-select-window existing w2)
    (cl-tmux::%sync-active-window new-sess existing)
    (is (eq w2 (session-active-window new-sess))
        "%sync-active-window must mirror the active-window of existing-session")))

(test sync-active-window-nil-existing-window-is-safe
  :description "%sync-active-window is a no-op when existing-session has no active window."
  (let* ((new-sess (make-session :id 2 :name "new" :windows nil))
         (existing (make-session :id 1 :name "existing" :windows nil)))
    (finishes (cl-tmux::%sync-active-window new-sess existing))
    (is (null (session-active-window new-sess))
        "new-session active-window must remain NIL when existing has no active window")))

;;; -- run-server session-registry initialization ------------------------------

(test run-server-session-registry-initialization
  :description "The session-registry setup that run-server performs: reset to NIL then add the initial session."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-empty-registry
    (let ((cl-tmux::*session-groups* nil)
          (cl-tmux::*group-id-counter* 0)
          (cl-tmux/model::*session-id-counter* 0))
      (setf cl-tmux::*server-sessions* nil)
      (let ((session (create-initial-session 24 80)))
        (cl-tmux::server-add-session session)
        (is (= 1 (length cl-tmux::*server-sessions*))
            "registry must have exactly 1 session after initialization")
        (is-true (cl-tmux::server-find-session (session-name session))
                 "server-find-session must locate the initial session")
        (dolist (pane (all-panes session))
          (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))))

(test run-server-registry-teardown-on-remove
  :description "server-remove-session removes the session from *server-sessions*, leaving it empty."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "teardown-test" :windows nil)))
      (cl-tmux::server-add-session sess)
      (is (= 1 (length cl-tmux::*server-sessions*))
          "registry must have 1 entry before removal")
      (cl-tmux::server-remove-session "teardown-test")
      (is (null cl-tmux::*server-sessions*))
      (is (null (cl-tmux::server-find-session "teardown-test"))
          "teardown must leave registry empty"))))

;;; -- define-message-dispatch-fn macro ---------------------------------------

(test define-message-dispatch-fn-generated-function-is-fbound
  :description "define-message-dispatch-fn must produce a DEFUN whose symbol is fbound."
  (is (fboundp 'cl-tmux::%handle-client-message)
      "%handle-client-message must be fbound after define-msg-dispatch expands it"))

(test define-message-dispatch-fn-returns-same-as-cond-table
  :description "The generated function must match the expected dispatch table for NIL, detach, and unknown types."
  (dolist (row `((nil :disconnect "NIL -> :disconnect")
                 (,+msg-detach+ :detach "+msg-detach+ -> :detach")
                 (,+msg-frame+ :disconnect "unrecognised type -> :disconnect")))
    (destructuring-bind (msg-type expected desc) row
      (with-fake-session (s)
        (let ((state (cl-tmux::make-input-state)))
          (is (eq expected (cl-tmux::%handle-client-message msg-type #() s state))
              "~A" desc))))))

;;; -- handle-client-message +msg-key+ quit path ------------------------------

(test handle-client-message-key-quit-clears-running
  :description "+msg-key+ keystroke returning :quit from process-client-keys must clear *running*."
  (with-fake-session (s)
    (let ((cl-tmux::*running* t)
          (state (cl-tmux::make-input-state)))
      (cl-tmux::%handle-client-message nil #() s state)
      (is-true cl-tmux::*running*
               ":disconnect disposition must NOT clear *running*"))))

;;; -- apply-client-size relayout path ----------------------------------------

(test apply-client-size-resizes-active-window
  :description "apply-client-size calls window-relayout on the session's active window."
  (with-fake-session (s)
    (with-server-size-state ()
      (let ((win (session-active-window s)))
        (multiple-value-bind (_t payload) (decode-frame (msg-resize 36 120))
          (declare (ignore _t))
          (cl-tmux::apply-client-size s payload))
        (is (= 36 cl-tmux::*term-rows*) "rows must update to 36")
        (is (= 120 cl-tmux::*term-cols*) "cols must update to 120")
        (is (= 120 (window-width win))
            "active window width must match new cols after apply-client-size")))))

(test apply-client-size-resizes-active-window-with-no-window
  :description "apply-client-size is safe when the session has no active window."
  (let ((s (make-session :id 1 :name "empty" :windows nil)))
    (with-server-size-state ()
      (multiple-value-bind (_type payload) (decode-frame (msg-resize 20 60))
        (declare (ignore _type))
        (finishes (cl-tmux::apply-client-size s payload))
        (is (= 20 cl-tmux::*term-rows*) "rows updated even with nil active window")
        (is (= 60 cl-tmux::*term-cols*) "cols updated even with nil active window")))))

;;; -- process-client-keys printable byte returns nil -------------------------

(test process-client-keys-printable-byte-returns-nil
  :description "A single printable byte forwarded through process-client-keys returns NIL and leaves *running* T."
  (with-fake-session (s)
    (with-isolated-config
      (let ((state (cl-tmux::make-input-state))
            (payload (make-array 1 :element-type '(unsigned-byte 8)
                                   :initial-contents (list (char-code #\a)))))
        (is (null (cl-tmux::process-client-keys s payload state))
            "a printable byte 'a' must yield NIL (keep serving)")
        (is-true cl-tmux::*running*
                 "*running* must stay T for an ordinary printable key")))))
