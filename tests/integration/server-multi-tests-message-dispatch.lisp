(in-package #:cl-tmux/test)

;;;; Per-client message dispatch tests for the multi-client server.

(describe "server-multi-suite"

  ;;; ── %handle-multi-client-message: per-client dispatch ────────────────────────

  ;; A resize message updates the client's geometry and re-applies the effective size.
  (it "multi-handle-resize-updates-conn-and-effective-size"
    (with-fake-session (s)
      (let* ((conn (%make-test-conn :rows 24 :cols 80))
             (cl-tmux::*clients* (list conn))
             (payload (cl-tmux/protocol::u16-octets-pair 40 100)))
        (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s conn)
        ;; Single client → effective size equals that client's size.
        (check-table (list (list (cl-tmux::client-conn-rows conn) 40 "conn rows updated from the resize")
                           (list (cl-tmux::client-conn-cols conn) 100 "conn cols updated from the resize")
                           (list cl-tmux::*term-rows* 40 "effective rows applied to *term-rows*")
                           (list cl-tmux::*term-cols* 100 "effective cols applied to *term-cols*"))))))

  ;; A resize moves the client to the front of *clients* so window-size "latest"
  ;; tracks the just-resized client.
  (it "multi-resize-marks-client-latest"
    (with-fresh-options
      (cl-tmux/options:set-option "window-size" "latest")
      (with-fake-session (s)
        (let* ((a (%make-test-conn :rows 24 :cols 80))
               (b (%make-test-conn :rows 30 :cols 100))
               (cl-tmux::*clients* (list a b))   ; a is front initially
               (payload (cl-tmux/protocol::u16-octets-pair 50 150)))
          (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s b)
          (expect (eq b (first cl-tmux::*clients*)))
          (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
            (check-table (list (list rows 50 "latest tracks the just-resized client's new rows")
                               (list cols 150 "latest tracks the just-resized client's new cols"))))))))

  ;; A ^B d key message yields :drop (the client detaches; the session survives).
  (it "multi-handle-key-detach-drops-client"
    (with-fake-session (s)
      (with-isolated-config
        (let ((conn    (%make-test-conn))
              (payload (make-array 2 :element-type '(unsigned-byte 8)
                                     :initial-contents (list 2 (char-code #\d)))))
          (expect (eq :drop (cl-tmux::%handle-multi-client-message
                             cl-tmux::+msg-key+ payload s conn)))
          (expect cl-tmux::*running* :to-be-truthy)))))

  ;; A +msg-attach+ frame whose flags byte sets +attach-flag-read-only+ marks the
  ;; connection read-only; a plain (no-flag) attach leaves it NIL.
  (it "multi-attach-readonly-flag-sets-conn-slot"
    (with-fake-session (s)
      (let* ((conn   (%make-test-conn))
             (cl-tmux::*clients* (list conn))
             (ro-payload (cl-tmux/protocol::to-octets
                          (concatenate 'list
                                       (cl-tmux/protocol::u16-octets-pair 30 100)
                                       (list cl-tmux/protocol:+attach-flag-read-only+)))))
        (cl-tmux::%handle-multi-client-message cl-tmux::+msg-attach+ ro-payload s conn)
        (expect (cl-tmux::client-conn-read-only-p conn) :to-be-truthy)
        ;; A subsequent plain attach (no flags byte) clears it again.
        (cl-tmux::%handle-multi-client-message
         cl-tmux::+msg-attach+ (cl-tmux/protocol::u16-octets-pair 30 100) s conn)
        (expect (cl-tmux::client-conn-read-only-p conn) :to-be-falsy))))

  ;; When a connection is read-only, a printable key dispatched through
  ;; %handle-multi-client-message must NOT reach the active pane (no pty-write).
  (it "multi-readonly-conn-suppresses-pane-input"
    (with-fake-session (s)
      (with-isolated-config
        (let* ((conn (%make-test-conn))
               (cl-tmux::*clients* (list conn))
               (writes nil))
          (setf (cl-tmux::client-conn-read-only-p conn) t)
          ;; Capture any pty-write the key would otherwise forward to the pane.
          (flet ((rec (fd bytes) (declare (ignore fd)) (push bytes writes)))
            (let ((orig (fdefinition 'cl-tmux::pty-write)))
              (unwind-protect
                   (progn
                     (setf (fdefinition 'cl-tmux::pty-write) #'rec)
                     (cl-tmux::%handle-multi-client-message
                      cl-tmux::+msg-key+
                      (make-array 1 :element-type '(unsigned-byte 8)
                                    :initial-contents (list (char-code #\a)))
                      s conn))
                (setf (fdefinition 'cl-tmux::pty-write) orig))))
          (expect (null writes))))))

  ;; An explicit +msg-detach+ message yields :drop.
  (it "multi-handle-detach-message-drops-client"
    (with-fake-session (s)
      (expect (eq :drop (cl-tmux::%handle-multi-client-message
                         cl-tmux::+msg-detach+ #() s (%make-test-conn))))))

  ;; EOF (NIL type) and an unknown message type both yield :drop.
  (it "multi-handle-nil-and-unknown-type-drop"
    (with-fake-session (s)
      (expect (eq :drop (cl-tmux::%handle-multi-client-message nil #() s (%make-test-conn))))
      (expect (eq :drop (cl-tmux::%handle-multi-client-message 99 #() s (%make-test-conn))))))

  ;; A detach-other-clients command message yields :detach-others.
  (it "multi-handle-detach-other-clients-command"
    (with-fake-session (s)
      (let ((payload (cl-tmux/protocol::encode-command-payload :detach-other-clients)))
        (expect (eq :detach-others (cl-tmux::%handle-multi-client-message
                                    cl-tmux::+msg-command+ payload s (%make-test-conn))))))))
