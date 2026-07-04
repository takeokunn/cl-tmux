(in-package #:cl-tmux/test)

(in-suite server-multi-suite)

;;;; Per-client message dispatch tests for the multi-client server.

;;; ── %handle-multi-client-message: per-client dispatch ────────────────────────

(test multi-handle-resize-updates-conn-and-effective-size
  "A resize message updates the client's geometry and re-applies the effective size."
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

(test multi-resize-marks-client-latest
  "A resize moves the client to the front of *clients* so window-size \"latest\"
   tracks the just-resized client."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "latest")
    (with-fake-session (s)
      (let* ((a (%make-test-conn :rows 24 :cols 80))
             (b (%make-test-conn :rows 30 :cols 100))
             (cl-tmux::*clients* (list a b))   ; a is front initially
             (payload (cl-tmux/protocol::u16-octets-pair 50 150)))
        (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s b)
        (is (eq b (first cl-tmux::*clients*)) "the resized client moves to the front")
        (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
          (check-table (list (list rows 50 "latest tracks the just-resized client's new rows")
                             (list cols 150 "latest tracks the just-resized client's new cols"))))))))

(test multi-handle-key-detach-drops-client
  "A ^B d key message yields :drop (the client detaches; the session survives)."
  (with-fake-session (s)
    (with-isolated-config
      (let ((conn    (%make-test-conn))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\d)))))
        (is (eq :drop (cl-tmux::%handle-multi-client-message
                       cl-tmux::+msg-key+ payload s conn))
            "^B d must produce :drop")
        (is-true cl-tmux::*running* "a detach must not end the session")))))

(test multi-attach-readonly-flag-sets-conn-slot
  "A +msg-attach+ frame whose flags byte sets +attach-flag-read-only+ marks the
   connection read-only; a plain (no-flag) attach leaves it NIL."
  (with-fake-session (s)
    (let* ((conn   (%make-test-conn))
           (cl-tmux::*clients* (list conn))
           (ro-payload (cl-tmux/protocol::to-octets
                        (concatenate 'list
                                     (cl-tmux/protocol::u16-octets-pair 30 100)
                                     (list cl-tmux/protocol:+attach-flag-read-only+)))))
      (cl-tmux::%handle-multi-client-message cl-tmux::+msg-attach+ ro-payload s conn)
      (is-true (cl-tmux::client-conn-read-only-p conn)
               "attach flags byte with the read-only bit must set conn read-only-p")
      ;; A subsequent plain attach (no flags byte) clears it again.
      (cl-tmux::%handle-multi-client-message
       cl-tmux::+msg-attach+ (cl-tmux/protocol::u16-octets-pair 30 100) s conn)
      (is-false (cl-tmux::client-conn-read-only-p conn)
                "a plain attach (no flags byte) must clear conn read-only-p"))))

(test multi-readonly-conn-suppresses-pane-input
  "When a connection is read-only, a printable key dispatched through
   %handle-multi-client-message must NOT reach the active pane (no pty-write)."
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
        (is (null writes)
            "a read-only connection must not pty-write a printable key to the pane")))))

(test multi-handle-detach-message-drops-client
  "An explicit +msg-detach+ message yields :drop."
  (with-fake-session (s)
    (is (eq :drop (cl-tmux::%handle-multi-client-message
                   cl-tmux::+msg-detach+ #() s (%make-test-conn))))))

(test multi-handle-nil-and-unknown-type-drop
  "EOF (NIL type) and an unknown message type both yield :drop."
  (with-fake-session (s)
    (is (eq :drop (cl-tmux::%handle-multi-client-message nil #() s (%make-test-conn)))
        "NIL type (EOF) → :drop")
    (is (eq :drop (cl-tmux::%handle-multi-client-message 99 #() s (%make-test-conn)))
        "unknown type → :drop")))

(test multi-handle-detach-other-clients-command
  "A detach-other-clients command message yields :detach-others."
  (with-fake-session (s)
    (let ((payload (cl-tmux/protocol::encode-command-payload :detach-other-clients)))
      (is (eq :detach-others (cl-tmux::%handle-multi-client-message
                              cl-tmux::+msg-command+ payload s (%make-test-conn)))
          "detach-other-clients command → :detach-others"))))
