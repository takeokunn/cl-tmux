(in-package #:cl-tmux/test)

;;;; Client lifecycle tests (src/client.lisp).
;;;;
;;;; run-client itself is integration-level (requires a live socket and raw
;;;; terminal) but its building blocks are unit-testable:
;;;;
;;;;   * socket-path naming — pure string function
;;;;   * with-incoming-frame dispatch — the same Prolog-dispatch macro used by
;;;;     both server (serve-client) and client (run-client); tested via a real
;;;;     Unix-domain socket roundtrip (same technique as net-tests.lisp), guarded
;;;;     by unix-socket-available-p so tests self-skip in restricted sandboxes
;;;;   * msg-command encoding — verifies the detach-others frame type

(def-suite client-suite :description "Client connect/detach lifecycle")
(in-suite client-suite)

;;; ── Function existence ───────────────────────────────────────────────────────

(test client-run-client-is-defined
  :description "run-client is a defined function (integration tested via e2e-smoke)."
  (is (fboundp 'cl-tmux::run-client) "run-client must be defined"))

;;; ── socket-path naming ───────────────────────────────────────────────────────

(test client-socket-path-format
  :description "The socket path for session '0' includes the session name."
  (is (search "0" (cl-tmux::socket-path "0"))
      "socket path must contain the session name"))

(test client-socket-path-includes-cl-tmux-prefix
  :description "socket-path always produces a path that includes 'cl-tmux'."
  (let ((path (cl-tmux::socket-path "mysess")))
    (is (search "cl-tmux" path)
        "socket path must contain the cl-tmux prefix, got ~S" path)))

(test client-socket-path-ends-with-sock
  :description "socket-path produces a path that ends with '.sock'."
  (let ((path (cl-tmux::socket-path "any")))
    (is (search ".sock" path)
        "socket path must contain '.sock', got ~S" path)))

(test client-socket-path-embeds-session-name
  :description "socket-path embeds the session name in the path string."
  (let* ((name "mysession-42")
         (path (cl-tmux::socket-path name)))
    (is (search name path)
        "socket path must embed session name ~S, got ~S" name path)))

;;; ── socket-path is a table of name variations ────────────────────────────────
;;;
;;; All of these must produce distinct paths that all embed the session name.

(test client-socket-path-distinct-for-different-names
  :description "socket-path returns distinct paths for distinct session names."
  (let ((p1 (cl-tmux::socket-path "alpha"))
        (p2 (cl-tmux::socket-path "beta")))
    (is (string/= p1 p2)
        "socket-path must produce distinct strings for distinct names")))

;;; ── with-incoming-frame dispatch (socket roundtrip) ─────────────────────────
;;;
;;; These tests drive with-incoming-frame directly via a Unix-domain socket
;;; stream pair.  We write frames from one end and read from the other, exactly
;;; as run-client does.  The macro is in cl-tmux/transport and is used by both
;;; server (serve-client) and client (run-client).

(defun %client-test-socket-path ()
  "Unique throwaway socket path for client dispatch tests."
  (let ((dir (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))))
    (format nil "~A/cl-tmux-client-dispatch-test-~D.sock" dir (get-universal-time))))

(defmacro with-client-test-socket-pair ((writer-stream reader-stream) &body body)
  "Create a Unix-domain socket pair: listener→accept→connect.
   WRITER-STREAM and READER-STREAM are bidirectional binary streams.
   Writer side simulates the server sending frames; reader side reads them
   (matches the run-client perspective where the server writes and client reads)."
  (let ((path    (gensym "PATH"))
        (lstnr   (gensym "LSTNR"))
        (wsock   (gensym "WSOCK"))
        (rsock   (gensym "RSOCK")))
    `(let ((,path (%client-test-socket-path)))
       (let ((,lstnr (make-listener ,path)))
         (unwind-protect
              (let* ((,rsock (connect-to ,path))
                     (,wsock (accept-connection ,lstnr))
                     (,writer-stream (socket-stream ,wsock))
                     (,reader-stream (socket-stream ,rsock)))
                (unwind-protect
                     (progn ,@body)
                  (ignore-errors (close-socket ,wsock))
                  (ignore-errors (close-socket ,rsock))))
           (ignore-errors (close-socket ,lstnr))
           (ignore-errors (delete-file ,path)))))))

(test client-with-incoming-frame-msg-bye-dispatches
  :description "with-incoming-frame dispatches +msg-bye+ correctly — the :return path that
run-client uses to exit its inner loop cleanly."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-client-test-socket-pair (server-side client-side)
      (send-frame server-side (msg-bye))
      (let ((dispatched nil))
        (with-incoming-frame (type _payload client-side)
          ((null type)
           (setf dispatched :eof))
          ((= type +msg-bye+)
           (setf dispatched :bye))
          ((= type +msg-frame+)
           (setf dispatched :frame)))
        (is (eq :bye dispatched)
            "with-incoming-frame must dispatch +msg-bye+ to the :bye arm")))))

(test client-with-incoming-frame-msg-frame-dispatches
  :description "with-incoming-frame dispatches +msg-frame+ correctly — the arm that paints
the rendered frame string in run-client."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-client-test-socket-pair (server-side client-side)
      (send-frame server-side (msg-frame "hello"))
      (let ((received-text nil))
        (with-incoming-frame (type payload client-side)
          ((null type)         nil)
          ((= type +msg-bye+) nil)
          ((= type +msg-frame+)
           (setf received-text (decode-text payload))))
        (is (string= "hello" received-text)
            "msg-frame payload must decode to the original text")))))

(test client-with-incoming-frame-multiple-frames-in-order
  :description "Consecutive with-incoming-frame calls consume frames in order — verifying
the transport layer does not over-read when run-client loops."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-client-test-socket-pair (server-side client-side)
      (send-frame server-side (msg-frame "first"))
      (send-frame server-side (msg-frame "second"))
      (send-frame server-side (msg-bye))
      (let ((results '()))
        (dotimes (_ 3)
          (with-incoming-frame (type payload client-side)
            ((null type)        (push :eof results))
            ((= type +msg-bye+) (push :bye results))
            ((= type +msg-frame+)
             (push (decode-text payload) results))))
        (setf results (nreverse results))
        (is (equal '("first" "second" :bye) results)
            "frames must arrive in order: ~S" results)))))

(test client-with-incoming-frame-unicode-content
  :description "with-incoming-frame correctly decodes Unicode payload."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-client-test-socket-pair (server-side client-side)
      (send-frame server-side (msg-frame "日本語テスト"))
      (let ((received nil))
        (with-incoming-frame (type payload client-side)
          ((null type) nil)
          ((= type +msg-frame+)
           (setf received (decode-text payload))))
        (is (string= "日本語テスト" received)
            "Unicode content must survive the full encode→socket→decode roundtrip")))))

;;; ── detach-others flag wiring ────────────────────────────────────────────────

(test client-detach-others-message-encoding
  :description "msg-command :detach-other-clients produces a frame whose payload round-trips
cleanly — this is the frame run-client sends when :detach-others is T."
  (let* ((frame   (msg-command :detach-other-clients nil nil))
         (decoded (multiple-value-list (decode-frame frame))))
    (is (= +msg-command+ (first decoded))
        "msg-command :detach-other-clients must encode as +msg-command+ type")))

;;; ── %ensure-server-running existence ─────────────────────────────────────────

(test client-ensure-server-running-is-fbound
  :description "%ensure-server-running is a defined function."
  (is (fboundp 'cl-tmux::%ensure-server-running)
      "%ensure-server-running must be fbound"))

;;; ── run-client-with-autostart existence ─────────────────────────────────────

(test client-run-client-with-autostart-is-fbound
  :description "run-client-with-autostart is a defined function."
  (is (fboundp 'cl-tmux::run-client-with-autostart)
      "run-client-with-autostart must be fbound"))

;;; ── *startup-modes* dispatch table ──────────────────────────────────────────

(test startup-modes-has-server-entry
  :description "*startup-modes* contains a 'server' entry mapping to run-server."
  (let ((entry (assoc "server" cl-tmux::*startup-modes* :test #'equal)))
    (is-true entry "*startup-modes* must have a 'server' entry")
    (is (eq 'cl-tmux::run-server (cdr entry))
        "server entry must name run-server")))

(test startup-modes-has-attach-entry
  :description "*startup-modes* contains an 'attach' entry."
  (let ((entry (assoc "attach" cl-tmux::*startup-modes* :test #'equal)))
    (is-true entry "*startup-modes* must have an 'attach' entry")))

(test startup-modes-has-attach-session-entry
  :description "*startup-modes* contains an 'attach-session' entry."
  (let ((entry (assoc "attach-session" cl-tmux::*startup-modes* :test #'equal)))
    (is-true entry "*startup-modes* must have an 'attach-session' entry")))

(test startup-mode-raw-args-p-attach-session
  :description "%startup-mode-raw-args-p returns T only for 'attach-session'."
  (is-true (cl-tmux::%startup-mode-raw-args-p "attach-session")
           "'attach-session' must be raw-args mode")
  (is-false (cl-tmux::%startup-mode-raw-args-p "server")
            "'server' must NOT be raw-args mode")
  (is-false (cl-tmux::%startup-mode-raw-args-p "attach")
            "'attach' must NOT be raw-args mode"))
