(in-package #:cl-tmux/test)

;;; ── Shared transport / net fixtures ─────────────────────────────────────────
;;;
;;; Both transport-tests and net-tests write and read protocol frames over file
;;; or socket streams.  The helpers below are shared to avoid duplicating the
;;; temp-path idiom and the write-frames pattern.

(defmacro with-temp-octet-file ((path-var) &body body)
  "Bind PATH-VAR to a unique fresh temp file path, run BODY, then delete the file.
   The filename includes a timestamp and random component so that concurrent test
   runs (or future parallel test execution) never collide on the same path.
   Shared by transport-tests.lisp and net-tests.lisp."
  (let ((label (gensym "LABEL")))
    `(let* ((,label (format nil "cl-tmux-wire-test-~D-~D.bin"
                            (get-universal-time) (random 1000000)))
            (,path-var (namestring
                        (merge-pathnames ,label (uiop:temporary-directory)))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-file ,path-var))))))

(defmacro with-output-octet-stream ((stream-var path) &body body)
  "Open PATH as a fresh binary output octet stream, bind STREAM-VAR, run BODY.
   Collapses the repeated (with-open-file (... :direction :output :if-exists
   :supersede :element-type '(unsigned-byte 8)) ...) opener used by tests that
   hand-construct malformed or partial frames directly onto a file stream."
  `(with-open-file (,stream-var ,path :direction :output :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
     ,@body))

(defun write-frames-to-file (path &rest frames)
  "Write each FRAME (octet vector) to PATH via cl-tmux/transport:send-frame.
   Shared by transport-tests.lisp and net-tests.lisp."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (dolist (frame frames)
      (cl-tmux/transport:send-frame out frame))))

(defun round-trip-frame (frame)
  "Write FRAME to a temp file and return the first decoded frame from it.
   Shared by transport tests that need the same write/read scaffold around
   different payload assertions."
  (with-temp-octet-file (path)
    (write-frames-to-file path frame)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (read-frame in))))

(defun assert-round-tripped-frame-type (frame expected-type)
  "Assert that FRAME round-trips with EXPECTED-TYPE."
  (multiple-value-bind (type payload) (round-trip-frame frame)
    (declare (ignore payload))
    (expect (= expected-type type))))

(defun assert-round-tripped-frame-payload (frame check-fn)
  "Assert that FRAME round-trips and pass its payload to CHECK-FN."
  (multiple-value-bind (type payload) (round-trip-frame frame)
    (declare (ignore type))
    (funcall check-fn payload)))

(defun assert-decoded-frame-type (frame expected-type)
  "Assert that FRAME decodes in-memory (via decode-frame, no file I/O) to
   EXPECTED-TYPE and that the whole frame was consumed. Shared by
   protocol-tests.lisp for pure codec-level round-trip assertions, as
   distinct from assert-round-tripped-frame-type's send-frame/read-frame
   transport-level check."
  (multiple-value-bind (type payload next) (cl-tmux/protocol:decode-frame frame)
    (declare (ignore payload))
    (expect (= expected-type type))
    (expect (= (length frame) next))))

(defun assert-decoded-frame-payload (frame check-fn)
  "Decode FRAME in-memory (via decode-frame, no file I/O) and pass its payload
   to CHECK-FN. Shared by protocol-tests.lisp; the transport-level counterpart
   is assert-round-tripped-frame-payload."
  (multiple-value-bind (type payload) (cl-tmux/protocol:decode-frame frame)
    (declare (ignore type))
    (funcall check-fn payload)))

(defun write-partial-frame-to-file (path frame byte-count)
  "Write only the first BYTE-COUNT bytes of FRAME to PATH (creating a truncated frame).
   Used by truncation tests to simulate mid-frame EOF conditions without duplicating
   the raw with-open-file / write-sequence / subseq boilerplate."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (write-sequence (subseq frame 0 byte-count) out)))

(defmacro with-temp-socket-path ((path-var) &body body)
  "Bind PATH-VAR to a unique temp socket path, run BODY, then delete it.
   Shared by net-tests.lisp to eliminate duplicated path-building patterns."
  (let ((label (gensym "LABEL")))
    `(let* ((,label (format nil "cl-tmux-test-~D-~D.sock"
                            (get-universal-time) (random 1000000)))
            (,path-var (namestring
                        (merge-pathnames ,label (uiop:temporary-directory)))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-file ,path-var))))))

(defmacro with-connected-sockets ((path listener-var client-var conn-var) &body body)
  "Establish a Unix-domain listener at PATH, connect a client, accept the
   connection.  Binds LISTENER-VAR, CLIENT-VAR, and CONN-VAR.  Closes all
   three sockets on exit, ignoring errors, eliminating the repeated
   listener→connect→accept→unwind-protect scaffold in the net test suite."
  `(let ((,listener-var (cl-tmux/net:make-listener ,path)))
     (unwind-protect
          (let* ((,client-var (cl-tmux/net:connect-to ,path))
                 (,conn-var   (cl-tmux/net:accept-connection ,listener-var)))
            (unwind-protect
                 (locally ,@body)
              (ignore-errors (cl-tmux/net:close-socket ,client-var))
              (ignore-errors (cl-tmux/net:close-socket ,conn-var))))
       (ignore-errors (cl-tmux/net:close-socket ,listener-var)))))
