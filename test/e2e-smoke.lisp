;;;; End-to-end smoke test: drive the *real* cl-tmux binary inside a PTY.
;;;;
;;;; This is not part of the ASDF test-system (it needs a built binary and a
;;;; real /dev/ptmx).  Run it from the dev shell:
;;;;
;;;;   nix build .
;;;;   sbcl --no-sysinit --no-userinit --script test/e2e-smoke.lisp result/bin/cl-tmux
;;;;
;;;; It spawns cl-tmux on a pseudo-terminal, types `echo <marker>` as if at the
;;;; keyboard, and verifies the marker shows up in cl-tmux's *rendered* output —
;;;; proving the full pipeline: stdin → key forward → inner shell → PTY reader
;;;; thread → screen → renderer.  Then it sends the detach key (C-b d) and
;;;; checks the process exits.

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :cl-tmux)

(use-package :cl-tmux/pty)

(defun e2e (binary)
  (format t "~&[e2e] driving ~A~%" binary)
  ;; forkpty-with-shell execs *default-shell*; point it at the cl-tmux binary.
  (setf cl-tmux/config:*default-shell* binary)
  (multiple-value-bind (fd pid) (forkpty-with-shell 24 80)
    (unwind-protect
         (let ((marker "E2E_PROOF_4242")
               (acc (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
           (flet ((accumulate-until (substr seconds)
                    ;; Scan only each freshly read chunk (joined with a small
                    ;; tail of prior bytes to catch markers split across reads),
                    ;; never the whole buffer — output volume can be large.
                    (let ((deadline (+ (get-internal-real-time)
                                       (* seconds internal-time-units-per-second)))
                          (mlen (length substr)))
                      (loop
                        (when (> (get-internal-real-time) deadline) (return nil))
                        (when (select-fds (list fd) 200000)
                          (let ((chunk (pty-read-blocking fd 8192)))
                            (when chunk
                              (let ((overlap (max 0 (- (fill-pointer acc) (1- mlen)))))
                                (loop for b across chunk do (vector-push-extend b acc))
                                (when (search substr
                                              (map 'string #'code-char
                                                   (subseq acc overlap)))
                                  (return t))))))))))
             ;; Let cl-tmux and its inner shell start up.
             (sleep 1.5)
             ;; Type a command at the (emulated) keyboard.
             (pty-write fd (format nil "echo ~A~%" marker))
             (let ((found (accumulate-until marker 6)))
               ;; Detach: prefix C-b (byte 2) then 'd'.
               (pty-write fd (make-array 2 :element-type '(unsigned-byte 8)
                                           :initial-contents (list 2 (char-code #\d))))
               (sleep 0.5)
               (if found
                   (progn (format t "[e2e] PASS — marker rendered by cl-tmux~%")
                          (sb-ext:exit :code 0))
                   (progn (format t "[e2e] FAIL — marker not found in rendered output~%")
                          (format t "[e2e] captured ~D bytes~%" (length acc))
                          (sb-ext:exit :code 1))))))
      (pty-close fd pid))))

(let ((binary (or (second sb-ext:*posix-argv*)
                  "result/bin/cl-tmux")))
  (e2e binary))
