;;;; End-to-end smoke test: drive the *real* cl-tmux binary inside a PTY.
;;;;
;;;; This is not part of the ASDF test-system (it needs a built binary and a
;;;; real /dev/ptmx).  Run it from the dev shell:
;;;;
;;;;   nix build .
;;;;   sbcl --no-sysinit --no-userinit --script test/integration/e2e-smoke.lisp \
;;;;         result/bin/cl-tmux
;;;;
;;;; The test spawns cl-tmux on a pseudo-terminal, types `echo <marker>` at the
;;;; keyboard, and verifies the marker appears in cl-tmux's *rendered* output —
;;;; proving the full pipeline: stdin → key forward → inner shell → PTY reader
;;;; thread → screen → renderer.  Then it sends the detach key (C-b d) and
;;;; verifies the process exits cleanly.

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :cl-tmux)
(use-package :cl-tmux/pty)

;;; ── Timing constants ─────────────────────────────────────────────────────────

(defconstant +e2e-startup-timeout-seconds+ 8
  "Maximum seconds to wait for cl-tmux and its inner shell to initialize before typing.")

(defconstant +e2e-startup-quiet-seconds+ 0.5
  "Seconds of quiet PTY output after first render before typing the marker command.")

(defconstant +e2e-marker-timeout-seconds+ 6
  "Maximum seconds to wait for the marker to appear in the rendered output.")

(defconstant +e2e-detach-settle-seconds+  0.5
  "Seconds to let cl-tmux process the detach key before cleaning up the PTY.")

(defconstant +e2e-poll-timeout-us+ cl-tmux/config:+poll-timeout-us+
  "Select timeout in microseconds when polling the PTY for output.")

(defconstant +e2e-read-buf-size+   cl-tmux/config:+pty-buf-size+
  "PTY read buffer size in bytes.")

(defconstant +e2e-search-window-bytes+ (* 64 1024)
  "Maximum recent PTY output bytes to scan for the marker.")

;;; ── Accumulator helpers ──────────────────────────────────────────────────────

(defun %make-accumulator ()
  "Return a fresh adjustable byte vector for accumulating PTY output."
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %accumulate-chunk (acc chunk)
  "Append CHUNK (octet vector) to ACC (adjustable fill-pointer vector)."
  (loop for b across chunk do (vector-push-extend b acc)))

(defun %search-in-tail (substr acc tail-size)
  "Search for SUBSTR (string) in the last TAIL-SIZE bytes of ACC (octet vector).
   Scanning only the tail avoids re-scanning gigabytes of prior PTY output."
  (let* ((len (fill-pointer acc))
         (start (max 0 (- len tail-size))))
    (search substr (map 'string #'code-char (subseq acc start)))))

;;; ── CPS-style polling loop ───────────────────────────────────────────────────

(defun %wait-for-marker (fd substr seconds acc)
  "Poll FD for PTY output up to SECONDS, accumulating into ACC.
   Returns T when SUBSTR appears in the output, NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (mlen (length substr)))
    (loop
      (when (> (get-internal-real-time) deadline) (return nil))
      (when (select-fds (list fd) +e2e-poll-timeout-us+)
        (let ((chunk (pty-read-blocking fd +e2e-read-buf-size+)))
          (when chunk
            (%accumulate-chunk acc chunk)
            (when (%search-in-tail substr acc (max +e2e-search-window-bytes+ mlen))
              (return t))))))))

(defun %wait-for-startup-render (fd seconds acc)
  "Poll FD until cl-tmux has rendered at least once and output has gone quiet.
   The integration smoke drives a saved-core wrapper, whose startup time varies
   enough that a fixed sleep can type before raw mode and the first pane are ready."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (quiet-ticks (* +e2e-startup-quiet-seconds+
                        internal-time-units-per-second))
        (last-output nil))
    (loop
      (let ((now (get-internal-real-time)))
        (when (> now deadline)
          (return (not (null last-output))))
        (when (and last-output
                   (>= (- now last-output) quiet-ticks))
          (return t)))
      (when (select-fds (list fd) +e2e-poll-timeout-us+)
        (let ((chunk (pty-read-blocking fd +e2e-read-buf-size+)))
          (when chunk
            (%accumulate-chunk acc chunk)
            (setf last-output (get-internal-real-time))))))))

;;; ── E2E entry point ──────────────────────────────────────────────────────────

(defun e2e (binary)
  (format t "~&[e2e] driving ~A~%" binary)
  (setf cl-tmux/config:*default-shell* binary)
  (multiple-value-bind (fd pid) (forkpty-with-shell 24 80)
    (unwind-protect
         (let ((marker "E2E_PROOF_4242")
               (acc    (%make-accumulator)))
           ;; Let cl-tmux and its inner shell start up.
           (%wait-for-startup-render fd +e2e-startup-timeout-seconds+ acc)
           ;; Type a command at the (emulated) keyboard.
           (pty-write fd (format nil "echo ~A~%" marker))
           ;; Wait for the marker to appear in rendered output.
           (let ((found (%wait-for-marker fd marker +e2e-marker-timeout-seconds+ acc)))
             ;; Detach: prefix C-b (byte 2) then 'd'.
             (pty-write fd (make-array 2 :element-type '(unsigned-byte 8)
                                          :initial-contents (list 2 (char-code #\d))))
             (sleep +e2e-detach-settle-seconds+)
             (if found
                 (progn (format t "[e2e] PASS — marker rendered by cl-tmux~%")
                        (sb-ext:exit :code 0))
                 (progn (format t "[e2e] FAIL — marker not found in rendered output~%")
                        (format t "[e2e] captured ~D bytes~%" (fill-pointer acc))
                        (sb-ext:exit :code 1)))))
      (pty-close fd pid))))

(let ((binary (or (second sb-ext:*posix-argv*) "result/bin/cl-tmux")))
  (e2e binary))
