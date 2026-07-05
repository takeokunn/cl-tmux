(in-package #:cl-tmux/commands)

;;; ── pipe-pane ───────────────────────────────────────────────────────────────
;;;
;;; pipe_pane(Pane, Cmd) :-
;;;   (existing_pipe(Pane) -> close_pipe(Pane) ; true),
;;;   (Cmd \= nil -> open_pipe(Pane, Cmd) ; true).

(defconstant +pipe-pane-close-timeout+ 1
  "Seconds to wait for a pipe-pane subprocess to exit after stdin closes.")

(defconstant +pipe-pane-open-timeout+ 1
  "Seconds to wait for launching the pipe-pane subprocess.")

(defmacro %with-timeout-cleanup ((timeout-seconds cleanup-thunk) &body body)
  "Run BODY under a TIMEOUT-SECONDS bt:with-timeout.  On success, return BODY's
   value.  On a bt:timeout or any other error, funcall CLEANUP-THUNK (a
   zero-argument function) and return NIL.  Consolidates the 'run with a
   deadline, clean up identically on either failure kind, else fall through to
   NIL' shape shared by the pipe-pane launch/wait sites."
  `(handler-case
       (bt:with-timeout (,timeout-seconds) ,@body)
     ((or bt:timeout error) ()
       (funcall ,cleanup-thunk)
       nil)))

(defun %pipe-pane-copy-output (pane output-stream)
  "Copy OUTPUT-STREAM from the command back into PANE's PTY."
  (unwind-protect
      (handler-case
          (let ((buffer (make-string 4096)))
            (loop
              for count = (read-sequence buffer output-stream)
              while (plusp count) do
                (ignore-errors
                  (pty-write (pane-fd pane) (subseq buffer 0 count)))))
        ((or end-of-file error) () nil))
    (ignore-errors (close output-stream))))

(defun %pipe-pane-start-output-thread (pane output-stream)
  "Start the background copier for command stdout into PANE."
  (bt:make-thread (lambda () (%pipe-pane-copy-output pane output-stream))
                  :name (format nil "pipe-pane-output-~D" (pane-id pane))))

(defun %pipe-pane-reset (pane)
  "Clear all pipe-pane state slots on PANE."
  (setf (pane-pipe-fd pane) nil
        (pane-pipe-output-stream pane) nil
        (pane-pipe-output-thread pane) nil
        (pane-pipe-process pane) nil))

(defun pipe-pane-open (pane command &key
                            (pane-output-to-command-p t)
                            (command-output-to-pane-p nil))
  "Connect PANE and COMMAND with pipe-pane direction flags.
   PANE-OUTPUT-TO-COMMAND-P routes pane output to the command's stdin.
   COMMAND-OUTPUT-TO-PANE-P routes command stdout back into the pane.
   Returns a non-NIL stream or process handle on success, NIL on failure."
  ;; Close any existing pipe in either direction.
  (when (pane-pipe-active-p pane)
    (pipe-pane-close pane))
  (let ((proc nil)
        (input-stream nil)
        (output-stream nil)
        (output-thread nil))
    (%with-timeout-cleanup
        (+pipe-pane-open-timeout+
         (lambda ()
           (%pipe-pane-cleanup pane
                               :input-stream input-stream
                               :output-stream output-stream
                               :output-thread output-thread
                               :process proc)))
      (let* ((shell (or cl-tmux/config:*default-shell* "/bin/sh"))
             (new-proc
               (uiop:launch-program (list shell "-c" command)
                                    :input (if pane-output-to-command-p :stream nil)
                                    :output (if command-output-to-pane-p :stream nil)
                                    :error-output nil))
             (new-input (and pane-output-to-command-p
                             (uiop:process-info-input new-proc)))
             (new-output (and command-output-to-pane-p
                              (uiop:process-info-output new-proc))))
        (setf proc new-proc
              input-stream new-input
              output-stream new-output
              (pane-pipe-fd pane) input-stream
              (pane-pipe-output-stream pane) output-stream
              (pane-pipe-process pane) proc)
        (when output-stream
          (setf output-thread
                (%pipe-pane-start-output-thread pane output-stream)
                (pane-pipe-output-thread pane) output-thread))
        (or input-stream output-stream proc t)))))

(defun %wait-pipe-process (process)
  "Return true when PROCESS exits before the pipe-pane close timeout."
  (when process
    (%with-timeout-cleanup (+pipe-pane-close-timeout+ (constantly nil))
      (uiop:wait-process process)
      t)))

(defun %terminate-pipe-process (process)
  "Reap a pipe-pane subprocess, terminating it only if it ignores stdin EOF."
  (when (and process (not (%wait-pipe-process process)))
    (ignore-errors
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process)))
    (%wait-pipe-process process)))

(defun %pipe-pane-cleanup (pane &key input-stream output-stream output-thread process)
  "Best-effort cleanup for pipe-pane resources, then reset PANE."
  (when input-stream
    (ignore-errors (close input-stream)))
  (when output-stream
    (ignore-errors (close output-stream)))
  (ignore-errors (%terminate-pipe-process process))
  (when output-thread
    (ignore-errors
      (cl-tmux::%join-thread-with-timeout output-thread
                                          +pipe-pane-close-timeout+)))
  (%pipe-pane-reset pane))

(defun pipe-pane-close (pane)
  "Close PANE's output pipe if one is open."
  (%pipe-pane-cleanup pane
                      :input-stream (pane-pipe-fd pane)
                      :output-stream (pane-pipe-output-stream pane)
                      :output-thread (pane-pipe-output-thread pane)
                      :process (pane-pipe-process pane)))

(defun pipe-pane-write (pane bytes)
  "Write BYTES to PANE's output pipe if one is active.
   Silently ignores write errors (pipe may have closed on the other end)."
  (when (pane-pipe-fd pane)
    (ignore-errors
      (write-sequence bytes (pane-pipe-fd pane))
      (force-output (pane-pipe-fd pane)))))
