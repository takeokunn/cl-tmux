(in-package #:cl-tmux)

;;;; Entry point, event loop, and prefix-key command dispatch.
;;;;
;;;; Threading model:
;;;;   • One reader thread per pane: blocking read(PTY fd) → pane-feed →
;;;;     screen update → sets *dirty* T.
;;;;   • Main thread: select(stdin, 50 ms) → key dispatch or PTY forward →
;;;;     render when *dirty*.
;;;;
;;;; All PTY children are forked before any threads are started to avoid
;;;; fork-in-multithreaded-process hazards.

;;; ── Shared state ───────────────────────────────────────────────────────────

(defvar *dirty*   t   "Set by reader threads; cleared by the main render step.")
(defvar *running* t   "Loop sentinel; set nil by :detach command.")
(defvar *resize-pending* nil
  "Set by the SIGWINCH handler; the event loop relayouts once and clears it.
   Polling terminal-size every frame is fragile (a transient garbage read
   triggers a spurious resize storm), so geometry is re-read only on signal.")
(defvar *term-rows* 24)
(defvar *term-cols* 80)

(defun install-sigwinch-handler ()
  "Arm SIGWINCH so terminal resizes flag a one-shot relayout."
  (sb-sys:enable-interrupt
   sb-unix:sigwinch
   (lambda (&rest _)
     (declare (ignore _))
     (setf *resize-pending* t
           *dirty*           t))))

;;; ── PTY reader thread ──────────────────────────────────────────────────────

(defun start-reader-thread (pane)
  "Spawn a thread that feeds PTY output into PANE's screen until EOF."
  (make-thread
   (lambda ()
     (loop while *running*
           for bytes = (pty-read-blocking (pane-fd pane) +pty-buf-size+)
           while bytes        ; nil = EOF (shell exited)
           do (pane-feed pane bytes)
              (setf *dirty* t)))
   :name (format nil "pty-reader-~D" (pane-id pane))))

;;; ── Command dispatch (after prefix key) ───────────────────────────────────

(defun next-cyclic (list current)
  "Element after CURRENT in LIST, wrapping around."
  (let* ((idx  (or (position current list) 0))
         (next (nth (mod (1+ idx) (length list)) list)))
    next))

(defun prev-cyclic (list current)
  (let* ((idx  (or (position current list) 0))
         (prev (nth (mod (1- idx) (length list)) list)))
    prev))

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Returns :quit when the session should end, NIL otherwise."
  (let* ((ch  (and byte (code-char byte)))
         (cmd (and ch (gethash ch *key-bindings*))))
    (case cmd

      (:detach
       (setf *running* nil)
       :quit)

      (:new-window
       (let* ((rows (- *term-rows* *status-height*))
              (cols *term-cols*)
              (name (format nil "~D" (1+ (length (session-windows session)))))
              (win  (session-new-window session name rows cols)))
         (start-reader-thread (window-active-pane win))
         (setf *dirty* t))
       nil)

      (:next-window
       (let ((w (next-cyclic (session-windows session)
                             (session-active-window session))))
         (when w (session-select-window session w)))
       (setf *dirty* t)
       nil)

      (:prev-window
       (let ((w (prev-cyclic (session-windows session)
                             (session-active-window session))))
         (when w (session-select-window session w)))
       (setf *dirty* t)
       nil)

      (:next-pane
       (let* ((win   (session-active-window session))
              (panes (window-panes win))
              (next  (next-cyclic panes (window-active-pane win))))
         (when next (window-select-pane win next)))
       (setf *dirty* t)
       nil)

      (:split-horizontal
       (let* ((win (session-active-window session))
              (new (window-split win :horizontal)))
         (start-reader-thread new)
         (setf *dirty* t))
       nil)

      (:split-vertical
       (let* ((win (session-active-window session))
              (new (window-split win :vertical)))
         (start-reader-thread new)
         (setf *dirty* t))
       nil)

      (otherwise
       ;; Unbound key: pass the raw prefix byte + key through to the shell.
       (let ((ap (session-active-pane session)))
         (when ap
           (let ((prefix-byte (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-element +prefix-key-code+)))
             (pty-write (pane-fd ap) prefix-byte))
           (when byte
             (pty-write (pane-fd ap)
                        (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element byte)))))
       nil))))

;;; ── Main event loop ────────────────────────────────────────────────────────

(defun event-loop (session)
  (let ((prefix-pending nil))
    (loop while *running* do
      ;; 50 ms select on stdin keeps render rate ≤ 20 fps when idle.
      (let ((b (read-byte-nonblock 50000)))
        (when b
          (cond
            (prefix-pending
             (setf prefix-pending nil)
             (when (eq :quit (dispatch-prefix-command session b))
               (setf *running* nil)))
            ((= b +prefix-key-code+)
             (setf prefix-pending t))
            (t
             (let ((ap (session-active-pane session)))
               (when ap
                 (pty-write (pane-fd ap)
                            (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-element b))))))))
      ;; Re-read geometry and relayout only when SIGWINCH fired — never per
      ;; frame.  This keeps a transient bad terminal-size read from driving a
      ;; resize storm, and matches how real multiplexers handle resizing.
      (when *resize-pending*
        (setf *resize-pending* nil)
        (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
        (let ((win (session-active-window session)))
          (when win
            (window-relayout win (- *term-rows* *status-height*) *term-cols*))))
      (when *dirty*
        (setf *dirty* nil)
        ;; A freshly selected window may have been laid out at a different
        ;; size; fit it (cheap no-op when already correct).
        (let ((win (session-active-window session)))
          (when win
            (ensure-window-fits win (- *term-rows* *status-height*) *term-cols*)))
        (render-session session *term-rows* *term-cols*)))))

;;; ── Entry point ────────────────────────────────────────────────────────────

(defun main ()
  "Binary entry point — invoked by the image built via (asdf:make :cl-tmux)."
  (require :sb-posix)

  ;; Discover terminal dimensions before any fork so children inherit them.
  (multiple-value-setq (*term-rows* *term-cols*)
    (terminal-size))

  ;; Create the session.  All forks happen here, before reader threads start.
  (let ((session (create-initial-session *term-rows* *term-cols*)))

    ;; Now it is safe to start threads — no more forks will occur at this point
    ;; unless the user explicitly creates a new window/pane via a prefix command
    ;; (those forks also happen on the main thread between render cycles).
    (dolist (pane (all-panes session))
      (start-reader-thread pane))

    (install-sigwinch-handler)

    (handler-case
        (with-raw-mode
          (clear-display)
          (setf *running* t *dirty* t *resize-pending* nil)
          (event-loop session))
      (sb-posix:syscall-error (c)
        ;; Most likely: stdin is not a TTY.
        (format *error-output*
                "~&cl-tmux: ~A~%  (is stdin a terminal?)~%" c)
        (sb-ext:exit :code 1))
      (error (c)
        (format *error-output*
                "~&cl-tmux: unhandled error: ~A~%" c)
        (sb-ext:exit :code 1)))

    ;; Cleanup: kill shells, close fds.
    (setf *running* nil)
    (dolist (pane (all-panes session))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))
