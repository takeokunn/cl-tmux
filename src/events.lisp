(in-package #:cl-tmux)

;;;; Event loop and prefix-key command dispatch.
;;;;
;;;; The main thread reads stdin one byte at a time (50 ms select), routes the
;;;; prefix key (C-b) and its follow-up through the binding table, handles
;;;; copy-mode escape sequences, and forwards everything else to the active
;;;; pane's PTY.  Runtime state (*running*, *dirty*, …) lives in runtime.lisp.

;;; -- Cyclic helpers ---------------------------------------------------------

(defun next-cyclic (list current)
  "Element after CURRENT in LIST, wrapping around."
  (let ((idx (or (position current list) 0)))
    (nth (mod (1+ idx) (length list)) list)))

(defun prev-cyclic (list current)
  "Element before CURRENT in LIST, wrapping around."
  (let ((idx (or (position current list) 0)))
    (nth (mod (1- idx) (length list)) list)))

;;; -- Private command helpers ------------------------------------------------

(defun %cmd-new-window (session)
  "Create a new window in SESSION and start a reader thread for it."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (name (format nil "~D" (1+ (length (session-windows session)))))
         (win  (session-new-window session name rows cols)))
    (start-reader-thread (window-active-pane win))))

(defun %cmd-cycle-window (session cycler)
  "Switch the active window using CYCLER (next-cyclic or prev-cyclic)."
  (let ((w (funcall cycler
                    (session-windows session)
                    (session-active-window session))))
    (when w (session-select-window session w))))

(defun %cmd-cycle-pane (session cycler)
  "Switch the active pane within the active window using CYCLER."
  (let* ((win   (session-active-window session))
         (panes (window-panes win))
         (next  (funcall cycler panes (window-active-pane win))))
    (when next (window-select-pane win next))))

(defun %cmd-split (session direction)
  "Split the active window in SESSION in DIRECTION (:horizontal or :vertical)."
  (let* ((win (session-active-window session))
         (new (window-split win direction)))
    (start-reader-thread new)))

(defun %passthrough-prefix (session byte)
  "Send the raw prefix byte followed by BYTE to the active pane."
  (let ((ap (session-active-pane session)))
    (when ap
      (pty-write (pane-fd ap)
                 (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-element +prefix-key-code+))
      (when byte
        (pty-write (pane-fd ap)
                   (make-array 1 :element-type '(unsigned-byte 8)
                                 :initial-element byte))))))

(defun %active-screen (session)
  "Return SESSION's active-pane screen, or NIL when there is no active pane.
   Copy-mode commands operate on a screen, not a session, so they are routed
   through here."
  (let ((ap (session-active-pane session)))
    (and ap (pane-screen ap))))

(defun handle-prompt-key (byte)
  "Route one input BYTE to the active prompt: Enter runs the prompt's on-submit
   closure with the buffer then dismisses it, Esc cancels, Backspace deletes,
   printable ASCII (32-126) inserts.  Multibyte/UTF-8 input is not yet supported
   (non-ASCII bytes are ignored).  Always marks the screen dirty so the
   status-bar prompt repaints."
  (cond
    ((= byte 13)                            ; Enter — submit and dismiss
     (let ((p *prompt*))
       (when (and p (prompt-on-submit p))
         (funcall (prompt-on-submit p) (prompt-buffer p)))
       (prompt-clear)))
    ((= byte 27) (prompt-clear))            ; Esc — cancel
    ((or (= byte 127) (= byte 8)) (prompt-backspace))   ; Backspace/DEL
    ((and (>= byte 32) (< byte 127)) (prompt-input (code-char byte))))
  (setf *dirty* t))

;;; -- Command dispatch -------------------------------------------------------

(defun dispatch-command (session cmd byte)
  "Execute CMD on SESSION.  Returns :quit when the session should end."
  (case cmd
    (:detach
     ;; Distinct from :quit — standalone treats both as "stop", but a server
     ;; detaches the client (keeping the session) on :detach and only dies on
     ;; :quit (last window killed).
     (return-from dispatch-command :detach))

    (:new-window
     (%cmd-new-window session))

    (:next-window
     (%cmd-cycle-window session #'next-cyclic))

    (:prev-window
     (%cmd-cycle-window session #'prev-cyclic))

    (:next-pane
     (%cmd-cycle-pane session #'next-cyclic))

    (:prev-pane
     (%cmd-cycle-pane session #'prev-cyclic))

    (:split-horizontal
     (%cmd-split session :horizontal))

    (:split-vertical
     (%cmd-split session :vertical))

    (:kill-pane
     (let ((result (kill-pane session)))
       (when (eq result :quit)
         (setf *running* nil)
         (return-from dispatch-command :quit))))

    (:kill-window
     (let* ((win    (session-active-window session))
            (result (kill-window session win)))
       (when (eq result :quit)
         (setf *running* nil)
         (return-from dispatch-command :quit))))

    (:rename-window
     ;; Open an interactive prompt seeded with the current name; on Enter the
     ;; closure renames this window (handle-prompt-key drives the editing).
     (let ((win (session-active-window session)))
       (when win
         (prompt-start "rename-window" (window-name win)
                       (lambda (name) (rename-window win name))))))

    (:list-keys
     ;; Show the key-binding help as an overlay; any key dismisses it.
     (show-overlay (describe-key-bindings)))

    (:copy-mode-enter
     (let ((s (%active-screen session))) (when s (copy-mode-enter s))))

    (:copy-mode-exit
     (let ((s (%active-screen session))) (when s (copy-mode-exit s))))

    (:copy-mode-up
     (let ((s (%active-screen session))) (when s (copy-mode-scroll s 3))))

    (:copy-mode-down
     (let ((s (%active-screen session))) (when s (copy-mode-scroll s -3))))

    (:resize-left   (resize-pane (session-active-window session) :left))
    (:resize-right  (resize-pane (session-active-window session) :right))
    (:resize-up     (resize-pane (session-active-window session) :up))
    (:resize-down   (resize-pane (session-active-window session) :down))

    ;; The digit pressed after the prefix selects that window (0-based).
    (:select-window
     (when byte
       (select-window-by-number session (- byte (char-code #\0)))))

    (otherwise
     ;; Unknown command: pass raw prefix + key through to the active pane.
     (%passthrough-prefix session byte)))

  (setf *dirty* t)
  nil)

;;; -- Prefix-key dispatch ----------------------------------------------------

(defun copy-mode-active-p (session)
  "Return T when the active pane's screen is in copy mode."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   When the active pane is in copy mode, a small set of keys are
   redirected to copy-mode scrolling instead of the normal prefix table.
   Returns :quit when the session should end, NIL otherwise."
  (let* ((ch  (and byte (code-char byte)))
         ;; Inside copy mode, intercept [, ], and q before the normal lookup.
         (cmd (cond
                ((copy-mode-active-p session)
                 (cond ((and ch (char= ch #\[)) :copy-mode-up)
                       ((and ch (char= ch #\])) :copy-mode-down)
                       ((and ch (char= ch #\q)) :copy-mode-exit)
                       (t (and ch (lookup-key-binding ch)))))
                (t
                 (and ch (lookup-key-binding ch))))))
    (dispatch-command session cmd byte)))

;;; -- Copy-mode escape-sequence handling -------------------------------------

(defun handle-copy-mode-escape (session bytes)
  "Check whether BYTES is an ANSI escape sequence for an arrow key while
   copy mode is active and dispatch the corresponding scroll command.
   Returns T if the sequence was consumed, NIL otherwise.
   Sequences handled:
     ESC [ A  (up-arrow)   -> scroll up 3 lines
     ESC [ B  (down-arrow) -> scroll down 3 lines
     q (plain)             -> exit copy mode"
  (let ((screen (%active-screen session)))
    (when (and screen (copy-mode-active-p session))
      (cond
        ;; ESC [ A = up-arrow
        ((and (= (length bytes) 3)
              (= (aref bytes 0) 27)
              (= (aref bytes 1) 91)
              (= (aref bytes 2) 65))
         (copy-mode-scroll screen 3)
         (setf *dirty* t)
         t)
        ;; ESC [ B = down-arrow
        ((and (= (length bytes) 3)
              (= (aref bytes 0) 27)
              (= (aref bytes 1) 91)
              (= (aref bytes 2) 66))
         (copy-mode-scroll screen -3)
         (setf *dirty* t)
         t)
        ;; Plain 'q' exits copy mode
        ((and (= (length bytes) 1)
              (= (aref bytes 0) (char-code #\q)))
         (copy-mode-exit screen)
         (setf *dirty* t)
         t)
        (t nil)))))

;;; -- Keystroke processing (shared by event loop and client/server attach) ---

(defstruct input-state
  "Per-connection keystroke-processing state for PROCESS-BYTE: whether the prefix
   key was just seen, and the in-progress escape-sequence accumulator."
  (prefix-pending nil :type boolean)
  (escape-pending nil :type boolean)
  (escape-buf (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0)))

(defun %forward-octets (session octets)
  "Forward raw OCTETS to SESSION's active-pane PTY."
  (let ((ap (session-active-pane session)))
    (when ap (pty-write (pane-fd ap) octets))))

(defun %forward-byte (session byte)
  "Forward one raw BYTE to SESSION's active-pane PTY."
  (%forward-octets session
                   (make-array 1 :element-type '(unsigned-byte 8)
                                 :initial-element byte)))

(defun %process-escape-byte (session byte state)
  "Accumulate BYTE into STATE's escape buffer; once a sequence completes, either
   dispatch a copy-mode arrow or flush the raw bytes through to the pane."
  (let ((buf (input-state-escape-buf state)))
    (vector-push byte buf)
    (cond
      ;; Completed ESC [ X sequence.
      ((and (= (fill-pointer buf) 3) (= (aref buf 1) 91))
       (setf (input-state-escape-pending state) nil)
       (unless (handle-copy-mode-escape session buf)
         (%forward-octets session (subseq buf 0 (fill-pointer buf))))
       (setf (fill-pointer buf) 0))
      ;; ESC not followed by '[' — flush immediately.
      ((and (= (fill-pointer buf) 2) (/= (aref buf 1) 91))
       (setf (input-state-escape-pending state) nil)
       (%forward-octets session (subseq buf 0 (fill-pointer buf)))
       (setf (fill-pointer buf) 0)))))

(defun process-byte (session byte state)
  "Process one input BYTE for SESSION, updating keystroke STATE and performing
   the resulting effect: prompt editing, copy-mode escape handling, prefix
   command dispatch, or forwarding the byte to the active pane.  Returns :QUIT
   when the session should end, :DETACH when the user requested detach (the
   standalone loop stops on either; a server disconnects the client on :DETACH
   but keeps the session alive, and only dies on :QUIT), or NIL otherwise.  This
   is the single keystroke pipeline shared by the in-process event loop and the
   attach (client/server) path, so both behave identically."
  (cond
    ;; A help/overlay is modal: any key dismisses it and is consumed.
    ((overlay-active-p) (clear-overlay) (setf *dirty* t) nil)
    ;; An active input prompt (e.g. rename) captures every key.
    ((prompt-active-p) (handle-prompt-key byte) nil)
    ;; Mid escape-sequence (arrow keys in copy mode).
    ((input-state-escape-pending state)
     (%process-escape-byte session byte state) nil)
    ;; The byte after the prefix key selects a command.
    ((input-state-prefix-pending state)
     (setf (input-state-prefix-pending state) nil)
     (dispatch-prefix-command session byte))            ; → :quit or nil
    ;; The prefix key itself (C-b): arm prefix dispatch.
    ((= byte +prefix-key-code+)
     (setf (input-state-prefix-pending state) t) nil)
    ;; ESC while in copy mode: start accumulating an escape sequence.
    ((and (= byte 27) (copy-mode-active-p session))
     (setf (input-state-escape-pending state) t
           (fill-pointer (input-state-escape-buf state)) 0)
     (vector-push byte (input-state-escape-buf state))
     nil)
    ;; Ordinary keystroke: hand it to the shell.
    (t (%forward-byte session byte) nil)))

;;; -- Main event loop --------------------------------------------------------

(defun event-loop (session)
  "In-process event loop: read stdin, run each byte through PROCESS-BYTE, and
   repaint *standard-output* when the session is dirty."
  (let ((state (make-input-state)))
    (loop while *running* do
      ;; 50 ms select on stdin keeps render rate <= 20 fps when idle.
      (let ((b (read-byte-nonblock 50000)))
        (when (and b (member (process-byte session b state) '(:quit :detach)))
          (setf *running* nil)))

      ;; Re-read geometry and relayout only when SIGWINCH fired -- never per
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
