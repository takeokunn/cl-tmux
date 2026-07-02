(in-package #:cl-tmux)

;;; -- Control mode (-C) REPL ------------------------------------------------
;;;
;;; A control client sends commands as text lines; each is run via %run-command-line
;;; and the reply is framed by cl-tmux/control.  A command's textual output (what it
;;; would show in an overlay — list-sessions, display-message, ...) is captured by
;;; binding *overlay* around the run and reading it back into the reply body.

(defun %control-run-command (session line number)
  "Run control-mode command LINE for SESSION as command NUMBER and return the
   framed %begin/.../%end reply string.  The command's overlay text is captured as
   the reply body; a signalled error closes the block with %error."
  (let ((cl-tmux/prompt:*overlay* nil)
        (success t))
    (handler-case
        ;; An unknown command returns the :unknown-command sentinel → %error.
        (when (eq :unknown-command (%run-command-line session line))
          (setf success nil))
      (error () (setf success nil)))
    (cl-tmux/control:control-format-reply
     number (or cl-tmux/prompt:*overlay* "") :success success)))

(defvar *control-output-lock* (make-lock "control-output")
  "Mutex serializing every write to a control-mode (-C) client's output stream.
   Reader threads emit %output / %window-* notifications through %control-emit on
   their own threads, while the REPL writes command replies and %exit from the
   control-mode loop thread; all of those write to the same OUTPUT.  Holding this
   lock around each write keeps the framed control protocol from interleaving and
   corrupting itself.  control-mode-loop rebinds this to a fresh per-client lock;
   the top-level default lets the notification callbacks (and single-threaded unit
   tests that fire hooks directly) run without a surrounding control-mode-loop.")

(defun %control-emit (output line)
  "Write LINE to OUTPUT and flush it, holding *control-output-lock* so the write
   does not interleave with a concurrent REPL reply or another notification."
  (with-lock-held (*control-output-lock*)
    (write-line line output)
    (force-output output)))

(defun %control-window-of (obj)
  "Return OBJ's window when OBJ is a pane, otherwise OBJ itself."
  (if (cl-tmux/model::pane-p obj) (cl-tmux/model:pane-window obj) obj))

(defun %control-emit-layout (output obj)
  "Emit a control-mode layout notification for OBJ when it resolves to a window."
  (let ((win (%control-window-of obj)))
    (when win
      (let ((layout (cl-tmux/model:layout->string win)))
        (%control-emit output
                       (cl-tmux/control:control-layout-change
                        (window-id win) layout layout
                        (if (cl-tmux/model:window-zoom-p win) "Z" "*")))))))

(defmacro %define-hook-emitters (&body rows)
  "Expand ROWS — each (HOOK ARGS-LAMBDA-LIST . BODY) — into a list of
   (hook . callback) conses.  ARGS-LAMBDA-LIST destructures the hook's &rest
   argument list; BODY closes over the enclosing lexical environment (e.g. an
   OUTPUT stream).  Used by %install-control-notifications to build its
   hook-pairs table declaratively instead of hand-rolling each
   `(cons hook (lambda ...))`."
  `(list
    ,@(mapcar (lambda (row)
                (destructuring-bind (hook args-lambda-list &body body) row
                  `(cons ,hook
                         (lambda (&rest %hook-args)
                           (destructuring-bind (&optional ,@args-lambda-list
                                                 &rest %hook-rest)
                               %hook-args
                             (declare (ignorable %hook-rest))
                             ,@body)))))
              rows)))

(defun %install-control-notifications (output)
  "Register hook callbacks that write control-mode (-C) %-notifications to OUTPUT as
   windows/sessions change (the asynchronous half of control mode).  Returns the
   list of (event . callback) pairs so %remove-control-notifications can unregister
   them when the client detaches.  Each callback is variadic so it tolerates the
   hook's argument list; the changed object is the first argument."
  (let ((hook-pairs
          (%define-hook-emitters
            (cl-tmux/hooks:+hook-after-new-window+ (win)
              (%control-emit output
                             (cl-tmux/control:control-window-add (window-id win))))
            (cl-tmux/hooks:+hook-after-kill-window+ (win)
              (%control-emit output
                             (cl-tmux/control:control-window-close (window-id win))))
            (cl-tmux/hooks:+hook-window-renamed+ (win)
              (%control-emit output
                             (cl-tmux/control:control-window-renamed
                              (window-id win) (window-name win))))
            (cl-tmux/hooks:+hook-session-renamed+ (sess)
              (%control-emit output
                             (cl-tmux/control:control-session-renamed
                              (session-id sess) (session-name sess))))
            ;; Active-pane changed within a window (%window-pane-changed) and a
            ;; session's active window changed (%session-window-changed).  Guard the
            ;; NIL active-pane / active-window case so a partially-torn-down object
            ;; can't emit a malformed "@N %NIL" line.
            (cl-tmux/hooks:+hook-window-pane-changed+ (win)
              (let ((ap (and win (window-active-pane win))))
                (when ap
                  (%control-emit output
                                 (cl-tmux/control:control-window-pane-changed
                                  (window-id win) (pane-id ap))))))
            (cl-tmux/hooks:+hook-session-window-changed+ (sess)
              (let ((win (and sess (session-active-window sess))))
                (when win
                  (%control-emit output
                                 (cl-tmux/control:control-session-window-changed
                                  (session-id sess) (window-id win))))))
            ;; Layout changes: resize fires with the window, split with the pane.
            (cl-tmux/hooks:+hook-after-resize-pane+ (obj)
              (%control-emit-layout output obj))
            (cl-tmux/hooks:+hook-after-split-window+ (obj)
              (%control-emit-layout output obj))
            ;; Pane PTY output: emit %output %<pane-id> <escaped-bytes>.
            (cl-tmux/hooks:+hook-pane-output+ (pane raw)
              (let ((data (if (stringp raw) raw (map 'string #'code-char raw))))
                (when (and pane (plusp (length data)))
                  (%control-emit output
                                 (cl-tmux/control:control-output
                                  (cl-tmux/model:pane-id pane) data))))))))
    (mapc (lambda (pair) (cl-tmux/hooks:add-hook (car pair) (cdr pair)))
          hook-pairs)
    hook-pairs))

(defun %remove-control-notifications (handlers)
  "Unregister the control-mode notification callbacks installed by
   %install-control-notifications (HANDLERS is its return value)."
  (dolist (h handlers) (cl-tmux/hooks:remove-hook (car h) (cdr h))))

(defun control-mode-loop (session input output)
  "The control-mode (-C) REPL: read command lines from INPUT, run each as the next
   numbered command, write its framed reply to OUTPUT, until EOF — then emit %exit.
   While the loop runs, %-notifications are emitted to OUTPUT as windows/sessions
   change (installed/removed around the loop).  Streams are parameters so the loop
   is testable without a real tty."
  (let ((handlers (%install-control-notifications output))
        (*control-output-lock* (make-lock "control-output")))
    (unwind-protect
         (loop with number = 0
               for line = (read-line input nil nil)
               while line
               unless (string= "" (string-trim '(#\Space #\Tab #\Return) line))
                 do (incf number)
                    ;; Serialize the reply against async %output notifications
                    ;; that reader threads emit to OUTPUT via %control-emit.
                    (with-lock-held (*control-output-lock*)
                      (write-line (%control-run-command session line number) output)
                      (force-output output)))
      (%remove-control-notifications handlers))
    (with-lock-held (*control-output-lock*)
      (write-line (cl-tmux/control:control-exit) output)
      (force-output output))))

;;; -- dispatch-prefix-command -----------------------------------------------

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Copy mode intercepts [ ] q before the normal binding table.  A binding whose
   value is a token LIST (from `bind key command args...`) runs as a command
   line; a keyword value dispatches as a built-in command.
   Returns :REPEATABLE when the binding had the -r (repeatable) flag set, so
   the caller can stay in after-prefix state for the next key."
  (let* ((ch  (and byte (code-char byte)))
         ;; Probe the prefix table by candidate spellings (raw char, named keys
         ;; like Tab/Enter/BSpace, and C-<letter>) so `bind Tab ...` / `bind
         ;; Enter ...` work — not just single printable chars.  Resolve the entry
         ;; once and derive BOTH the command and the -r flag from it.
         (entry (if (%copy-mode-active-p session)
                    nil
                    (and byte
                         (%key-table-entry-by-candidates
                          +table-prefix+ (%single-byte-key-candidates byte)))))
         (repeatable-p (and entry (key-table-repeatable-p entry)))
         (cmd (if (%copy-mode-active-p session)
                  (%copy-mode-cmd ch)
                  (and entry (key-table-command entry))))
         (result (cond
                   ;; (:sequence cmd1 cmd2 ...) — run each sub-command in order.
                   ((and (consp cmd) (eq (car cmd) :sequence))
                    (let (last-result)
                      (dolist (subcmd (cdr cmd) last-result)
                        (setf last-result (%run-command-tokens session subcmd)))))
                   ;; Token list (arg-bearing command).
                   ((consp cmd)
                    (%run-command-tokens session cmd))
                   ;; Built-in command keyword.
                   (t
                    (dispatch-command session cmd byte)))))
    ;; Propagate :quit/:detach outcomes to the caller.  For other outcomes,
    ;; signal :repeatable when the binding had the -r flag so the caller can
    ;; stay in after-prefix state (resize without re-pressing the prefix key).
    (or (and (member result '(:quit :detach)) result)
        (and repeatable-p :repeatable))))
