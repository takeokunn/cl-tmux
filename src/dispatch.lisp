(in-package #:cl-tmux)

;;;; Declarative command dispatch for the prefix-key handler.
;;;;
;;;; define-command-handlers generates DISPATCH-COMMAND from a table of
;;;; (keyword &body forms) rules.  The macro keeps the dispatch table and
;;;; the function definition in sync automatically.

;;; ── Cyclic navigation macro ─────────────────────────────────────────────────
;;;
;;; next-cyclic and prev-cyclic are the same modular-arithmetic pattern with
;;; the step direction as the only difference.  A Prolog-like fact table:
;;;   navigate(next, List, Current) :- idx + 1.
;;;   navigate(prev, List, Current) :- idx - 1.

(defmacro define-cyclic-navigators (&rest specs)
  "Build cyclic list navigator functions from a declarative step table.
   Each SPEC is (name step docstring)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name step docstring) spec
            `(defun ,name (list current)
               ,docstring
               (let ((idx (or (position current list) 0)))
                 (nth (mod (+ idx ,step) (length list)) list)))))
        specs)))

(define-cyclic-navigators
  (next-cyclic  1  "Element after CURRENT in LIST, wrapping around.")
  (prev-cyclic -1  "Element before CURRENT in LIST, wrapping around."))

;;; ── Active-pane access macro ─────────────────────────────────────────────────
;;;
;;; The pattern (let ((ap (session-active-pane session))) (when ap body))
;;; appears in %passthrough-prefix, %active-screen, and %forward-octets.
;;; with-active-pane names the intent directly.

(defmacro with-active-pane ((pane-var session) &body body)
  "Bind PANE-VAR to SESSION's active pane and evaluate BODY.
   Returns NIL when no active pane is present (no-op guard)."
  `(let ((,pane-var (session-active-pane ,session)))
     (when ,pane-var ,@body)))

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

(defun %cmd-split (session orient)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked."
  (let* ((win (session-active-window session))
         (new (window-split win orient)))
    (when new
      (start-reader-thread new))))

(defun %passthrough-prefix (session byte)
  "Send the raw prefix byte followed by BYTE to the active pane."
  (flet ((byte-vec (b)
           (make-array 1 :element-type '(unsigned-byte 8) :initial-element b)))
    (with-active-pane (ap session)
      (pty-write (pane-fd ap) (byte-vec +prefix-key-code+))
      (when byte
        (pty-write (pane-fd ap) (byte-vec byte))))))

(defun %active-screen (session)
  "Return SESSION's active-pane screen, or NIL when there is no active pane."
  (with-active-pane (ap session)
    (pane-screen ap)))

;;; -- copy-mode-active-p ----------------------------------------------------

(defun copy-mode-active-p (session)
  "Return T when the active pane's screen is in copy mode."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))

;;; ── Copy-mode key overrides macro ────────────────────────────────────────────

(defmacro define-copy-mode-key-overrides (&rest rules)
  "Build a copy-mode key-lookup function from a declarative override table.
   Each RULE is (char keyword). When in copy mode, CH is checked against the
   override table before the normal key-binding lookup.
   Generates %COPY-MODE-CMD that returns the override or the normal binding."
  `(defun %copy-mode-cmd (ch)
     "Return the command for CH when copy mode is active."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (char kw) rule
                     `((and ch (char= ch ,char)) ,kw)))
                 rules)
       (t (and ch (lookup-key-binding ch))))))

(define-copy-mode-key-overrides
  (#\[ :copy-mode-up)
  (#\] :copy-mode-down)
  (#\q :copy-mode-exit))

;;; -- dispatch-prefix-command -----------------------------------------------

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Copy mode intercepts [ ] q before the normal binding table."
  (let* ((ch  (and byte (code-char byte)))
         (cmd (if (copy-mode-active-p session)
                  (%copy-mode-cmd ch)
                  (and ch (lookup-key-binding ch)))))
    (dispatch-command session cmd byte)))

;;; -- define-command-handlers macro + dispatch-command ----------------------

(defmacro define-command-handlers (&rest rules)
  "Each RULE is (keyword &body forms); SESSION and BYTE are bound in each body.
   If a rule evaluates to :QUIT or :DETACH that outcome is returned directly.
   All other outcomes mark *DIRTY* and return NIL."
  `(defun dispatch-command (session cmd byte)
     (declare (ignorable byte))
     (let ((outcome
             (case cmd
               ,@(mapcar (lambda (rule)
                           (destructuring-bind (kw &rest body) rule
                             `(,kw (progn ,@body))))
                         rules)
               (otherwise (%passthrough-prefix session byte) nil))))
       (case outcome
         ((:quit :detach) outcome)
         (otherwise (setf *dirty* t) nil)))))

(define-command-handlers
  (:detach :detach)
  (:new-window (%cmd-new-window session))
  (:next-window (%cmd-cycle-window session #'next-cyclic))
  (:prev-window (%cmd-cycle-window session #'prev-cyclic))
  (:next-pane (%cmd-cycle-pane session #'next-cyclic))
  (:prev-pane (%cmd-cycle-pane session #'prev-cyclic))
  (:split-horizontal (%cmd-split session :v))   ; C-b " adds a horizontal bar → :v stacking
  (:split-vertical   (%cmd-split session :h))   ; C-b % adds a vertical bar   → :h side-by-side
  (:kill-pane
   (let ((result (kill-pane session)))
     (when (eq result :quit) (setf *running* nil))
     result))
  (:kill-window
   (let ((result (kill-window session (session-active-window session))))
     (when (eq result :quit) (setf *running* nil))
     result))
  (:rename-window
   (let ((win (session-active-window session)))
     (when win
       (prompt-start "rename-window" (window-name win)
                     (lambda (name) (rename-window win name))))))
  (:list-keys (show-overlay (describe-key-bindings)))
  (:copy-mode-enter (let ((s (%active-screen session))) (when s (copy-mode-enter s))))
  (:copy-mode-exit  (let ((s (%active-screen session))) (when s (copy-mode-exit s))))
  (:copy-mode-up    (let ((s (%active-screen session))) (when s (copy-mode-scroll s 3))))
  (:copy-mode-down  (let ((s (%active-screen session))) (when s (copy-mode-scroll s -3))))
  (:resize-left   (resize-pane (session-active-window session) :left))
  (:resize-right  (resize-pane (session-active-window session) :right))
  (:resize-up     (resize-pane (session-active-window session) :up))
  (:resize-down   (resize-pane (session-active-window session) :down))
  (:select-window (when byte (select-window-by-number session (- byte (char-code #\0)))))
  (:paste-buffer
   (let* ((text (cl-tmux/buffer:get-paste-buffer))
          (win  (session-active-window session))
          (ap   (and win (window-active-pane win))))
     (when (and text ap (> (pane-fd ap) 0))
       (pty-write (pane-fd ap) (babel:string-to-octets text :encoding :utf-8)))))
  (:select-layout-even-h
   (let ((win (session-active-window session)))
     (when win
       (cl-tmux/model:apply-named-layout win :even-horizontal)
       (layout-assign (window-tree win) 0 0 (window-width win) (window-height win)))))
  (:select-layout-even-v
   (let ((win (session-active-window session)))
     (when win
       (cl-tmux/model:apply-named-layout win :even-vertical)
       (layout-assign (window-tree win) 0 0 (window-width win) (window-height win)))))
  (:select-layout-tiled
   (let ((win (session-active-window session)))
     (when win
       (cl-tmux/model:apply-named-layout win :tiled)
       (layout-assign (window-tree win) 0 0 (window-width win) (window-height win))))))
