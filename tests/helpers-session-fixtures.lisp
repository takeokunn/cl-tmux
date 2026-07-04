(in-package #:cl-tmux/test)

;;;; Session and buffer fixtures.

(defmacro with-two-window-status-session ((sess win0 win1
                                           &key (rows 6) (cols 80)
                                           (mouse t)
                                           (current-format "A")
                                           (format "B")
                                           (separator "|"))
                                          &body body)
  "Run BODY with a 2-window status-bar session tailored for click-hit tests."
  `(with-isolated-options ("mouse" ,mouse
                           "window-status-current-format" ,current-format
                           "window-status-format" ,format
                           "window-status-separator" ,separator)
     (multiple-value-bind (,sess ,win0 _p0 ,win1 _p1)
         (make-two-window-session ,cols (1- ,rows))
       (declare (ignore _p0 _p1))
       (session-select-window ,sess ,win0)
       (with-loop-state
         (let ((cl-tmux::*term-rows* ,rows)
               (cl-tmux::*term-cols* ,cols))
           ,@body)))))

(defmacro with-empty-session ((var) &body body)
  "Bind VAR to a windowless session suitable for empty-state guard tests.
   The session has id 1, name \"0\", and an empty window list."
  `(let ((,var (make-session :id 1 :name "0" :windows nil)))
     ,@body))

(defmacro with-empty-buffers (&body body)
  "Run BODY with an empty paste buffer ring.
   Isolates buffer state so tests cannot contaminate each other."
  `(let ((old-buffers cl-tmux/buffer:*paste-buffers*)
         (old-index cl-tmux/buffer:*buffer-auto-index*))
     (unwind-protect
          (progn
            (cl-tmux/buffer:clear-paste-buffers)
            ,@body)
       (setf cl-tmux/buffer:*paste-buffers* old-buffers
             cl-tmux/buffer:*buffer-auto-index* old-index))))

(defmacro with-auto-rename-session ((screen-var pane-var win-var sess-var
                                     &key (win-name "w") (pid -1)) &body body)
  "Build a 20x5 single-pane session for %maybe-rename-window-from-title tests.
   Runs BODY inside WITH-LOOP-STATE for event-loop isolation."
  `(let* ((,screen-var (make-screen 20 5))
          (,pane-var   (make-pane :id 1 :fd -1 :pid ,pid :x 0 :y 0 :width 20 :height 5
                                  :screen ,screen-var))
          (,win-var    (make-window :id 1 :name ,win-name :width 20 :height 5
                                   :panes (list ,pane-var)
                                   :tree  (make-layout-leaf ,pane-var)))
          (,sess-var   (make-session :id 1 :name "0" :windows (list ,win-var))))
     (window-select-pane ,win-var ,pane-var)
     (session-select-window ,sess-var ,win-var)
     (with-loop-state ,@body)))

(defmacro with-minimal-loop-session ((pane-var win-var sess-var &rest keys) &body body)
  "Combine with-minimal-session + with-loop-state for dispatch tests."
  `(with-minimal-session (,pane-var ,win-var ,sess-var ,@keys)
     (with-loop-state
       ,@body)))

(defmacro with-session ((var rows cols) &body body)
  "Bind VAR to a fresh session of ROWS x COLS, run BODY, then close all PTYs."
  `(let ((,var (create-initial-session ,rows ,cols)))
     (unwind-protect
          (progn ,@body)
       (dolist (p (all-panes ,var))
         (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

(defun make-fake-window (id name &key (npanes 1))
  "A window with NPANES fake panes (fd -1) and a matching tree; the first pane is active.
   Sets :active directly in make-window rather than calling window-select-pane to
   avoid stamping window-last-active-time during construction; that timestamp is a
   session-level concept updated only by session-select-window."
  (let* ((panes (loop for i below npanes
                      collect (make-no-pty-pane (1+ i) 0 0 20 5)))
         (tree  (%fake-window-tree panes)))
    (let ((win (make-window :id id :name name :width 20 :height 5
                            :panes panes :tree tree :active (first panes))))
      (dolist (p panes) (setf (cl-tmux/model:pane-window p) win))
      win)))

(defun %fake-window-tree (panes)
  "Build the left-spine layout tree used by fake-window fixtures."
  (if (null (rest panes))
      (make-layout-leaf (first panes))
      (make-layout-split :h
                         (make-layout-leaf (first panes))
                         (%fake-window-tree (rest panes))
                         1/2)))

(defun make-fake-session (&key (nwindows 1) (npanes 1))
  "A session of NWINDOWS fake windows (each with NPANES fake panes), no PTYs.
   Window ids start at 0 (base-index), matching the real session-new-window behaviour."
  (let* ((windows (loop for i below nwindows
                        collect (make-fake-window i (format nil "~D" i)
                                                  :npanes npanes)))
         (sess    (make-session :id 1 :name "0" :windows windows)))
    (session-select-window sess (first windows))
    sess))

(defmacro with-fake-session ((var &rest make-args) &body body)
  "Bind VAR to a fresh fake session built from MAKE-ARGS and run BODY inside
   WITH-LOOP-STATE isolation.  Composes MAKE-FAKE-SESSION with WITH-LOOP-STATE
   to eliminate the repeated (let ((s (make-fake-session ...))) (with-loop-state ...))
   pattern in dispatch-tests and events-tests.
   MAKE-ARGS are passed verbatim to MAKE-FAKE-SESSION (e.g. :nwindows 2 :npanes 3)."
  `(let ((,var (make-fake-session ,@make-args)))
     (with-loop-state
       ,@body)))

(defmacro with-fake-two-pane-session ((var) &body body)
  "Bind VAR to the common one-window, two-pane fake session used by the
   select-pane command tests and similar command dispatch checks."
  `(with-fake-session (,var :nwindows 1 :npanes 2)
     ,@body))

(defmacro with-copy-mode-state ((session-var screen-var state-var) &body body)
  "Run BODY with SESSION-VAR bound to a fresh fake session in copy mode,
   SCREEN-VAR bound to its active screen, and STATE-VAR bound to a fresh input-state.
   Wraps everything in WITH-LOOP-STATE for proper event-loop isolation.
   Leading DECLARE forms in BODY are hoisted before the copy-mode-enter dispatch
   so they remain valid (CL prohibits declare after an executable form)."
  (let* ((decls (loop for f in body
                      while (and (consp f) (eq (car f) 'declare))
                      collect f))
         (forms (nthcdr (length decls) body)))
    `(let ((,session-var (make-fake-session)))
       (with-loop-state
         (let ((,screen-var (active-screen ,session-var))
               (,state-var  (cl-tmux::make-input-state)))
           ,@decls
           (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
           ,@forms)))))

(defmacro with-copy-mode-vi-state ((session-var screen-var state-var) &body body)
  "Run BODY in an isolated vi copy-mode key-table configuration."
  `(with-copy-mode-keys-state (,session-var ,screen-var ,state-var "vi")
     ,@body))

(defmacro with-copy-mode-emacs-state ((session-var screen-var state-var) &body body)
  "Run BODY in an isolated emacs copy-mode key-table configuration."
  `(with-copy-mode-keys-state (,session-var ,screen-var ,state-var "emacs")
     ,@body))

(defmacro with-copy-mode-keys-state ((session-var screen-var state-var mode-keys)
                                     &body body)
  "Run BODY with MODE-KEYS selected and copy mode already active."
  `(with-isolated-config
     (cl-tmux/options:set-option "mode-keys" ,mode-keys)
     (with-copy-mode-state (,session-var ,screen-var ,state-var)
       ,@body)))

(defun send-copy-mode-bytes (session state bytes)
  "Feed BYTES through PROCESS-BYTE for copy-mode dispatch tests."
  (dolist (byte bytes)
    (cl-tmux::process-byte session byte state)))

(defmacro with-option-session ((var &rest make-args) &body body)
  "Bind VAR to a fresh fake session and run BODY inside WITH-ISOLATED-CONFIG.
   Use this when the test exercises option/config mutations (set-option, prefix,
   key-table rewrites) that must not leak between tests.  Unlike WITH-FAKE-SESSION
   this does NOT wrap in WITH-LOOP-STATE; add it explicitly when needed:
     (with-option-session (s) (with-loop-state ...))"
  `(with-isolated-config
     (let ((,var (make-fake-session ,@make-args)))
       ,@body)))

(defmacro with-isolated-mouse-session ((var &key (nwindows 1) (npanes 1)
                                            (rows 25) (cols 40)
                                            (mouse t))
                                       &body body)
  "Run BODY with isolated config, mouse enabled, and a fake session.
   NWINDOWS/NPANES control the session shape; ROWS/COLS default to the geometry
   used by the mouse dispatch tests."
  `(with-isolated-config
     (with-mouse-option (,mouse)
       (with-fake-session (,var :nwindows ,nwindows :npanes ,npanes)
         (let ((cl-tmux::*term-rows* ,rows)
               (cl-tmux::*term-cols* ,cols))
           ,@body)))))

(defmacro with-minimal-session ((pane-var win-var sess-var
                                 &key (width 20) (height 5)) &body body)
  "Bind PANE-VAR, WIN-VAR, SESS-VAR to a fresh single-pane session of WIDTH x HEIGHT.
   The pane has :fd -1 and :pid -1 (no real PTY).  The window and session are
   selected so session-active-window / window-active-pane work immediately.
   Eliminates the repetitive let*/window-select-pane/session-select-window scaffold
   that appears throughout events-tests.lisp."
  (let ((w-sym (gensym "W")) (h-sym (gensym "H")))
    `(let* ((,w-sym ,width)
            (,h-sym ,height)
            (,pane-var (make-pane :id 1 :fd -1 :pid -1
                                  :x 0 :y 0 :width ,w-sym :height ,h-sym
                                  :screen (make-screen ,w-sym ,h-sym)))
            (,win-var  (make-window :id 1 :name "w"
                                    :width ,w-sym :height ,h-sym
                                    :panes (list ,pane-var)
                                    :tree  (make-layout-leaf ,pane-var)))
            (,sess-var (make-session :id 1 :name "s"
                                     :windows (list ,win-var))))
       (window-select-pane ,win-var ,pane-var)
       (session-select-window ,sess-var ,win-var)
       (locally ,@body))))

(defun active-screen (session)
  (pane-screen (window-active-pane (session-active-window session))))

(defun make-single-pane-session (&key (session-name "s") (window-name "w")
                                       (width 80) (height 24)
                                       (session-id 1) (window-id 1) (pane-id 1))
  "Build and return a minimal (session window pane) triple.
   The pane is no-PTY (fd = -1, pid = -1) sized WIDTH x HEIGHT.
   The window wraps the pane in a leaf tree, with the pane as active.
   The session holds the window as its sole entry and active window.
   Returns (values session window pane).
   Callers that only need the session can ignore the extra values."
  (let* ((pane (make-pane :id pane-id :x 0 :y 0 :width width :height height
                           :fd -1 :pid -1 :screen (make-screen width height)))
         (win  (make-window :id window-id :name window-name
                            :width width :height height
                            :panes (list pane)
                            :tree  (make-layout-leaf pane)
                            :active pane))
         (sess (make-session :id session-id :name session-name
                             :windows (list win) :active win)))
    (window-select-pane win pane)
    (session-select-window sess win)
    (values sess win pane)))
