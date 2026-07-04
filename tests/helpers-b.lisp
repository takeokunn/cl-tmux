(in-package #:cl-tmux/test)

;;;; Aggregate helper file kept only for renderer, session, loop, and single-pane fixtures.

;;; ── Renderer pane fixture helpers ────────────────────────────────────────────
;;;
;;; These eliminate the repeated (make-screen N M) + (make-pane …) pattern that
;;; appeared 8+ times inline across renderer-pane-tests.lisp.

(defun make-test-pane (w h &key (id 1) (content "") (x 0) (y 0))
  "Build a no-PTY pane of W x H at (X, Y) with ID.
   CONTENT is fed into the pane's screen if non-empty.
   Returns the pane; the screen is accessible via (pane-screen pane)."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id id :x x :y y :width w :height h
                            :fd -1 :screen screen)))
    (unless (string= content "")
      (feed screen content))
    pane))

(defun make-selecting-screen (w h mark-row mark-col cursor-row cursor-col
                              &key (offset 0) rect)
  "Build a screen of W x H in copy-mode with an active selection.
   MARK-ROW/COL and CURSOR-ROW/COL define the selection anchor and cursor.
   OFFSET (default 0) sets the copy-mode scroll offset.
   RECT non-nil sets rectangle-select mode."
  (let ((screen (make-screen w h)))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p        screen) t
          (cl-tmux/terminal/types:screen-copy-selecting     screen) t
          (cl-tmux/terminal/types:screen-copy-offset        screen) offset
          (cl-tmux/terminal/types:screen-copy-mark          screen) (cons mark-row   mark-col)
          (cl-tmux/terminal/types:screen-copy-cursor        screen) (cons cursor-row cursor-col)
          (cl-tmux/terminal/types:screen-copy-rect-select-p screen) (and rect t))
    screen))

;;; ---- Shared renderer session fixture ------------------------------------------

(defun make-renderer-test-session (w h &key (content ""))
  "A 1-window, 1-pane session whose pane screen has CONTENT fed into it.
   No PTY is allocated (fd -1), so this is safe in any environment.
   Shared by renderer-tests.lisp, renderer-pane-tests.lisp, and prompt-tests.lisp."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen screen))
         (win    (make-window :id 1 :name "1" :width w :height h :panes (list pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (unless (string= content "") (feed screen content))
    sess))

(defun make-test-session (w h &key (content ""))
  "Convenience alias for make-renderer-test-session; available to all test files."
  (make-renderer-test-session w h :content content))

(defun make-two-window-session (w h &key (w0-content "") (w1-content ""))
  "Build a 2-window session.  Each window has one pane of W x H with no PTY.
   W0-CONTENT / W1-CONTENT are fed into the respective pane screens.
   The first window is selected on return.
   Returns (values session window0 pane0 window1 pane1)."
  (let* ((screen0 (make-screen w h))
         (pane0   (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen screen0))
         (win0    (make-window :id 1 :name "alpha" :width w :height h :panes (list pane0)))
         (screen1 (make-screen w h))
         (pane1   (make-pane :id 2 :x 0 :y 0 :width w :height h :fd -1 :screen screen1))
         (win1    (make-window :id 2 :name "beta"  :width w :height h :panes (list pane1)))
         (sess    (make-session :id 1 :name "0" :windows (list win0 win1))))
    (window-select-pane win0 pane0)
    (window-select-pane win1 pane1)
    (session-select-window sess win0)
    (unless (string= w0-content "") (feed screen0 w0-content))
    (unless (string= w1-content "") (feed screen1 w1-content))
    (values sess win0 pane0 win1 pane1)))

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

;;; ── Empty-session fixture ────────────────────────────────────────────────────
;;;
;;; The pattern (make-session :id 1 :name "0" :windows nil) appears verbatim
;;; in several dispatch tests.  with-empty-session encodes the intent once and
;;; makes the fixture contract explicit.

(defmacro with-empty-session ((var) &body body)
  "Bind VAR to a windowless session suitable for empty-state guard tests.
   The session has id 1, name \"0\", and an empty window list."
  `(let ((,var (make-session :id 1 :name "0" :windows nil)))
     ,@body))

;;; ── Buffer test helpers ──────────────────────────────────────────────────────

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

;;; ── Shared session/runtime fixture helpers ─────────────────────────────────

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
   avoid stamping window-last-active-time during construction — that timestamp is a
   session-level concept updated only by session-select-window."
  (let* ((panes (loop for i below npanes
                      collect (make-no-pty-pane (1+ i) 0 0 20 5)))
         (tree  (%fake-window-tree panes)))
    (let ((win (make-window :id id :name name :width 20 :height 5
                            :panes panes :tree tree :active (first panes))))
      ;; Wire each pane's back-pointer so pane-window returns the real window.
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
  "Bind PANE-VAR, WIN-VAR, SESS-VAR to a fresh single-pane session of WIDTH×HEIGHT.
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

(defmacro with-global-running (value &body body)
  "Run BODY with the GLOBAL value of cl-tmux::*running* set to VALUE, restoring
   the prior global value afterward.

   Why not (let ((cl-tmux::*running* value)) ...)?  A LET establishes a
   thread-LOCAL dynamic binding visible only in the current thread.  Reader and
   status-timer threads spawned inside BODY do NOT inherit the parent's dynamic
   bindings — they observe the GLOBAL value of *running*.  A LET binding is
   therefore invisible to them: they never see the stop signal, loop forever,
   outlive join-thread's timeout, and leak into later suites as background work.
   Mutating the global with SETF is what those threads actually observe, so any
   test that spawns a reader/timer thread must drive *running* through this
   macro rather than a LET."
  (let ((saved (gensym "SAVED-RUNNING")))
    `(let ((,saved cl-tmux::*running*))
       (setf cl-tmux::*running* ,value)
       (unwind-protect (progn ,@body)
         (setf cl-tmux::*running* ,saved)))))

(defun stop-cl-tmux-threads ()
  "Stop and join every PTY-reader / status-timer / background-shell thread that
   a test may have spawned, so none leaks into a later test.

   Dispatching :split-*, :new-window, :new-session or :respawn-pane spawns a real
   pane and calls START-READER-THREAD; that reader loops while the GLOBAL
   *running* is true.  We clear the global so the loops exit, join the named
   threads (bounded), then restore *running* to T for the next test.  Threads
   are matched by name, so no global registry is required.

   IMPORTANT: after signaling *running*=NIL we SLEEP before restoring it.
   Reader/timer loops only observe *running* between poll cycles (readers poll
   every +pty-poll-timeout-us+ ≈ 50 ms).  Without the pause, *running* could
   flip back to T while a reader is still mid-poll and it would never stop.
   Sleeping ~3 poll cycles gives every reader a chance to observe the stop and
   exit before the bounded join."
  (let ((targets
          (remove-if-not
           (lambda (th)
             (let ((name (bordeaux-threads:thread-name th)))
               (and (stringp name)
                    (or (search "pty-reader" name)
                        (search "cl-tmux-status-timer" name)
                        (search "shell-bg" name)))))
           (bordeaux-threads:all-threads))))
    (when targets
      (setf cl-tmux::*running* nil)
      (sleep 0.15)
      (dolist (th targets)
        (ignore-errors (cl-tmux::%join-thread-with-timeout th 2)))
      (setf cl-tmux::*running* t))))

(defmacro with-loop-state (&body body)
  "Run BODY with the event-loop specials isolated, then stop any reader/timer
   threads BODY spawned (e.g. by dispatching a :split that creates a real pane).

   *running* is driven through its GLOBAL value (via WITH-GLOBAL-RUNNING) rather
   than a LET, because reader threads spawned during BODY read the global; a LET
   binding would be invisible to them and they would leak into later tests.
   STOP-CL-TMUX-THREADS joins them before returning.

   Also isolates prompt/overlay/menu/popup state so that UI state created by
   one test does not leak into subsequent event-loop tests."
  `(let ((cl-tmux::*dirty* nil)
         (cl-tmux::*last-mouse-click* nil)
         (cl-tmux::*key-table* nil)
         ;; Tests feed key bytes microseconds apart — a rate no real terminal
         ;; produces for typed keys — which would trip the assume-paste-time
         ;; heuristic on every second key; start each test with no key history.
         (cl-tmux::*last-ground-key-time* nil)
         (cl-tmux::*server-marked-pane* nil)
         (cl-tmux::*client-read-only* nil)
         (cl-tmux/prompt:*prompt* nil)
         (cl-tmux/prompt:*overlay* nil)
         (cl-tmux/prompt:*overlay-scroll-offset* 0)
         (cl-tmux/prompt:*overlay-shown-at* 0)
         (cl-tmux/prompt:*display-panes-active* nil)
         (cl-tmux/prompt:*active-menu* nil)
         (cl-tmux/prompt:*active-popup* nil))
     (with-global-running t
       (unwind-protect (progn ,@body)
         (stop-cl-tmux-threads)))))

;;; ── Single-pane session fixture ──────────────────────────────────────────────
;;;
;;; Many target-resolution and session tests need the same fixture:
;;;   one no-PTY pane + one window + one session, with focus properly set.
;;; make-single-pane-session encodes that pattern once, eliminating the
;;; ≥9 repetitions of the 5-line inline boilerplate.

(defun make-single-pane-session (&key (session-name "s") (window-name "w")
                                       (width 80) (height 24)
                                       (session-id 1) (window-id 1) (pane-id 1))
  "Build and return a minimal (session window pane) triple.
   The pane is no-PTY (fd = -1, pid = -1) sized WIDTH × HEIGHT.
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
