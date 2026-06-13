;;;; Test DSL helpers for cl-tmux.
;;;;
;;;; Provides screen builder macros, byte-feeding utilities, grid inspection
;;;; accessors, a table-driven test macro, and layout invariant checkers.

(in-package #:cl-tmux/test)

;;; ── Hooks isolation ─────────────────────────────────────────────────────────

(defmacro with-isolated-hooks (&body body)
  "Run BODY with fresh *hook-registry* and *command-hooks* tables so neither
   lisp-function hooks nor command hooks (set-hook) leak between tests."
  `(let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal))
         (cl-tmux/hooks:*command-hooks* (make-hash-table :test #'equal)))
     ,@body))

;;; ── Config isolation ────────────────────────────────────────────────────────

(defmacro with-isolated-config (&body body)
  "Run BODY with the mutable config specials dynamically rebound to copies,
   so directives applied in a test never leak into other suites.
   Isolates: key-tables, default-shell, status-height, prefix-key-code,
             global-options (copy), server-options (copy)."
  `(let ((cl-tmux/config:*key-tables*  (make-hash-table :test #'equal))
          (cl-tmux/config:*default-shell* cl-tmux/config:*default-shell*)
          (cl-tmux/config:*status-height* cl-tmux/config:*status-height*)
          (cl-tmux/config:*prefix-key-code*  cl-tmux/config:*prefix-key-code*)
          (cl-tmux/config:*prefix2-key-code* cl-tmux/config:*prefix2-key-code*)
          (cl-tmux/options:*global-options*
           (let ((h (make-hash-table :test #'equal)))
             (maphash (lambda (k v) (setf (gethash k h) v))
                      cl-tmux/options:*global-options*)
             h))
          (cl-tmux/options:*server-options*
           (let ((h (make-hash-table :test #'equal)))
             (maphash (lambda (k v) (setf (gethash k h) v))
                      cl-tmux/options:*server-options*)
             h)))
     ;; Re-initialize with fresh key tables: config.lisp defaults PLUS the
     ;; extended prefix bindings installed by events-loop.lisp (C-b z, C-b L,
     ;; etc.).  Without the latter the isolated table would diverge from the live
     ;; image and tests like bind-multichar would not find #\z bound.
     (cl-tmux/config::initialize-default-key-tables)
     (cl-tmux::install-extended-key-bindings)
     ,@body))

(defmacro with-isolated-key-tables (&body body)
  "Run BODY with a fresh *KEY-TABLES* inside WITH-ISOLATED-CONFIG isolation.
   Prevents key-table mutations from leaking between tests that exercise
   bind/unbind directives across multiple key-tables (root, copy-mode, etc.)."
  `(let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
     (with-isolated-config
       ,@body)))

(defmacro with-temp-config-file ((path-var &rest lines) &body body)
  "Write LINES (each a string) to a fresh temporary config file, bind PATH-VAR
   to that file's pathname, run BODY, then delete the file.
   Used by load-config-file and source-file tests to avoid repetition of the
   create->unwind-protect->delete-file scaffold."
  (let ((path-sym (gensym "PATH")))
    `(let ((,path-sym (merge-pathnames
                        (format nil "cl-tmux-test-~D.conf" (random 1000000))
                        (uiop:temporary-directory))))
       (unwind-protect
            (progn
              (with-open-file (out ,path-sym :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create)
                ,@(mapcar (lambda (line) `(write-line ,line out)) lines)
                (finish-output out))
              (let ((,path-var ,path-sym))
                ,@body))
         (when (probe-file ,path-sym)
           (delete-file ,path-sym))))))

;;; ── Screen builder ──────────────────────────────────────────────────────────

(defmacro with-screen ((var w h) &body body)
  "Bind VAR to a fresh screen of width W and height H for BODY."
  `(let ((,var (make-screen ,w ,h))) ,@body))

;;; ── Byte feeding ────────────────────────────────────────────────────────────

(defun octets (string)
  "Convert STRING to an (unsigned-byte 8) vector (Latin-1; each char maps
   directly to its char-code, so #\\Escape = #x1B)."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun feed (screen string)
  "Process STRING (one byte per character) through SCREEN's emulator."
  (screen-process-bytes screen (octets string))
  screen)

;;; ── Semantic escape sequence builders ──────────────────────────────────────

(defun esc (fmt &rest args)
  "Build an escape sequence string with ESC (char code 27) prefix.
   FMT and ARGS are passed to FORMAT after the ESC character."
  (format nil "~C~?" #\Escape fmt args))

(defun csi (params final)
  "Build the string ESC [ PARAMS FINAL."
  (format nil "~C[~A~A" #\Escape params (string final)))

;;; ── Grid inspection ─────────────────────────────────────────────────────────

(defun row-string (screen y &key (start 0) end)
  "Return the characters of row Y from START to END (default: full width)."
  (let* ((w (screen-width screen))
         (e (or end w)))
    (with-output-to-string (s)
      (loop for x from start below (min e w)
            do (write-char (cell-char (screen-cell screen x y)) s)))))

(defun cell-at  (screen x y) (screen-cell screen x y))
(defun char-at  (screen x y) (cell-char   (screen-cell screen x y)))
(defun fg-at    (screen x y) (cell-fg     (screen-cell screen x y)))
(defun bg-at    (screen x y) (cell-bg     (screen-cell screen x y)))
(defun attrs-at (screen x y) (cell-attrs  (screen-cell screen x y)))

;;; ── Terminal emulator helpers ───────────────────────────────────────────────

(defmacro check-cursor (screen cx cy)
  "Assert that SCREEN's cursor is at column CX, row CY."
  `(progn
     (is (= ,cx (screen-cursor-x ,screen))
         "cursor-x: expected ~D got ~D" ,cx (screen-cursor-x ,screen))
     (is (= ,cy (screen-cursor-y ,screen))
         "cursor-y: expected ~D got ~D" ,cy (screen-cursor-y ,screen))))

(defun row-blank-p (screen y)
  "Return T when every cell in row Y of SCREEN contains a space."
  (every (lambda (c) (char= #\Space c))
         (coerce (row-string screen y) 'list)))

(defun utf8-feed (screen lisp-string)
  "Encode LISP-STRING as UTF-8 and feed the bytes to SCREEN."
  (screen-process-bytes screen
                        (babel:string-to-octets lisp-string :encoding :utf-8))
  screen)

(defun feed-lines (screen &rest lines)
  "Feed LINES to SCREEN separated by CR/LF, scrolling as needed.  Returns SCREEN."
  (loop for (line . more) on lines
        do (feed screen line)
        when more do (feed screen (format nil "~C~C" #\Return #\Linefeed)))
  screen)

(defun display-row-string (screen y &key end)
  "Characters of viewport row Y via screen-display-cell (honors copy-offset)."
  (let ((end (or end (screen-width screen))))
    (with-output-to-string (s)
      (loop for x below end
            do (write-char (cell-char (screen-display-cell screen x y)) s)))))

;;; ── PTY availability probe (test-only) ─────────────────────────────────────
;;;
;;; pty-available-p is a testing artifact: it forks a real shell and immediately
;;; kills it purely to check PTY access.  It lives here (test helpers) rather
;;; than in production source so the production pty.lisp has no test-only code.

(defun pty-available-p ()
  "Return T if a PTY can be opened and forked on this system, NIL otherwise.
   Used as a skip guard in integration tests that require /dev/ptmx."
  (handler-case
      (multiple-value-bind (fd pid) (forkpty-with-shell 8 20)
        (cl-tmux/pty:pty-close fd pid)
        t)
    (error () nil)))

(defmacro with-pty-shell ((fd-var pid-var &key (rows 24) (cols 80)) &body body)
  "Fork a shell on a fresh PTY of ROWS×COLS; bind FD-VAR and PID-VAR.
   Closes the PTY via unwind-protect on exit, even if BODY signals."
  `(multiple-value-bind (,fd-var ,pid-var) (forkpty-with-shell ,rows ,cols)
     (unwind-protect
          (progn ,@body)
       (pty-close ,fd-var ,pid-var))))

;;; ── No-PTY pane builder ─────────────────────────────────────────────────────

(defun make-no-pty-pane (id x y w h)
  "Build a pane with no real PTY and a matching virtual screen."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1
             :screen (make-screen w h)))

;;; ── Two-pane horizontal window fixture ──────────────────────────────────────

(defun make-two-pane-h-window ()
  "Build a laid-out :h split window: p0 (x=0 w=40) | p1 (x=41 w=40), h=24, w=81.
   Returns (values window p0 p1).  Used by pane-neighbor and directional tests."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-split :h (make-layout-leaf p0)
                                                       (make-layout-leaf p1) 1/2))))
    (window-select-pane win p0)
    (values win p0 p1)))

;;; ── Session fixture ─────────────────────────────────────────────────────────

(defmacro with-session ((var rows cols) &body body)
  "Bind VAR to a fresh session of ROWS x COLS, run BODY, then close all PTYs."
  `(let ((,var (create-initial-session ,rows ,cols)))
     (unwind-protect
          (progn ,@body)
       (dolist (p (all-panes ,var))
         (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

;;; ── Event-dispatch fixtures ─────────────────────────────────────────────────

(defun make-fake-window (id name &key (npanes 1))
  "A window with NPANES fake panes (fd -1) and a matching tree; the first pane is active.
   Sets :active directly in make-window rather than calling window-select-pane to
   avoid stamping window-last-active-time during construction — that timestamp is a
   session-level concept updated only by session-select-window."
  (let* ((panes (loop for i below npanes
                      collect (make-pane :id (1+ i) :x 0 :y 0 :width 20 :height 5
                                         :fd -1 :pid -1 :screen (make-screen 20 5))))
         ;; Build a balanced left-spine tree: each pane wrapped in a leaf.
         ;; For 1 pane: just a leaf. For 2+: chain of :h splits.
         (tree  (labels ((build (ps)
                           (if (null (rest ps))
                               (make-layout-leaf (first ps))
                               (make-layout-split :h
                                  (make-layout-leaf (first ps))
                                  (build (rest ps))
                                  1/2))))
                  (build panes))))
    (let ((win (make-window :id id :name name :width 20 :height 5
                            :panes panes :tree tree :active (first panes))))
      ;; Wire each pane's back-pointer so pane-window returns the real window.
      (dolist (p panes) (setf (cl-tmux/model:pane-window p) win))
      win)))

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
   outlive join-thread's timeout, and leak into later suites where a leftover
   thread makes fork() fail with \"Cannot fork with multiple threads running.\"
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
   a test may have spawned, so none leaks into a later test where a leftover
   thread makes sb-posix:fork signal \"Cannot fork with multiple threads
   running.\"

   Dispatching :split-*, :new-window, :new-session or :respawn-pane forks a real
   pane and calls START-READER-THREAD; that reader loops while the GLOBAL
   *running* is true.  We clear the global so the loops exit, join the named
   threads (bounded), then restore *running* to T for the next test.  Threads
   are matched by name, so no global registry is required.

   IMPORTANT: after signaling *running*=NIL we SLEEP before restoring it.
   Reader/timer loops only observe *running* between poll cycles (readers poll
   every +pty-poll-timeout-us+ ≈ 50 ms).  bordeaux-threads:join-thread does not
   reliably honour :timeout on this build, so we cannot count on the join to
   block until the thread dies — without the pause, *running* would flip back to
   T while a reader is still mid-poll and it would never stop.  Sleeping ~3 poll
   cycles guarantees every reader observes the stop and exits (leaving
   sb-thread:list-all-threads, which is what makes the next fork succeed)."
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
        (ignore-errors (bordeaux-threads:join-thread th :timeout 2)))
      (setf cl-tmux::*running* t))))

(defmacro with-loop-state (&body body)
  "Run BODY with the event-loop specials isolated, then stop any reader/timer
   threads BODY spawned (e.g. by dispatching a :split that forks a real pane).

   *running* is driven through its GLOBAL value (via WITH-GLOBAL-RUNNING) rather
   than a LET, because reader threads spawned during BODY read the global; a LET
   binding would be invisible to them and they would leak into later forking
   tests.  STOP-CL-TMUX-THREADS joins them before returning.

   Also isolates *active-menu* (cl-tmux/prompt) and *active-popup* so that
   menu/popup state created by one test does not leak into subsequent tests."
  `(let ((cl-tmux::*dirty* nil)
         (cl-tmux::*last-mouse-click* nil)
         (cl-tmux::*key-table* nil)
         (cl-tmux::*server-marked-pane* nil)
         (cl-tmux::*client-read-only* nil)
         (cl-tmux/prompt:*active-menu* nil)
         (cl-tmux/prompt:*active-popup* nil))
     (with-global-running t
       (unwind-protect (progn ,@body)
         (stop-cl-tmux-threads)))))

(defmacro with-clean-prompt (&body body)
  "Dynamically bind *prompt* to NIL and cl-tmux::*dirty* to NIL so prompt
   state never leaks between tests and dirty flags start clean."
  `(let ((*prompt* nil) (cl-tmux::*dirty* nil)) ,@body))

(defmacro with-empty-registry (&body body)
  "Bind *server-sessions* to NIL for the duration of BODY.
   Eliminates the repeated (let ((cl-tmux::*server-sessions* nil)) ...) pattern
   and makes the registry isolation contract explicit in a single named macro."
  `(let ((cl-tmux::*server-sessions* nil)) ,@body))

(defmacro with-input-state ((var) &body body)
  "Bind VAR to a fresh make-input-state for use with process-byte tests."
  `(let ((,var (cl-tmux::make-input-state)))
     ,@body))

(defun seed-scrollback (screen n)
  "Give SCREEN N dummy scrollback rows so copy-mode-scroll has room to move."
  (setf (cl-tmux/terminal/types::screen-scrollback screen)
        (loop repeat n collect (vector))))

