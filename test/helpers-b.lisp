(in-package #:cl-tmux/test)

;;;; Test helpers — part B: higher-level assertion DSL, layout-tree builders,
;;;; 2-pane fixtures, session fixtures, options isolation, renderer/transport helpers.

;;; ── Higher-level screen assertion DSL ──────────────────────────────────────
;;;
;;; These macros raise the abstraction level for common screen assertions,
;;; making test intent visible and reducing boilerplate IS calls.

(defmacro check-row (screen y expected-string)
  "Assert that row Y of SCREEN starts with EXPECTED-STRING."
  `(is (string= ,expected-string
                (row-string ,screen ,y :end (length ,expected-string)))
       "row ~D: expected ~S got ~S"
       ,y ,expected-string
       (row-string ,screen ,y :end (length ,expected-string))))

(defmacro check-cell (screen x y &key char fg bg attrs)
  "Assert cell attributes at column X, row Y of SCREEN.
   Only non-NIL keyword arguments are checked."
  (let ((forms '()))
    (when char
      (push `(is (char= ,char (char-at ,screen ,x ,y))
                 "char at (~D,~D): expected ~C got ~C" ,x ,y ,char
                 (char-at ,screen ,x ,y))
            forms))
    (when fg
      (push `(is (= ,fg (fg-at ,screen ,x ,y))
                 "fg at (~D,~D): expected ~D got ~D" ,x ,y ,fg
                 (fg-at ,screen ,x ,y))
            forms))
    (when bg
      (push `(is (= ,bg (bg-at ,screen ,x ,y))
                 "bg at (~D,~D): expected ~D got ~D" ,x ,y ,bg
                 (bg-at ,screen ,x ,y))
            forms))
    (when attrs
      (push `(is (= ,attrs (attrs-at ,screen ,x ,y))
                 "attrs at (~D,~D): expected #x~X got #x~X" ,x ,y ,attrs
                 (attrs-at ,screen ,x ,y))
            forms))
    `(progn ,@(nreverse forms))))

(defmacro check-sgr-state (screen &key (fg 7) (bg 0) (attrs 0))
  "Assert the current SGR pen state (foreground, background, attribute bitmask)."
  `(progn
     (is (= ,fg (cl-tmux/terminal/types:screen-cur-fg ,screen))
         "cur-fg: expected ~D got ~D" ,fg
         (cl-tmux/terminal/types:screen-cur-fg ,screen))
     (is (= ,bg (cl-tmux/terminal/types:screen-cur-bg ,screen))
         "cur-bg: expected ~D got ~D" ,bg
         (cl-tmux/terminal/types:screen-cur-bg ,screen))
     (is (= ,attrs (cl-tmux/terminal/types:screen-cur-attrs ,screen))
         "cur-attrs: expected #x~X got #x~X" ,attrs
         (cl-tmux/terminal/types:screen-cur-attrs ,screen))))

(defmacro with-filled-screen ((var w h fill-char) &body body)
  "Bind VAR to a W×H screen pre-filled with FILL-CHAR, then run BODY."
  `(let ((,var (make-screen ,w ,h)))
     (dotimes (row ,h)
       (dotimes (col ,w)
         (feed ,var (string ,fill-char))))
     ,@body))

;;; ── Shared layout-tree builders ─────────────────────────────────────────────
;;;
;;; tl-pane, tl-leaf, and tl-window are defined here (not in layout-tests.lisp)
;;; so that layout-geometry-tests.lisp, window-tests.lisp, and any future test
;;; file can use them without a fragile cross-file dependency.

(defun tl-pane (id w h)
  "Build a no-PTY pane of width W and height H with a matching screen."
  (make-pane :id id :x 0 :y 0 :width w :height h
             :fd -1 :pid -1 :screen (make-screen w h)))

(defun tl-leaf (id w h)
  "Build a layout-leaf wrapping a no-PTY pane of ID, width W, height H."
  (make-layout-leaf (tl-pane id w h)))

(defun tl-window (tree rows cols &key active)
  "Build a window wrapping TREE, laid out at ROWS x COLS, with ACTIVE pane selected."
  (let ((win (make-window :id 1 :name "w" :width cols :height rows :tree tree)))
    (window-refresh-panes win)
    (window-relayout win rows cols)
    (window-select-pane win (or active (first (window-panes win))))
    win))

;;; ── Minimal 2-pane layout fixture ─────────────────────────────────────────────
;;;
;;; Many layout-geometry tests pair two 1×1 placeholder panes as inputs to
;;; layout-assign and %assign-split.  with-two-1x1-panes removes the repeated
;;; boilerplate of constructing p0 and p1 inline.

(defmacro with-two-1x1-panes ((p0-var p1-var) &body body)
  "Bind P0-VAR and P1-VAR to two 1x1 no-PTY panes (ids 1 and 2) for BODY.
   Used by layout-assign and %assign-split tests to avoid repeating the
   same (make-pane :id N :fd -1 :pid -1 :width 1 :height 1 ...) boilerplate."
  `(let* ((,p0-var (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1
                               :screen (make-screen 1 1)))
          (,p1-var (make-pane :id 2 :fd -1 :pid -1 :width 1 :height 1
                               :screen (make-screen 1 1))))
     ,@body))

;;; ── Blank-window fixture macro ──────────────────────────────────────────────
;;;
;;; apply-named-layout tests work on windows whose initial pane geometry is
;;; irrelevant — the layout algorithm assigns final positions.  The pattern:
;;;   (let* ((p0 (make-no-pty-pane 1 0 0 1 1))
;;;          ...
;;;          (win (make-window :id 1 :name "w" :width W :height H
;;;                            :panes (list p0 ...) :tree (make-layout-leaf p0))))
;;;     body)
;;; repeats ~11 times with only the window dimensions and pane count changing.

(defmacro with-blank-window ((win-var &rest pane-vars) (&key (width 80) (height 24))
                             &body body)
  "Bind WIN-VAR to a window of WIDTH x HEIGHT containing one no-pty pane per
   symbol in PANE-VARS (IDs 1..N, all initially 1×1 at (0,0)).  The window tree
   is a single leaf on the first pane — suitable for testing layout algorithms
   that assign final geometry.  No session is created; no loop-state is entered."
  (let ((bindings (loop for var in pane-vars
                        for id from 1
                        collect `(,var (make-no-pty-pane ,id 0 0 1 1)))))
    `(let* (,@bindings
            (,win-var (make-window :id 1 :name "w" :width ,width :height ,height
                                   :panes (list ,@pane-vars)
                                   :tree  (make-layout-leaf ,(first pane-vars)))))
       ,@body)))

;;; ── Shared 2-pane fixture macros ─────────────────────────────────────────────
;;;
;;; Many dispatch tests need identical 2-pane sessions.  These macros eliminate
;;; ~110 lines of repetition and enforce a single source of truth for geometry.

(defmacro with-h-split-window ((win-var p0-var p1-var) &body body)
  "Bind WIN-VAR P0-VAR P1-VAR to a 2-pane horizontal split window:
   p0 (x=0 w=40) | p1 (x=41 w=40), window 81x24, p0 active.
   No session is created; use this for pure pane-neighbor / geometry tests."
  `(let* ((,p0-var  (make-no-pty-pane 1  0 0 40 24))
          (,p1-var  (make-no-pty-pane 2 41 0 40 24))
          (,win-var (make-window :id 1 :name "w" :width 81 :height 24
                                 :panes (list ,p0-var ,p1-var)
                                 :tree (make-layout-split :h
                                          (make-layout-leaf ,p0-var)
                                          (make-layout-leaf ,p1-var)
                                          1/2))))
     (window-select-pane ,win-var ,p0-var)
     ,@body))

;;; ── Concrete-geometry 2-pane fixtures ───────────────────────────────────────
;;;
;;; These macros mirror with-h-split-window / with-v-split-window but pre-set
;;; concrete pane geometry (x/y/width/height) without relying on layout-assign.
;;; They are shared across layout-geometry-tests.lisp, window-neighbor tests, and
;;; any future test that needs exact pre-positioned panes.

(defmacro with-h-split-81-24 ((p0-var p1-var win-var) &body body)
  "A shared 2-pane horizontal split window: 81×24, p0 x=0 w=40, p1 x=41 w=40.
   Geometry is pre-set; no relayout is performed."
  `(let* ((,p0-var (make-pane :id 1 :fd -1 :pid -1
                               :x 0 :y 0 :width 40 :height 24
                               :screen (make-screen 40 24)))
           (,p1-var (make-pane :id 2 :fd -1 :pid -1
                               :x 41 :y 0 :width 40 :height 24
                               :screen (make-screen 40 24)))
           (,win-var (make-window :id 1 :name "w" :width 81 :height 24
                                  :panes (list ,p0-var ,p1-var)
                                  :tree  (make-layout-split :h
                                           (make-layout-leaf ,p0-var)
                                           (make-layout-leaf ,p1-var)
                                           1/2))))
     ,@body))

(defmacro with-v-split-window ((win-var p0-var p1-var) &body body)
  "Bind WIN-VAR P0-VAR P1-VAR to a 2-pane vertical split window:
   p0 (y=0 h=10) above p1 (y=11 h=10), window 80x21, p0 active.
   No session is created; use this for pure pane-neighbor / geometry tests."
  `(let* ((,p0-var  (make-no-pty-pane 1 0  0 80 10))
          (,p1-var  (make-no-pty-pane 2 0 11 80 10))
          (,win-var (make-window :id 1 :name "w" :width 80 :height 21
                                 :panes (list ,p0-var ,p1-var)
                                 :tree (make-layout-split :v
                                          (make-layout-leaf ,p0-var)
                                          (make-layout-leaf ,p1-var)
                                          1/2))))
     (window-select-pane ,win-var ,p0-var)
     ,@body))

(defmacro with-two-pane-h-session ((sess-var win-var p0-var p1-var) &body body)
  "Bind SESS-VAR WIN-VAR P0-VAR P1-VAR to a 2-pane horizontal split session:
   p0 (x=0 w=40) | p1 (x=41 w=40), window 81x24, first pane active.
   Runs BODY inside WITH-LOOP-STATE for event-loop isolation."
  `(let* ((,p0-var  (make-no-pty-pane 1  0 0 40 24))
          (,p1-var  (make-no-pty-pane 2 41 0 40 24))
          (,win-var (make-window :id 1 :name "w" :width 81 :height 24
                                 :panes (list ,p0-var ,p1-var)
                                 :tree (make-layout-split :h
                                          (make-layout-leaf ,p0-var)
                                          (make-layout-leaf ,p1-var)
                                          1/2)))
          (,sess-var (make-session :id 1 :name "0" :windows (list ,win-var))))
     (window-select-pane ,win-var ,p0-var)
     (session-select-window ,sess-var ,win-var)
     (with-loop-state ,@body)))

;;; ── Two-pane layout session fixture ──────────────────────────────────────────
;;;
;;; Layout tests in the dispatch suite (apply-named-layout-even-horizontal,
;;; apply-named-layout-even-vertical, apply-named-layout-tiled, and
;;; run-command-line-select-layout-*) share the same manual build pattern:
;;; make-no-pty-pane × 2 + make-window + make-session + window-select-pane +
;;; session-select-window.  with-two-pane-layout-session encodes that pattern
;;; once and eliminates the repeated boilerplate.

(defmacro with-two-pane-layout-session ((sess-var win-var p0-var p1-var
                                         &key (win-width 81) (win-height 24))
                                        &body body)
  "Bind SESS-VAR WIN-VAR P0-VAR P1-VAR to a 2-pane horizontal split session
   ready for layout-assign tests.  WIN-WIDTH × WIN-HEIGHT default to 81 × 24.
   p0 occupies the left half, p1 the right half, with p0 active.
   Runs BODY inside WITH-LOOP-STATE for event-loop isolation."
  (let ((half-width (gensym "HALF-W")))
    `(let* ((,half-width (floor (- ,win-width 1) 2))
            (,p0-var  (make-no-pty-pane 1  0 0 ,half-width ,win-height))
            (,p1-var  (make-no-pty-pane 2 (1+ ,half-width) 0 ,half-width ,win-height))
            (,win-var (make-window :id 1 :name "w"
                                   :width ,win-width :height ,win-height
                                   :panes (list ,p0-var ,p1-var)
                                   :tree (make-layout-split :h
                                            (make-layout-leaf ,p0-var)
                                            (make-layout-leaf ,p1-var)
                                            1/2)))
            (,sess-var (make-session :id 1 :name "0" :windows (list ,win-var))))
       (window-select-pane ,win-var ,p0-var)
       (session-select-window ,sess-var ,win-var)
       (with-loop-state ,@body))))

(defmacro with-two-pane-v-session ((sess-var win-var p0-var p1-var) &body body)
  "Bind SESS-VAR WIN-VAR P0-VAR P1-VAR to a 2-pane vertical split session:
   p0 (y=0 h=10) above p1 (y=11 h=10), window 80x21, first pane active.
   Runs BODY inside WITH-LOOP-STATE for event-loop isolation."
  `(let* ((,p0-var  (make-no-pty-pane 1 0  0 80 10))
          (,p1-var  (make-no-pty-pane 2 0 11 80 10))
          (,win-var (make-window :id 1 :name "w" :width 80 :height 21
                                 :panes (list ,p0-var ,p1-var)
                                 :tree (make-layout-split :v
                                          (make-layout-leaf ,p0-var)
                                          (make-layout-leaf ,p1-var)
                                          1/2)))
          (,sess-var (make-session :id 1 :name "0" :windows (list ,win-var))))
     (window-select-pane ,win-var ,p0-var)
     (session-select-window ,sess-var ,win-var)
     (with-loop-state ,@body)))

(defmacro with-two-pane-mouse-session ((sess-var win-var p0-var p1-var) &body body)
  "Bind SESS-VAR WIN-VAR P0-VAR P1-VAR to a 2-pane horizontal split session
   suitable for mouse event tests: p0 (x=0 w=40) | p1 (x=41 w=40), window 81x24.
   Enables the 'mouse' session option for the duration of BODY, then restores it.
   BODY runs inside WITH-LOOP-STATE with *term-rows*=25 and *term-cols*=81."
  `(let* ((,p0-var  (make-pane :id 1 :fd -1 :pid -1
                                :x 0 :y 0 :width 40 :height 24
                                :screen (make-screen 40 24)))
          (,p1-var  (make-pane :id 2 :fd -1 :pid -1
                                :x 41 :y 0 :width 40 :height 24
                                :screen (make-screen 40 24)))
          (,win-var (make-window :id 1 :name "w" :width 81 :height 24
                                 :panes (list ,p0-var ,p1-var)
                                 :tree  (make-layout-split :h
                                           (make-layout-leaf ,p0-var)
                                           (make-layout-leaf ,p1-var)
                                           1/2)
                                 :active ,p0-var))
          (,sess-var (make-session :id 1 :name "0"
                                   :windows (list ,win-var) :active ,win-var)))
     (cl-tmux/options:set-option "mouse" t)
     (unwind-protect
          (with-loop-state
            (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 81))
              ,@body))
       (cl-tmux/options:set-option "mouse" nil))))

(defmacro with-single-pane-mouse-session ((sess-var win-var p0-var) &body body)
  "1-pane session (40×24) with mouse=t; restores mouse=nil via unwind-protect.
   BODY runs inside WITH-LOOP-STATE with *term-rows*=25 and *term-cols*=40."
  `(let* ((,p0-var  (make-no-pty-pane 1 0 0 40 24))
          (,win-var (make-window :id 1 :name "w" :width 40 :height 24
                                 :panes (list ,p0-var)
                                 :tree  (make-layout-leaf ,p0-var)
                                 :active ,p0-var))
          (,sess-var (make-session :id 1 :name "0"
                                   :windows (list ,win-var) :active ,win-var)))
     (cl-tmux/options:set-option "mouse" t)
     (unwind-protect
          (with-loop-state
            (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
              ,@body))
       (cl-tmux/options:set-option "mouse" nil))))

;;; ---- Options fixture macros --------------------------------------------------
;;;
;;; These macros are defined here (not in options-tests.lisp) so that
;;; config-directives-tests and any future test file can reuse them without
;;; a fragile cross-file dependency.

(defmacro with-fresh-options (&body body)
  "Run BODY with empty, isolated option hash tables (no registered specs).
   Neither *global-options* nor *option-registry* carry state from load time."
  `(let ((cl-tmux/options:*global-options*   (make-hash-table :test #'equal))
         (cl-tmux/options:*option-registry*  (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-fresh-global-options (&body body)
  "Run BODY with a copy of *global-options* so mutations do not leak.
   *option-registry* is shared so type coercion continues to work."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k ht) v))
                     cl-tmux/options:*global-options*)
            ht)))
     ,@body))

(defmacro with-single-option ((name value) &body body)
  "Run BODY with *global-options* bound to a hash-table containing only NAME → VALUE."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (setf (gethash ,name ht) ,value)
            ht)))
     ,@body))

(defmacro with-fresh-server-options (&body body)
  "Run BODY with an empty, isolated *server-options* hash-table.
   Changes do not leak back to the real *server-options* table."
  `(let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal))
         (cl-tmux/options:*server-option-registry* cl-tmux/options:*server-option-registry*))
     ,@body))

(defmacro with-single-server-option ((name value) &body body)
  "Run BODY with *server-options* bound to a hash-table containing only NAME → VALUE."
  `(let ((cl-tmux/options:*server-options*
          (let ((ht (make-hash-table :test #'equal)))
            (setf (gethash ,name ht) ,value)
            ht)))
     ,@body))

;;; ---- Options isolation --------------------------------------------------------

(defmacro with-isolated-options ((&rest overrides) &body body)
  "Run BODY with a fresh copy of *global-options*, applying OVERRIDES.
   OVERRIDES is a flat list of alternating option-name and value pairs.
   Changes do not leak back to the real *global-options* table."
  (let ((ht-sym (gensym "HT")))
    `(let ((cl-tmux/options:*global-options*
            (let ((,ht-sym (make-hash-table :test #'equal)))
              (maphash (lambda (k v) (setf (gethash k ,ht-sym) v))
                       cl-tmux/options:*global-options*)
              ,@(loop for (k v) on overrides by #'cddr
                      collect `(setf (gethash ,k ,ht-sym) ,v))
              ,ht-sym)))
       ,@body)))

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

;;; ── Shared transport / net fixtures ─────────────────────────────────────────
;;;
;;; Both transport-tests and net-tests write and read protocol frames over file
;;; or socket streams.  The helpers below are shared to avoid duplicating the
;;; temp-path idiom and the write-frames pattern.

(defmacro with-temp-octet-file ((path-var) &body body)
  "Bind PATH-VAR to a unique fresh temp file path, run BODY, then delete the file.
   The filename includes a timestamp and random component so that concurrent test
   runs (or future parallel test execution) never collide on the same path.
   Shared by transport-tests.lisp and net-tests.lisp."
  (let ((label (gensym "LABEL")))
    `(let* ((,label (format nil "cl-tmux-wire-test-~D-~D.bin"
                            (get-universal-time) (random 1000000)))
            (,path-var (namestring
                        (merge-pathnames ,label (uiop:temporary-directory)))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-file ,path-var))))))

(defun write-frames-to-file (path &rest frames)
  "Write each FRAME (octet vector) to PATH via cl-tmux/transport:send-frame.
   Shared by transport-tests.lisp and net-tests.lisp."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (dolist (frame frames)
      (cl-tmux/transport:send-frame out frame))))

(defun write-partial-frame-to-file (path frame byte-count)
  "Write only the first BYTE-COUNT bytes of FRAME to PATH (creating a truncated frame).
   Used by truncation tests to simulate mid-frame EOF conditions without duplicating
   the raw with-open-file / write-sequence / subseq boilerplate."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (write-sequence (subseq frame 0 byte-count) out)))

(defmacro with-temp-socket-path ((path-var) &body body)
  "Bind PATH-VAR to a unique temp socket path, run BODY, then delete it.
   Shared by net-tests.lisp to eliminate duplicated path-building patterns."
  (let ((label (gensym "LABEL")))
    `(let* ((,label (format nil "cl-tmux-test-~D-~D.sock"
                            (get-universal-time) (random 1000000)))
            (,path-var (namestring
                        (merge-pathnames ,label (uiop:temporary-directory)))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-file ,path-var))))))

(defmacro with-connected-sockets ((path listener-var client-var conn-var) &body body)
  "Establish a Unix-domain listener at PATH, connect a client, accept the
   connection.  Binds LISTENER-VAR, CLIENT-VAR, and CONN-VAR.  Closes all
   three sockets on exit, ignoring errors, eliminating the repeated
   listener→connect→accept→unwind-protect scaffold in the net test suite."
  `(let ((,listener-var (cl-tmux/net:make-listener ,path)))
     (unwind-protect
          (let* ((,client-var (cl-tmux/net:connect-to ,path))
                 (,conn-var   (cl-tmux/net:accept-connection ,listener-var)))
            (unwind-protect
                 (locally ,@body)
              (ignore-errors (cl-tmux/net:close-socket ,client-var))
              (ignore-errors (cl-tmux/net:close-socket ,conn-var))))
       (ignore-errors (cl-tmux/net:close-socket ,listener-var)))))

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
  `(let ((cl-tmux/buffer:*paste-buffers* nil)) ,@body))


;;; ── POSIX pipe fixture ───────────────────────────────────────────────────────
;;;
;;; Several test suites (input-tests, pty-tests, pty-rawmode-tests) open pipe
;;; pairs to exercise select/read mechanics without a real TTY.  This macro
;;; consolidates the pattern in one place.

(defun write-byte-to-fd (fd byte-value)
  "Write a single BYTE-VALUE (0–255) to file descriptor FD via CFFI.
   Returns the write(2) return value (1 on success, negative on error).
   Shared by input-tests.lisp and pty-tests.lisp to eliminate repeated
   cffi:with-foreign-object / mem-ref / foreign-funcall write patterns."
  (cffi:with-foreign-object (buf :uint8)
    (setf (cffi:mem-ref buf :uint8) byte-value)
    (cffi:foreign-funcall "write" :int fd :pointer buf :unsigned-long 1 :long)))

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

;;; ── Format-context fixture macro ────────────────────────────────────────────
;;;
;;; The 4-line let* that builds sess/win/pane/ctx from make-fake-session appears
;;; 30+ times across format-tests.lisp and format-tests-d.lisp.  This macro
;;; encodes the standard extraction chain (first window, first pane) once.

(defmacro with-format-context ((sess-var win-var pane-var ctx-var)
                               (&key (nwindows 1) (npanes 1))
                               &body body)
  "Bind SESS-VAR/WIN-VAR/PANE-VAR/CTX-VAR to the first window, first pane, and
   format context of a fresh fake session with NWINDOWS windows and NPANES panes.
   Eliminates the recurring 4-line let* fixture in format-tests.lisp."
  `(let* ((,sess-var (make-fake-session :nwindows ,nwindows :npanes ,npanes))
          (,win-var  (first (cl-tmux/model:session-windows ,sess-var)))
          (,pane-var (first (cl-tmux/model:window-panes ,win-var)))
          (,ctx-var  (cl-tmux/format:format-context-from-session
                      ,sess-var ,win-var ,pane-var)))
     ,@body))

(defmacro with-pipe-fds ((read-fd write-fd) &body body)
  "Open a POSIX pipe; bind READ-FD and WRITE-FD; close both on exit.
   Shared by input-tests.lisp, pty-tests.lisp, and pty-rawmode-tests.lisp."
  (let ((pair-sym (gensym "PAIR")))
    `(let* ((,pair-sym (multiple-value-list (sb-posix:pipe)))
            (,read-fd  (first  ,pair-sym))
            (,write-fd (second ,pair-sym)))
       (unwind-protect
            (progn ,@body)
         (ignore-errors (sb-posix:close ,read-fd))
         (ignore-errors (sb-posix:close ,write-fd))))))
