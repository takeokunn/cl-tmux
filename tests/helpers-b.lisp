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

(defmacro with-command-test-state ((sess &key overlay) &body body)
  "Run BODY with a single-session server state and a clean dirty flag."
  `(let ((cl-tmux::*server-sessions* (list (cons "0" ,sess)))
         (cl-tmux::*dirty* nil)
         ,@(when overlay `((*overlay* nil))))
     ,@body))

(defmacro with-server-size-state ((&key (rows 24) (cols 80)) &body body)
  "Run BODY with the server's terminal-size and event-loop specials isolated:
   *term-rows*/*term-cols* seeded to ROWS/COLS, *dirty* cleared, and *running*
   set.  Extracted so tests exercising +msg-resize+/+msg-attach+/apply-client-size
   share one fixture instead of repeating the same four-binding LET."
  `(let ((cl-tmux::*term-rows* ,rows)
         (cl-tmux::*term-cols* ,cols)
         (cl-tmux::*dirty*    nil)
         (cl-tmux::*running*  t))
     ,@body))

(defmacro with-command-rejection-state ((sess command-form overlay-message
                                              description)
                                        &body body)
  "Assert that COMMAND-FORM is rejected and reports OVERLAY-MESSAGE."
  `(with-command-test-state (,sess :overlay t)
     (is (null ,command-form)
         "~A must be rejected" ,description)
     (is (search ,overlay-message *overlay*)
         "~A must explain that the argument is unsupported" ,description)
     ,@body
     (is-false cl-tmux::*dirty*
               "~A must not mark the model dirty after rejection" ,description)))

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

;;; ── Multi-pane window fixtures ──────────────────────────────────────────────
;;;
;;; %three-pane-window is shared by apply-named-layout tests (main-horizontal,
;;; main-vertical, other-pane-* overrides) in layout-tests-c.lisp.
;;; Defined here so any future test file can use it without cross-file coupling.

(defun %three-pane-window (width height)
  "Build a window of WIDTH x HEIGHT containing three no-PTY panes with no preset tree.
   Used by apply-named-layout tests that set up and check main/other pane sizes."
  (make-window :id 1 :name "w" :width width :height height
               :panes (list (make-no-pty-pane 1 0 0 width height)
                            (make-no-pty-pane 2 0 0 width height)
                            (make-no-pty-pane 3 0 0 width height))))

;;; ── %closest-to-center fixture macro ─────────────────────────────────────────
;;;
;;; Three %closest-to-center tests in layout-geometry-tests-b.lisp each build
;;; a reference pane plus two or three candidate panes with repeated
;;; (make-pane :id N :fd -1 :pid -1 ...) calls.  with-center-test-panes
;;; captures that boilerplate in one place.

(defmacro with-center-test-panes ((&rest pane-specs) &body body)
  "Bind panes according to PANE-SPECS and run BODY.
   Each PANE-SPEC is (VAR id x y width height).
   Eliminates the repeated (make-pane :id N :fd -1 :pid -1 :x X :y Y
   :width W :height H :screen (make-screen W H)) boilerplate in
   %closest-to-center tests."
  `(let* ,(mapcar (lambda (spec)
                    (destructuring-bind (var id x y width height) spec
                      `(,var (make-pane :id ,id :fd -1 :pid -1
                                        :x ,x :y ,y :width ,width :height ,height
                                        :screen (make-screen ,width ,height)))))
                  pane-specs)
     ,@body))

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

;;; ── Concrete 2-pane window fixture helper ─────────────────────────────────

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

(defmacro with-two-pane-h-session ((sess-var win-var p0-var p1-var
                                    &key (mouse t))
                                   &body body)
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
     (with-mouse-option (,mouse)
       (with-loop-state ,@body))))

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

(defmacro with-mouse-option ((mouse) &body body)
  "Run BODY with the session mouse option set to MOUSE, then restore NIL.
   This keeps mouse-enabled and mouse-disabled tests symmetric."
  `(unwind-protect
       (progn
         (cl-tmux/options:set-option "mouse" ,mouse)
         ,@body)
     (cl-tmux/options:set-option "mouse" nil)))

(defmacro with-two-pane-mouse-session ((sess-var win-var p0-var p1-var
                                        &key (mouse t))
                                       &body body)
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
     (with-mouse-option (,mouse)
       (with-loop-state
         (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 81))
           ,@body)))))

(defmacro with-single-pane-mouse-session ((sess-var win-var p0-var &key (mouse t))
                                          &body body)
  "1-pane session (40×24) with optional MOUSE state; restores mouse=nil via
   unwind-protect. BODY runs inside WITH-LOOP-STATE with *term-rows*=25 and
   *term-cols*=40."
  `(let* ((,p0-var  (make-no-pty-pane 1 0 0 40 24))
          (,win-var (make-window :id 1 :name "w" :width 40 :height 24
                                 :panes (list ,p0-var)
                                 :tree  (make-layout-leaf ,p0-var)
                                 :active ,p0-var))
          (,sess-var (make-session :id 1 :name "0"
                                   :windows (list ,win-var) :active ,win-var)))
     (with-mouse-option (,mouse)
       (with-loop-state
         (let ((cl-tmux::*term-rows* 25) (cl-tmux::*term-cols* 40))
           ,@body)))))

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

(defmacro assert-set-directive-option-state (form option expected
                                             &key (context "set directive")
                                                  server-p
                                                  (present-p t))
  "Apply FORM and assert that OPTION is present or absent in the selected scope."
  (let ((actual-sym (gensym "ACTUAL")))
    `(progn
       (assert-config-directive-applied ,form ,context)
       (let ((,actual-sym ,(if server-p
                               `(cl-tmux/options:get-server-option ,option nil)
                               `(cl-tmux/options:get-option ,option nil))))
         ,(if present-p
              `(is (equal ,expected ,actual-sym)
                   "~A must store ~S in ~A options, got ~S"
                   ,context ,expected ,(if server-p "server" "global") ,actual-sym)
              `(is (null ,actual-sym)
                   "~A must remove ~S from ~A options, got ~S"
                   ,context ,option ,(if server-p "server" "global") ,actual-sym))))))

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

(defmacro with-output-octet-stream ((stream-var path) &body body)
  "Open PATH as a fresh binary output octet stream, bind STREAM-VAR, run BODY.
   Collapses the repeated (with-open-file (... :direction :output :if-exists
   :supersede :element-type '(unsigned-byte 8)) ...) opener used by tests that
   hand-construct malformed or partial frames directly onto a file stream."
  `(with-open-file (,stream-var ,path :direction :output :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
     ,@body))

(defun write-frames-to-file (path &rest frames)
  "Write each FRAME (octet vector) to PATH via cl-tmux/transport:send-frame.
   Shared by transport-tests.lisp and net-tests.lisp."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (dolist (frame frames)
      (cl-tmux/transport:send-frame out frame))))

(defun round-trip-frame (frame)
  "Write FRAME to a temp file and return the first decoded frame from it.
   Shared by transport tests that need the same write/read scaffold around
   different payload assertions."
  (with-temp-octet-file (path)
    (write-frames-to-file path frame)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (read-frame in))))

(defun assert-round-tripped-frame-type (frame expected-type)
  "Assert that FRAME round-trips with EXPECTED-TYPE."
  (multiple-value-bind (type payload) (round-trip-frame frame)
    (declare (ignore payload))
    (is (= expected-type type)
        "round-trip type mismatch: expected ~D got ~S"
        expected-type type)))

(defun assert-round-tripped-frame-payload (frame check-fn)
  "Assert that FRAME round-trips and pass its payload to CHECK-FN."
  (multiple-value-bind (type payload) (round-trip-frame frame)
    (declare (ignore type))
    (funcall check-fn payload)))

(defun assert-decoded-frame-type (frame expected-type)
  "Assert that FRAME decodes in-memory (via decode-frame, no file I/O) to
   EXPECTED-TYPE and that the whole frame was consumed. Shared by
   protocol-tests.lisp for pure codec-level round-trip assertions, as
   distinct from assert-round-tripped-frame-type's send-frame/read-frame
   transport-level check."
  (multiple-value-bind (type payload next) (cl-tmux/protocol:decode-frame frame)
    (declare (ignore payload))
    (is (= expected-type type)
        "decode type mismatch: expected ~D got ~S" expected-type type)
    (is (= (length frame) next) "consumed the whole frame")))

(defun assert-decoded-frame-payload (frame check-fn)
  "Decode FRAME in-memory (via decode-frame, no file I/O) and pass its payload
   to CHECK-FN. Shared by protocol-tests.lisp; the transport-level counterpart
   is assert-round-tripped-frame-payload."
  (multiple-value-bind (type payload) (cl-tmux/protocol:decode-frame frame)
    (declare (ignore type))
    (funcall check-fn payload)))

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

(defmacro with-clean-prompt (&body body)
  "Dynamically bind *prompt* to NIL and cl-tmux::*dirty* to NIL so prompt
   state never leaks between tests and dirty flags start clean."
  `(let ((*prompt* nil) (cl-tmux::*dirty* nil)) ,@body))

(defmacro with-clean-overlay (&body body)
  "Dynamically bind the four overlay specials (*overlay*, *overlay-scroll-offset*,
   *overlay-shown-at*, *display-panes-active*) to their inactive defaults so
   overlay state never leaks between tests.  Mirrors with-clean-prompt for the
   sibling overlay/popup/menu test file."
  `(let ((*overlay* nil)
         (*overlay-scroll-offset* 0)
         (*overlay-shown-at* 0)
         (*display-panes-active* nil))
     ,@body))

(defmacro with-empty-registry (&body body)
  "Bind *server-sessions* to NIL for the duration of BODY.
   Thin wrapper over `with-registered-sessions` for the empty-registry case."
  `(with-registered-sessions () ,@body))

(defmacro with-input-state ((var) &body body)
  "Bind VAR to a fresh make-input-state for use with process-byte tests."
  `(let ((,var (cl-tmux::make-input-state)))
     ,@body))

(defun feed-bytes (session input-state bytes)
  "Feed each element of BYTES to SESSION through INPUT-STATE one byte at a
   time via cl-tmux::process-byte, returning the outcome of the final byte.
   Removes the repeated 'feed ESC, feed the next byte, ...' one-call-per-byte
   pattern used to simulate multi-byte escape sequences (arrow keys, X10/SGR
   mouse reports, focus-in/out) arriving on the wire one octet at a time."
  (let ((outcome nil))
    (dolist (byte bytes outcome)
      (setf outcome (cl-tmux::process-byte session byte input-state)))))

(defun seed-scrollback (screen n)
  "Give SCREEN N dummy scrollback rows so copy-mode-scroll has room to move."
  (setf (cl-tmux/terminal/types::screen-scrollback screen)
        (loop repeat n collect (vector))))

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
   BODY may begin with (declare ...) forms; they are valid in locally's body.
   Shared by input-tests.lisp, pty-tests.lisp, and pty-rawmode-tests.lisp."
  (let ((pair-sym (gensym "PAIR")))
    `(let* ((,pair-sym (multiple-value-list (sb-posix:pipe)))
            (,read-fd  (first  ,pair-sym))
            (,write-fd (second ,pair-sym)))
       (declare (ignore ,pair-sym))
       (unwind-protect
            (locally ,@body)
         (ignore-errors (sb-posix:close ,read-fd))
         (ignore-errors (sb-posix:close ,write-fd))))))

(defmacro assert-pipe-pane-open-output-to-command-state (pane)
  "Assert the state of PANE after opening a command that consumes pane output."
  `(progn
     (is-true (cl-tmux/model:pane-pipe-active-p ,pane)
              "pane must be marked active after pipe-pane-open")
     (is-true (cl-tmux/model:pane-pipe-fd ,pane)
              "pane-pipe-fd must hold the command stdin stream")
     (is (null (cl-tmux/model:pane-pipe-output-stream ,pane))
         "pane-pipe-output-stream must remain NIL in output-to-command mode")
     (is (null (cl-tmux/model:pane-pipe-output-thread ,pane))
         "pane-pipe-output-thread must remain NIL in output-to-command mode")
     (is-true (cl-tmux/model:pane-pipe-process ,pane)
              "pane-pipe-process must keep the subprocess handle")))

(defmacro assert-pipe-pane-open-command-output-state (pane)
  "Assert the state of PANE after opening a command that writes back to pane."
  `(progn
     (is-true (cl-tmux/model:pane-pipe-active-p ,pane)
              "pane must be marked active after pipe-pane-open")
     (is (null (cl-tmux/model:pane-pipe-fd ,pane))
         "pane-pipe-fd must remain NIL in command-output-to-pane mode")
     (is-true (cl-tmux/model:pane-pipe-output-stream ,pane)
              "pane-pipe-output-stream must hold the command stdout stream")
     (is-true (cl-tmux/model:pane-pipe-output-thread ,pane)
              "pane-pipe-output-thread must hold the copier thread")
     (is-true (cl-tmux/model:pane-pipe-process ,pane)
              "pane-pipe-process must keep the subprocess handle")))

(defmacro assert-pipe-pane-closed-state (pane)
  "Assert that PANE has no pipe resources left."
  `(progn
     (is (null (cl-tmux/model:pane-pipe-active-p ,pane))
         "pane must be inactive after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-fd ,pane))
         "pane-pipe-fd must be NIL after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-output-stream ,pane))
         "pane-pipe-output-stream must be NIL after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-output-thread ,pane))
         "pane-pipe-output-thread must be NIL after pipe-pane-close")
     (is (null (cl-tmux/model:pane-pipe-process ,pane))
         "pane-pipe-process must be NIL after pipe-pane-close")))

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

;;; ── Session + env-var fixture macro ──────────────────────────────────────────
;;;
;;; Three session-environment-value tests share the same outer shape:
;;;   (let ((sess ...) (name ...))
;;;     (with-temporary-posix-environment-variable (name "from-process")
;;;       <body using sess and name>))
;;; with-session-and-env-var encodes that pattern once.

(defmacro with-session-and-env-var ((sess-var name-var env-name env-value) &body body)
  "Bind SESS-VAR to a fresh empty session and NAME-VAR to ENV-NAME.
   Sets ENV-NAME to ENV-VALUE in the real process environment for the duration
   of BODY, then restores the old value (or unsets it if it was absent).
   Uses with-temporary-posix-environment-variable for POSIX isolation."
  `(let ((,sess-var (make-session :id 1 :name "s"))
         (,name-var ,env-name))
     (with-temporary-posix-environment-variable (,name-var ,env-value)
       ,@body)))

;;; ── Process env-var fixture macro ────────────────────────────────────────────
;;;
;;; The process-environment tests also repeat the same outer shape.  Keep the
;;; per-test body focused on the assertion by hiding the temporary POSIX env
;;; setup behind a tiny helper.

(defmacro with-process-env-var ((name-var env-name env-value) &body body)
  "Bind NAME-VAR to ENV-NAME and set ENV-NAME to ENV-VALUE for BODY.
   Restores the original process environment entry after BODY exits."
  `(let ((,name-var ,env-name))
     (with-temporary-posix-environment-variable (,name-var ,env-value)
       ,@body)))

;;; ── Generic fdefinition-swap fixture ────────────────────────────────────────
;;;
;;; Many CLI-dispatch tests (main-tests.lisp) replace one or more function
;;; cells with a recording stub for the duration of a test, then restore the
;;; originals via unwind-protect.  with-stubbed-fdefinition generalizes that
;;; save/swap/restore scaffold to an arbitrary number of (symbol stub-form)
;;; pairs, so call sites no longer hand-roll the let/unwind-protect/setf dance.

(defmacro with-stubbed-fdefinition ((&rest bindings) &body body)
  "Replace the function cell of each SYMBOL in BINDINGS with its STUB-FORM for
   the extent of BODY, restoring every original definition afterwards even if
   BODY signals.  Each element of BINDINGS is (symbol stub-form)."
  (let ((saved (loop for (symbol) in bindings
                     collect (list symbol (gensym (format nil "ORIG-~A" symbol))))))
    `(let ,(loop for (symbol orig-var) in saved
                collect `(,orig-var (fdefinition ',symbol)))
       (unwind-protect
            (progn
              ,@(loop for (symbol stub-form) in bindings
                     collect `(setf (fdefinition ',symbol) ,stub-form))
              ,@body)
         ,@(loop for (symbol orig-var) in saved
                collect `(setf (fdefinition ',symbol) ,orig-var))))))
