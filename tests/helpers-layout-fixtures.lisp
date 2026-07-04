(in-package #:cl-tmux/test)

;;;; Layout fixture helpers.

;;; ── Shared layout-tree builders ─────────────────────────────────────────────
;;;
;;; tl-pane, tl-leaf, and tl-window are defined here (not in the layout-tree test files)
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
