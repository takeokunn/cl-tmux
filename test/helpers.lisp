;;;; Test DSL helpers for cl-tmux.
;;;;
;;;; Provides screen builder macros, byte-feeding utilities, grid inspection
;;;; accessors, a table-driven test macro, and layout invariant checkers.

(in-package #:cl-tmux/test)

;;; ── Hooks isolation ─────────────────────────────────────────────────────────

(defmacro with-isolated-hooks (&body body)
  "Run BODY with a fresh *hook-registry* so hook registrations do not leak."
  (let ((registry (gensym "REGISTRY")))
    `(let ((,registry (make-hash-table :test #'equal)))
       (let ((cl-tmux/hooks:*hook-registry* ,registry))
         (progn ,@body)))))

;;; ── Config isolation ────────────────────────────────────────────────────────

(defmacro with-isolated-config (&body body)
  "Run BODY with the mutable config specials dynamically rebound to copies,
   so directives applied in a test never leak into other suites."
  `(let ((cl-tmux/config:*key-tables*  (make-hash-table :test #'equal))
          (cl-tmux/config:*default-shell* cl-tmux/config:*default-shell*)
          (cl-tmux/config:*status-height* cl-tmux/config:*status-height*))
     ;; Re-initialize with fresh key tables
     (cl-tmux/config::initialize-default-key-tables)
     ,@body))

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

;;; ── Generic table-driven test generator ────────────────────────────────────
;;;
;;; define-table-tests generates one (test …) form per row from a declarative
;;; input/expected table.  Reusable across test files; belongs in helpers so
;;; any suite can import it.

(defmacro define-table-tests (suite test-prefix fn &rest cases)
  "Generate one FiveAM test per CASE from a declarative table.
   SUITE       — suite name symbol (informational; tests join the current in-suite).
   TEST-PREFIX — a symbol whose print-name is used as the test-name prefix;
                 each test is named <PREFIX>-1, <PREFIX>-2, …
   FN          — a function designator called via FUNCALL with each input.
   CASES       — each case is (INPUT EXPECTED) or (INPUT EXPECTED DESCRIPTION-STRING).
   Emits one (test <PREFIX>-N …) form per case at macroexpansion time."
  (declare (ignore suite))
  (loop for (input expected . rest) in cases
        for i from 1
        for desc = (if rest (car rest) "")
        for test-name = (intern (format nil "~A-~A" test-prefix i)
                                (find-package :cl-tmux/test))
        collect `(test ,test-name
                   ,desc
                   (is (equal ,expected (funcall ,fn ,input))))
        into forms
        finally (return `(progn ,@forms))))

;;; ── Table-driven test macro ─────────────────────────────────────────────────

(defmacro test-table (test-name description &rest cases)
  "Run a table of cases as a single fiveam test named TEST-NAME.
   DESCRIPTION is a documentation string (currently unused at runtime).
   Each CASE has the form:
     (input-string &key x y char fg bg attrs cx cy row)
   where:
     X, Y     -- cell coordinates for char/fg/bg/attrs checks (default 0)
     CHAR     -- expected character at (X, Y)
     FG       -- expected foreground colour index at (X, Y)
     BG       -- expected background colour index at (X, Y)
     ATTRS    -- expected attribute bitmask at (X, Y)
     CX, CY   -- expected cursor position after processing INPUT
     ROW      -- expected prefix string starting at column 0 of row 0

   Each case creates a fresh 20x5 screen, feeds INPUT to it, then checks
   every non-nil keyword assertion with fiveam IS."
  (declare (ignore description))
  (let ((cases-sym (gensym "CASES")))
    `(test ,test-name
       (let ((,cases-sym
              (list ,@(mapcar
                       (lambda (case-form)
                         (destructuring-bind (input &key (x 0) (y 0)
                                                    char fg bg attrs
                                                    (cx nil) (cy nil) row)
                             case-form
                           `(list ,input ,x ,y ,char ,fg ,bg ,attrs ,cx ,cy ,row)))
                       cases))))
         (dolist (c ,cases-sym)
           (destructuring-bind (input x y expected-char expected-fg expected-bg
                                expected-attrs expected-cx expected-cy expected-row)
               c
             (with-screen (s 20 5)
               (when (plusp (length input))
                 (feed s input))
               (when expected-char
                 (is (char= expected-char (char-at s x y))
                     "char-at ~D,~D: expected ~C got ~C"
                     x y expected-char (char-at s x y)))
               (when expected-fg
                 (is (= expected-fg (fg-at s x y))
                     "fg-at ~D,~D: expected ~D got ~D"
                     x y expected-fg (fg-at s x y)))
               (when expected-bg
                 (is (= expected-bg (bg-at s x y))
                     "bg-at ~D,~D: expected ~D got ~D"
                     x y expected-bg (bg-at s x y)))
               (when expected-attrs
                 (is (= expected-attrs (attrs-at s x y))
                     "attrs-at ~D,~D: expected ~D got ~D"
                     x y expected-attrs (attrs-at s x y)))
               (when expected-cx
                 (is (= expected-cx (screen-cursor-x s))
                     "cursor-x: expected ~D got ~D"
                     expected-cx (screen-cursor-x s)))
               (when expected-cy
                 (is (= expected-cy (screen-cursor-y s))
                     "cursor-y: expected ~D got ~D"
                     expected-cy (screen-cursor-y s)))
               (when expected-row
                 (is (string= expected-row
                               (row-string s 0 :end (length expected-row)))
                     "row 0: expected ~S got ~S"
                     expected-row
                     (row-string s 0 :end (length expected-row)))))))))))

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
  "A window with NPANES fake panes (fd -1) and a matching tree; the first pane is active."
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
                  (build panes)))
         (win   (make-window :id id :name name :width 20 :height 5
                             :panes panes :tree tree)))
    (window-select-pane win (first panes))
    win))

(defun make-fake-session (&key (nwindows 1) (npanes 1))
  "A session of NWINDOWS fake windows (each with NPANES fake panes), no PTYs.
   Window ids start at 0 (base-index), matching the real session-new-window behaviour."
  (let* ((windows (loop for i below nwindows
                        collect (make-fake-window i (format nil "~D" i)
                                                  :npanes npanes)))
         (sess    (make-session :id 1 :name "0" :windows windows)))
    (session-select-window sess (first windows))
    sess))

(defun active-screen (session)
  (pane-screen (window-active-pane (session-active-window session))))

(defmacro with-loop-state (&body body)
  "Dynamically bind the event-loop specials so dispatch side effects are isolated."
  `(let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil)) ,@body))

(defmacro with-clean-prompt (&body body)
  "Dynamically bind *prompt* to NIL and cl-tmux::*dirty* to NIL so prompt
   state never leaks between tests and dirty flags start clean."
  `(let ((*prompt* nil) (cl-tmux::*dirty* nil)) ,@body))

(defmacro with-empty-registry (&body body)
  "Bind *server-sessions* to NIL for the duration of BODY.
   Eliminates the repeated (let ((cl-tmux::*server-sessions* nil)) ...) pattern
   and makes the registry isolation contract explicit in a single named macro."
  `(let ((cl-tmux::*server-sessions* nil)) ,@body))

(defun seed-scrollback (screen n)
  "Give SCREEN N dummy scrollback rows so copy-mode-scroll has room to move."
  (setf (cl-tmux/terminal/types::screen-scrollback screen)
        (loop repeat n collect (vector))))

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
   Runs BODY with those bindings."
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
     ,@body))

(defmacro with-two-pane-v-session ((sess-var win-var p0-var p1-var) &body body)
  "Bind SESS-VAR WIN-VAR P0-VAR P1-VAR to a 2-pane vertical split session:
   p0 (y=0 h=10) above p1 (y=11 h=10), window 80x21, first pane active.
   Runs BODY with those bindings."
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
     ,@body))

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
  "Bind PATH-VAR to a fresh temp file path, run BODY, then delete the file.
   Shared by transport-tests.lisp and net-tests.lisp."
  `(let ((,path-var (merge-pathnames "cl-tmux-wire-test.bin"
                                     (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,path-var)))))

(defun write-frames-to-file (path &rest frames)
  "Write each FRAME (octet vector) to PATH via cl-tmux/transport:send-frame.
   Shared by transport-tests.lisp and net-tests.lisp."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (dolist (frame frames)
      (cl-tmux/transport:send-frame out frame))))

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
