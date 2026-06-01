;;;; Test DSL helpers for cl-tmux.
;;;;
;;;; Provides screen builder macros, byte-feeding utilities, grid inspection
;;;; accessors, a table-driven test macro, and layout invariant checkers.

(in-package #:cl-tmux/test)

;;; ── Config isolation ────────────────────────────────────────────────────────

(defmacro with-isolated-config (&body body)
  "Run BODY with the mutable config specials dynamically rebound to copies,
   so directives applied in a test never leak into other suites."
  `(let ((cl-tmux/config:*key-bindings*  (copy-alist cl-tmux/config:*key-bindings*))
         (cl-tmux/config:*default-shell* cl-tmux/config:*default-shell*)
         (cl-tmux/config:*status-height* cl-tmux/config:*status-height*))
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

;;; ── No-PTY pane builder ─────────────────────────────────────────────────────

(defun make-no-pty-pane (id x y w h)
  "Build a pane with no real PTY and a matching virtual screen."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1
             :screen (make-screen w h)))

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
  "A session of NWINDOWS fake windows (each with NPANES fake panes), no PTYs."
  (let* ((windows (loop for i below nwindows
                        collect (make-fake-window (1+ i) (format nil "~D" (1+ i))
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
