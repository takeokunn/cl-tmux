(in-package #:cl-tmux/test)

;;;; rename-window, kill-window, run-shell, if-shell, selection-text, swap-pane, capture-pane — part III

(in-suite commands-suite)

;;; ── rename-window ────────────────────────────────────────────────────────────

(test rename-window-sets-name
  "rename-window sets the window name to the supplied string."
  (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win "new")
    (is (string= "new" (window-name win))
        "window name must be updated to \"new\"")))

(test rename-window-nil-window-is-noop
  "rename-window with NIL window does not signal an error."
  (finishes (cl-tmux/commands:rename-window nil "irrelevant")))

(test rename-window-invalid-name-is-noop-table
  "rename-window with an empty or NIL name leaves the window name unchanged."
  (dolist (c '(("" "original" "empty string → no-op")
               (nil "keep"    "nil → no-op")))
    (destructuring-bind (new-name original desc) c
      (let ((win (make-window :id 1 :name original :width 20 :height 5 :panes nil)))
        (cl-tmux/commands:rename-window win new-name)
        (is (string= original (window-name win)) "~A" desc)))))

;;; ── kill-window (direct path) ────────────────────────────────────────────────

(test kill-window-explicit-window-arg-removes-that-window
  "kill-window with an explicit WINDOW removes that specific window even when it
   is not the active one."
  (let* ((p0  (%make-test-pane :id 1))
         (p1  (%make-test-pane :id 2))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :tree (make-layout-leaf p1) :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (session-select-window sess w1)          ; active = w1
    ;; Kill the non-active window w2 explicitly.
    (is (null (kill-window sess w2))
        "killing a non-active window must return NIL (session survives)")
    (is (equal (list w1) (session-windows sess))
        "only w2 must be removed from the session")
    (is (eq w1 (session-active-window sess))
        "active window must remain w1 when the killed window was not active")))

(test kill-window-last-window-returns-quit
  "Destroying the sole window of a session returns :quit."
  (let* ((p0  (%make-test-pane))
         (w1  (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (sess (make-session :id 1 :name "0" :windows (list w1))))
    (session-select-window sess w1)
    (is (eq :quit (kill-window sess))
        "killing the sole window must return :quit")
    (is (null (session-windows sess)) "session must have no windows")))

(test kill-window-active-switches-to-remaining
  "Killing the active window of two switches the active pointer to the survivor."
  (let* ((p0  (%make-test-pane :id 1))
         (p1  (%make-test-pane :id 2))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :tree (make-layout-leaf p1) :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (session-select-window sess w1)
    (is (null (kill-window sess))
        "session with a remaining window must not quit")
    (is (eq w2 (session-active-window sess))
        "active window must switch to the survivor after killing the active one")))

(test kill-window-active-reselects-mru-not-nearest
  "End-to-end: killing the active window selects the last-used (MRU) survivor, not
   the numerically-nearest one (tmux session_detach / session_last).  Timestamps
   are preset (session-select-window has 1-second universal-time resolution, so
   live switches would tie); killed=1 with remaining {0,2} is an id-distance tie
   the OLD %nearest-window rule broke toward the higher id (w2)."
  (let* ((p0 (%make-test-pane :id 1))
         (p1 (%make-test-pane :id 2))
         (p2 (%make-test-pane :id 3))
         (w0 (make-window :id 0 :name "a" :width 20 :height 5
                          :tree (make-layout-leaf p0) :panes (list p0)
                          :last-active-time 200))   ; MRU survivor
         (w1 (make-window :id 1 :name "b" :width 20 :height 5
                          :tree (make-layout-leaf p1) :panes (list p1)))
         (w2 (make-window :id 2 :name "c" :width 20 :height 5
                          :tree (make-layout-leaf p2) :panes (list p2)
                          :last-active-time 100))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1 w2))))
    ;; Make w1 active (its timestamp becomes 'now', irrelevant — it is killed).
    (session-select-window sess w1)
    (kill-window sess)
    (is (eq w0 (session-active-window sess))
        "MRU survivor w0 (time 200 > w2's 100) is selected, NOT nearest-tie w2")))

;;; ── run-shell ────────────────────────────────────────────────────────────────
;;;
;;; Tests use /bin/true (always exits 0) and /bin/echo (prints output) which are
;;; universally available on POSIX systems.  Background mode is verified via the
;;; T return value without inspecting the process object.

(test run-shell-foreground-captures-stdout
  "run-shell (background nil) returns a string containing the command's output."
  (let ((out (cl-tmux/commands:run-shell "echo hello")))
    (is (stringp out) "return value must be a string")
    (is (search "hello" out) "output must contain the echoed word")))

(test run-shell-background-returns-t
  "run-shell :background T returns T immediately without waiting."
  (let ((result (cl-tmux/commands:run-shell "true" :background t)))
    (is (eq t result) "background run must return T")))

(test run-shell-foreground-empty-command-returns-string
  "run-shell with a no-op command returns an empty or whitespace-only string."
  (let ((out (cl-tmux/commands:run-shell "true")))
    (is (stringp out) "return value must be a string even for a no-op command")))

;;; ── if-shell ─────────────────────────────────────────────────────────────────

(test if-shell-zero-exit-calls-then-fn
  "if-shell calls THEN-FN when the command exits with code 0."
  (let ((called nil))
    (cl-tmux/commands:if-shell "true" (lambda () (setf called t)))
    (is-true called "then-fn must be invoked for a zero-exit command")))

(test if-shell-nonzero-exit-calls-else-fn
  "if-shell calls ELSE-FN when the command exits non-zero."
  (let ((else-called nil))
    (cl-tmux/commands:if-shell "false"
                               (lambda () nil)
                               :else-fn (lambda () (setf else-called t)))
    (is-true else-called "else-fn must be invoked for a non-zero-exit command")))

(test if-shell-nonzero-exit-no-else-fn-is-noop
  "if-shell with a non-zero exit and no ELSE-FN does not signal an error."
  (finishes (cl-tmux/commands:if-shell "false" (lambda () nil))))

(test if-shell-zero-exit-no-then-fn-is-noop
  "if-shell with a zero exit and NIL THEN-FN does not signal an error."
  (finishes (cl-tmux/commands:if-shell "true" nil)))

(test if-shell-timeout-returns-calls-else-fn
  "if-shell with a very short timeout calls ELSE-FN (timeout treated as non-zero exit)."
  (let ((else-called nil))
    (cl-tmux/commands:if-shell "sleep 60"
                               (lambda () nil)
                               :else-fn (lambda () (setf else-called t))
                               :timeout 1/1000)
    (is-true else-called "else-fn must be invoked when if-shell times out")))

;;; ── %selection-text ──────────────────────────────────────────────────────────
;;;
;;; %selection-text is a private helper in cl-tmux/commands that extracts the
;;; selected text from a copy-mode screen.  It returns NIL when no selection is
;;; active, a string for a single-row selection, and a newline-joined string for
;;; a multi-row selection.

(defun %make-selecting-screen (content mark cursor &key (w 20) (h 5))
  "Return a copy-mode screen pre-filled with CONTENT, with mark and cursor set
   to MARK and CURSOR respectively, and copy-selecting T."
  (let ((s (make-screen w h)))
    (unless (string= content "")
      (feed s content))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) mark
          (cl-tmux/terminal/types:screen-copy-cursor    s) cursor)
    s))

(test selection-text-returns-nil-when-no-selection
  "%selection-text returns NIL when copy-selecting is NIL (no active selection)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (is (null (cl-tmux/commands::%selection-text s))
        "%selection-text must return NIL when no selection is active")))

(test selection-text-returns-nil-when-mark-nil
  "%selection-text returns NIL when copy-selecting is T but mark is NIL."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (is (null (cl-tmux/commands::%selection-text s))
        "%selection-text must return NIL when mark is NIL")))

(test selection-text-single-row-returns-correct-text
  "%selection-text returns the correct string for a single-row selection."
  (let ((s (%make-selecting-screen "hello world"
                                   (cons 0 0)    ; mark: row 0, col 0
                                   (cons 0 5)))) ; cursor: row 0, col 5
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "%selection-text must return a string for a valid selection")
      (is (string= "hello" text)
          "%selection-text must return \"hello\" for cols 0-4 of row 0 (got ~S)" text))))

(test selection-text-multi-row-returns-newline-joined-text
  "%selection-text returns newline-joined text for a multi-row selection."
  ;; Feed two rows: row 0 = "abc", then CR+LF, row 1 = "def".
  (let ((s (make-screen 20 5)))
    (feed s "abc")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "def")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Select from row 0 col 0 to row 1 col 3.
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "result must be a string")
      (is (find #\Newline text) "multi-row result must contain a newline")
      ;; Row 0 contributes cols 0..2 = "abc"; row 1 contributes cols 0..2 = "def".
      (is (string= (format nil "abc~%def") text)
          "%selection-text must be \"abc\\ndef\" for rows 0-1 (got ~S)" text))))

(test selection-text-reversed-mark-cursor-order
  "%selection-text normalises selection when cursor is before mark."
  ;; mark at col 5, cursor at col 0: result should still be cols 0-4.
  (let ((s (%make-selecting-screen "hello world"
                                   (cons 0 5)    ; mark: row 0, col 5
                                   (cons 0 0)))) ; cursor: row 0, col 0
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "%selection-text must return a string even when mark > cursor")
      (is (string= "hello" text)
          "%selection-text must normalise reversed mark/cursor (got ~S)" text))))
