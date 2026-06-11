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

(test rename-window-empty-string-is-noop
  "rename-window with an empty name leaves the window name unchanged."
  (let ((win (make-window :id 1 :name "original" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win "")
    (is (string= "original" (window-name win))
        "empty-string rename must not change the window name")))

(test rename-window-nil-name-is-noop
  "rename-window with a NIL name leaves the window name unchanged."
  (let ((win (make-window :id 1 :name "keep" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win nil)
    (is (string= "keep" (window-name win))
        "nil rename must not change the window name")))

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

;;; ── swap-pane ────────────────────────────────────────────────────────────────

(test swap-pane-right-cycles-panes-forward
  "swap-pane :right on a two-pane window moves the active pane to index 1 and
   swaps the positions (pane-x) of the two panes."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (x0-before (pane-x p0))
         (x1-before (pane-x p1)))
    ;; p0 is active (index 0); swap :right -> p0 moves to index 1
    (let ((result (swap-pane win :right)))
      (is (eq p0 result)
          "swap-pane must return the active pane")
      (is (eq p1 (first  (window-panes win)))
          "after :right swap, the former neighbour occupies index 0")
      (is (eq p0 (second (window-panes win)))
          "after :right swap, the active pane occupies index 1")
      ;; Geometry must be exchanged
      (is (= x1-before (pane-x p0))
          "active pane x must equal former neighbour's x after swap")
      (is (= x0-before (pane-x p1))
          "former neighbour x must equal active pane's former x after swap"))))

(test swap-pane-left-cycles-panes-backward
  "swap-pane :left wraps the active pane modularly backward in the panes list."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; p0 is active at index 0; :left -> mod(-1, 2) = 1 -> swaps with p1
    (swap-pane win :left)
    (is (eq p1 (first  (window-panes win)))
        "after :left wrap from index 0, neighbour at (mod -1 n) occupies index 0")
    (is (eq p0 (second (window-panes win)))
        "active pane wraps to index 1 on :left from index 0")))

(test swap-pane-right-from-last-wraps-to-first
  "swap-pane :right from the last-index pane wraps modularly to index 0."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; Make p1 (index 1, last) the active pane, then swap :right
    (window-select-pane win p1)
    (swap-pane win :right)
    ;; mod(1+1, 2) = 0 => p1 and p0 swap
    (is (eq p0 (second (window-panes win)))
        "p0 moves to index 1 after :right wrap from index 1")
    (is (eq p1 (first  (window-panes win)))
        "p1 wraps to index 0 after :right from the last slot")))

(test swap-pane-single-pane-returns-nil
  "swap-pane on a window with exactly one pane returns NIL (no neighbour to swap)."
  (let* ((p0  (%make-test-pane))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0)
                           :panes (list p0))))
    (window-select-pane win p0)
    (is (null (swap-pane win :right))
        "swap-pane on a single-pane window must return NIL")))

(test swap-pane-geometry-exchanged
  "The x/y/width/height of both panes are exchanged by swap-pane."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (p0-x (pane-x p0)) (p0-y (pane-y p0))
         (p0-w (pane-width p0)) (p0-h (pane-height p0))
         (p1-x (pane-x p1)) (p1-y (pane-y p1))
         (p1-w (pane-width p1)) (p1-h (pane-height p1)))
    (swap-pane win :right)
    (is (= p1-x (pane-x p0)) "active pane x must be former neighbour x")
    (is (= p1-y (pane-y p0)) "active pane y must be former neighbour y")
    (is (= p1-w (pane-width  p0)) "active pane width must be former neighbour width")
    (is (= p1-h (pane-height p0)) "active pane height must be former neighbour height")
    (is (= p0-x (pane-x p1)) "former neighbour x must be original active pane x")
    (is (= p0-y (pane-y p1)) "former neighbour y must be original active pane y")
    (is (= p0-w (pane-width  p1)) "former neighbour width must be original active width")
    (is (= p0-h (pane-height p1)) "former neighbour height must be original active height")))

;;; ── capture-pane ─────────────────────────────────────────────────────────────

(defun %make-pane-with-content (content &key (w 20) (h 5))
  "Build a no-PTY pane whose screen has been fed CONTENT."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h
                            :fd -1 :pid -1 :screen screen)))
    (unless (string= content "")
      (feed screen content))
    pane))

(test capture-pane-returns-string
  "capture-pane always returns a string (even on an empty pane)."
  (let* ((pane   (%make-test-pane))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane must return a string")))

(test capture-pane-visible-content-contains-fed-text
  "capture-pane returns the visible screen content including text fed to the pane."
  (let* ((pane   (%make-pane-with-content "ABC"))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane result must be a string")
    (is-true (search "ABC" result)
        "capture-pane output must contain the fed text \"ABC\" (got ~S)" result)))

(test capture-color-sgr-encodes-cell-colours
  "%capture-color-sgr maps a cell colour value to its SGR fragment."
  (is (string= "31"  (cl-tmux/commands::%capture-color-sgr 1 nil))  "fg standard")
  (is (string= "41"  (cl-tmux/commands::%capture-color-sgr 1 t))    "bg standard")
  (is (string= "94"  (cl-tmux/commands::%capture-color-sgr 12 nil)) "fg bright")
  (is (string= "104" (cl-tmux/commands::%capture-color-sgr 12 t))   "bg bright")
  (is (string= "38;5;200" (cl-tmux/commands::%capture-color-sgr 200 nil)) "fg 256")
  (is (string= "48;5;200" (cl-tmux/commands::%capture-color-sgr 200 t))   "bg 256")
  (is (string= "38;2;255;128;0"
               (cl-tmux/commands::%capture-color-sgr (logior #x1000000 #xff8000) nil))
      "fg true-colour"))

(test capture-cell-sgr-includes-attrs-and-colours
  "%capture-cell-sgr emits reset + attrs + fg + bg."
  (is (string= (format nil "~C[0;31;40m" #\Escape)
               (cl-tmux/commands::%capture-cell-sgr 1 0 0))
      "fg red, bg black, no attrs")
  (is (string= (format nil "~C[0;1;31;40m" #\Escape)
               (cl-tmux/commands::%capture-cell-sgr 1 0 1))
      "bold (attr bit 0) adds SGR 1"))

(test capture-pane-escapes-preserves-colour
  "capture-pane :escapes t keeps SGR colour sequences; plain capture does not."
  (let* ((screen (make-screen 10 2))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 2
                            :fd -1 :pid -1 :screen screen)))
    (feed screen (esc "[31m"))     ; foreground red
    (feed screen "hi")
    (let ((plain   (capture-pane pane))
          (colored (capture-pane pane :escapes t)))
      (is (search "hi" plain) "plain capture contains the text")
      (is (not (find (code-char 27) plain)) "plain capture has no escape bytes")
      (is (search "hi" colored) "colour capture contains the text")
      (is (search "31" colored) "colour capture includes the fg=red SGR (31)")
      (is (search (format nil "~C[0m" #\Escape) colored)
          "colour capture ends each row with a reset"))))

(test capture-pane-visible-only-excludes-scrollback
  "capture-pane without :include-scrollback only dumps visible rows, not scrollback."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen))
         (sb-row (make-array 20 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
    (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
    (feed screen "visible")
    (let ((result (capture-pane pane)))
      (is-true (search "visible" result)
          "visible content must appear in capture-pane output")
      (is (null (search "XXXXXXXXXXXXXXXXX" result))
          "scrollback content must NOT appear when include-scrollback is nil"))))

(test capture-pane-with-scrollback-prepends-history
  "capture-pane with :include-scrollback T prepends scrollback rows before visible rows."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen))
         (sb-row (make-array 20 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\Q :fg 7 :bg 0 :attrs 0 :width 1))))
    (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
    (feed screen "visible")
    (let ((result (capture-pane pane :include-scrollback t)))
      (is-true (search "QQ" result)
          "scrollback content must appear when include-scrollback is T")
      (is-true (search "visible" result)
          "visible content must also appear when include-scrollback is T")
      ;; Scrollback should come before visible content in the output
      (let ((q-pos       (search "QQ"      result))
            (visible-pos (search "visible" result)))
        (is (< q-pos visible-pos)
            "scrollback rows must precede visible rows in the output")))))

(test capture-pane-height-rows-newlines
  "capture-pane emits exactly (screen-height) newline-terminated rows."
  (let* ((pane   (%make-pane-with-content "" :w 10 :h 3))
         (result (capture-pane pane))
         (lines  (count #\Newline result)))
    (is (= 3 lines)
        "capture-pane must emit exactly height (~D) newline characters (got ~D)"
        3 lines)))

(test capture-pane-default-trims-trailing-spaces
  "capture-pane's default (no -J) strips trailing whitespace from each line —
   tmux's default behaviour.  A 'hi' on a 10-wide row captures as just \"hi\"."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi~%") (capture-pane pane))
        "default capture trims the 8 trailing spaces")))

(test capture-pane-J-preserves-trailing-spaces
  "capture-pane -J (:join t) PRESERVES trailing spaces — the row keeps full width."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi        ~%") (capture-pane pane :join t))
        "join capture keeps the row padded to its full width of 10")))

(test capture-pane-N-preserves-trailing-spaces
  "capture-pane -N (:preserve-trailing t) keeps trailing spaces like -J — the row
   stays padded to its full width."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi        ~%") (capture-pane pane :preserve-trailing t))
        "-N keeps the row padded to its full width of 10")))

(test capture-pane-N-preserves-trailing-but-does-not-join
  "capture-pane -N preserves trailing spaces but, unlike -J, does NOT rejoin a
   wrapped line — the distinguishing behaviour between -N and -J."
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "ABCDEFGH")            ; wraps: row0 "ABCDE" → row1 "FGH"
    (let ((preserved (capture-pane pane :preserve-trailing t)))
      (is-true (search "FGH  " preserved)
               "-N keeps the FGH continuation row padded to full width (got ~S)"
               preserved)
      (is (null (search "ABCDEFGH" preserved))
          "-N must NOT join the wrapped line into one logical line (got ~S)"
          preserved))))

(test capture-pane-J-joins-wrapped-lines
  "capture-pane -J rejoins a line that wrapped at the right margin into one
   logical line (no newline at the wrap boundary); default capture keeps them
   on separate lines."
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "ABCDEFGH")          ; wraps: row0 "ABCDE" → row1 "FGH"
    (let ((joined  (capture-pane pane :join t))
          (default (capture-pane pane)))
      (is-true (search "ABCDEFGH" joined)
          "with -J the wrapped line is one logical line ABCDEFGH (got ~S)" joined)
      (is (null (search "ABCDEFGH" default))
          "without -J the wrapped halves stay on separate lines (got ~S)" default))))

(test capture-pane-J-keeps-unwrapped-lines-separate
  "capture-pane -J does NOT join lines that did not wrap (a hard CR+LF break)."
  (let* ((screen (make-screen 10 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen (format nil "foo~C~Cbar" #\Return #\Linefeed))
    (let ((joined (capture-pane pane :join t)))
      (is-true (search "foo" joined) "foo present")
      (is-true (search "bar" joined) "bar present")
      (is (null (search "foobar" joined))
          "foo and bar did not wrap — they stay separate, not joined (got ~S)" joined))))

(test shift-line-wrapped-up-moves-flags
  "%shift-line-wrapped-up (scroll-up of the wrap flags): a flag at row Y in the
   region moves to Y-1, mirroring the content shift."
  (let ((s (make-screen 5 4)))
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)        ; row 2 wraps
    (cl-tmux/terminal/types:%shift-line-wrapped-up s 0 3)  ; scroll region rows 0..3
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 1)
              "the row-2 wrap flag moved up to row 1")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 2)
              "row 2 no longer carries the flag")))

(test line-wrapped-flag-cleared-on-erase
  "Erasing a row clears its wrap flag (erase-region), so a rewritten short line
   does not over-join under -J."
  (let ((s (make-screen 5 3)))
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 marked wrapped")
    (cl-tmux/terminal/actions:erase-region s 0 0 4 0)      ; erase row 0
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0)
              "erasing row 0 clears its wrap flag")))

(test capture-pane-blank-row-trims-to-empty-line
  "A fully blank row trims to an empty captured line (just the newline) by default,
   but stays full-width under -J."
  (let* ((screen (make-screen 5 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (is (string= (format nil "~%")      (capture-pane pane))
        "blank row trims to an empty line")
    (is (string= (format nil "     ~%") (capture-pane pane :join t))
        "blank row stays 5 spaces wide under -J")))

(test capture-pane-escapes-trims-trailing-by-default
  "capture-pane -e also drops trailing blank cells by default — no trailing-space
   run survives, and no stray reset is emitted for the trimmed region."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (let ((result (capture-pane pane :escapes t)))
      (is (null (find #\Space result))
          "escaped default capture has no trailing spaces (got ~S)" result)
      (is-true (search "hi" result) "still contains the fed text")
      (is-true (search (format nil "~C[0m" #\Escape) result)
          "still ends the row with an SGR reset"))))
