(in-package #:cl-tmux/test)

;;;; Model-level tests: session / window / pane lifecycle.
;;;;
;;;; Tests that create real PTYs (via create-initial-session / window-split)
;;;; skip themselves when PTY allocation is unavailable — the same guard used
;;;; in pty-tests.lisp — so the suite runs cleanly in sandboxed Nix builds.

(def-suite model-suite :description "Session / window / pane model")
(in-suite model-suite)

;;; ── Helpers ────────────────────────────────────────────────────────────────

(defmacro with-session ((var rows cols) &body body)
  "Bind VAR to a fresh session of ROWS x COLS, run BODY, then close all PTYs."
  `(let ((,var (create-initial-session ,rows ,cols)))
     (unwind-protect
          (progn ,@body)
       (dolist (p (all-panes ,var))
         (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

;;; ── divide-window count (pure, no PTY) ────────────────────────────────────

(test divide-window-result-count
  "divide-window always returns exactly N slots."
  (dolist (n '(1 2 3 4))
    (is (= n (length (divide-window :vertical   n 24 80)))
        ":vertical n=~A returned wrong count" n)
    (is (= n (length (divide-window :horizontal n 24 80)))
        ":horizontal n=~A returned wrong count" n)))

;;; ── Session bootstrap ──────────────────────────────────────────────────────

(test initial-session
  "create-initial-session produces 1 window containing 1 full-width pane."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    ;; Exactly one window.
    (is (= 1 (length (session-windows session))))
    (let* ((win  (session-active-window session))
           (panes (window-panes win)))
      ;; Exactly one pane.
      (is (= 1 (length panes)))
      (let ((pane (first panes)))
        ;; Pane geometry: full width; height shrunk by *status-height* (= 1).
        (is (= 80 (pane-width  pane)) "initial pane width must equal cols")
        (is (= 23 (pane-height pane))
            "initial pane height must equal rows - *status-height* (23)")
        ;; window-active-pane must return the same pane.
        (is (eq pane (window-active-pane win))
            "window-active-pane must return the sole pane")))))

;;; ── Adding a second window ─────────────────────────────────────────────────

(test session-new-window
  "session-new-window appends a window and switches the active window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (session-new-window session "2" 23 80)
      ;; Two windows now.
      (is (= 2 (length (session-windows session))))
      ;; Active window switched to the new one.
      (let ((new-win (session-active-window session)))
        (is (not (eq first-win new-win))
            "active window must have changed after session-new-window")
        ;; New window starts with exactly one pane.
        (is (= 1 (length (window-panes new-win)))
            "new window must have exactly one pane")))))

;;; ── Selecting a window by reference ───────────────────────────────────────

(test session-select-window
  "session-select-window switches the active window back to an earlier one."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (session-new-window session "2" 23 80)
      ;; Sanity: active is now the second window.
      (is (not (eq first-win (session-active-window session))))
      ;; Select the first window back.
      (session-select-window session first-win)
      (is (eq first-win (session-active-window session))
          "session-active-window must return the window passed to session-select-window"))))

;;; ── session-active-pane ────────────────────────────────────────────────────

(test session-active-pane
  "session-active-pane returns the active pane of the active window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win  (session-active-window session))
           (pane (window-active-pane win)))
      (is (eq pane (session-active-pane session))
          "session-active-pane must match window-active-pane of the active window"))))

;;; ── Splitting and selecting panes ─────────────────────────────────────────

(test window-select-pane
  "After a split the first pane can be re-selected as active."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win        (session-active-window session))
           (first-pane (window-active-pane win)))
      ;; Split vertically → two panes; active switches to the new one.
      (window-split win :vertical)
      (is (= 2 (length (window-panes win))) "must have 2 panes after split")
      (is (not (eq first-pane (window-active-pane win)))
          "active pane must be the new (second) pane after split")
      ;; Select the first pane back.
      (window-select-pane win first-pane)
      (is (eq first-pane (window-active-pane win))
          "window-active-pane must return the pane passed to window-select-pane"))))

;;; ── Resizing a pane ─────────────────────────────────────────────────────────

(test resize-pane-vertical
  "resize-pane :right grows the active pane and shrinks its right neighbour."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split win :vertical)        ; p0 | p1, active becomes p1
      (window-select-pane win p0)         ; make the left pane active
      (let* ((p1        (second (window-panes win)))
             (w0-before (pane-width p0))
             (w1-before (pane-width p1)))
        (resize-pane win :right 5)
        (is (= (+ w0-before 5) (pane-width p0))
            "active (left) pane should grow by 5: ~D → ~D"
            w0-before (pane-width p0))
        (is (= (- w1-before 5) (pane-width p1))
            "right neighbour should shrink by 5: ~D → ~D"
            w1-before (pane-width p1))))))

(test resize-pane-horizontal
  "resize-pane :down grows the active pane and shrinks the pane below it."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split win :horizontal)      ; p0 / p1 stacked, active becomes p1
      (window-select-pane win p0)         ; make the top pane active
      (let* ((p1        (second (window-panes win)))
             (h0-before (pane-height p0))
             (h1-before (pane-height p1)))
        (resize-pane win :down 3)
        (is (= (+ h0-before 3) (pane-height p0))
            "active (top) pane should grow by 3: ~D → ~D"
            h0-before (pane-height p0))
        (is (= (- h1-before 3) (pane-height p1))
            "lower neighbour should shrink by 3: ~D → ~D"
            h1-before (pane-height p1))))))

(test resize-pane-wrong-axis-is-noop
  "A :up/:down resize on a vertical split leaves pane widths unchanged."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split win :vertical)
      (window-select-pane win p0)
      (let ((w0-before (pane-width p0)))
        (resize-pane win :up 5)            ; wrong axis for a vertical split
        (is (= w0-before (pane-width p0))
            "vertical split must ignore an :up resize")))))

;;;; ════════════════════════════════════════════════════════════════════════
;;;; NO-PTY relayout tests.
;;;;
;;;; These build panes by hand with :fd -1 :pid -1 (no real PTY) and a directly
;;;; constructed virtual screen.  pane-reposition / window-relayout call
;;;; set-pty-size on fd -1, which is a tolerated EBADF no-op (ioctl returns -1
;;;; without signalling a Lisp condition), so the only observable effect is the
;;;; pane geometry update and the screen-resize.  They therefore run real
;;;; assertions in the sandbox without gating on pty-available-p.
;;;; ════════════════════════════════════════════════════════════════════════

(defun make-no-pty-pane (id x y w h)
  "Build a pane with no real PTY and a matching virtual screen."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1
             :screen (make-screen w h)))

;;; ── window-relayout preserves a vertical split (no PTY) ────────────────────

(test window-relayout-vertical-preserves-split-no-pty
  "window-relayout reflows a vertical 2-pane window into the new geometry,
   updating both pane rectangles and the underlying screen dimensions."
  ;; Start from an initial 24x80 vertical split built with divide-window so the
  ;; starting geometry is exactly what the model would produce.
  (let* ((start-rows 24) (start-cols 80)
         (slots0 (divide-window :vertical 2 start-rows start-cols))
         (p0 (apply #'make-no-pty-pane 1 (first slots0)))
         (p1 (apply #'make-no-pty-pane 2 (second slots0)))
         (win (make-window :id 1 :name "w" :layout :vertical
                           :width start-cols :height start-rows
                           :panes (list p0 p1) :active p1)))
    ;; Relayout into a new size.
    (let* ((new-rows 40) (new-cols 100)
           (expected (divide-window :vertical 2 new-rows new-cols)))
      (window-relayout win new-rows new-cols)
      ;; Window stored size updated.
      (is (= new-cols (window-width  win)))
      (is (= new-rows (window-height win)))
      ;; Each pane matches the corresponding divide-window slot, including the
      ;; +1 separator gap reflected in pane 1's x offset.
      (loop for pane in (list p0 p1)
            for slot in expected
            do (destructuring-bind (ex ey ew eh) slot
                 (is (= ex (pane-x      pane)) "pane ~D x"      (pane-id pane))
                 (is (= ey (pane-y      pane)) "pane ~D y"      (pane-id pane))
                 (is (= ew (pane-width  pane)) "pane ~D width"  (pane-id pane))
                 (is (= eh (pane-height pane)) "pane ~D height" (pane-id pane))
                 ;; Screen resized to match the pane's new width/height.
                 (is (= ew (screen-width  (pane-screen pane)))
                     "pane ~D screen-width"  (pane-id pane))
                 (is (= eh (screen-height (pane-screen pane)))
                     "pane ~D screen-height" (pane-id pane))))
      ;; Sanity: separator gap really is one column between the two panes.
      (is (= 1 (- (pane-x p1) (+ (pane-x p0) (pane-width p0))))
          "exactly one separator column between vertical panes"))))

;;; ── window-relayout with NIL layout ────────────────────────────────────────

(test window-relayout-nil-layout-single-fullscreen-slot
  "With a NIL layout the sole pane is given the full (0 0 cols rows) rectangle."
  (let* ((pane (make-no-pty-pane 1 5 5 10 10))
         (win  (make-window :id 1 :name "w" :layout nil
                            :width 10 :height 10
                            :panes (list pane) :active pane)))
    (window-relayout win 30 90)
    (is (null (window-layout win)) "layout stays nil")
    (is (= 0  (pane-x      pane)))
    (is (= 0  (pane-y      pane)))
    (is (= 90 (pane-width  pane)))
    (is (= 30 (pane-height pane)))
    (is (= 90 (screen-width  (pane-screen pane))))
    (is (= 30 (screen-height (pane-screen pane))))))

(test window-relayout-nil-layout-multi-pane
  "With a NIL layout and multiple panes the relayout slot list has exactly one
   element, so only the FIRST pane is repositioned; later panes keep their
   stale geometry (the loop terminates at the shorter list)."
  (let* ((p0 (make-no-pty-pane 1 5 5 10 10))
         (p1 (make-no-pty-pane 2 7 7 12 12))
         (win (make-window :id 1 :name "w" :layout nil
                           :width 10 :height 10
                           :panes (list p0 p1) :active p0)))
    (window-relayout win 30 90)
    ;; First pane gets the single full-screen slot.
    (is (= 0  (pane-x      p0)))
    (is (= 0  (pane-y      p0)))
    (is (= 90 (pane-width  p0)))
    (is (= 30 (pane-height p0)))
    (is (= 90 (screen-width  (pane-screen p0))))
    (is (= 30 (screen-height (pane-screen p0))))
    ;; Second pane is untouched: original geometry and screen size preserved.
    (is (= 7  (pane-x      p1)) "stale pane x preserved")
    (is (= 7  (pane-y      p1)) "stale pane y preserved")
    (is (= 12 (pane-width  p1)) "stale pane width preserved")
    (is (= 12 (pane-height p1)) "stale pane height preserved")
    (is (= 12 (screen-width  (pane-screen p1))) "stale screen-width preserved")
    (is (= 12 (screen-height (pane-screen p1))) "stale screen-height preserved")))

;;; ── ensure-window-fits ───────────────────────────────────────────────────────

(test ensure-window-fits-relayouts-on-size-change
  "ensure-window-fits relayouts when the requested size differs from the
   window's stored size, fixing a pane left with stale geometry."
  (let* ((pane (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :layout :vertical
                            :width 80 :height 24
                            :panes (list pane) :active pane)))
    ;; Deliberately leave the window's stored size inconsistent with the
    ;; requested target so ensure-window-fits must act.
    (cl-tmux/model::ensure-window-fits win 30 100)
    (is (= 100 (window-width  win)))
    (is (= 30  (window-height win)))
    ;; Single vertical pane spans the whole window.
    (is (= 100 (pane-width  pane)))
    (is (= 30  (pane-height pane)))
    (is (= 100 (screen-width  (pane-screen pane))))
    (is (= 30  (screen-height (pane-screen pane))))))

(test ensure-window-fits-noop-when-size-matches
  "ensure-window-fits is a no-op when the window already matches the requested
   size: geometry is left untouched and the same screen object is retained
   (no screen-resize, so EQ holds)."
  (let* ((screen (make-screen 80 24))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 80 :height 24
                            :fd -1 :pid -1 :screen screen))
         (win    (make-window :id 1 :name "w" :layout :vertical
                              :width 80 :height 24
                              :panes (list pane) :active pane)))
    ;; Same size as stored → nothing should happen.
    (cl-tmux/model::ensure-window-fits win 24 80)
    (is (= 80 (window-width  win)))
    (is (= 24 (window-height win)))
    (is (= 0  (pane-x      pane)))
    (is (= 0  (pane-y      pane)))
    (is (= 80 (pane-width  pane)))
    (is (= 24 (pane-height pane)))
    ;; The exact screen object must be preserved (relayout was skipped).
    (is (eq screen (pane-screen pane))
        "pane screen object must be unchanged when size already matches")
    (is (= 80 (screen-width  (pane-screen pane))))
    (is (= 24 (screen-height (pane-screen pane))))))
