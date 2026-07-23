(in-package #:cl-tmux/test)

;;;; Window-level tests: pure split math and axis helpers.

(describe "model-suite"

  ;;; ── %grow-first-p direct tests (pure, no PTY) ────────────────────────────────

  ;; %grow-first-p returns T when the given SIDE should grow for DIRECTION.
  ;; :first grows on :right/:down; :second grows on :left/:up.
  ;; Each row: (side direction expected description).
  (it "grow-first-p-table"
    (dolist (row '((:first  :right t   ":first grows on :right")
                   (:first  :down  t   ":first grows on :down")
                   (:first  :left  nil ":first does not grow on :left")
                   (:first  :up    nil ":first does not grow on :up")
                   (:second :left  t   ":second grows on :left")
                   (:second :up    t   ":second grows on :up")
                   (:second :right nil ":second does not grow on :right")
                   (:second :down  nil ":second does not grow on :down")))
      (destructuring-bind (side direction expected desc) row
        (declare (ignore desc))
        (if expected
            (expect (cl-tmux/model::%grow-first-p side direction) :to-be-truthy)
            (expect (cl-tmux/model::%grow-first-p side direction) :to-be-falsy)))))

  ;;; ── split-child-geometry direct tests (pure, no PTY) ─────────────────────

  ;; split-child-geometry returns the correct child position and size for :h and :v.
  ;; For :h the child is the right half; for :v it is the bottom half.
  ;; Each row: (orient pane-w pane-h expected-x expected-y expected-w expected-h).
  (it "split-child-geometry-table"
    (dolist (row '((:h 41 20 21 0  20 20)
                   (:v 80 25 0  13 80 12)))
      (destructuring-bind (orient w h ex ey ew eh) row
        (let ((p (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :pid -1
                            :screen (make-screen w h))))
          (multiple-value-bind (px py pw ph)
              (cl-tmux/model::split-child-geometry p orient)
            (expect (eql ex px))
            (expect (eql ey py))
            (expect (eql ew pw))
            (expect (eql eh ph)))))))

  ;;; ── %new-split-ratio
  ;;; ── %new-split-ratio direct tests (pure, no PTY) ─────────────────────────

  ;; %new-split-ratio: positive delta grows, clamped case → NIL, negative delta shrinks.
  (it "new-split-ratio-table"
    (dolist (row '((:h 80 1/2  5 t  45/80 "grow: cur=40, +5 → 45/80")
                   (:h 10 1/2 10 t  nil   "blocked: new=15 > max=8 → NIL")
                   (:v 20 1/2  3 nil 7/20  "shrink: cur=10, -3 → 7/20")))
      (destructuring-bind (orient avail ratio delta grow-first expected desc) row
        (declare (ignore desc))
        (expect (equal expected
                   (cl-tmux/model::%new-split-ratio orient avail ratio delta grow-first))))))

  ;;; ── %requested-cells-from-hint direct tests (pure, no PTY) ───────────────────

  ;; %requested-cells-from-hint converts a size HINT to a cell count within AVAIL.
  ;; Integer hints > 0 pass through unchanged; non-positive integers fall back to
  ;; half of AVAIL.  Real hints in (0,1) scale AVAIL; reals outside that range
  ;; also fall back to half of AVAIL.
  ;; Each row: (hint avail orient expected description).
  (it "requested-cells-from-hint-table"
    (dolist (row '((20   80 :h 20 "positive integer hint passes through unchanged")
                   (0    80 :h 40 "zero integer hint falls back to half of avail")
                   (-5   80 :h 40 "negative integer hint falls back to half of avail")
                   (0.25 80 :h 20 "real hint in (0,1) scales avail proportionally")
                   (0.3  80 :h 24 "real hint 0.3 scales and rounds to nearest cell")
                   (1.0  80 :h 40 "real hint >= 1.0 falls back to half of avail")
                   (0.0  80 :h 40 "real hint <= 0.0 falls back to half of avail")
                   (nil  80 :h 40 "non-numeric hint falls back to half of avail")))
      (destructuring-bind (hint avail orient expected desc) row
        (declare (ignore desc))
        (expect (eql expected
                 (cl-tmux/model::%requested-cells-from-hint hint avail orient))))))

  ;;; ── %ratio-from-size-hint direct tests (pure, no PTY) ─────────────────────────

  ;; %ratio-from-size-hint clamps the requested cell count so both the new pane
  ;; and its sibling keep at least the axis floor (+pane-min-width+ for :h).
  (it "ratio-from-size-hint-clamps-to-axis-floor"
    ;; avail=10, :h axis-floor=2; requesting 1 cell must clamp up to 2/10.
    (expect (= 1/5 (cl-tmux/model::%ratio-from-size-hint 1 10 :h)))
    ;; avail=10, :h axis-floor=2; requesting 9 cells must clamp down to leave
    ;; axis-floor=2 for the first child, i.e. (10-2)/10 = 8/10.
    (expect (= 4/5 (cl-tmux/model::%ratio-from-size-hint 9 10 :h))))

  ;; %ratio-from-size-hint returns the exact ratio for a hint safely within bounds.
  (it "ratio-from-size-hint-mid-range-passes-through"
    (expect (= 1/4 (cl-tmux/model::%ratio-from-size-hint 20 80 :h))))

  ;;; ── Private helper tests ────────────────────────────────────────────────────

  ;; %split-fits-p returns T when the pane axis meets the minimum, NIL otherwise.
  ;; :h needs width >= 5 (2*2+1); :v needs height >= 3 (2*1+1).
  ;; Each row: (orient width height expected description).
  (it "split-fits-p-table"
    (dolist (row '((:h 5  3  t   "h exactly-minimum width of 5 → fits")
                   (:v 5  3  t   "v exactly-minimum height of 3 → fits")
                   (:h 4  5  nil "h width 4 < 5 → does not fit")
                   (:v 5  2  nil "v height 2 < 3 → does not fit")))
      (destructuring-bind (orient w h expected desc) row
        (declare (ignore desc))
        (let ((p (make-pane :id 1 :fd -1 :pid -1 :width w :height h
                            :screen (make-screen w h))))
          (if expected
              (expect (cl-tmux/model::%split-fits-p p orient) :to-be-truthy)
              (expect (cl-tmux/model::%split-fits-p p orient) :to-be-falsy))))))

  ;; window-split :full refuses root splits that cannot leave both panes at min size.
  (it "window-split-full-obeys-axis-minimums"
    (with-session (session 24 80)
      (dolist (row '((:h 4 24 "full h-split needs at least 5 columns")
                     (:v 80 2 "full v-split needs at least 3 rows")))
        (destructuring-bind (direction width height desc) row
          (declare (ignore desc))
          (let* ((p0   (make-no-pty-pane 1 0 0 width height))
                 (leaf (make-layout-leaf p0))
                 (win  (make-window :id 1 :name "w" :width width :height height
                                    :panes (list p0)
                                    :tree leaf)))
            (window-select-pane win p0)
            (expect (null (window-split session win direction :full t)))
            (expect (eq leaf (window-tree win)))
            (expect (equal (list p0) (window-panes win)))
            (expect (eq p0 (window-active-pane win)))))))))
