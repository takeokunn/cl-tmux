(in-package #:cl-tmux/test)

;;;; Screen tests (src/terminal/screen.lisp).
;;;; Tests: resize suite (screen-resize behaviour).

;;; ── SUITE: resize ───────────────────────────────────────────────────────────

(def-suite resize
  :description "Screen resize behaviour"
  :in terminal-suite)
(in-suite resize)

(test resize-larger
  "Resizing to a larger screen preserves existing content and updates dimensions."
  (with-screen (s 10 5)
    (feed s "hello")
    (screen-resize s 20 8)
    (is (= 20 (screen-width  s)))
    (is (= 8  (screen-height s)))
    (is (string= "hello" (row-string s 0 :end 5)))))

(test resize-smaller-clamps-cursor
  "Shrinking the screen clamps an out-of-bounds cursor into the new bounds."
  (with-screen (s 20 10)
    (feed s (esc "[10;20H"))  ; cursor near bottom-right
    (screen-resize s 5 3)
    (is (<= (screen-cursor-x s) 4)
        "cursor-x ~D exceeds new width-1=4" (screen-cursor-x s))
    (is (<= (screen-cursor-y s) 2)
        "cursor-y ~D exceeds new height-1=2" (screen-cursor-y s))))

(test resize-noop
  "Resizing to the same dimensions leaves content and cursor unchanged."
  (with-screen (s 10 5)
    (feed s "abc")
    (let ((cx (screen-cursor-x s))
          (cy (screen-cursor-y s)))
      (screen-resize s 10 5)
      (is (string= "abc" (row-string s 0 :end 3)))
      (is (= cx (screen-cursor-x s)))
      (is (= cy (screen-cursor-y s))))))

;;; ── screen-clear-dirty ───────────────────────────────────────────────────────

(test screen-clear-dirty-resets-flag
  "screen-clear-dirty sets screen-dirty-p to NIL."
  (with-screen (s 10 5)
    ;; A freshly created screen starts dirty.
    (is-true (cl-tmux/terminal/types:screen-dirty-p s) "new screen is dirty")
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must be NIL after screen-clear-dirty")))

;;; ── bell-pending slot ────────────────────────────────────────────────────────

(def-suite bell-pending-suite
  :description "screen-bell-pending slot: default value, set/clear"
  :in terminal-suite)
(in-suite bell-pending-suite)

(test bell-pending-default-is-nil
  "A fresh screen has bell-pending NIL."
  (with-screen (s 10 5)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL on a fresh screen")))

(test bell-pending-can-be-set-and-cleared
  "screen-bell-pending can be toggled via setf."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-bell-pending s) t)
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after setf t")
    (setf (cl-tmux/terminal/types:screen-bell-pending s) nil)
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL after setf nil")))
