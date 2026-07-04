(in-package #:cl-tmux/test)

;;;; copy-mode WORD motion and cursor movement (src/commands.lisp) — part II

(in-suite commands-suite)

(test copy-mode-x-line-positions-vs-history-extremes
  "top/middle/bottom-line (vi H/M/L) move within the viewport; history-top/bottom
   (vi g/G) jump to the scrollback extremes — they must map to distinct actions."
  (is (eq :copy-mode-high   (copy-mode-x-command-value "top-line"))
      "top-line → high (viewport top)")
  (is (eq :copy-mode-middle (copy-mode-x-command-value "middle-line"))
      "middle-line → middle (was missing)")
  (is (eq :copy-mode-low    (copy-mode-x-command-value "bottom-line"))
      "bottom-line → low (viewport bottom)")
  (is (eq :copy-mode-top    (copy-mode-x-command-value "history-top"))
      "history-top → scrollback top")
  (is (eq :copy-mode-bottom (copy-mode-x-command-value "history-bottom"))
      "history-bottom → scrollback bottom"))

(test copy-mode-high-middle-low-set-viewport-row
  "copy-mode-high/middle/low move the cursor to viewport row 0 / mid / height-1
   without changing the scroll offset."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 7
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
    (cl-tmux/commands::copy-mode-low s)
    (is (= (1- (screen-height s)) (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "low → last viewport row")
    (cl-tmux/commands::copy-mode-high s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "high → viewport row 0")
    (cl-tmux/commands::copy-mode-middle s)
    (is (= (floor (screen-height s) 2)
           (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "middle → middle viewport row")
    (is (= 7 (cl-tmux/terminal/types:screen-copy-offset s))
        "scroll offset must be unchanged (H/M/L do not scroll)")
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must be preserved")))

;;; ── WORD motion: copy-mode-space-{forward,backward,end} (vi W/B/E) ───────────

(test copy-mode-space-motion-is-whitespace-delimited
  "WORD motion (W/B/E) treats punctuation as part of the WORD — only whitespace
   separates — unlike w/b/e which honour word-separators (here '-')."
  (let ((s (copy-mode-screen :content "foo-bar baz")))
    ;; forward: w stops at 'bar' (col 4, '-' is a separator); W skips to 'baz' (8).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-forward s)
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))) "w → start of 'bar'")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-space-forward s)
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "W → start of 'baz' (foo-bar is one WORD)")
    ;; backward from 'baz' (8): b → 'bar' (4); B → start of 'foo-bar' WORD (0).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-word-backward s)
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))) "b → start of 'bar'")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-space-backward s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "B → start of 'foo-bar' WORD")))

(test copy-mode-space-end-lands-on-word-final-char
  "copy-mode-space-end (vi E) moves to the last char of the current/next WORD."
  (let ((s (copy-mode-screen :content "foo-bar baz")))
    ;; From col 0, E → last char of 'foo-bar' (col 6, the 'r').
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-space-end s)
    (is (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "E → last char of the 'foo-bar' WORD")))

(test copy-mode-x-word-vs-space-mappings
  "send -X next-word/etc. map to word motion; next-space/etc. to WORD motion."
  (is (eq :copy-mode-word-forward  (copy-mode-x-command-value "next-word")))
  (is (eq :copy-mode-space-forward (copy-mode-x-command-value "next-space")))
  (is (eq :copy-mode-space-backward (copy-mode-x-command-value "previous-space")))
  (is (eq :copy-mode-space-end      (copy-mode-x-command-value "next-space-end"))))

;;; ── back-to-indentation (vi ^): first non-blank vs line-start (vi 0) ─────────

(test copy-mode-back-to-indentation-stops-at-first-non-blank
  "copy-mode-back-to-indentation (vi ^) moves to the first non-blank column —
   unlike copy-mode-line-start (vi 0), which always goes to column 0."
  (let ((s (copy-mode-screen :content "   foo")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-back-to-indentation s)
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "^ must land on the first non-blank char (col 3)")
    ;; line-start still goes to column 0 — the two are distinct.
    (cl-tmux/commands::copy-mode-line-start s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "0 (line-start) must go to column 0")))

(test copy-mode-back-to-indentation-blank-line-goes-to-zero
  "On an all-blank row, ^ falls back to column 0."
  (let ((s (copy-mode-screen)))             ; default content is blank
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
    (cl-tmux/commands::copy-mode-back-to-indentation s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "an entirely blank row → column 0")))

(test copy-mode-x-back-to-indentation-mapped
  "send -X back-to-indentation maps to the distinct :copy-mode-back-to-indentation
   action, not line-start."
  (is (eq :copy-mode-back-to-indentation
          (copy-mode-x-command-value "back-to-indentation"))))

;;; ── copy-mode-move-cursor ────────────────────────────────────────────────────

(test copy-mode-move-cursor-direction-table
  "Each direction moves the cursor by 1 step and marks the screen dirty."
  (dolist (c '((:left  2 5  2 4)   ; (dir start-row start-col expected-row expected-col)
               (:right 2 5  2 6)
               (:up    2 5  1 5)
               (:down  2 5  3 5)))
    (destructuring-bind (dir sr sc er ec) c
      (with-copy-mode-cursor (s sr sc)
        (cl-tmux/commands::copy-mode-move-cursor s dir)
        (is (equal (cons er ec) (cl-tmux/terminal/types:screen-copy-cursor s))
            "~A: expected (~D . ~D), got ~S" dir er ec
            (cl-tmux/terminal/types:screen-copy-cursor s))
        (is-true (cl-tmux/terminal/types:screen-dirty-p s)
                 "screen must be dirty after ~A" dir)))))

(test copy-mode-move-cursor-boundary-clamping
  "At each axis boundary, move-cursor clamps rather than wrapping or crashing."
  (dolist (c '((:left  2  0  cdr  0  "col must not go below 0")
               (:up    0  5  car  0  "row must not go below 0")
               (:right 2 19  cdr 19  "col must clamp at width-1=19")
               (:down  4  5  car  4  "row must clamp at height-1=4")))
    (destructuring-bind (dir sr sc accessor expected msg) c
      (with-copy-mode-cursor (s sr sc)
        (cl-tmux/commands::copy-mode-move-cursor s dir)
        (is (= expected (funcall accessor (cl-tmux/terminal/types:screen-copy-cursor s)))
            msg)))))

(test copy-mode-selection-cursor-can-reach-width
  "While selecting, :right may advance the cursor to WIDTH (the exclusive end past
   the last column) so the selection can include the rightmost cell — navigation
   still caps at WIDTH-1 (covered by the test above)."
  (let ((s (make-screen 5 3)))
    (feed s "abcde")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-begin-selection s)
    (dotimes (i 6) (cl-tmux/commands::copy-mode-move-cursor s :right))
    (is (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "selecting cursor reaches width (5), got ~D"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
    (is (string= "abcde" (cl-tmux/commands::%selection-text s))
        "selection includes the rightmost column 'e'")))


(test copy-mode-enter-places-cursor-at-bottom-left
  "copy-mode-enter initialises the cursor at the bottom-left of the viewport (row height-1, col 0)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (is (equal (cons 4 0) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must start at (height-1 . 0) — bottom-left of the viewport")))

(test copy-mode-move-cursor-nil-fallback
  "If copy-cursor is manually reset to NIL, move-cursor falls back to (height-1 . 0) before moving."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Force cursor to NIL to exercise the fallback path inside move-cursor.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (equal (cons 4 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "nil cursor falls back to (height-1 . 0) then moves right to (height-1 . 1)")))

(test copy-mode-move-cursor-sets-mark-anchor-when-selecting-and-mark-nil
  "When copy-selecting is T and mark is NIL, the first move sets the mark anchor."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3)
          (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil)
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is-true (cl-tmux/terminal/types:screen-copy-mark s)
        "mark must be placed when copy-selecting is T and mark was nil")))

(test copy-mode-move-cursor-noop-outside-copy-mode
  "copy-mode-move-cursor does nothing when copy mode is not active."
  (let ((s (make-screen 20 5)))
    ;; do NOT enter copy mode
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-move-cursor s :left)
    (is (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged outside copy mode")))

;;; ── send-keys -X *-and-cancel (window-copy.c parity) ─────────────────────────

(test copy-mode-scroll-down-and-cancel-exits-at-bottom
  "scroll-down-and-cancel scrolls down one line and exits copy mode when the live
   bottom (scroll-offset 0) is reached."
  (let ((s (copy-mode-screen)))
    (seed-scrollback s 5)
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 1)
    (cl-tmux/commands::copy-mode-scroll-down-and-cancel s)
    (is-false (cl-tmux/terminal/types:screen-copy-mode-p s)
              "reaching the live bottom must exit copy mode")))

(test copy-mode-scroll-down-and-cancel-stays-when-scrolled-back
  "scroll-down-and-cancel stays in copy mode while still scrolled back, moving the
   viewport one line newer."
  (let ((s (copy-mode-screen)))
    (seed-scrollback s 5)
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 3)
    (cl-tmux/commands::copy-mode-scroll-down-and-cancel s)
    (is-true (cl-tmux/terminal/types:screen-copy-mode-p s)
             "still scrolled back must stay in copy mode")
    (is (= 2 (cl-tmux/terminal/types:screen-copy-offset s))
        "the viewport moved one line newer")))

(test copy-mode-page-down-and-cancel-exits-at-bottom
  "page-down-and-cancel scrolls a full page down and exits at the live bottom."
  (let ((s (copy-mode-screen)))
    (seed-scrollback s 2)
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 1)
    (cl-tmux/commands::copy-mode-page-down-and-cancel s)
    (is-false (cl-tmux/terminal/types:screen-copy-mode-p s)
              "a full page down reaches the bottom and exits copy mode")))
