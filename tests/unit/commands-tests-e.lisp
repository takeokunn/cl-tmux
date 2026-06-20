(in-package #:cl-tmux/test)

;;;; copy-mode clear-selection, WORD motion, select-word, move-cursor (src/commands.lisp) — part II

(in-suite commands-suite)

;;; ── copy-mode-clear-selection (send -X clear-selection) ──────────────────────

(test copy-mode-clear-selection-drops-selection-keeps-cursor
  "copy-mode-clear-selection clears the mark + selection flags but keeps the
   cursor and stays in copy mode (tmux clear-selection / default vi Escape)."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting        s) t
          (cl-tmux/terminal/types:screen-copy-mark             s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor           s) (cons 0 5)
          (cl-tmux/terminal/types:screen-copy-rect-select-p    s) t)
    (cl-tmux/commands::copy-mode-clear-selection s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selection flag must be cleared")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be dropped")
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rectangle-select flag must be reset")
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor position must be preserved (stay put in copy mode)")
    (is-true (cl-tmux/terminal/types:screen-copy-mode-p s)
             "must remain in copy mode (clear-selection does not cancel)")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after clearing")))

(test copy-mode-clear-selection-noop-without-selection
  "copy-mode-clear-selection is a clean no-op when there is no selection/mark."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 3)
          (cl-tmux/terminal/types:screen-dirty-p        s) nil)
    (finishes (cl-tmux/commands::copy-mode-clear-selection s))
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor unchanged")
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "no dirty mark when there was nothing to clear")))

(test copy-mode-clear-selection-x-command-mapped
  "The send -X name clear-selection maps to the :copy-mode-clear-selection
   dispatch keyword."
  (is (eq :copy-mode-clear-selection
          (copy-mode-x-command-value "clear-selection"))
      "clear-selection must be a known send -X command")
  (is-false (copy-mode-x-command-value "stop-selection")
            "stop-selection is no longer a supported send -X command")
  ;; copy-selection-and-cancel IS now supported (audit #21) — copies then exits.
  (is (eq :copy-mode-yank
          (copy-mode-x-command-value "copy-selection-and-cancel"))
      "copy-selection-and-cancel copies the selection and exits copy mode")
  (is-false (copy-mode-x-command-value "toggle-position")
            "toggle-position is no longer a supported send -X command")
  (is-false (copy-mode-x-command-value "scroll-mouse")
            "scroll-mouse is no longer a supported send -X command"))

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

(test copy-mode-other-end-preserves-selection-text
  "Swapping the two ends must not change the selected text or normalised bounds —
   this is the defining invariant of other-end."
  (let ((s (copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 4)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
    (let ((text-before (cl-tmux/commands::%selection-text s)))
      (multiple-value-bind (sr0 er0 sc0 ec0) (cl-tmux/commands::%selection-bounds s)
        (cl-tmux/commands::copy-mode-other-end s)
        (let ((text-after (cl-tmux/commands::%selection-text s)))
          (multiple-value-bind (sr1 er1 sc1 ec1) (cl-tmux/commands::%selection-bounds s)
            (is (string= text-before text-after)
                "selected text must be identical after other-end")
            (is (and (= sr0 sr1) (= er0 er1) (= sc0 sc1) (= ec0 ec1))
                "normalised selection bounds must be identical after other-end")))))))

(test copy-mode-other-end-double-swap-restores-original
  "Two successive swaps restore the original cursor and mark."
  (let ((s (copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (cl-tmux/commands::copy-mode-other-end s)
    (cl-tmux/commands::copy-mode-other-end s)
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must return to its original position after two swaps")
    (is (equal (cons 0 2) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must return to its original position after two swaps")))

;;; ── copy-mode-select-word ────────────────────────────────────────────────────

(defmacro with-copy-mode-select-word-screen ((screen &key content w h row col) &body body)
  `(let ((,screen (copy-mode-screen
                   ,@(when w `(:w ,w))
                   ,@(when h `(:h ,h))
                   ,@(when content `(:content ,content)))))
     ,(when (or row col)
        `(setf (cl-tmux/terminal/types:screen-copy-cursor ,screen)
               (cons ,(or row 0) ,(or col 0))))
     ,@body))

(test copy-mode-select-word-selects-word-under-cursor
  "copy-mode-select-word selects exactly the word under the cursor.
   The %selection-text round-trip pins the column off-by-one: for \"bar\" at
   cols 4-6 the mark sits at col 4 and the cursor at col 7 (exclusive end)."
  (with-copy-mode-select-word-screen (s :content "foo bar baz" :row 0 :col 5)
    ;; "foo bar baz": b=4 a=5 r=6 — put the cursor inside "bar" on row 0.
    (cl-tmux/commands::copy-mode-select-word s)
    (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
             "selecting must be T after select-word")
    (is (equal (cons 0 4) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must sit on the first word character (col 4)")
    (is (equal (cons 0 7) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must sit just past the last word character (col 7)")
    (is (string= "bar" (cl-tmux/commands::%selection-text s))
        "%selection-text must extract exactly the word \"bar\"")))

(test copy-mode-select-word-on-separator-selects-single-cell
  "copy-mode-select-word on a separator (space) selects just the single cell."
  (with-copy-mode-select-word-screen (s :content "foo bar baz" :row 0 :col 3)
    ;; Column 3 is the space between "foo" and "bar".
    (finishes (cl-tmux/commands::copy-mode-select-word s))
    (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
             "selecting must be T after select-word on a separator")
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must sit on the single cell under the cursor")
    (is (equal (cons 0 4) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must sit one column past the single cell")))

(test copy-mode-select-word-at-rightmost-column-keeps-last-char
  "A word ending at the rightmost column must NOT lose its final character: the
   cursor's exclusive end is allowed to reach width.  PINS the rightmost off-by-one."
  ;; Width-3 screen, content \"cat\": c=0 a=1 t=2 (t is at the last column).
  (with-copy-mode-select-word-screen (s :w 3 :h 3 :content "cat" :row 0 :col 1)
    (cl-tmux/commands::copy-mode-select-word s)
    (is (equal (cons 0 0) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must sit on the first word character (col 0)")
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor exclusive end must reach width (col 3), not clamp to col 2")
    (is (string= "cat" (cl-tmux/commands::%selection-text s))
        "%selection-text must keep the rightmost-column character: \"cat\"")))

(test copy-mode-select-word-at-start-of-row-clamps-start
  "select-word with the cursor at column 0 leaves the mark at column 0."
  (with-copy-mode-select-word-screen (s :content "foo bar baz" :row 0 :col 0)
    (cl-tmux/commands::copy-mode-select-word s)
    (is (equal (cons 0 0) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must clamp to column 0 at the start of the row")
    (is (string= "foo" (cl-tmux/commands::%selection-text s))
        "%selection-text must extract \"foo\"")))

(test copy-mode-select-word-stops-at-multi-space-gap
  "select-word must not span a multi-space gap between words."
  ;; \"ab   cd\": a=0 b=1 spaces=2,3,4 c=5 d=6.
  (with-copy-mode-select-word-screen (s :content "ab   cd" :row 0 :col 5)
    (cl-tmux/commands::copy-mode-select-word s)
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must stop at the start of \"cd\" (col 5), not cross the gap")
    (is (string= "cd" (cl-tmux/commands::%selection-text s))
        "%selection-text must extract \"cd\" without spanning the space gap")))

(test copy-mode-select-word-sets-dirty-flag
  "select-word marks the screen dirty."
  (with-copy-mode-select-word-screen (s :content "foo bar baz" :row 0 :col 5)
    (setf (cl-tmux/terminal/types:screen-dirty-p s) nil)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "precondition: dirty-p NIL before select-word")
    (cl-tmux/commands::copy-mode-select-word s)
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "dirty-p must be T after select-word")))

(test copy-mode-select-word-no-op-when-not-in-copy-mode
  "select-word is a harmless no-op when copy mode is not active."
  (let ((s (make-screen 20 5)))
    (feed s "foo bar baz")
    ;; Do NOT enter copy mode.
    (finishes (cl-tmux/commands::copy-mode-select-word s))
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selecting must remain NIL when not in copy mode")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL when not in copy mode")))

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
