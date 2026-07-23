(in-package #:cl-tmux/test)

;;;; copy-mode WORD motion and cursor movement (src/commands.lisp) — part II

(describe "commands-suite"

  ;; top/middle/bottom-line (vi H/M/L) move within the viewport; history-top/bottom
  ;; (vi g/G) jump to the scrollback extremes — they must map to distinct actions.
  (it "copy-mode-x-line-positions-vs-history-extremes"
    (expect (eq :copy-mode-high   (copy-mode-x-command-value "top-line")))
    (expect (eq :copy-mode-middle (copy-mode-x-command-value "middle-line")))
    (expect (eq :copy-mode-low    (copy-mode-x-command-value "bottom-line")))
    (expect (eq :copy-mode-top    (copy-mode-x-command-value "history-top")))
    (expect (eq :copy-mode-bottom (copy-mode-x-command-value "history-bottom"))))

  ;; copy-mode-high/middle/low move the cursor to viewport row 0 / mid / height-1
  ;; without changing the scroll offset.
  (it "copy-mode-high-middle-low-set-viewport-row"
    (let ((s (copy-mode-screen)))
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 7
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
      (cl-tmux/commands::copy-mode-low s)
      (expect (= (1- (screen-height s)) (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (cl-tmux/commands::copy-mode-high s)
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (cl-tmux/commands::copy-mode-middle s)
      (expect (= (floor (screen-height s) 2)
                 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (= 7 (cl-tmux/terminal/types:screen-copy-offset s)))
      (expect (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;;; ── WORD motion: copy-mode-space-{forward,backward,end} (vi W/B/E) ───────────

  ;; WORD motion (W/B/E) treats punctuation as part of the WORD — only whitespace
  ;; separates — unlike w/b/e which honour word-separators (here '-').
  (it "copy-mode-space-motion-is-whitespace-delimited"
    (let ((s (copy-mode-screen :content "foo-bar baz")))
      ;; forward: w stops at 'bar' (col 4, '-' is a separator); W skips to 'baz' (8).
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-word-forward s)
      (expect (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-space-forward s)
      (expect (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      ;; backward from 'baz' (8): b → 'bar' (4); B → start of 'foo-bar' WORD (0).
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
      (cl-tmux/commands::copy-mode-word-backward s)
      (expect (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
      (cl-tmux/commands::copy-mode-space-backward s)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-space-end (vi E) moves to the last char of the current/next WORD.
  (it "copy-mode-space-end-lands-on-word-final-char"
    (let ((s (copy-mode-screen :content "foo-bar baz")))
      ;; From col 0, E → last char of 'foo-bar' (col 6, the 'r').
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-space-end s)
      (expect (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; send -X next-word/etc. map to word motion; next-space/etc. to WORD motion.
  (it "copy-mode-x-word-vs-space-mappings"
    (expect (eq :copy-mode-word-forward  (copy-mode-x-command-value "next-word")))
    (expect (eq :copy-mode-space-forward (copy-mode-x-command-value "next-space")))
    (expect (eq :copy-mode-space-backward (copy-mode-x-command-value "previous-space")))
    (expect (eq :copy-mode-space-end      (copy-mode-x-command-value "next-space-end"))))

  ;;; ── back-to-indentation (vi ^): first non-blank vs line-start (vi 0) ─────────

  ;; copy-mode-back-to-indentation (vi ^) moves to the first non-blank column —
  ;; unlike copy-mode-line-start (vi 0), which always goes to column 0.
  (it "copy-mode-back-to-indentation-stops-at-first-non-blank"
    (let ((s (copy-mode-screen :content "   foo")))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-back-to-indentation s)
      (expect (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      ;; line-start still goes to column 0 — the two are distinct.
      (cl-tmux/commands::copy-mode-line-start s)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; On an all-blank row, ^ falls back to column 0.
  (it "copy-mode-back-to-indentation-blank-line-goes-to-zero"
    (let ((s (copy-mode-screen)))             ; default content is blank
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
      (cl-tmux/commands::copy-mode-back-to-indentation s)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; send -X back-to-indentation maps to the distinct :copy-mode-back-to-indentation
  ;; action, not line-start.
  (it "copy-mode-x-back-to-indentation-mapped"
    (expect (eq :copy-mode-back-to-indentation
                (copy-mode-x-command-value "back-to-indentation"))))

  ;;; ── copy-mode-move-cursor ────────────────────────────────────────────────────

  ;; Each direction moves the cursor by 1 step and marks the screen dirty.
  (it "copy-mode-move-cursor-direction-table"
    (dolist (c '((:left  2 5  2 4)   ; (dir start-row start-col expected-row expected-col)
                 (:right 2 5  2 6)
                 (:up    2 5  1 5)
                 (:down  2 5  3 5)))
      (destructuring-bind (dir sr sc er ec) c
        (with-copy-mode-cursor (s sr sc)
          (cl-tmux/commands::copy-mode-move-cursor s dir)
          (expect (equal (cons er ec) (cl-tmux/terminal/types:screen-copy-cursor s)))
          (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-truthy)))))

  ;; At each axis boundary, move-cursor clamps rather than wrapping or crashing.
  (it "copy-mode-move-cursor-boundary-clamping"
    (dolist (c '((:left  2  0  cdr  0  "col must not go below 0")
                 (:up    0  5  car  0  "row must not go below 0")
                 (:right 2 19  cdr 19  "col must clamp at width-1=19")
                 (:down  4  5  car  4  "row must clamp at height-1=4")))
      (destructuring-bind (dir sr sc accessor expected msg) c
        (declare (ignore msg))
        (with-copy-mode-cursor (s sr sc)
          (cl-tmux/commands::copy-mode-move-cursor s dir)
          (expect (= expected (funcall accessor (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;; While selecting, :right may advance the cursor to WIDTH (the exclusive end past
  ;; the last column) so the selection can include the rightmost cell — navigation
  ;; still caps at WIDTH-1 (covered by the test above).
  (it "copy-mode-selection-cursor-can-reach-width"
    (let ((s (make-screen 5 3)))
      (feed s "abcde")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-begin-selection s)
      (dotimes (i 6) (cl-tmux/commands::copy-mode-move-cursor s :right))
      (expect (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (string= "abcde" (cl-tmux/commands::%selection-text s)))))

  ;; copy-mode-enter initialises the cursor at the bottom-left of the viewport (row height-1, col 0).
  (it "copy-mode-enter-places-cursor-at-bottom-left"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (expect (equal (cons 4 0) (cl-tmux/terminal/types:screen-copy-cursor s)))))

  ;; If copy-cursor is manually reset to NIL, move-cursor falls back to (height-1 . 0) before moving.
  (it "copy-mode-move-cursor-nil-fallback"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Force cursor to NIL to exercise the fallback path inside move-cursor.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
      (cl-tmux/commands::copy-mode-move-cursor s :right)
      (expect (equal (cons 4 1) (cl-tmux/terminal/types:screen-copy-cursor s)))))

  ;; When copy-selecting is T and mark is NIL, the first move sets the mark anchor.
  (it "copy-mode-move-cursor-sets-mark-anchor-when-selecting-and-mark-nil"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3)
            (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) nil)
      (cl-tmux/commands::copy-mode-move-cursor s :right)
      (expect (cl-tmux/terminal/types:screen-copy-mark s) :to-be-truthy)))

  ;; copy-mode-move-cursor does nothing when copy mode is not active.
  (it "copy-mode-move-cursor-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      ;; do NOT enter copy mode
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
      (cl-tmux/commands::copy-mode-move-cursor s :left)
      (expect (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-cursor s)))))

  ;;; ── send-keys -X *-and-cancel (window-copy.c parity) ─────────────────────────

  ;; scroll-down-and-cancel scrolls down one line and exits copy mode when the live
  ;; bottom (scroll-offset 0) is reached.
  (it "copy-mode-scroll-down-and-cancel-exits-at-bottom"
    (let ((s (copy-mode-screen)))
      (seed-scrollback s 5)
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 1)
      (cl-tmux/commands::copy-mode-scroll-down-and-cancel s)
      (expect (cl-tmux/terminal/types:screen-copy-mode-p s) :to-be-falsy)))

  ;; scroll-down-and-cancel stays in copy mode while still scrolled back, moving the
  ;; viewport one line newer.
  (it "copy-mode-scroll-down-and-cancel-stays-when-scrolled-back"
    (let ((s (copy-mode-screen)))
      (seed-scrollback s 5)
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 3)
      (cl-tmux/commands::copy-mode-scroll-down-and-cancel s)
      (expect (cl-tmux/terminal/types:screen-copy-mode-p s) :to-be-truthy)
      (expect (= 2 (cl-tmux/terminal/types:screen-copy-offset s)))))

  ;; page-down-and-cancel scrolls a full page down and exits at the live bottom.
  (it "copy-mode-page-down-and-cancel-exits-at-bottom"
    (let ((s (copy-mode-screen)))
      (seed-scrollback s 2)
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 1)
      (cl-tmux/commands::copy-mode-page-down-and-cancel s)
      (expect (cl-tmux/terminal/types:screen-copy-mode-p s) :to-be-falsy))))
