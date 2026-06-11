(in-package #:cl-tmux/test)

;;;; commands tests — part B: copy-mode line/page/word motions, send-keys, tokenize,
;;;; kill-window-reselection, find-forward/backward, join-pane, break-pane, pipe-pane.

(in-suite commands-suite)

;;; ── copy-mode-line-start / copy-mode-line-end ────────────────────────────────

(test copy-mode-line-start-moves-to-col-0
  "copy-mode-line-start sets the cursor column to 0."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::copy-mode-line-start s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-line-start must set col to 0")))

(test copy-mode-line-end-moves-to-last-col
  "copy-mode-line-end sets the cursor column to width-1."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 3))
    (cl-tmux/commands::copy-mode-line-end s)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-line-end must set col to width-1 (19 for width=20)")))

(test copy-mode-line-start-noop-outside-copy-mode
  "copy-mode-line-start is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 10))
    (cl-tmux/commands::copy-mode-line-start s)
    (is (= 10 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must be unchanged when not in copy mode")))

(test copy-mode-line-end-noop-outside-copy-mode
  "copy-mode-line-end is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
    (cl-tmux/commands::copy-mode-line-end s)
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must be unchanged when not in copy mode")))

;;; ── copy-mode-high / copy-mode-middle / copy-mode-low ───────────────────────

(test copy-mode-high-moves-cursor-to-row-0
  "copy-mode-high sets the cursor row to 0, keeping column."
  (let ((s (make-screen 20 10)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 7 5))
    (cl-tmux/commands::copy-mode-high s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-high must move cursor to row 0")
    (is (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-high must preserve column")))

(test copy-mode-middle-moves-cursor-to-mid-row
  "copy-mode-middle sets the cursor row to floor(height/2), keeping column."
  (let ((s (make-screen 20 10)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-middle s)
    (is (= 5 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-middle must move cursor to floor(10/2)=5 for height=10")))

(test copy-mode-low-moves-cursor-to-last-row
  "copy-mode-low sets the cursor row to height-1, keeping column."
  (let ((s (make-screen 20 10)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-low s)
    (is (= 9 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-low must move cursor to height-1=9 for height=10")))

(test copy-mode-high-noop-outside-copy-mode
  "copy-mode-high is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 5))
    (cl-tmux/commands::copy-mode-high s)
    (is (= 3 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must be unchanged outside copy mode")))

(test copy-mode-middle-noop-outside-copy-mode
  "copy-mode-middle is a no-op when not in copy mode."
  (let ((s (make-screen 20 10)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 7 5))
    (cl-tmux/commands::copy-mode-middle s)
    (is (= 7 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must be unchanged outside copy mode")))

(test copy-mode-low-noop-outside-copy-mode
  "copy-mode-low is a no-op when not in copy mode."
  (let ((s (make-screen 20 10)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-low s)
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must be unchanged outside copy mode")))

;;; ── copy-mode-page-up / copy-mode-page-down ─────────────────────────────────

(test copy-mode-page-up-scrolls-by-full-height
  "copy-mode-page-up scrolls back by screen-height lines."
  (let ((s (%screen-with-scrollback 30)))
    (cl-tmux/commands::copy-mode-page-up s)
    (is (= 5 (screen-copy-offset s))
        "copy-mode-page-up must scroll by screen-height=5")))

(test copy-mode-page-down-scrolls-forward-by-full-height
  "copy-mode-page-down scrolls forward by screen-height lines."
  (let ((s (%screen-with-scrollback 30)))
    ;; First scroll back enough to allow scrolling forward
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 20)
    (cl-tmux/commands::copy-mode-page-down s)
    (is (= 15 (screen-copy-offset s))
        "copy-mode-page-down must reduce offset by screen-height=5")))

(test copy-mode-half-page-up-scrolls-by-half-height
  "copy-mode-half-page-up scrolls back by floor(screen-height/2) lines."
  (let ((s (%screen-with-scrollback 30)))
    (cl-tmux/commands::copy-mode-half-page-up s)
    (is (= 2 (screen-copy-offset s))
        "copy-mode-half-page-up must scroll by floor(5/2)=2 for height=5")))

(test copy-mode-scroll-up-line-scrolls-by-one
  "copy-mode-scroll-up-line scrolls back by exactly 1 line."
  (let ((s (%screen-with-scrollback 10)))
    (cl-tmux/commands::copy-mode-scroll-up-line s)
    (is (= 1 (screen-copy-offset s))
        "copy-mode-scroll-up-line must scroll back by 1")))

(test copy-mode-scroll-down-line-scrolls-forward-by-one
  "copy-mode-scroll-down-line scrolls forward by exactly 1 line."
  (let ((s (%screen-with-scrollback 10)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 5)
    (cl-tmux/commands::copy-mode-scroll-down-line s)
    (is (= 4 (screen-copy-offset s))
        "copy-mode-scroll-down-line must reduce offset by 1")))

(test copy-mode-page-up-noop-outside-copy-mode
  "copy-mode-page-up is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 10 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-page-up s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-page-down-noop-outside-copy-mode
  "copy-mode-page-down is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
    (cl-tmux/commands::copy-mode-page-down s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-half-page-up-noop-outside-copy-mode
  "copy-mode-half-page-up is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 10 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-half-page-up s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-half-page-down-noop-outside-copy-mode
  "copy-mode-half-page-down is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
    (cl-tmux/commands::copy-mode-half-page-down s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-scroll-up-line-noop-outside-copy-mode
  "copy-mode-scroll-up-line is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 5 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-scroll-up-line s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-scroll-down-line-noop-outside-copy-mode
  "copy-mode-scroll-down-line is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
    (cl-tmux/commands::copy-mode-scroll-down-line s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

;;; ── copy-mode-word-forward / word-backward / word-end ──────────────────────

(defun %copy-mode-screen-with-text (text &key (w 40) (h 5))
  "Return a copy-mode screen with TEXT fed at row 0."
  (let ((s (make-screen w h)))
    (feed s text)
    (cl-tmux/commands::copy-mode-enter s)
    s))

(test copy-mode-word-forward-jumps-to-next-word
  "copy-mode-word-forward moves the cursor to the start of the next word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Cursor at col 0 (start of "hello")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-forward s)
    ;; Should land at col 6 (start of "world")
    (is (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-forward must jump to col 6 (start of 'world') from col 0 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-backward-jumps-to-prev-word-start
  "copy-mode-word-backward moves the cursor to the start of the previous word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Cursor in the middle of "world" at col 8
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-word-backward s)
    ;; Should land at col 6 (start of "world")
    (is (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward from col 8 must jump to start of 'world' at col 6 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-forward-noop-outside-copy-mode
  "copy-mode-word-forward is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-forward s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must not change outside copy mode")))


(test copy-mode-word-end-jumps-to-end-of-word
  "copy-mode-word-end moves the cursor to the last character of the current or next word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Cursor at col 0 (start of "hello")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-end s)
    ;; Should land at col 4 (last char of "hello")
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-end from col 0 must jump to col 4 (end of 'hello') (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-end-noop-outside-copy-mode
  "copy-mode-word-end is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-end s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must not change outside copy mode")))

;;; ── copy-mode-top / copy-mode-bottom ────────────────────────────────────────

(test copy-mode-top-jumps-to-max-scrollback
  "copy-mode-top scrolls the viewport to the oldest scrollback line."
  (let ((s (%screen-with-scrollback 10)))
    (cl-tmux/commands::copy-mode-top s)
    (is (= 10 (screen-copy-offset s))
        "copy-mode-top must set offset to the scrollback length (10)")))

(test copy-mode-bottom-returns-to-live-view
  "copy-mode-bottom scrolls back to offset 0 (live view)."
  (let ((s (%screen-with-scrollback 10)))
    ;; First scroll to top
    (cl-tmux/commands::copy-mode-top s)
    (is (= 10 (screen-copy-offset s)) "precondition: at top after copy-mode-top")
    ;; Then jump to bottom
    (cl-tmux/commands::copy-mode-bottom s)
    (is (= 0 (screen-copy-offset s))
        "copy-mode-bottom must reset offset to 0 (live view)")))

(test copy-mode-top-noop-outside-copy-mode
  "copy-mode-top is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 5 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-top s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

;;; ── copy-mode-begin-line-selection ──────────────────────────────────────────

(test copy-mode-begin-line-selection-sets-line-selection-p
  "copy-mode-begin-line-selection sets line-selection-p and activates the selection."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-begin-line-selection s)
    (is-true (cl-tmux/terminal/types:screen-copy-line-selection-p s)
             "copy-line-selection-p must be T after begin-line-selection")
    (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
             "copy-selecting must be T after begin-line-selection")
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-mark s)))
        "mark col must be 0 for line selection")
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be width-1 for line selection")))

(test copy-mode-begin-line-selection-noop-outside-copy-mode
  "copy-mode-begin-line-selection is a no-op when not in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
      ;; Do NOT enter copy mode.
      (cl-tmux/commands::copy-mode-begin-line-selection s)
      (is-false (cl-tmux/terminal/types:screen-copy-line-selection-p s)
                "line-selection-p must remain NIL when not in copy mode")
      (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
                "copy-selecting must remain NIL when not in copy mode"))))

;;; ── copy-mode-copy-end-of-line (D) ──────────────────────────────────────────

(test copy-mode-copy-end-of-line-yanks-from-cursor
  "copy-mode-copy-end-of-line copies text from cursor to end of row and exits."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 6))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (is-false (screen-copy-mode-p s)
                "copy mode must exit after D command")
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (string= "world" yanked))
            "D command must copy from col 6 to end (got ~S)" yanked)))))

(test copy-mode-copy-end-of-line-noop-outside-copy-mode
  "copy-mode-copy-end-of-line is a no-op when not in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      ;; Do NOT enter copy mode.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when not in copy mode"))))

;;; ── copy-mode-copy-line (Y) ──────────────────────────────────────────────────

(test copy-mode-copy-line-yanks-full-row
  "copy-mode-copy-line copies the full current row content and exits."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 10))
      (cl-tmux/commands::copy-mode-copy-line s)
      (is-false (screen-copy-mode-p s)
                "copy mode must exit after Y command")
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (search "hello" yanked))
            "Y command must copy the full row containing 'hello' (got ~S)" yanked)))))

(test copy-mode-copy-line-noop-outside-copy-mode
  "copy-mode-copy-line is a no-op when not in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      ;; Do NOT enter copy mode.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-line s)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when not in copy mode"))))

;;; ── copy-mode-search-forward / search-backward ──────────────────────────────

(test copy-mode-search-forward-finds-term
  "copy-mode-search-forward moves cursor to the first match after current position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    ;; First search from col 1 onward should find "abc" at col 8
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-forward must find second 'abc' at col 8 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-forward-saves-term
  "copy-mode-search-forward saves the search term for n/N repeats."
  (let ((s (make-screen 30 5)))
    (feed s "foo bar foo")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "foo")
    (is (string= "foo" (cl-tmux/terminal/types:screen-copy-search-term s))
        "search term must be saved after search-forward")))

(test copy-mode-search-backward-finds-term
  "copy-mode-search-backward moves cursor to the nearest match before current position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Start cursor at col 11 (past the end of second "abc" at cols 8-10).
    ;; The backward scan uses end-col=11 for row 0, so positions 0..10 are
    ;; eligible.  The rightmost match before col 11 is the second "abc" at col 8.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
    (cl-tmux/commands::copy-mode-search-backward s "abc")
    ;; Search backward should find second "abc" at col 8 (nearest match before col 11)
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-backward must find 'abc' at col 8 (nearest before col 11) (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-forward-regex-dot
  "search-forward treats the term as a regex: 'a.c' matches 'abc'."
  (let ((s (make-screen 30 5)))
    (feed s "xy abc z")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "a.c")
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "regex a.c must match 'abc' at col 3 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-forward-regex-char-class
  "search-forward regex character class '[0-9]+' finds the first digit run."
  (let ((s (make-screen 30 5)))
    (feed s "abc 123 def")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "[0-9]+")
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "regex [0-9]+ must match '123' starting at col 4 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-invalid-regex-falls-back-to-literal
  "An invalid regex (unbalanced paren) falls back to a literal substring search,
   so search terms with regex metacharacters still work."
  (let ((s (make-screen 30 5)))
    (feed s "a (b) c")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "(")
    (is (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "literal '(' must be found at col 2 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

;;; ── wrap-search: search wraps around the buffer ends (default on) ────────────

(test copy-mode-search-forward-wraps-to-top
  "With wrap-search on (default), a forward search that finds nothing below the
   cursor wraps to the top and lands on the first match in the buffer."
  (let ((s (make-screen 30 5)))
    (feed s "abc")                              ; only row 0 contains the term
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 0)) ; below the match
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    (is (equal (cons 0 0) (cl-tmux/terminal/types:screen-copy-cursor s))
        "no match below → wrap to the match at row 0 col 0")))

(test copy-mode-search-forward-no-wrap-when-off
  "With wrap-search off, a forward search with no match below leaves the cursor
   where it is (no wrap-around)."
  (with-isolated-options ("wrap-search" nil)
    (let ((s (make-screen 30 5)))
      (feed s "abc")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 0))
      (cl-tmux/commands::copy-mode-search-forward s "abc")
      (is (equal (cons 2 0) (cl-tmux/terminal/types:screen-copy-cursor s))
          "wrap-search off → cursor stays put when nothing is found below"))))

(test copy-mode-search-backward-wraps-to-bottom
  "With wrap-search on, a backward search that finds nothing above the cursor
   wraps to the bottom and lands on the last match in the buffer."
  (let ((s (make-screen 30 5)))
    (feed-lines s "" "" "" "" "abc")            ; only row 4 contains the term
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0)) ; above the match
    (cl-tmux/commands::copy-mode-search-backward s "abc")
    (is (equal (cons 4 0) (cl-tmux/terminal/types:screen-copy-cursor s))
        "no match above → wrap to the match at row 4 col 0")))

(test copy-mode-search-backward-regex
  "search-backward matches a regex and finds the nearest match before the cursor."
  (let ((s (make-screen 30 5)))
    (feed s "a1b a2b a3b")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
    (cl-tmux/commands::copy-mode-search-backward s "a.b")
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "regex a.b backward must find the last 'aNb' at col 8 before col 11 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-next-repeats-forward
  "copy-mode-search-next uses the saved term to repeat forward search; with
   wrap-search on (default) it wraps to the first match when none lies below."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; Save a term and jump to the second "abc" at col 8.
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
    ;; search-next from col 8: nothing further below on row 0, so it wraps around
    ;; to the first "abc" at col 0 (tmux's wrapping n).
    (cl-tmux/commands::copy-mode-search-next s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-next wraps to the first match (col 0) when none lies below")))

(test copy-mode-search-prev-noop-without-term
  "copy-mode-search-prev does nothing when no search term is saved."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5)
          (cl-tmux/terminal/types:screen-copy-search-term s) nil)
    (cl-tmux/commands::copy-mode-search-prev s)
    (is (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-prev must not move cursor when no term is saved")))

;;; ── copy-mode search across scrollback boundary ─────────────────────────────

(defun %make-text-row (width text)
  "Create a scrollback row vector WIDTH wide with TEXT followed by space cells."
  (let ((row (make-array width
                         :initial-element
                         (cl-tmux/terminal/types:make-cell
                          :char #\Space :fg 7 :bg 0 :attrs 0 :width 1))))
    (loop for i from 0 below (min (length text) width)
          do (setf (aref row i)
                   (cl-tmux/terminal/types:make-cell
                    :char (char text i) :fg 7 :bg 0 :attrs 0 :width 1)))
    row))

(test copy-mode-search-forward-wraps-into-scrollback
  "Forward search with wrap-search wraps from the live grid into the scrollback buffer
   when the term is only present in the scrollback."
  ;; Screen 20x3; scrollback newest-first: sb[0]=row with term, sb[1]=blank.
  ;; Virtual rows: vrow0=sb[1](blank), vrow1=sb[0](term), vrow2-4=live(blank).
  (let* ((s    (make-screen 20 3))
         (sb0  (%make-text-row 20 "findme here"))
         (sb1  (%make-text-row 20 "")))
    (setf (cl-tmux/terminal/types:screen-scrollback s) (list sb0 sb1))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Cursor starts at bottom of live grid (row 2, col 0), offset 0.
    (cl-tmux/commands::copy-mode-search-forward s "findme")
    ;; After wrap the term is at virtual row 1 (newest scrollback); set_vrow
    ;; sets offset=1, cursor-row=0.
    (is (= 1 (cl-tmux/terminal/types:screen-copy-offset s))
        "offset must scroll into scrollback (expected 1, got ~D)"
        (cl-tmux/terminal/types:screen-copy-offset s))
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must be 0 (top of viewport showing the found scrollback row)")
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be 0 (start of 'findme')")))

(test copy-mode-search-backward-finds-term-in-scrollback
  "Backward search from the live grid finds a term in the scrollback without wrapping."
  ;; Screen 20x3; sb[0]=newest='target row', sb[1]=oldest=blank.
  ;; Cursor at live-grid top (row 0, offset 0).
  (let* ((s    (make-screen 20 3))
         (sb0  (%make-text-row 20 "target row"))
         (sb1  (%make-text-row 20 "")))
    (setf (cl-tmux/terminal/types:screen-scrollback s) (list sb0 sb1))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-backward s "target")
    ;; target is in vrow 1 (newest scrollback); set_vrow → offset=1, row=0.
    (is (= 1 (cl-tmux/terminal/types:screen-copy-offset s))
        "backward search must scroll to scrollback (expected offset 1, got ~D)"
        (cl-tmux/terminal/types:screen-copy-offset s))
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be 0 (start of 'target')")))

;;; ── send-keys-to-pane ────────────────────────────────────────────────────────

(test send-keys-to-pane-noop-with-negative-fd
  "send-keys-to-pane is a no-op (no error) when the pane has fd=-1."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:send-keys-to-pane pane "hello")
              "send-keys-to-pane with fd=-1 must not signal an error")))

(test send-keys-to-pane-noop-with-nil-pane
  "send-keys-to-pane with NIL pane does not signal an error."
  (finishes (cl-tmux/commands:send-keys-to-pane nil "hello")
            "send-keys-to-pane with nil pane must not signal an error"))

;;; ── send-keys key-name translation ───────────────────────────────────────────

(test key-name-to-bytes-named-keys
  "%key-name-to-bytes maps named keys to their byte sequences."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(13)  (bytes "Enter"))   "Enter → CR")
    (is (equal '(9)   (bytes "Tab"))     "Tab → HT")
    (is (equal '(27)  (bytes "Escape"))  "Escape → ESC")
    (is (equal '(32)  (bytes "Space"))   "Space → SP")
    (is (equal '(127) (bytes "BSpace"))  "BSpace → DEL")
    (is (equal '(27 91 65) (bytes "Up"))      "Up → ESC [ A")
    (is (equal '(27 91 66) (bytes "Down"))    "Down → ESC [ B")
    (is (equal '(27 79 80) (bytes "F1"))      "F1 → ESC O P")
    (is (equal '(27 91 53 126) (bytes "PageUp")) "PageUp → ESC [ 5 ~")))

(test key-name-to-bytes-control-keys
  "%key-name-to-bytes maps C-<char> to the corresponding control byte."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(3)  (bytes "C-c")) "C-c → 0x03")
    (is (equal '(1)  (bytes "C-a")) "C-a → 0x01")
    (is (equal '(26) (bytes "C-z")) "C-z → 0x1a")
    (is (equal '(0)  (bytes "C-@")) "C-@ → 0x00")))

(test key-name-to-bytes-meta-keys
  "%key-name-to-bytes maps M-<char> to ESC followed by the char."
  (is (equal '(27 120) (coerce (cl-tmux/commands::%key-name-to-bytes "M-x") 'list))
      "M-x → ESC x"))

(test split-key-modifiers-decodes-csi-modifier
  "%split-key-modifiers strips C-/M-/S- prefixes into the CSI modifier code."
  (flet ((mods (name) (multiple-value-list (cl-tmux/commands::%split-key-modifiers name))))
    (is (equal '(1 "Up")   (mods "Up"))    "no modifier → 1")
    (is (equal '(5 "Up")   (mods "C-Up"))  "Ctrl → 5")
    (is (equal '(3 "Left") (mods "M-Left")) "Alt → 3")
    (is (equal '(2 "Down") (mods "S-Down")) "Shift → 2")
    (is (equal '(7 "Left") (mods "C-M-Left")) "Ctrl+Alt → 7")
    (is (equal '(6 "Up")   (mods "C-S-Up")) "Ctrl+Shift → 6")))

(test key-name-to-bytes-modified-arrows-and-nav
  "%key-name-to-bytes encodes modified arrows / Home / End as ESC [ 1 ; mod final."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(27 91 49 59 53 65) (bytes "C-Up"))    "C-Up → ESC[1;5A")
    (is (equal '(27 91 49 59 51 68) (bytes "M-Left"))  "M-Left → ESC[1;3D")
    (is (equal '(27 91 49 59 50 66) (bytes "S-Down"))  "S-Down → ESC[1;2B")
    (is (equal '(27 91 49 59 55 68) (bytes "C-M-Left")) "C-M-Left → ESC[1;7D")
    (is (equal '(27 91 49 59 50 72) (bytes "S-Home"))  "S-Home → ESC[1;2H")))

(test key-name-to-bytes-modified-function-keys
  "%key-name-to-bytes encodes modified F-keys / page keys as ESC [ param ; mod ~."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(27 91 49 53 59 53 126) (bytes "C-F5"))     "C-F5 → ESC[15;5~")
    (is (equal '(27 91 53 59 53 126)    (bytes "C-PageUp")) "C-PageUp → ESC[5;5~")
    (is (equal '(27 91 51 59 50 126)    (bytes "S-Delete")) "S-Delete → ESC[3;2~")))

(test key-name-to-bytes-modified-does-not-break-control-chars
  "A C-/M- prefix on a plain char still yields the control/meta byte, not a CSI
   sequence (the modified-special path only triggers for named special keys)."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(3)      (bytes "C-c")) "C-c stays the control byte")
    (is (equal '(27 120) (bytes "M-x")) "M-x stays ESC x")))

(test key-name-to-bytes-unknown-returns-nil
  "%key-name-to-bytes returns NIL for text that is not a key name."
  (is (null (cl-tmux/commands::%key-name-to-bytes "hello")))
  (is (null (cl-tmux/commands::%key-name-to-bytes "echo"))))

(test translate-send-keys-keys-vs-literal
  "%translate-send-keys parses arguments shell-style and translates each: key
   names become their byte sequences, other args are sent literally.  Spaces
   separate arguments unless quoted (tmux semantics)."
  (flet ((bytes (s) (coerce (cl-tmux/commands::%translate-send-keys s) 'list)))
    (is (equal '(13) (bytes "Enter")) "single key → its bytes")
    (is (equal '(27 91 65 27 91 65 27 91 66) (bytes "Up Up Down"))
        "all-keys → concatenated (ESC[A ESC[A ESC[B)")
    ;; tmux semantics: unquoted spaces split args, so they vanish between literals.
    (is (equal (map 'list #'char-code "echohi") (bytes "echo hi"))
        "unquoted 'echo hi' → two literal args, no space (tmux-correct)")
    ;; A literal arg before a key: text then CR.
    (is (equal (append (map 'list #'char-code "foo") '(13)) (bytes "foo Enter"))
        "literal arg followed by a key → text then the key's bytes")
    ;; Quoting preserves the embedded space.
    (is (equal (append (map 'list #'char-code "echo hi") '(13))
               (bytes "\"echo hi\" Enter"))
        "quoted arg keeps its space, then Enter → CR")))

(test send-keys-to-pane-translates-named-key-to-pty
  "send-keys-to-pane translates a named key (Enter) and writes CR to the PTY."
  (with-pipe-fds (rfd wfd)
    (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                           :screen (make-screen 20 5))))
      (cl-tmux/commands:send-keys-to-pane pane "Enter")
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
        (is-true ready "the translated key must reach the PTY")
        (when ready
          (cffi:with-foreign-object (buf :uint8 8)
            (let ((n (cffi:foreign-funcall "read"
                                           :int rfd :pointer buf :unsigned-long 4
                                           :long)))
              (is (= 1 n) "Enter is one byte (got ~D)" n)
              (is (= 13 (cffi:mem-aref buf :uint8 0)) "byte must be CR (13)"))))))))

;;; ── tokenize-command-string (shell-style command lexer) ──────────────────────

(test tokenize-command-string-basic-whitespace
  "Whitespace separates arguments; runs of spaces/tabs collapse."
  (is (equal '("a" "b" "c") (cl-tmux/commands:tokenize-command-string "a b c")))
  (is (equal '("a" "b") (cl-tmux/commands:tokenize-command-string "  a   b  ")))
  (is (equal '() (cl-tmux/commands:tokenize-command-string "   "))))

(test tokenize-command-string-single-quotes-literal
  "'...' is a literal span: spaces inside are kept and no escapes are processed."
  (is (equal '("a b" "c") (cl-tmux/commands:tokenize-command-string "'a b' c")))
  (is (equal '("a\\b") (cl-tmux/commands:tokenize-command-string "'a\\b'")))
  (is (equal '("") (cl-tmux/commands:tokenize-command-string "''"))
      "an explicit empty quoted token yields an empty-string argument"))

(test tokenize-command-string-double-quotes-with-escapes
  "\"...\" keeps spaces and processes backslash escapes."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "\"a b\"")))
  (is (equal '("a\"b") (cl-tmux/commands:tokenize-command-string "\"a\\\"b\""))
      "escaped double-quote stays inside the argument"))

(test tokenize-command-string-bare-backslash-escape
  "A bare backslash escapes the next character (e.g. a space joins one arg)."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "a\\ b")))
  (is (equal '("ab") (cl-tmux/commands:tokenize-command-string "a\\b"))))

(test tokenize-command-string-adjacent-spans-join
  "Adjacent quoted/bare spans concatenate into a single argument."
  (is (equal '("foobar baz")
             (cl-tmux/commands:tokenize-command-string "foo\"bar baz\"")))
  (is (equal '("ab cd")
             (cl-tmux/commands:tokenize-command-string "'ab'' cd'"))))

(test tokenize-command-string-unterminated-quote-tolerated
  "An unterminated quote consumes to end of string without error."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "'a b")))
  (is (equal '("xy") (cl-tmux/commands:tokenize-command-string "\"xy"))))

;;; ── add-message-log ──────────────────────────────────────────────────────────

(test add-message-log-prepends-entry
  "add-message-log prepends a (timestamp . text) cons to *message-log*."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first-message")
    (is-true cl-tmux::*message-log*
        "*message-log* must be non-nil after add-message-log")
    (is (string= "first-message" (cdr (first cl-tmux::*message-log*)))
        "message text must be in cdr of first entry (got ~S)"
        (cdr (first cl-tmux::*message-log*)))))

(test add-message-log-caps-honors-message-limit-option
  "add-message-log caps *message-log* at the message-limit option, not a constant."
  (with-isolated-options ("message-limit" 3)
    (let ((cl-tmux::*message-log* nil))
      (loop repeat 10 do (cl-tmux::add-message-log "x"))
      (is (= 3 (length cl-tmux::*message-log*))
          "*message-log* must be capped at message-limit (3, got ~D)"
          (length cl-tmux::*message-log*)))))

(test add-message-log-ordering
  "add-message-log puts newest entry first."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first")
    (cl-tmux::add-message-log "second")
    (is (string= "second" (cdr (first cl-tmux::*message-log*)))
        "second (most recent) message must be at the head of *message-log*")))

;;; ── kill-window reselection: tmux session_detach order ───────────────────────
;;; %window-after-kill matches tmux: the last-used (MRU) window first
;;; (session_last), else the previous window by index, wrapping to the highest id
;;; (session_previous).  Verified against tmux source (session.c) via deepwiki.

(test window-after-kill-prefers-mru
  "The last-used (MRU) window — strictly greatest positive last-active-time — is
   selected first, regardless of id distance (tmux session_last)."
  (let ((w0 (make-window :id 0 :name "a" :width 20 :height 5 :panes nil
                         :last-active-time 100))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil
                         :last-active-time 300))
        (w7 (make-window :id 7 :name "c" :width 20 :height 5 :panes nil
                         :last-active-time 200)))
    (is (eq w3 (cl-tmux/commands::%window-after-kill (list w0 w3 w7) 5))
        "MRU window (w3, latest last-active-time) wins over id distance")))

(test window-after-kill-previous-by-index-without-mru
  "With no focus history (all timestamps 0), falls back to the previous window by
   index: the greatest id strictly less than the killed id (tmux session_previous)."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil))
        (w8 (make-window :id 8 :name "c" :width 20 :height 5 :panes nil)))
    (is (eq w3 (cl-tmux/commands::%window-after-kill (list w1 w3 w8) 5))
        "picks w3 (greatest id < 5)")))

(test window-after-kill-differs-from-old-nearest
  "Regression: the OLD numerically-nearest rule broke ties toward the HIGHER id.
   killed-id=2, remaining {1,3}, no MRU: tmux picks the PREVIOUS (w1); the old
   %nearest-window wrongly picked w3 (equidistant tie → higher id)."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil)))
    (is (eq w1 (cl-tmux/commands::%window-after-kill (list w1 w3) 2))
        "previous-by-index picks w1; old %nearest-window picked w3")))

(test window-after-kill-previous-wraps-to-highest
  "When no window has a lower id than the killed one, previous-by-index wraps to
   the HIGHEST id (tmux session_previous wrap)."
  (let ((w2 (make-window :id 2 :name "a" :width 20 :height 5 :panes nil))
        (w5 (make-window :id 5 :name "b" :width 20 :height 5 :panes nil))
        (w8 (make-window :id 8 :name "c" :width 20 :height 5 :panes nil)))
    (is (eq w8 (cl-tmux/commands::%window-after-kill (list w2 w5 w8) 0))
        "wraps to the highest-id window (w8) when killed was the lowest")))

(test window-after-kill-mru-tie-falls-back-to-index
  "A TIE at the greatest last-active-time is no unambiguous last-used window (like
   tmux's empty lastw) → fall back to previous-by-index."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil
                         :last-active-time 50))
        (w4 (make-window :id 4 :name "b" :width 20 :height 5 :panes nil
                         :last-active-time 50)))
    (is (eq w1 (cl-tmux/commands::%window-after-kill (list w1 w4) 3))
        "tie at max time → previous-by-index picks w1 (greatest id < 3)")))

(test window-after-kill-single-window
  "A single remaining window is always selected."
  (let ((w2 (make-window :id 2 :name "a" :width 20 :height 5 :panes nil)))
    (is (eq w2 (cl-tmux/commands::%window-after-kill (list w2) 99))
        "the sole remaining window is selected regardless of killed id")))

;;; ── %copy-mode-find-forward / %copy-mode-find-backward ──────────────────────

(test copy-mode-find-forward-locates-term
  "%copy-mode-find-forward finds TERM at the correct row/col from start position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-forward s "abc" 0 1)
      (is (= 0 row) "forward search must find match on row 0 (got ~S)" row)
      (is (= 8 col) "forward search from col 1 must find second 'abc' at col 8 (got ~S)" col))))

(test copy-mode-find-forward-no-match-returns-nil-nil
  "%copy-mode-find-forward returns (values nil nil) when no match exists."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-forward s "zzz" 0 0)
      (is (null row) "no match: row must be NIL")
      (is (null col) "no match: col must be NIL"))))

(test copy-mode-find-backward-locates-term
  "%copy-mode-find-backward finds the nearest match before the cursor position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Search backward from col 11 on row 0 => nearest match before col 11 is
    ;; the second "abc" at col 8.
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-backward s "abc" 0 11)
      (is (= 0 row) "backward search must find match on row 0 (got ~S)" row)
      (is (= 8 col) "backward search from col 11 must find 'abc' at col 8 (got ~S)" col))))

(test copy-mode-find-backward-no-match-returns-nil-nil
  "%copy-mode-find-backward returns (values nil nil) when no match exists."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-backward s "zzz" 0 5)
      (is (null row) "no match: row must be NIL")
      (is (null col) "no match: col must be NIL"))))

;;; ── join-pane ────────────────────────────────────────────────────────────────

(test join-pane-moves-pane-into-destination-window
  "join-pane removes SRC-PANE from SRC-WINDOW and inserts it into DST-WINDOW."
  (let* ((src-pane (%make-test-pane :id 1))
         (dst-pane (%make-test-pane :id 2))
         (src-win  (make-window :id 1 :name "src" :width 20 :height 5
                                :tree (make-layout-leaf src-pane)
                                :panes (list src-pane)))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :tree (make-layout-leaf dst-pane)
                                :panes (list dst-pane)))
         (sess     (make-session :id 1 :name "0"
                                 :windows (list src-win dst-win))))
    (session-select-window sess src-win)
    (window-select-pane src-win src-pane)
    (window-select-pane dst-win dst-pane)
    (let ((result (cl-tmux/commands:join-pane sess src-win src-pane dst-win :h)))
      (is (eq src-pane result) "join-pane must return src-pane on success")
      ;; src-window had only one pane -- it must have been killed.
      (is-false (member src-win (session-windows sess))
          "src-window must be removed from session when it becomes empty after join-pane")
      ;; dst-window must now contain both dst-pane and src-pane.
      (is (member src-pane (window-panes dst-win))
          "src-pane must appear in dst-window's pane list after join-pane"))))

(test join-pane-returns-nil-on-nil-args
  "join-pane returns NIL immediately when any required argument is NIL."
  (is (null (cl-tmux/commands:join-pane nil nil nil nil :h))
      "join-pane with all-nil args must return NIL without signalling"))

;;; ── join-pane / move-pane (scriptable %cmd-join-pane-arg) ────────────────────

(defun %join-arg-fixture ()
  "Two single-pane windows (\"src\", \"dst\") in one session, dst current.
   Returns (values sess src-win src-pane dst-win dst-pane)."
  (let* ((src-pane (%make-test-pane :id 1))
         (dst-pane (%make-test-pane :id 2))
         (src-win  (make-window :id 1 :name "src" :width 20 :height 6
                                :tree (make-layout-leaf src-pane) :panes (list src-pane)))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 6
                                :tree (make-layout-leaf dst-pane) :panes (list dst-pane)))
         (sess     (make-session :id 1 :name "0" :windows (list src-win dst-win))))
    (session-select-window sess dst-win)          ; current window = dst
    (window-select-pane src-win src-pane)
    (window-select-pane dst-win dst-pane)
    (values sess src-win src-pane dst-win dst-pane)))

(test cmd-join-pane-moves-source-into-destination
  "join-pane -s SRC -t DST moves SRC's active pane into DST's window and, without
   -d, makes the joined pane active.  The emptied source window is removed."
  (multiple-value-bind (sess src-win src-pane dst-win dst-pane) (%join-arg-fixture)
    (declare (ignore dst-pane))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":dst" "-v"))
      (is (member src-pane (window-panes dst-win))
          "src-pane must now be in dst-window")
      (is-false (member src-win (session-windows sess))
          "emptied src-window must be removed from the session")
      (is (eq src-pane (window-active-pane dst-win))
          "the joined pane becomes active (no -d)"))))

(test cmd-join-pane-d-keeps-destination-active
  "join-pane -d moves the pane but leaves the destination's original pane active."
  (multiple-value-bind (sess src-win src-pane dst-win dst-pane) (%join-arg-fixture)
    (declare (ignore src-win))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":dst" "-d"))
      (is (member src-pane (window-panes dst-win))
          "src-pane is still moved into dst-window with -d")
      (is (eq dst-pane (window-active-pane dst-win))
          "-d keeps the destination's original pane active"))))

(test cmd-join-pane-same-window-is-noop
  "join-pane with src and dst the same window is a no-op (guarded, no crash)."
  (multiple-value-bind (sess src-win src-pane dst-win dst-pane) (%join-arg-fixture)
    (declare (ignore src-pane dst-win dst-pane))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess))))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":src"))
      (is (= 1 (length (window-panes src-win)))
          "same-window join leaves the source pane in place")
      (is (member src-win (session-windows sess))
          "the source window is not removed by a same-window no-op"))))

;;; ── copy-mode-exit ───────────────────────────────────────────────────────────

(test copy-mode-exit-resets-all-copy-state
  "copy-mode-exit resets copy-mode-p, offset, mark, cursor, and selecting."
  (let ((s (%copy-mode-screen)))
    ;; Set all copy-mode fields to non-default values.
    (setf (cl-tmux/terminal/types:screen-copy-offset    s) 5
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 2 3)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 2 5)
          (cl-tmux/terminal/types:screen-copy-selecting s) t)
    (cl-tmux/commands::copy-mode-exit s)
    (is-false (screen-copy-mode-p s)
              "copy-mode-p must be NIL after exit")
    (is (= 0 (cl-tmux/terminal/types:screen-copy-offset s))
        "copy-offset must be 0 after exit")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "copy-mark must be NIL after exit")
    (is (null (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-cursor must be NIL after exit")
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be NIL after exit")))

;;; ── copy-mode-half-page-down ─────────────────────────────────────────────────

(test copy-mode-half-page-down-scrolls-forward-by-half-height
  "copy-mode-half-page-down scrolls forward by floor(screen-height/2) lines."
  (let ((s (%screen-with-scrollback 30)))
    ;; First scroll back enough to allow scrolling forward.
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 20)
    (cl-tmux/commands::copy-mode-half-page-down s)
    ;; height=5, floor(5/2)=2, so offset decreases by 2: 20-2=18.
    (is (= 18 (screen-copy-offset s))
        "copy-mode-half-page-down must reduce offset by floor(5/2)=2 for height=5")))

;;; ── break-pane ───────────────────────────────────────────────────────────────

(test break-pane-sole-pane-returns-nil
  "break-pane on a window with only one pane is a no-op and returns NIL."
  (let* ((pane (%make-test-pane))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :tree (make-layout-leaf pane)
                            :panes (list pane)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pane)
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane on a sole-pane window must return NIL")))

(test break-pane-nil-src-win-returns-nil
  "break-pane when session has no active window returns NIL."
  ;; Build a session with no windows to exercise the nil-src-win guard.
  (let ((sess (make-session :id 1 :name "0" :windows nil)))
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane with no active window must return NIL")))

(test break-pane-moves-pane-to-new-window
  "break-pane removes the active pane and places it in a new window."
  (let* ((p0  (%make-test-pane :id 1 :w 10))
         (p1  (%make-test-pane :id 2 :x 11 :w 10))
         (win (make-window :id 1 :name "w" :width 21 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win p0)
    (let ((new-win (cl-tmux/commands:break-pane sess)))
      (is-true new-win
          "break-pane must return a new window on success")
      (is (member new-win (session-windows sess))
          "new window must appear in the session's window list")
      (is (member p0 (window-panes new-win))
          "the active pane must be the sole pane of the new window")
      (is (= 1 (length (window-panes new-win)))
          "the new window must have exactly one pane")
      ;; Source window still has p1.
      (is (member p1 (window-panes win))
          "the source window must retain the non-active pane"))))

;;; ── break-pane (scriptable %cmd-break-pane-arg) ──────────────────────────────

(defun %break-arg-fixture ()
  "A window \"w\" with two panes p0 (active), p1 in session \"0\".
   Returns (values sess win p0 p1)."
  (let* ((p0  (%make-test-pane :id 1 :w 10))
         (p1  (%make-test-pane :id 2 :x 11 :w 10))
         (win (make-window :id 1 :name "w" :width 21 :height 5
                           :tree (make-layout-split :h (make-layout-leaf p0)
                                                    (make-layout-leaf p1) 1/2)
                           :panes (list p0 p1)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win p0)
    (values sess win p0 p1)))

(test cmd-break-pane-moves-active-pane-and-switches
  "break-pane (no -d) moves the active pane into a new window and switches to it."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-break-pane-arg sess '())
      (is (= 2 (length (session-windows sess)))
          "a new window is created (the session now has two)")
      (let ((new-win (find-if (lambda (w) (not (eq w win))) (session-windows sess))))
        (is (member p0 (window-panes new-win)) "the active pane moved to the new window")
        (is (eq new-win (session-active-window sess))
            "the session switches to the new window without -d")))))

(test cmd-break-pane-d-stays-on-current-window
  "break-pane -d creates the new window but does NOT switch to it."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p0 p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-break-pane-arg sess '("-d"))
      (is (= 2 (length (session-windows sess))) "the new window is still created")
      (is (eq win (session-active-window sess))
          "-d keeps the current window active"))))

(test cmd-break-pane-n-names-new-window
  "break-pane -n NAME gives the new window that name."
  (multiple-value-bind (sess win p0 p1) (%break-arg-fixture)
    (declare (ignore p0 p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-break-pane-arg sess '("-n" "logs"))
      (let ((new-win (find-if (lambda (w) (not (eq w win))) (session-windows sess))))
        (is (string= "logs" (window-name new-win))
            "the new window must be named 'logs'")))))

;;; ── clear-history (scriptable %cmd-clear-history-arg) ────────────────────────

(defun %clear-history-fixture ()
  "Single-pane window \"w\" in session \"0\" whose screen has a non-empty
   scrollback.  Returns (values sess win screen)."
  (let* ((screen (make-screen 10 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3
                            :fd -1 :pid -1 :screen screen))
         (win    (make-window :id 1 :name "w" :width 10 :height 3
                              :tree (make-layout-leaf pane) :panes (list pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pane)
    (setf (cl-tmux/terminal/types:screen-scrollback screen)
          (list (make-array 10 :initial-element
                            (cl-tmux/terminal/types:make-cell
                             :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
    (values sess win screen)))

(test cmd-clear-history-clears-target-pane-scrollback
  "clear-history -t :w clears the target pane's scrollback."
  (multiple-value-bind (sess win screen) (%clear-history-fixture)
    (declare (ignore win))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-clear-history-arg sess '("-t" ":w"))
      (is (null (cl-tmux/terminal/types:screen-scrollback screen))
          "clear-history -t must empty the target pane's scrollback"))))

(test cmd-clear-history-defaults-to-active-pane
  "clear-history with no -t clears the active pane's scrollback."
  (multiple-value-bind (sess win screen) (%clear-history-fixture)
    (declare (ignore win))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-clear-history-arg sess '())
      (is (null (cl-tmux/terminal/types:screen-scrollback screen))
          "clear-history must default to the active pane and empty its scrollback"))))

;;; ── rotate-window (scriptable %cmd-rotate-window-arg) ────────────────────────

(defun %rotate-window-fixture ()
  "Three-pane window \"w\" (p0 p1 p2) in session \"0\".
   Returns (values sess win p0 p1 p2)."
  (let* ((p0 (%make-test-pane :id 1))
         (p1 (%make-test-pane :id 2))
         (p2 (%make-test-pane :id 3))
         (win (make-window :id 1 :name "w" :width 30 :height 6
                           :tree (make-layout-split :h (make-layout-leaf p0)
                                   (make-layout-split :h (make-layout-leaf p1)
                                                      (make-layout-leaf p2) 1/2)
                                   1/2)
                           :panes (list p0 p1 p2)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (values sess win p0 p1 p2)))

(test cmd-rotate-window-rotates-forward-by-default
  "rotate-window -t :w (no direction) rotates forward: first pane moves to the end."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p2))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-rotate-window-arg sess '("-t" ":w"))
      (is (eq p1 (first (window-panes win)))
          "forward rotate makes the second pane first")
      (is (eq p0 (car (last (window-panes win))))
          "the original first pane moves to the end"))))

(test cmd-rotate-window-d-rotates-backward
  "rotate-window -D -t :w rotates backward: the last pane moves to the front."
  (multiple-value-bind (sess win p0 p1 p2) (%rotate-window-fixture)
    (declare (ignore p0 p1))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-rotate-window-arg sess '("-D" "-t" ":w"))
      (is (eq p2 (first (window-panes win)))
          "-D (backward) makes the last pane first"))))

;;; ── find-window (scriptable %cmd-find-window-arg) ────────────────────────────

(defun %find-window-fixture ()
  "Session \"0\" with three named windows alpha/beta/gamma (alpha current).
   Returns (values sess wa wb wg)."
  (let* ((pa (%make-test-pane :id 1)) (pb (%make-test-pane :id 2))
         (pg (%make-test-pane :id 3))
         (wa (make-window :id 1 :name "alpha" :width 20 :height 5
                          :tree (make-layout-leaf pa) :panes (list pa)))
         (wb (make-window :id 2 :name "beta" :width 20 :height 5
                          :tree (make-layout-leaf pb) :panes (list pb)))
         (wg (make-window :id 3 :name "gamma" :width 20 :height 5
                          :tree (make-layout-leaf pg) :panes (list pg)))
         (sess (make-session :id 1 :name "0" :windows (list wa wb wg))))
    (session-select-window sess wa)
    (values sess wa wb wg)))

(test cmd-find-window-selects-matching-window
  "find-window <pattern> selects the window whose name matches (case-insensitive)."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wg))
    (let ((cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-find-window-arg sess '("BET"))
      (is (eq wb (session-active-window sess))
          "find-window BET must select the 'beta' window (case-insensitive)"))))

(test cmd-find-window-no-match-leaves-active
  "find-window with no matching window leaves the active window unchanged."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (cl-tmux::%cmd-find-window-arg sess '("zzz"))
    (is (eq wa (session-active-window sess))
        "no match must leave the original active window selected")))

(test window-matches-pattern-p-name
  "%window-matches-pattern-p matches the window name case-insensitively."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore sess wb wg))
    (is-true  (cl-tmux::%window-matches-pattern-p wa "ALP") "case-insensitive name match")
    (is-false (cl-tmux::%window-matches-pattern-p wa "beta") "non-matching name → NIL")))

;;; ── next-window / previous-window (scriptable -t) ────────────────────────────

(test cmd-next-window-cycles-current-session
  "next-window (no -t) advances the current session's active window."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)   ; alpha(active) beta gamma
    (declare (ignore wa wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-next-window-arg sess '())
      (is (eq wb (session-active-window sess))
          "next-window advances alpha → beta"))))

(test cmd-previous-window-wraps-backward
  "previous-window from the first window wraps to the last."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wa wb))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-previous-window-arg sess '())
      (is (eq wg (session-active-window sess))
          "previous-window from alpha wraps to gamma"))))

(test cmd-next-window-t-targets-named-session
  "next-window -t NAME advances the NAMED session's window, leaving the current
   session's active window unchanged."
  (let* ((pc (%make-test-pane :id 1)) (poa (%make-test-pane :id 2))
         (pob (%make-test-pane :id 3))
         (cur-win (make-window :id 1 :name "cur" :width 20 :height 5
                               :tree (make-layout-leaf pc) :panes (list pc)))
         (cur     (make-session :id 1 :name "cur" :windows (list cur-win)))
         (o-a (make-window :id 2 :name "oa" :width 20 :height 5
                           :tree (make-layout-leaf poa) :panes (list poa)))
         (o-b (make-window :id 3 :name "ob" :width 20 :height 5
                           :tree (make-layout-leaf pob) :panes (list pob)))
         (other (make-session :id 2 :name "other" :windows (list o-a o-b))))
    (session-select-window cur cur-win)
    (session-select-window other o-a)
    (let ((cl-tmux::*server-sessions* (list (cons "cur" cur) (cons "other" other)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-next-window-arg cur '("-t" "other"))
      (is (eq o-b (session-active-window other))
          "next-window -t other advanced the OTHER session to its second window")
      (is (eq cur-win (session-active-window cur))
          "the current session's active window stays unchanged"))))

(test cmd-next-window-a-jumps-to-alerted-window
  "next-window -a skips windows without an alert and selects the next window whose
   activity (or silence) flag is set."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)  ; alpha(active) beta gamma
    (declare (ignore wa wb))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (setf (cl-tmux/model:window-activity-flag wg) t)   ; only gamma has an alert
      (cl-tmux::%cmd-next-window-arg sess '("-a"))
      (is (eq wg (session-active-window sess))
          "next-window -a skips beta (no alert) and selects gamma"))))

(test cmd-next-window-a-no-alerts-is-noop
  "next-window -a with no alerted windows leaves the active window unchanged."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)
    (declare (ignore wb wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-next-window-arg sess '("-a"))
      (is (eq wa (session-active-window sess))
          "next-window -a with no alerts stays on the active window"))))

(test cmd-previous-window-a-jumps-backward-to-alerted-window
  "previous-window -a scans backward to the nearest window with an alert."
  (multiple-value-bind (sess wa wb wg) (%find-window-fixture)  ; alpha(active) beta gamma
    (declare (ignore wa wg))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (setf (cl-tmux/model:window-silence-flag wb) t)    ; beta has a silence alert
      (cl-tmux::%cmd-previous-window-arg sess '("-a"))
      (is (eq wb (session-active-window sess))
          "previous-window -a selects beta (the alerted window)"))))

