(in-package #:cl-tmux/test)

;;;; copy-mode line/page/word motions, top/bottom, begin-line-sel, copy-D/Y, search — part II

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
