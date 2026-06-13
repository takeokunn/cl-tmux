(in-package #:cl-tmux/test)

;;;; commands tests — part K: copy-mode-begin-line-selection, copy-end-of-line (D),
;;;; copy-line (Y), search-forward/backward, wrap-search, search-across-scrollback.

(in-suite commands-suite)

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

(test copy-mode-copy-yank-noop-outside-copy-mode-table
  "copy-mode-copy-end-of-line and copy-mode-copy-line leave paste-buffers empty outside copy mode."
  (dolist (c '((cl-tmux/commands::copy-mode-copy-end-of-line "copy-end-of-line")
               (cl-tmux/commands::copy-mode-copy-line        "copy-line")))
    (destructuring-bind (fn desc) c
      (let ((cl-tmux/buffer:*paste-buffers* nil))
        (let ((s (make-screen 20 5)))
          (feed s "hello world")
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
          (funcall fn s)
          (is (null cl-tmux/buffer:*paste-buffers*)
              "~A: paste buffers must remain empty outside copy mode" desc))))))

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
