(in-package #:cl-tmux/test)

;;;; commands tests — part J: join-pane helpers, resize-pane directions,
;;;; copy-mode word/bottom noop, search helpers, scroll helpers,
;;;; extract-chars, copy-row-range, screen-row-string, rename-session hooks.

(in-suite commands-suite)

;;; ── %join-pane-kill-empty-src direct tests ───────────────────────────────────

(test join-pane-kill-empty-src-removes-empty-window-from-session
  "%join-pane-kill-empty-src removes a window with no panes from the session."
  (let* ((src-win  (make-window :id 1 :name "src" :width 20 :height 5 :panes nil))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :panes (list (%make-test-pane :id 1))))
         (sess     (make-session :id 1 :name "0" :windows (list src-win dst-win))))
    (session-select-window sess src-win)
    (cl-tmux/commands::%join-pane-kill-empty-src sess src-win)
    (is-false (member src-win (session-windows sess))
              "empty src-win must be removed from session")
    ;; Active window switches to the remaining window.
    (is (eq dst-win (session-active-window sess))
        "active window must switch to dst-win after empty src-win is killed")))

(test join-pane-kill-empty-src-noop-when-panes-remain
  "%join-pane-kill-empty-src is a no-op when src-window still has panes."
  (let* ((pane     (%make-test-pane :id 1))
         (src-win  (make-window :id 1 :name "src" :width 20 :height 5 :panes (list pane)))
         (sess     (make-session :id 1 :name "0" :windows (list src-win))))
    (session-select-window sess src-win)
    (cl-tmux/commands::%join-pane-kill-empty-src sess src-win)
    ;; Window must still be in the session.
    (is (member src-win (session-windows sess))
        "non-empty src-win must not be removed from session")))

;;; ── %join-pane-insert-into-dst direct tests ──────────────────────────────────

(test join-pane-insert-into-dst-returns-src-pane
  "%join-pane-insert-into-dst returns src-pane on successful insertion."
  (let* ((src-pane (%make-test-pane :id 10))
         (dst-pane (%make-test-pane :id 20))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :tree (make-layout-leaf dst-pane)
                                :panes (list dst-pane))))
    (window-select-pane dst-win dst-pane)
    (let ((result (cl-tmux/commands::%join-pane-insert-into-dst src-pane dst-win :h)))
      (is (eq src-pane result)
          "%join-pane-insert-into-dst must return src-pane on success"))))

(test join-pane-insert-into-dst-returns-nil-when-no-active-pane
  "%join-pane-insert-into-dst returns NIL when dst-window has no active pane."
  ;; window-active-pane falls back to (first (window-panes w)), so a window
  ;; truly has "no active pane" only when its pane list is empty.  Build dst-win
  ;; with no panes and no tree to exercise the NIL-return contract.
  (let* ((src-pane (%make-test-pane :id 10))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :tree nil :panes nil)))
    (is (null (cl-tmux/commands::%join-pane-insert-into-dst src-pane dst-win :h))
        "%join-pane-insert-into-dst must return NIL when dst has no active pane")))

;;; ── resize-pane: up direction ────────────────────────────────────────────────

(test resize-horizontal-up-shrinks-active-grows-upper
  "On a horizontal split, :up from the lower pane shrinks the active pane
   (moves its top border down) and grows the upper neighbour.
   This is symmetric with :left from the right pane shrinking the active pane."
  (let* ((win (%hsplit-window 10))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; Make p1 (lower) the active pane.
    (window-select-pane win p1)
    (is (eq p1 (resize-pane win :up 3)))
    (is (= 13 (pane-height p0)) "upper neighbour grows on :up from lower pane")
    (is (= 7  (pane-height p1)) "lower (active) pane shrinks on :up")))

;;; ── copy-mode-search-backward: saves term ────────────────────────────────────

(test copy-mode-search-backward-saves-term
  "copy-mode-search-backward saves the search term for n/N repeats."
  (let ((s (make-screen 30 5)))
    (feed s "foo bar foo")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
    (cl-tmux/commands::copy-mode-search-backward s "foo")
    (is (string= "foo" (cl-tmux/terminal/types:screen-copy-search-term s))
        "search term must be saved after search-backward")))

;;; ── copy-mode-search-prev: positive case ─────────────────────────────────────

(test copy-mode-search-prev-repeats-backward
  "copy-mode-search-prev uses the saved term to repeat backward search."
  ;; Use a two-row screen: row 0 = "abc", row 1 = "abc def"
  (let ((s (make-screen 30 5)))
    (feed s "abc")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "abc def")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Save term via forward search first
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    ;; Cursor should be on row 1 col 0 (second "abc")
    (is (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "precondition: forward search found second 'abc' on row 1")
    ;; Now search-prev should go back to row 0
    (cl-tmux/commands::copy-mode-search-prev s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-prev must find 'abc' on row 0")))

(test copy-mode-search-next-honors-backward-direction
  "n/N are relative to the LAST search heading, not hardcoded (audit #19): after a
   backward search (?), n continues BACKWARD and N reverses to forward."
  ;; Three rows, each containing "abc".
  (let ((s (make-screen 30 5)))
    (feed s "abc")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "abc")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "abc")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Backward search from row 2 finds the previous "abc" on row 1.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 0))
    (cl-tmux/commands::copy-mode-search-backward s "abc")
    (is (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "precondition: backward search lands on row 1")
    (is (eq :backward (cl-tmux/terminal/types:screen-copy-search-direction s))
        "? must record :backward as the search heading")
    ;; n repeats in the SAME (backward) direction → row 0.
    (cl-tmux/commands::copy-mode-search-next s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "n after ? must continue BACKWARD to row 0")
    (is (eq :backward (cl-tmux/terminal/types:screen-copy-search-direction s))
        "n must not overwrite the stored search direction")
    ;; N reverses to forward → returns to row 1.
    (cl-tmux/commands::copy-mode-search-prev s)
    (is (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "N after ? must reverse to FORWARD, returning to row 1")))

;;; ── %scroll-up-one-line direct tests ─────────────────────────────────────────

(test scroll-up-one-line-moves-cursor-up-within-viewport
  "%scroll-up-one-line decrements row when cursor is not at top of viewport."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Place cursor at row 3 (well within viewport, no scrollback needed)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 2))
    (cl-tmux/commands::%scroll-up-one-line s 3 2 0)
    (is (equal (cons 2 2) (cl-tmux/terminal/types:screen-copy-cursor s))
        "%scroll-up-one-line must decrement row when cursor is within viewport")))

(test scroll-up-one-line-scrolls-viewport-at-top-edge
  "%scroll-up-one-line scrolls the viewport when cursor is at row 0 and scrollback exists."
  (let ((s (%screen-with-scrollback 5)))
    ;; Place cursor at row 0 so the viewport needs to scroll
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
    (let ((before-offset (screen-copy-offset s)))
      (cl-tmux/commands::%scroll-up-one-line s 0 2 5)
      (is (= (1+ before-offset) (screen-copy-offset s))
          "%scroll-up-one-line must increment viewport offset at top edge")
      (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
          "cursor row must stay at 0 when viewport scrolls"))))

(test scroll-up-one-line-noop-at-oldest-scrollback
  "%scroll-up-one-line is a no-op when cursor is at row 0 and offset equals max."
  (let ((s (%screen-with-scrollback 3)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 3)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
    (cl-tmux/commands::%scroll-up-one-line s 0 2 3)
    (is (= 3 (screen-copy-offset s))
        "%scroll-up-one-line must not increment offset past max-offset")
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must remain 0")))

;;; ── %scroll-down-one-line direct tests ───────────────────────────────────────

(test scroll-down-one-line-moves-cursor-down-within-viewport
  "%scroll-down-one-line increments row when cursor is not at viewport bottom."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Place cursor at row 1 (within viewport)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 2))
    (cl-tmux/commands::%scroll-down-one-line s 1 2 5)
    (is (equal (cons 2 2) (cl-tmux/terminal/types:screen-copy-cursor s))
        "%scroll-down-one-line must increment row when cursor is within viewport")))

(test scroll-down-one-line-scrolls-viewport-at-bottom-edge
  "%scroll-down-one-line scrolls the viewport when cursor is at bottom and offset > 0."
  (let ((s (%screen-with-scrollback 10)))
    ;; Set offset > 0 so we can scroll forward
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 5)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 2))
    (cl-tmux/commands::%scroll-down-one-line s 4 2 5)
    (is (= 4 (screen-copy-offset s))
        "%scroll-down-one-line must decrement viewport offset at bottom edge")
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must stay at h-1 when viewport scrolls")))

(test scroll-down-one-line-noop-at-live-view-bottom
  "%scroll-down-one-line is a no-op when cursor is at the bottom and offset is 0."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 2))
    (cl-tmux/commands::%scroll-down-one-line s 4 2 5)
    (is (= 0 (screen-copy-offset s))
        "%scroll-down-one-line must not move past live view (offset must stay 0)")
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must remain 4")))

;;; ── %extract-row-chars direct tests ──────────────────────────────────────────

(test extract-row-chars-returns-substring-of-row
  "%extract-row-chars returns the correct string slice from the given row."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (let ((result (cl-tmux/commands::%extract-row-chars s 0 0 5)))
      (is (stringp result)
          "%extract-row-chars must return a string")
      (is (string= "hello" result)
          "%extract-row-chars must return cols 0-4 as \"hello\" (got ~S)" result))))

(test extract-row-chars-empty-range-returns-empty-string
  "%extract-row-chars with from-col = to-col returns an empty string."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (let ((result (cl-tmux/commands::%extract-row-chars s 0 3 3)))
      (is (string= "" result)
          "%extract-row-chars with empty range must return empty string"))))

;;; ── %copy-mode-word-at-cursor direct tests ──────────────────────────────────

(test copy-mode-word-bounds-returns-surrounding-word
  "%copy-mode-word-bounds expands to the full word under the cursor."
  (let* ((chars (coerce "foo bar baz" 'vector))
         (max-col (1- (length chars))))
    (multiple-value-bind (start end)
        (cl-tmux/commands::%copy-mode-word-bounds chars 5 max-col #'cl-tmux/commands::%word-separator-p)
      (is (= 4 start)
          "%copy-mode-word-bounds must start at the first character of the word")
      (is (= 6 end)
          "%copy-mode-word-bounds must end at the last character of the word"))))

(test copy-mode-word-bounds-keeps-separator-cell
  "%copy-mode-word-bounds keeps a separator cell unchanged."
  (let* ((chars (coerce "foo bar baz" 'vector))
         (max-col (1- (length chars))))
    (multiple-value-bind (start end)
        (cl-tmux/commands::%copy-mode-word-bounds chars 3 max-col #'cl-tmux/commands::%word-separator-p)
      (is (= 3 start)
          "%copy-mode-word-bounds must keep the separator start column")
      (is (= 3 end)
          "%copy-mode-word-bounds must keep the separator end column"))))

(test copy-mode-word-at-cursor-returns-surrounding-word
  "%copy-mode-word-at-cursor expands to the full word under the cursor."
  (let ((s (copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (is (string= "bar" (cl-tmux/commands::%copy-mode-word-at-cursor s))
        "%copy-mode-word-at-cursor must return the surrounding word")))

(test copy-mode-word-at-cursor-returns-single-separator-cell
  "%copy-mode-word-at-cursor returns a single separator cell when cursor lands on one."
  (let ((s (copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
    (is (string= " " (cl-tmux/commands::%copy-mode-word-at-cursor s))
        "%copy-mode-word-at-cursor must return the separator cell unchanged")))

(test copy-mode-word-at-cursor-noop-outside-copy-mode
  "%copy-mode-word-at-cursor returns NIL when copy mode is inactive."
  (let ((s (make-screen 20 5)))
    (feed s "foo bar baz")
    (is (null (cl-tmux/commands::%copy-mode-word-at-cursor s))
        "%copy-mode-word-at-cursor must be NIL outside copy mode")))

;;; ── %copy-row-range-to-paste-buffer direct tests ─────────────────────────────

(test copy-row-range-to-paste-buffer-adds-trimmed-text
  "%copy-row-range-to-paste-buffer pushes right-trimmed text to paste buffers."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::%copy-row-range-to-paste-buffer s 0 0 10)
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "one paste buffer entry must be added")
      (let ((got (cl-tmux/buffer:get-paste-buffer 0)))
        (is (string= "hello" got)
            "%copy-row-range-to-paste-buffer must push right-trimmed text (got ~S)" got)))))

(test copy-row-range-to-paste-buffer-noop-when-all-spaces
  "%copy-row-range-to-paste-buffer does nothing when the trimmed result is empty."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      ;; Row 0 is blank (all spaces) — the trimmed result will be empty.
      (cl-tmux/commands::%copy-row-range-to-paste-buffer s 0 0 10)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when the selected range is all spaces"))))

;;; ── %copy-mode-row-chars direct tests ────────────────────────────────────────

(test copy-mode-row-chars-returns-character-vector
  "%copy-mode-row-chars returns a simple-vector of characters for the given row."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (cl-tmux/commands::copy-mode-enter s)
    (let ((chars (cl-tmux/commands::%copy-mode-row-chars s 0)))
      (is (vectorp chars)
          "%copy-mode-row-chars must return a vector")
      (is (= 20 (length chars))
          "%copy-mode-row-chars vector length must equal screen-width")
      (is (char= #\h (aref chars 0))
          "first character must be #\\h"))))

;;; ── %screen-row-string and %scrollback-row-string direct tests ───────────────

(test screen-row-string-returns-full-row-as-string
  "%screen-row-string trims trailing blanks by default (capture-pane default) but
   returns the full width when TRIM is NIL (capture-pane -J)."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (let ((trimmed (cl-tmux/commands::%screen-row-string s 0))
          (full    (cl-tmux/commands::%screen-row-string s 0 nil)))
      (is (stringp trimmed) "%screen-row-string must return a string")
      (is (string= "hello" trimmed)
          "default trims the 15 trailing spaces down to the content")
      (is (= 20 (length full))
          "with TRIM NIL the row is the full screen-width (20)")
      (is (string= "hello" (subseq full 0 5))
          "the full-width row still has the fed text at cols 0-4"))))

(test scrollback-row-string-converts-cell-vector
  "%scrollback-row-string returns a string built from a cell vector."
  (let* ((cells (make-array 5 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\A :fg 7 :bg 0 :attrs 0 :width 1)))
         (result (cl-tmux/commands::%scrollback-row-string cells)))
    (is (stringp result)
        "%scrollback-row-string must return a string")
    (is (= 5 (length result))
        "%scrollback-row-string length must equal cell-vector length")
    (is (every (lambda (c) (char= #\A c)) (coerce result 'list))
        "%scrollback-row-string must extract char from each cell")))

;;; ── rename-session via hooks ─────────────────────────────────────────────────

(test rename-session-does-not-run-hooks
  "rename-session is a pure setter; it fires no hooks."
  (with-isolated-hooks
    (let ((hook-called nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+
                              (lambda (&rest _) (declare (ignore _)) (setf hook-called t)))
      (let ((sess (make-session :id 1 :name "old" :windows nil)))
        (cl-tmux/commands:rename-session sess "new"))
      (is-false hook-called
                "rename-session must not fire any hooks"))))
