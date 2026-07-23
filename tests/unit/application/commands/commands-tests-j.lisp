(in-package #:cl-tmux/test)

;;;; commands tests — part J: join-pane helpers, resize-pane directions,
;;;; copy-mode word/bottom noop, search helpers, scroll helpers,
;;;; extract-chars, copy-row-range, screen-row-string, rename-session hooks.

(describe "commands-suite"

  ;;; ── %join-pane-kill-empty-src direct tests ───────────────────────────────────

  ;; %join-pane-kill-empty-src removes a window with no panes from the session.
  (it "join-pane-kill-empty-src-removes-empty-window-from-session"
    (let* ((src-win  (make-window :id 1 :name "src" :width 20 :height 5 :panes nil))
           (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                  :panes (list (%make-test-pane :id 1))))
           (sess     (make-session :id 1 :name "0" :windows (list src-win dst-win))))
      (session-select-window sess src-win)
      (cl-tmux/commands::%join-pane-kill-empty-src sess src-win)
      (expect (member src-win (session-windows sess)) :to-be-falsy)
      ;; Active window switches to the remaining window.
      (expect (eq dst-win (session-active-window sess)))))

  ;; %join-pane-kill-empty-src is a no-op when src-window still has panes.
  (it "join-pane-kill-empty-src-noop-when-panes-remain"
    (let* ((pane     (%make-test-pane :id 1))
           (src-win  (make-window :id 1 :name "src" :width 20 :height 5 :panes (list pane)))
           (sess     (make-session :id 1 :name "0" :windows (list src-win))))
      (session-select-window sess src-win)
      (cl-tmux/commands::%join-pane-kill-empty-src sess src-win)
      ;; Window must still be in the session.
      (expect (member src-win (session-windows sess)))))

  ;;; ── %join-pane-insert-into-dst direct tests ──────────────────────────────────

  ;; %join-pane-insert-into-dst returns src-pane on successful insertion.
  (it "join-pane-insert-into-dst-returns-src-pane"
    (let* ((src-pane (%make-test-pane :id 10))
           (dst-pane (%make-test-pane :id 20))
           (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                  :tree (make-layout-leaf dst-pane)
                                  :panes (list dst-pane))))
      (window-select-pane dst-win dst-pane)
      (let ((result (cl-tmux/commands::%join-pane-insert-into-dst src-pane dst-win :h)))
        (expect (eq src-pane result)))))

  ;; %join-pane-insert-into-dst returns NIL when dst-window has no active pane.
  (it "join-pane-insert-into-dst-returns-nil-when-no-active-pane"
    ;; window-active-pane falls back to (first (window-panes w)), so a window
    ;; truly has "no active pane" only when its pane list is empty.  Build dst-win
    ;; with no panes and no tree to exercise the NIL-return contract.
    (let* ((src-pane (%make-test-pane :id 10))
           (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                  :tree nil :panes nil)))
      (expect (null (cl-tmux/commands::%join-pane-insert-into-dst src-pane dst-win :h)))))

  ;;; ── resize-pane: up direction ────────────────────────────────────────────────

  ;; On a horizontal split, :up from the lower pane shrinks the active pane
  ;; (moves its top border down) and grows the upper neighbour.
  ;; This is symmetric with :left from the right pane shrinking the active pane.
  (it "resize-horizontal-up-shrinks-active-grows-upper"
    (let* ((win (%hsplit-window 10))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      ;; Make p1 (lower) the active pane.
      (window-select-pane win p1)
      (expect (eq p1 (resize-pane win :up 3)))
      (expect (= 13 (pane-height p0)))
      (expect (= 7  (pane-height p1)))))

  ;;; ── copy-mode-search-backward: saves term ────────────────────────────────────

  ;; copy-mode-search-backward saves the search term for n/N repeats.
  (it "copy-mode-search-backward-saves-term"
    (let ((s (make-screen 30 5)))
      (feed s "foo bar foo")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
      (cl-tmux/commands::copy-mode-search-backward s "foo")
      (expect (string= "foo" (cl-tmux/terminal/types:screen-copy-search-term s)))))

  ;;; ── copy-mode-search-prev: positive case ─────────────────────────────────────

  ;; copy-mode-search-prev uses the saved term to repeat backward search.
  (it "copy-mode-search-prev-repeats-backward"
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
      (expect (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      ;; Now search-prev should go back to row 0
      (cl-tmux/commands::copy-mode-search-prev s)
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; n/N are relative to the LAST search heading, not hardcoded (audit #19): after a
  ;; backward search (?), n continues BACKWARD and N reverses to forward.
  (it "copy-mode-search-next-honors-backward-direction"
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
      (expect (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (eq :backward (cl-tmux/terminal/types:screen-copy-search-direction s)))
      ;; n repeats in the SAME (backward) direction → row 0.
      (cl-tmux/commands::copy-mode-search-next s)
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (eq :backward (cl-tmux/terminal/types:screen-copy-search-direction s)))
      ;; N reverses to forward → returns to row 1.
      (cl-tmux/commands::copy-mode-search-prev s)
      (expect (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;;; ── %scroll-up-one-line direct tests ─────────────────────────────────────────

  ;; %scroll-up-one-line decrements row when cursor is not at top of viewport.
  (it "scroll-up-one-line-moves-cursor-up-within-viewport"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Place cursor at row 3 (well within viewport, no scrollback needed)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 2))
      (cl-tmux/commands::%scroll-up-one-line s 3 2 0)
      (expect (equal (cons 2 2) (cl-tmux/terminal/types:screen-copy-cursor s)))))

  ;; %scroll-up-one-line scrolls the viewport when cursor is at row 0 and scrollback exists.
  (it "scroll-up-one-line-scrolls-viewport-at-top-edge"
    (let ((s (%screen-with-scrollback 5)))
      ;; Place cursor at row 0 so the viewport needs to scroll
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
      (let ((before-offset (screen-copy-offset s)))
        (cl-tmux/commands::%scroll-up-one-line s 0 2 5)
        (expect (= (1+ before-offset) (screen-copy-offset s)))
        (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))))))

  ;; %scroll-up-one-line is a no-op when cursor is at row 0 and offset equals max.
  (it "scroll-up-one-line-noop-at-oldest-scrollback"
    (let ((s (%screen-with-scrollback 3)))
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 3)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
      (cl-tmux/commands::%scroll-up-one-line s 0 2 3)
      (expect (= 3 (screen-copy-offset s)))
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;;; ── %scroll-down-one-line direct tests ───────────────────────────────────────

  ;; %scroll-down-one-line increments row when cursor is not at viewport bottom.
  (it "scroll-down-one-line-moves-cursor-down-within-viewport"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Place cursor at row 1 (within viewport)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 2))
      (cl-tmux/commands::%scroll-down-one-line s 1 2 5)
      (expect (equal (cons 2 2) (cl-tmux/terminal/types:screen-copy-cursor s)))))

  ;; %scroll-down-one-line scrolls the viewport when cursor is at bottom and offset > 0.
  (it "scroll-down-one-line-scrolls-viewport-at-bottom-edge"
    (let ((s (%screen-with-scrollback 10)))
      ;; Set offset > 0 so we can scroll forward
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 5)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 2))
      (cl-tmux/commands::%scroll-down-one-line s 4 2 5)
      (expect (= 4 (screen-copy-offset s)))
      (expect (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; %scroll-down-one-line is a no-op when cursor is at the bottom and offset is 0.
  (it "scroll-down-one-line-noop-at-live-view-bottom"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 2))
      (cl-tmux/commands::%scroll-down-one-line s 4 2 5)
      (expect (= 0 (screen-copy-offset s)))
      (expect (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;;; ── %extract-row-chars direct tests ──────────────────────────────────────────

  ;; %extract-row-chars returns the correct string slice from the given row.
  (it "extract-row-chars-returns-substring-of-row"
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (let ((result (cl-tmux/commands::%extract-row-chars s 0 0 5)))
        (expect (stringp result))
        (expect (string= "hello" result)))))

  ;; %extract-row-chars with from-col = to-col returns an empty string.
  (it "extract-row-chars-empty-range-returns-empty-string"
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (let ((result (cl-tmux/commands::%extract-row-chars s 0 3 3)))
        (expect (string= "" result)))))

  ;;; ── %copy-mode-word-at-cursor direct tests ──────────────────────────────────

  ;; %copy-mode-word-bounds expands to the full word under the cursor.
  (it "copy-mode-word-bounds-returns-surrounding-word"
    (let* ((chars (coerce "foo bar baz" 'vector))
           (max-col (1- (length chars))))
      (multiple-value-bind (start end)
          (cl-tmux/commands::%copy-mode-word-bounds chars 5 max-col #'cl-tmux/commands::%word-separator-p)
        (expect (= 4 start))
        (expect (= 6 end)))))

  ;; %copy-mode-word-bounds keeps a separator cell unchanged.
  (it "copy-mode-word-bounds-keeps-separator-cell"
    (let* ((chars (coerce "foo bar baz" 'vector))
           (max-col (1- (length chars))))
      (multiple-value-bind (start end)
          (cl-tmux/commands::%copy-mode-word-bounds chars 3 max-col #'cl-tmux/commands::%word-separator-p)
        (expect (= 3 start))
        (expect (= 3 end)))))

  ;; %copy-mode-word-at-cursor expands to the full word under the cursor.
  (it "copy-mode-word-at-cursor-returns-surrounding-word"
    (let ((s (copy-mode-screen :content "foo bar baz")))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (expect (string= "bar" (cl-tmux/commands::%copy-mode-word-at-cursor s)))))

  ;; %copy-mode-word-at-cursor returns a single separator cell when cursor lands on one.
  (it "copy-mode-word-at-cursor-returns-single-separator-cell"
    (let ((s (copy-mode-screen :content "foo bar baz")))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
      (expect (string= " " (cl-tmux/commands::%copy-mode-word-at-cursor s)))))

  ;; %copy-mode-word-at-cursor returns NIL when copy mode is inactive.
  (it "copy-mode-word-at-cursor-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (feed s "foo bar baz")
      (expect (null (cl-tmux/commands::%copy-mode-word-at-cursor s)))))

  ;;; ── %copy-row-range-to-paste-buffer direct tests ─────────────────────────────

  ;; %copy-row-range-to-paste-buffer pushes right-trimmed text to paste buffers.
  (it "copy-row-range-to-paste-buffer-adds-trimmed-text"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (cl-tmux/commands::%copy-row-range-to-paste-buffer s 0 0 10)
        (expect (= 1 (length cl-tmux/buffer:*paste-buffers*)))
        (let ((got (cl-tmux/buffer:get-paste-buffer 0)))
          (expect (string= "hello" got))))))

  ;; %copy-row-range-to-paste-buffer does nothing when the trimmed result is empty.
  (it "copy-row-range-to-paste-buffer-noop-when-all-spaces"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        ;; Row 0 is blank (all spaces) — the trimmed result will be empty.
        (cl-tmux/commands::%copy-row-range-to-paste-buffer s 0 0 10)
        (expect (null cl-tmux/buffer:*paste-buffers*)))))

  ;;; ── %copy-mode-row-chars direct tests ────────────────────────────────────────

  ;; %copy-mode-row-chars returns a simple-vector of characters for the given row.
  (it "copy-mode-row-chars-returns-character-vector"
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (let ((chars (cl-tmux/commands::%copy-mode-row-chars s 0)))
        (expect (vectorp chars))
        (expect (= 20 (length chars)))
        (expect (char= #\h (aref chars 0))))))

  ;;; ── %screen-row-string and %scrollback-row-string direct tests ───────────────

  ;; %screen-row-string trims trailing blanks by default (capture-pane default) but
  ;; returns the full width when TRIM is NIL (capture-pane -J).
  (it "screen-row-string-returns-full-row-as-string"
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (let ((trimmed (cl-tmux/commands::%screen-row-string s 0))
            (full    (cl-tmux/commands::%screen-row-string s 0 nil)))
        (expect (stringp trimmed))
        (expect (string= "hello" trimmed))
        (expect (= 20 (length full)))
        (expect (string= "hello" (subseq full 0 5))))))

  ;; %scrollback-row-string returns a string built from a cell vector.
  (it "scrollback-row-string-converts-cell-vector"
    (let* ((cells (make-array 5 :initial-element
                               (cl-tmux/terminal/types:make-cell
                                :char #\A :fg 7 :bg 0 :attrs 0 :width 1)))
           (result (cl-tmux/commands::%scrollback-row-string cells)))
      (expect (stringp result))
      (expect (= 5 (length result)))
      (expect (every (lambda (c) (char= #\A c)) (coerce result 'list)))))

  ;;; ── rename-session via hooks ─────────────────────────────────────────────────

  ;; rename-session is a pure setter; it fires no hooks.
  (it "rename-session-does-not-run-hooks"
    (with-isolated-hooks
      (let ((hook-called nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+
                                (lambda (&rest _) (declare (ignore _)) (setf hook-called t)))
        (let ((sess (make-session :id 1 :name "old" :windows nil)))
          (cl-tmux/commands:rename-session sess "new"))
        (expect hook-called :to-be-falsy)))))
