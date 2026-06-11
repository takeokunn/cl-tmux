(in-package #:cl-tmux/test)

;;;; commands tests — part L: copy-mode-begin-line-selection multi-row,
;;;; copy-line right-trim, copy-end-of-line col-0, with-shell-timeout,
;;;; window-after-kill, kill-pane/kill-window hooks, copy-mode-toggle-rectangle,
;;;; copy-mode-append-selection, copy-mode-copy-pipe, rectangle-text, renumber-windows.

(in-suite commands-suite)

;;; ── copy-mode-begin-line-selection: multi-row window ────────────────────────

(test copy-mode-begin-line-selection-selects-correct-width
  "copy-mode-begin-line-selection marks col width-1 on a non-default screen width."
  (let ((s (make-screen 40 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::copy-mode-begin-line-selection s)
    (is (= 39 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be width-1=39 for 40-column screen")
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-mark s)))
        "mark col must be 0 for line selection")))

;;; ── copy-mode-copy-line: preserves content without trailing spaces ───────────

(test copy-mode-copy-line-right-trims-trailing-spaces
  "copy-mode-copy-line right-trims trailing spaces before pushing to paste buffer."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hi")          ; "hi" followed by 18 spaces on row 0
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-copy-line s)
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (string= "hi" yanked))
            "copy-mode-copy-line must right-trim spaces (got ~S)" yanked)))))

;;; ── copy-mode-copy-end-of-line: cursor at column 0 ──────────────────────────

(test copy-mode-copy-end-of-line-from-col-0-copies-entire-row
  "copy-mode-copy-end-of-line from col 0 copies the full row content."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (search "hello world" yanked))
            "D from col 0 must copy 'hello world' (got ~S)" yanked)))))

;;; ── with-shell-timeout macro coverage ───────────────────────────────────────

(test with-shell-timeout-returns-result-on-success
  "with-shell-timeout macro returns the result when thunk completes in time."
  (let ((result (cl-tmux/commands::with-shell-timeout (shell 30)
                  (string= "/bin/sh" shell)
                  42)))
    ;; result is the value of the last form in the body
    (is (= 42 result)
        "with-shell-timeout must return the last form result when no timeout")))

;;; ── %window-after-kill: empty list returns nil ───────────────────────────────

(test window-after-kill-empty-list-returns-nil
  "%window-after-kill with an empty remaining list returns NIL."
  (is (null (cl-tmux/commands::%window-after-kill nil 5))
      "%window-after-kill with empty list must return NIL"))

;;; ── kill-pane: fires hook ────────────────────────────────────────────────────

(test kill-pane-fires-after-kill-pane-hook
  "kill-pane fires +hook-after-kill-pane+ with the killed pane."
  (with-isolated-hooks
    (let ((hooked-pane nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                              (lambda (p) (setf hooked-pane p)))
      (let* ((win  (%vsplit-window 20))
             (p0   (first  (window-panes win)))
             (p1   (second (window-panes win)))
             (sess (make-session :id 1 :name "0" :windows (list win))))
        (session-select-window sess win)
        (window-select-pane win p0)
        (kill-pane sess p1)
        (is (eq p1 hooked-pane)
            "+hook-after-kill-pane+ must be called with the killed pane")))))

;;; ── kill-window: fires hook ──────────────────────────────────────────────────

(test kill-window-fires-after-kill-window-hook
  "kill-window fires +hook-after-kill-window+ with the killed window."
  (with-isolated-hooks
    (let ((hooked-win nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-window+
                              (lambda (w) (setf hooked-win w)))
      (let* ((p0   (%make-test-pane))
             (w1   (make-window :id 1 :name "a" :width 20 :height 5
                                :tree (make-layout-leaf p0) :panes (list p0)))
             (w2   (make-window :id 2 :name "b" :width 20 :height 5
                                :panes (list (%make-test-pane :id 2))))
             (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
        (session-select-window sess w1)
        (kill-window sess w1)
        (is (eq w1 hooked-win)
            "+hook-after-kill-window+ must be called with the killed window")))))

;;; ── copy-mode-toggle-rectangle ───────────────────────────────────────────────

(test copy-mode-toggle-rectangle-flips-flag
  "copy-mode-toggle-rectangle toggles screen-copy-rect-select-p between NIL and T."
  (let ((s (%copy-mode-screen)))
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must start NIL")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-true  (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must be T after first toggle")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must return to NIL after second toggle")))

(test copy-mode-toggle-rectangle-noop-outside-copy-mode
  "copy-mode-toggle-rectangle does nothing when not in copy mode."
  (let ((s (make-screen 20 5)))
    (is-false (screen-copy-mode-p s) "precondition: not in copy mode")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must remain NIL outside copy mode")))

(test copy-mode-exit-resets-rect-select
  "copy-mode-exit clears screen-copy-rect-select-p."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t)
    (cl-tmux/commands::copy-mode-exit s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must be NIL after exit")))

;;; ── copy-mode-append-selection ───────────────────────────────────────────────

(test copy-mode-append-selection-appends-to-existing-buffer
  "copy-mode-append-selection appends selected text to the current paste buffer entry."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    ;; Seed a buffer entry.
    (cl-tmux/buffer:add-paste-buffer "hello")
    (let ((s (make-screen 20 5)))
      (feed s " world")
      (cl-tmux/commands::copy-mode-enter s)
      ;; Manually set a selection spanning " world" on row 0.
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
      (cl-tmux/commands::copy-mode-append-selection s)
      ;; Exactly one buffer entry (appended, not pushed).
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "append-selection must not add a second paste buffer entry")
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (search "hello" buf))
            "appended buffer must contain original text")
        (is (and (stringp buf) (search " world" buf))
            "appended buffer must contain the newly appended text")))))

(test copy-mode-append-selection-creates-new-entry-when-empty
  "copy-mode-append-selection pushes a new entry when the paste buffer is empty."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection s)
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "append-selection must create one entry when buffer is empty")
      (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
          "new entry must equal the selected text"))))

(test copy-mode-append-selection-stays-in-copy-mode
  "copy-mode-append-selection must NOT exit copy mode (tmux append-selection stays in copy mode)."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection s)
      (is (cl-tmux/terminal/types:screen-copy-mode-p s)
          "append-selection must leave copy mode active"))))

(test copy-mode-append-selection-and-cancel-exits-copy-mode
  "copy-mode-append-selection-and-cancel exits copy mode after appending."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection-and-cancel s)
      (is (not (cl-tmux/terminal/types:screen-copy-mode-p s))
          "append-selection-and-cancel must exit copy mode")
      (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
          "buffer entry must be created"))))

;;; ── copy-mode-copy-pipe ──────────────────────────────────────────────────────

(test copy-mode-copy-pipe-puts-text-in-paste-buffer
  "copy-mode-copy-pipe adds the selected text to the paste buffer."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "pipe-me")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 7))
      ;; Pass an empty CMD so only the buffer side runs (no real shell invoked).
      (cl-tmux/commands::copy-mode-copy-pipe s "")
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "copy-pipe must push selected text to paste buffers")
      (is (string= "pipe-me" (cl-tmux/buffer:get-paste-buffer 0))
          "paste buffer must contain the selected text"))))

(test copy-mode-copy-pipe-exits-copy-mode
  "copy-mode-copy-pipe exits copy mode after yanking."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "data")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 4))
      (cl-tmux/commands::copy-mode-copy-pipe s "")
      (is-false (screen-copy-mode-p s)
                "copy mode must be inactive after copy-pipe"))))

;;; ── rectangle selection text ─────────────────────────────────────────────────

(test copy-mode-yank-rectangle-uses-fixed-columns
  "When rect-select is T, yank uses column bounds from mark and cursor on every row."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 10 5)))
      ;; Write row 0 "abcde" and row 1 "ABCDE" using CR+LF to ensure row 1 starts at col 0.
      (feed s (format nil "abcde~C~CABCDE" #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Rectangle col 1-3, rows 0-1.
      ;; %extract-row-chars from-col=1 to-col=3 → 2 chars at cols 1 and 2.
      ;; Row 0: "bc"; row 1: "BC".
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t
            (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 1)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
      (cl-tmux/commands::copy-mode-yank s)
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (search "bc" buf))
            "rectangle yank must include chars from first row")
        (is (and (stringp buf) (search "BC" buf))
            "rectangle yank must include chars from second row")))))

;;; ── renumber-windows option ───────────────────────────────────────────────────

(test renumber-windows-renumbers-after-kill
  "kill-window renumbers remaining windows from base-index when renumber-windows is on."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "renumber-windows" h) t
                 (gethash "base-index"       h) 0)
           h)))
    (let* ((s    (make-fake-session :nwindows 3))
           (wins (cl-tmux/model:session-windows s))
           ;; Manually give them non-contiguous IDs as if gaps already existed.
           (_ (setf (cl-tmux/model:window-id (first  wins)) 1
                    (cl-tmux/model:window-id (second wins)) 3
                    (cl-tmux/model:window-id (third  wins)) 5))
           ;; Kill the first window (id=1); remaining are 3 and 5.
           (_2 (kill-window s (first wins))))
      (declare (ignore _ _2))
      (let ((ids (mapcar #'cl-tmux/model:window-id (cl-tmux/model:session-windows s))))
        (is (equal '(0 1) ids)
            "After kill with renumber-windows, windows should be renumbered 0,1; got ~S" ids)))))

(test renumber-windows-off-preserves-ids
  "kill-window does not renumber windows when renumber-windows is off."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "renumber-windows" h) nil)
           h)))
    (let* ((s    (make-fake-session :nwindows 3))
           (wins (cl-tmux/model:session-windows s))
           (_ (setf (cl-tmux/model:window-id (first  wins)) 1
                    (cl-tmux/model:window-id (second wins)) 3
                    (cl-tmux/model:window-id (third  wins)) 5))
           (_2 (kill-window s (first wins))))
      (declare (ignore _ _2))
      (let ((ids (mapcar #'cl-tmux/model:window-id (cl-tmux/model:session-windows s))))
        (is (equal '(3 5) ids)
            "Without renumber-windows, IDs stay as-is; got ~S" ids)))))
