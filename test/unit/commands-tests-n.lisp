(in-package #:cl-tmux/test)

;;;; Commands tests — part XIV: copy-mode-begin-selection multi-row, yank, other-end.

(in-suite commands-suite)

;;; ── copy-mode-begin-selection and copy-mode-yank ────────────────────────────

(defun %copy-mode-screen (&key (w 20) (h 5) (content ""))
  "Return a copy-mode screen pre-filled with CONTENT (no PTY required)."
  (let ((s (make-screen w h)))
    (unless (string= content "")
      (feed s content))
    (cl-tmux/commands::copy-mode-enter s)
    s))

(test copy-mode-begin-selection-sets-selecting-flag
  "copy-mode-begin-selection sets screen-copy-selecting to T and places mark at cursor."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-begin-selection s)
    (is-true  (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be T after begin-selection")
    (is (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be placed at the cursor position on begin-selection")))

(test copy-mode-begin-selection-noop-outside-copy-mode
  "copy-mode-begin-selection is a no-op when copy mode is not active."
  (let ((s (make-screen 20 5)))
    ;; Do NOT enter copy mode — screen-copy-mode-p is NIL.
    (cl-tmux/commands::copy-mode-begin-selection s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selecting must remain NIL when not in copy mode")))

(test copy-mode-yank-pushes-text-to-paste-buffers
  "copy-mode-yank copies the selected region to *paste-buffers* and exits copy mode."
  ;; Use a small screen so we can predict cell content precisely.
  ;; Feed "hello" to row 0; mark at col 0 row 0, cursor at col 4 row 0.
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      ;; mark at col 0, cursor at col 5 (exclusive end), both on row 0
      ;; → the copy loop runs col from 0 below 5 → "hello"
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-yank s)
      ;; Copy mode must be deactivated after yank.
      (is-false (screen-copy-mode-p s) "copy mode must exit after yank")
      ;; Selection must be cleared.
      (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
                "copy-selecting must be NIL after yank")
      ;; Text must have landed in *paste-buffers*.
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "exactly one paste buffer entry must be present after yank")
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (string= "hello" yanked)
            "yanked text must equal the selected content \"hello\" (got ~S)" yanked)))))

(test copy-mode-yank-enqueues-osc52-when-set-clipboard-on
  "With set-clipboard on (tmux default), copy-mode-yank enqueues an OSC 52
   sequence on the screen's clipboard-queue so the renderer copies the selection
   to the host system clipboard."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (with-isolated-options ("set-clipboard" "on")
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
              (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 0)
              (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
        (cl-tmux/commands::copy-mode-yank s)
        (let ((q (cl-tmux/terminal/types:screen-clipboard-queue s)))
          (is (= 1 (length q)) "exactly one OSC 52 sequence enqueued")
          (is (search "]52;c;" (first q)) "the sequence is an OSC 52 clipboard set")
          (is (search "aGVsbG8=" (first q)) "encodes the yanked text (base64 of hello)"))))))

(test copy-mode-yank-no-osc52-when-set-clipboard-off
  "With set-clipboard off, copy-mode-yank does NOT enqueue an OSC 52 sequence."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (with-isolated-options ("set-clipboard" "off")
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
              (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 0)
              (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
        (cl-tmux/commands::copy-mode-yank s)
        (is (null (cl-tmux/terminal/types:screen-clipboard-queue s))
            "no OSC 52 enqueued when set-clipboard is off")))))

(test copy-mode-yank-noop-when-no-selection
  "copy-mode-yank with no active selection does not push to *paste-buffers*."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%copy-mode-screen :content "data")))
      ;; Ensure no selection is active.
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
      (cl-tmux/commands::copy-mode-yank s)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when no selection was active"))))

(test copy-mode-cancel-selection-clears-all-state
  "copy-mode-cancel-selection resets mark, cursor, and selecting flag."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 1 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 5))
    (cl-tmux/commands::copy-mode-cancel-selection s)
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "copy-mark must be NIL after cancel")
    (is (null (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-cursor must be NIL after cancel")
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be NIL after cancel")))

;;; ── copy-mode-other-end ──────────────────────────────────────────────────────

(test copy-mode-other-end-swaps-cursor-and-mark
  "copy-mode-other-end exchanges the cursor and mark ends of the selection."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (cl-tmux/commands::copy-mode-other-end s)
    (is (equal (cons 0 2) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must take the former mark end")
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must take the former cursor end")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after other-end")))

(test copy-mode-other-end-no-op-when-not-selecting
  "copy-mode-other-end is a harmless no-op when no selection is active."
  (let ((s (%copy-mode-screen)))
    ;; No selection: selecting NIL, mark/cursor stay as set by copy-mode-enter.
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 3))
    (finishes (cl-tmux/commands::copy-mode-other-end s))
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL when not selecting")
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged when not selecting")))

(test copy-mode-other-end-no-op-when-mark-nil
  "copy-mode-other-end does not swap (and stays clean) when mark is NIL even
   though selecting is T — guards against a half-initialised selection."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 4)
          (cl-tmux/terminal/types:screen-dirty-p        s) nil)
    (finishes (cl-tmux/commands::copy-mode-other-end s))
    (is (equal (cons 0 4) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged when mark is NIL")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL")
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "screen must not be marked dirty when no swap occurs")))
