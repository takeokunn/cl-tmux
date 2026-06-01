(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/buffer: paste-buffer ring operations.

(def-suite buffer-suite :description "Paste buffer ring")
(in-suite buffer-suite)

;;; ── Helpers ─────────────────────────────────────────────────────────────────

(defmacro with-empty-buffers (&body body)
  "Run BODY with an empty paste buffer ring."
  `(let ((cl-tmux/buffer:*paste-buffers* nil)) ,@body))

;;; ── add + get round-trip ─────────────────────────────────────────────────────

(test add-and-get-buffer
  "add-paste-buffer then get-paste-buffer returns the added text."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "hello")
    (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0)))))

(test buffer-lifo-order
  "Most recently added buffer is returned as index 0."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "first")
    (cl-tmux/buffer:add-paste-buffer "second")
    (is (string= "second" (cl-tmux/buffer:get-paste-buffer 0))
        "index 0 must be most recently added")
    (is (string= "first"  (cl-tmux/buffer:get-paste-buffer 1))
        "index 1 must be previous buffer")))

(test buffer-get-nil-when-empty
  "get-paste-buffer returns NIL when *paste-buffers* is empty."
  (with-empty-buffers
    (is (null (cl-tmux/buffer:get-paste-buffer 0)))))

(test buffer-get-nil-out-of-range
  "get-paste-buffer returns NIL for an index past the end."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "only")
    (is (null (cl-tmux/buffer:get-paste-buffer 5)))))

(test buffer-delete-removes-entry
  "delete-paste-buffer removes the buffer at the given index."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "a")
    (cl-tmux/buffer:add-paste-buffer "b")  ; b is index 0, a is index 1
    (cl-tmux/buffer:delete-paste-buffer 0)
    (is (= 1 (length (cl-tmux/buffer:list-paste-buffers))))
    (is (string= "a" (cl-tmux/buffer:get-paste-buffer 0)))))

(test buffer-delete-returns-t-on-success
  "delete-paste-buffer returns T when the index is valid."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "x")
    (is-true (cl-tmux/buffer:delete-paste-buffer 0))))

(test buffer-delete-returns-nil-out-of-range
  "delete-paste-buffer returns NIL when the index is out of range."
  (with-empty-buffers
    (is-false (cl-tmux/buffer:delete-paste-buffer 0))))

(test buffer-clear-empties-ring
  "clear-paste-buffers sets *paste-buffers* to NIL."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "a")
    (cl-tmux/buffer:add-paste-buffer "b")
    (cl-tmux/buffer:clear-paste-buffers)
    (is (null (cl-tmux/buffer:list-paste-buffers)))))

(test buffer-list-returns-copy
  "list-paste-buffers returns a fresh list, not the internal one."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "x")
    (let ((lst (cl-tmux/buffer:list-paste-buffers)))
      (setf (car lst) "mutated")
      (is (string= "x" (cl-tmux/buffer:get-paste-buffer 0))
          "mutation of list must not affect internal buffer"))))
