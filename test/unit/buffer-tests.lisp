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

;;; ── buffer-limit enforcement ─────────────────────────────────────────────────

(test buffer-limit-enforced-via-option
  "add-paste-buffer trims the ring to the buffer-limit option."
  (with-empty-buffers
    ;; Temporarily override the option to a small limit.
    (let ((saved (cl-tmux/options:get-option "buffer-limit")))
      (cl-tmux/options:set-option "buffer-limit" 3)
      (unwind-protect
           (progn
             (cl-tmux/buffer:add-paste-buffer "a")
             (cl-tmux/buffer:add-paste-buffer "b")
             (cl-tmux/buffer:add-paste-buffer "c")
             (cl-tmux/buffer:add-paste-buffer "d")  ; should evict "a"
             (is (= 3 (length (cl-tmux/buffer:list-paste-buffers)))
                 "ring must not grow beyond buffer-limit")
             (is (string= "d" (cl-tmux/buffer:get-paste-buffer 0)))
             (is (string= "c" (cl-tmux/buffer:get-paste-buffer 1)))
             (is (string= "b" (cl-tmux/buffer:get-paste-buffer 2)))
             (is (null (cl-tmux/buffer:get-paste-buffer 3))
                 "oldest buffer must have been evicted"))
        (cl-tmux/options:set-option "buffer-limit" saved)))))

(test buffer-limit-default-50
  "Without a configured limit, at most 50 entries are kept."
  (with-empty-buffers
    (dotimes (i 55)
      (cl-tmux/buffer:add-paste-buffer (format nil "buf~D" i)))
    (is (<= (length (cl-tmux/buffer:list-paste-buffers)) 50)
        "default limit must cap ring at 50 entries")))

(test buffer-delete-middle-index
  "delete-paste-buffer at a non-zero index removes the correct entry."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "a")
    (cl-tmux/buffer:add-paste-buffer "b")
    (cl-tmux/buffer:add-paste-buffer "c")  ; c=0, b=1, a=2
    (cl-tmux/buffer:delete-paste-buffer 1)  ; remove b
    (is (= 2 (length (cl-tmux/buffer:list-paste-buffers))))
    (is (string= "c" (cl-tmux/buffer:get-paste-buffer 0)))
    (is (string= "a" (cl-tmux/buffer:get-paste-buffer 1)))))

(test buffer-add-returns-text
  "add-paste-buffer returns the inserted text."
  (with-empty-buffers
    (is (string= "hello" (cl-tmux/buffer:add-paste-buffer "hello")))))

;;; ── %buffer-limit fallback (ignore-errors path) ─────────────────────────────

(test buffer-limit-fallback-when-options-error
  "When get-option signals a condition, %buffer-limit falls back to 50."
  ;; Verify the fallback by shadowing *global-options* with a hash table that
  ;; causes get-option to return NIL (unregistered option name), which exercises
  ;; the (or (ignore-errors ...) 50) fallback in %buffer-limit.
  ;; We use with-isolated-options with an intentionally wrong type value so that
  ;; the coercion does not signal; instead we temporarily remove buffer-limit
  ;; from the options hash so get-option returns NIL.
  (let ((empty-ht (make-hash-table :test #'equal)))
    (let ((cl-tmux/options:*global-options* empty-ht))
      ;; With no buffer-limit key in *global-options*, get-option returns NIL.
      ;; %buffer-limit must then return 50 (the hardcoded default).
      (is (= 50 (cl-tmux/buffer::%buffer-limit))
          "%buffer-limit must return 50 when get-option returns NIL"))))
