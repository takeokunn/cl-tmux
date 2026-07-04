(in-package #:cl-tmux/test)
(in-suite buffer-suite)

;;; ── list-paste-buffers-with-names ────────────────────────────────────────────

(test list-paste-buffers-with-names-returns-name-text-pairs
  "list-paste-buffers-with-names returns (NAME . TEXT) conses, most recent first."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "one" "first")
    (cl-tmux/buffer:add-paste-buffer "two" "second")
    (let ((entries (cl-tmux/buffer:list-paste-buffers-with-names)))
      (is (= 2 (length entries)) "must have one entry per buffer")
      (is (equal '("second" . "two") (first entries))
          "most recent buffer must come first, as (NAME . TEXT)"))))

(test list-paste-buffers-with-names-empty-ring
  "list-paste-buffers-with-names returns NIL for an empty ring."
  (with-empty-buffers
    (is (null (cl-tmux/buffer:list-paste-buffers-with-names))
        "empty ring must yield NIL")))

(test list-paste-buffers-with-names-returns-copy
  "list-paste-buffers-with-names returns a fresh alist; mutating it does not
   affect the internal buffer ring."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "x" "name1")
    (let ((entries (cl-tmux/buffer:list-paste-buffers-with-names)))
      (setf (car entries) (cons "mutated" "mutated"))
      (is (string= "x" (cl-tmux/buffer:get-named-buffer "name1"))
          "mutating the returned alist must not affect the internal ring"))))

;;; ── Named buffers (set-buffer -b / paste-buffer -b) ──────────────────────────

(test named-buffer-set-and-get-by-name
  "set-named-buffer stores under a name; get-named-buffer retrieves it."
  (with-empty-buffers
    (cl-tmux/buffer:set-named-buffer "foo" "hello")
    (is (string= "hello" (cl-tmux/buffer:get-named-buffer "foo"))
        "named buffer is retrievable by name")
    (is (null (cl-tmux/buffer:get-named-buffer "bar"))
        "an unknown name returns NIL")))

(test named-buffer-same-name-replaces-in-place
  "Setting an existing name replaces its content without adding a duplicate."
  (with-empty-buffers
    (cl-tmux/buffer:set-named-buffer "foo" "first")
    (cl-tmux/buffer:set-named-buffer "foo" "second")
    (is (string= "second" (cl-tmux/buffer:get-named-buffer "foo"))
        "same name replaces the content")
    (is (= 1 (length (cl-tmux/buffer:list-paste-buffers)))
        "no duplicate entry is created")))

(test named-buffer-delete-by-name
  "delete-buffer-by-name removes a named buffer and reports whether it existed."
  (with-empty-buffers
    (cl-tmux/buffer:set-named-buffer "foo" "x")
    (is (cl-tmux/buffer:delete-buffer-by-name "foo") "delete returns T when present")
    (is (null (cl-tmux/buffer:get-named-buffer "foo")) "gone after delete")
    (is (null (cl-tmux/buffer:delete-buffer-by-name "foo"))
        "deleting an absent name returns NIL")))

(test add-paste-buffer-auto-names-and-public-api
  "add-paste-buffer with no name auto-assigns a name; the public string API is
   unchanged (get/list return text, not (name . text) pairs)."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "a")
    (cl-tmux/buffer:add-paste-buffer "b")
    (is (string= "b" (cl-tmux/buffer:get-paste-buffer 0)) "get returns text")
    (is (equal '("b" "a") (cl-tmux/buffer:list-paste-buffers))
        "list returns texts, most recent first")
    (is (= 2 (length (cl-tmux/buffer:buffer-names))) "two auto-named buffers")
    (is (every #'stringp (cl-tmux/buffer:buffer-names)) "names are strings")))

;;; ── rename-paste-buffer direct unit tests ───────────────────────────────────
;;;
;;; These tests cover the cases identified in the audit:
;;;   1. Rename with an explicit source name
;;;   2. Rename when source-name is NIL (rename the most recent buffer)
;;;   3. Source and target names are the same (no-op, returns text)
;;;   4. Source does not exist (returns NIL)

(test rename-paste-buffer-renames-by-name
  "rename-paste-buffer renames the named buffer and returns its text."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "content" "old")
    (let ((result (cl-tmux/buffer:rename-paste-buffer "old" "new")))
      (is (string= "content" result)
          "rename must return the preserved text")
      (is (null (cl-tmux/buffer:get-named-buffer "old"))
          "old name must be absent after rename")
      (is (string= "content" (cl-tmux/buffer:get-named-buffer "new"))
          "text must be accessible under the new name"))))

(test rename-paste-buffer-nil-source-renames-most-recent
  "rename-paste-buffer with NIL source renames the most recent buffer."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "older")
    (cl-tmux/buffer:add-paste-buffer "newest")
    (let ((result (cl-tmux/buffer:rename-paste-buffer nil "myname")))
      (is (string= "newest" result)
          "must return the text of the most recent buffer")
      (is (string= "newest" (cl-tmux/buffer:get-named-buffer "myname"))
          "most recent buffer must now be accessible as 'myname'")
      (is (string= "older" (cl-tmux/buffer:get-paste-buffer
                             (1- (length (cl-tmux/buffer:list-paste-buffers)))))
          "older buffer must remain unchanged"))))

(test rename-paste-buffer-same-name-is-noop
  "rename-paste-buffer returns the text without reordering when source = target."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "value" "foo")
    (let ((result (cl-tmux/buffer:rename-paste-buffer "foo" "foo")))
      (is (string= "value" result)
          "same-name rename must return the text")
      (is (string= "value" (cl-tmux/buffer:get-named-buffer "foo"))
          "buffer must still exist under the same name")
      (is (= 1 (length (cl-tmux/buffer:list-paste-buffers)))
          "must not create a duplicate entry"))))

(test rename-paste-buffer-absent-source-returns-nil
  "rename-paste-buffer returns NIL when the source buffer does not exist."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "other" "something")
    (let ((result (cl-tmux/buffer:rename-paste-buffer "nonexistent" "newname")))
      (is (null result)
          "absent source must return NIL")
      (is (null (cl-tmux/buffer:get-named-buffer "newname"))
          "target name must not appear when source was absent"))))
