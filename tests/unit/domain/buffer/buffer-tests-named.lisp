(in-package #:cl-tmux/test)

(describe "buffer-suite"

  ;;; ── list-paste-buffers-with-names ────────────────────────────────────────────

  ;; list-paste-buffers-with-names returns (NAME . TEXT) conses, most recent first.
  (it "list-paste-buffers-with-names-returns-name-text-pairs"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "one" "first")
      (cl-tmux/buffer:add-paste-buffer "two" "second")
      (let ((entries (cl-tmux/buffer:list-paste-buffers-with-names)))
        (expect (= 2 (length entries)))
        (expect (equal '("second" . "two") (first entries))))))

  ;; list-paste-buffers-with-names returns NIL for an empty ring.
  (it "list-paste-buffers-with-names-empty-ring"
    (with-empty-buffers
      (expect (null (cl-tmux/buffer:list-paste-buffers-with-names)))))

  ;; list-paste-buffers-with-names returns a fresh alist; mutating it does not
  ;; affect the internal buffer ring.
  (it "list-paste-buffers-with-names-returns-copy"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "x" "name1")
      (let ((entries (cl-tmux/buffer:list-paste-buffers-with-names)))
        (setf (car entries) (cons "mutated" "mutated"))
        (expect (string= "x" (cl-tmux/buffer:get-named-buffer "name1"))))))

  ;;; ── Named buffers (set-buffer -b / paste-buffer -b) ──────────────────────────

  ;; set-named-buffer stores under a name; get-named-buffer retrieves it.
  (it "named-buffer-set-and-get-by-name"
    (with-empty-buffers
      (cl-tmux/buffer:set-named-buffer "foo" "hello")
      (expect (string= "hello" (cl-tmux/buffer:get-named-buffer "foo")))
      (expect (null (cl-tmux/buffer:get-named-buffer "bar")))))

  ;; Setting an existing name replaces its content without adding a duplicate.
  (it "named-buffer-same-name-replaces-in-place"
    (with-empty-buffers
      (cl-tmux/buffer:set-named-buffer "foo" "first")
      (cl-tmux/buffer:set-named-buffer "foo" "second")
      (expect (string= "second" (cl-tmux/buffer:get-named-buffer "foo")))
      (expect (= 1 (length (cl-tmux/buffer:list-paste-buffers))))))

  ;; delete-buffer-by-name removes a named buffer and reports whether it existed.
  (it "named-buffer-delete-by-name"
    (with-empty-buffers
      (cl-tmux/buffer:set-named-buffer "foo" "x")
      (expect (cl-tmux/buffer:delete-buffer-by-name "foo"))
      (expect (null (cl-tmux/buffer:get-named-buffer "foo")))
      (expect (null (cl-tmux/buffer:delete-buffer-by-name "foo")))))

  ;; add-paste-buffer with no name auto-assigns a name; the public string API is
  ;; unchanged (get/list return text, not (name . text) pairs).
  (it "add-paste-buffer-auto-names-and-public-api"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "a")
      (cl-tmux/buffer:add-paste-buffer "b")
      (expect (string= "b" (cl-tmux/buffer:get-paste-buffer 0)))
      (expect (equal '("b" "a") (cl-tmux/buffer:list-paste-buffers)))
      (expect (= 2 (length (cl-tmux/buffer:buffer-names))))
      (expect (every #'stringp (cl-tmux/buffer:buffer-names)))))

  ;;; ── rename-paste-buffer direct unit tests ───────────────────────────────────
  ;;;
  ;;; These tests cover the cases identified in the audit:
  ;;;   1. Rename with an explicit source name
  ;;;   2. Rename when source-name is NIL (rename the most recent buffer)
  ;;;   3. Source and target names are the same (no-op, returns text)
  ;;;   4. Source does not exist (returns NIL)

  ;; rename-paste-buffer renames the named buffer and returns its text.
  (it "rename-paste-buffer-renames-by-name"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "content" "old")
      (let ((result (cl-tmux/buffer:rename-paste-buffer "old" "new")))
        (expect (string= "content" result))
        (expect (null (cl-tmux/buffer:get-named-buffer "old")))
        (expect (string= "content" (cl-tmux/buffer:get-named-buffer "new"))))))

  ;; rename-paste-buffer with NIL source renames the most recent buffer.
  (it "rename-paste-buffer-nil-source-renames-most-recent"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "older")
      (cl-tmux/buffer:add-paste-buffer "newest")
      (let ((result (cl-tmux/buffer:rename-paste-buffer nil "myname")))
        (expect (string= "newest" result))
        (expect (string= "newest" (cl-tmux/buffer:get-named-buffer "myname")))
        (expect (string= "older" (cl-tmux/buffer:get-paste-buffer
                               (1- (length (cl-tmux/buffer:list-paste-buffers)))))))))

  ;; rename-paste-buffer returns the text without reordering when source = target.
  (it "rename-paste-buffer-same-name-is-noop"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "value" "foo")
      (let ((result (cl-tmux/buffer:rename-paste-buffer "foo" "foo")))
        (expect (string= "value" result))
        (expect (string= "value" (cl-tmux/buffer:get-named-buffer "foo")))
        (expect (= 1 (length (cl-tmux/buffer:list-paste-buffers)))))))

  ;; rename-paste-buffer returns NIL when the source buffer does not exist.
  (it "rename-paste-buffer-absent-source-returns-nil"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "other" "something")
      (let ((result (cl-tmux/buffer:rename-paste-buffer "nonexistent" "newname")))
        (expect (null result))
        (expect (null (cl-tmux/buffer:get-named-buffer "newname")))))))
