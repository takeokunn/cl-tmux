(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/buffer: paste-buffer ring operations.
;;;;
;;;; Uses with-empty-buffers from tests/helpers-b.lisp (shared DSL) to isolate
;;;; *paste-buffers* state between tests.

(def-suite buffer-suite :description "Paste buffer ring")
(in-suite buffer-suite)

;;; ── add + get round-trip ─────────────────────────────────────────────────────

(test add-and-get-buffer
  "add-paste-buffer then get-paste-buffer returns the added text."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "hello")
    (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0)))))

(test osc52-inbound-respects-set-clipboard-off
  "Inbound OSC 52 (an application writing the clipboard) is IGNORED when
   set-clipboard is off — tmux drops application clipboard writes then."
  (with-empty-buffers
    (with-fresh-options
      (cl-tmux/options:set-option "set-clipboard" "off")
      (cl-tmux/buffer::%osc52-inbound-clipboard "secret")
      (is (null (cl-tmux/buffer:get-paste-buffer 0))
          "set-clipboard off must drop the inbound clipboard write"))))

(test osc52-inbound-accepted-when-set-clipboard-on
  "Inbound OSC 52 adds to the paste-buffer ring when set-clipboard is on (default)."
  (with-empty-buffers
    (with-fresh-options
      (cl-tmux/options:set-option "set-clipboard" "on")
      (cl-tmux/buffer::%osc52-inbound-clipboard "copied")
      (is (string= "copied" (cl-tmux/buffer:get-paste-buffer 0))
          "set-clipboard on must accept the inbound clipboard write"))))

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
             (dolist (c '((0 "d") (1 "c") (2 "b")))
               (destructuring-bind (idx expected) c
                 (is (string= expected (cl-tmux/buffer:get-paste-buffer idx))
                     "index ~D must be ~S" idx expected)))
             (is (null (cl-tmux/buffer:get-paste-buffer 3))
                 "oldest buffer must have been evicted"))
        (cl-tmux/options:set-option "buffer-limit" saved)))))

(test buffer-limit-default-50
  "Without a configured limit, at most +default-buffer-limit+ (50) entries are kept."
  (with-empty-buffers
    (let ((limit cl-tmux/buffer:+default-buffer-limit+))
      (dotimes (i (+ limit 5))
        (cl-tmux/buffer:add-paste-buffer (format nil "buf~D" i)))
      (is (<= (length (cl-tmux/buffer:list-paste-buffers)) limit)
          "default limit must cap ring at +default-buffer-limit+ entries"))))

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
  "When get-option signals a condition, %buffer-limit falls back to +default-buffer-limit+."
  ;; Verify the fallback by shadowing *global-options* with a hash table that
  ;; causes get-option to return NIL (unregistered option name), which exercises
  ;; the (or (ignore-errors ...) +default-buffer-limit+) fallback in %buffer-limit.
  ;; We use with-isolated-options with an intentionally wrong type value so that
  ;; the coercion does not signal; instead we temporarily remove buffer-limit
  ;; from the options hash so get-option returns NIL.
  (let ((empty-ht (make-hash-table :test #'equal)))
    (let ((cl-tmux/options:*global-options* empty-ht))
      ;; With no buffer-limit key in *global-options*, get-option returns NIL.
      ;; %buffer-limit must then return +default-buffer-limit+ (the named constant).
      (is (= cl-tmux/buffer:+default-buffer-limit+ (cl-tmux/buffer::%buffer-limit))
          "%buffer-limit must return +default-buffer-limit+ when get-option returns NIL"))))

;;; ── Table-driven add-paste-buffer cases ──────────────────────────────────────

(test add-paste-buffer-table
  "Table-driven: add-paste-buffer handles varied text lengths correctly."
  (dolist (entry
           '(("" "empty string is stored as index 0")
             ("x" "single character is stored")
             ("hello world" "multi-word string is stored")
             ("line1\nline2" "multi-line string is stored")))
    (destructuring-bind (text desc) entry
      (with-empty-buffers
        (cl-tmux/buffer:add-paste-buffer text)
        (is (string= text (cl-tmux/buffer:get-paste-buffer 0)) desc)))))

;;; ── +default-buffer-limit+ constant ─────────────────────────────────────────

(test default-buffer-limit-is-positive-integer
  "+default-buffer-limit+ is a positive integer."
  (is (and (integerp cl-tmux/buffer:+default-buffer-limit+)
           (plusp cl-tmux/buffer:+default-buffer-limit+))
      "+default-buffer-limit+ must be a positive integer"))

;;; ── list-paste-buffers order ─────────────────────────────────────────────────

(test list-paste-buffers-most-recent-first
  "list-paste-buffers returns buffers in most-recently-added order."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "first")
    (cl-tmux/buffer:add-paste-buffer "second")
    (cl-tmux/buffer:add-paste-buffer "third")
    (let ((lst (cl-tmux/buffer:list-paste-buffers)))
      (dolist (c '((0 "third" "most recent first") (1 "second" "second most recent") (2 "first" "oldest last")))
        (destructuring-bind (idx expected desc) c
          (is (string= expected (nth idx lst)) "~A" desc))))))

;;; ── delete-paste-buffer on non-empty then empty ──────────────────────────────

(test delete-paste-buffer-last-entry-empties-ring
  "delete-paste-buffer on the only entry leaves an empty ring."
  (with-empty-buffers
    (cl-tmux/buffer:add-paste-buffer "only-one")
    (cl-tmux/buffer:delete-paste-buffer 0)
    (is (null (cl-tmux/buffer:list-paste-buffers))
        "ring must be empty after deleting the sole entry")))

;;; ── clear-paste-buffers with empty ring is safe ──────────────────────────────

(test clear-paste-buffers-idempotent-on-empty-ring
  "clear-paste-buffers on an already-empty ring is a no-op (does not signal)."
  (with-empty-buffers
    (finishes (cl-tmux/buffer:clear-paste-buffers))
    (is (null (cl-tmux/buffer:list-paste-buffers))
        "ring must remain NIL after clearing an empty ring")))

;;; ── OSC 52 end-to-end: handler wired to paste buffer ─────────────────────────

(test osc52-handler-is-set-to-add-paste-buffer
  "The *osc52-handler* is wired to add-paste-buffer at load time."
  (is-true cl-tmux/terminal/parser:*osc52-handler*
           "*osc52-handler* must be non-NIL after loading buffer.lisp")
  (is (functionp cl-tmux/terminal/parser:*osc52-handler*)
      "*osc52-handler* must be a function"))

(test osc52-handler-populates-paste-buffer
  "Calling *osc52-handler* with text pushes it onto the paste buffer ring."
  (with-empty-buffers
    (funcall cl-tmux/terminal/parser:*osc52-handler* "clipboard-text")
    (is (string= "clipboard-text" (cl-tmux/buffer:get-paste-buffer 0))
        "paste buffer must contain the text passed to *osc52-handler*")))

(test osc52-screen-process-bytes-populates-paste-buffer
  "An OSC 52 escape sequence processed by screen-process-bytes fills the paste buffer.
   ESC ] 52 ; c ; SGVsbG8= ST  (Hello in base64)."
  (with-empty-buffers
    (let ((s (make-screen 10 5)))
      ;; OSC 52: ESC ] 52 ; c ; SGVsbG8= ST
      ;; ST = ESC backslash (0x1b 0x5c)
      (let ((seq (babel:string-to-octets
                  (format nil "~C]52;c;SGVsbG8=~C\\" #\Escape #\Escape)
                  :encoding :latin-1)))
        (cl-tmux/terminal/emulator:screen-process-bytes s seq))
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (string= "Hello" buf))
            "OSC 52 clipboard write must put decoded 'Hello' into paste buffer, got ~S" buf)))))

(test base64-encode-known-value
  "%base64-encode produces standard padded Base64."
  (is (string= "aGVsbG8="
               (cl-tmux/terminal/parser::%base64-encode
                (babel:string-to-octets "hello" :encoding :utf-8)))
      "base64 of 'hello' must be aGVsbG8=")
  (is (string= "aGk="
               (cl-tmux/terminal/parser::%base64-encode
                (babel:string-to-octets "hi" :encoding :utf-8)))
      "base64 of 'hi' must be aGk= (one pad char)"))

(test base64-encode-decode-round-trip
  "%base64-encode then %base64-decode recovers the original bytes."
  (let* ((bytes (babel:string-to-octets "Round-trip 123! αβγ" :encoding :utf-8))
         (round (cl-tmux/terminal/parser::%base64-decode
                 (cl-tmux/terminal/parser::%base64-encode bytes))))
    (is (equalp bytes (coerce round '(vector (unsigned-byte 8))))
        "encode->decode must be the identity on the original bytes")))

(test osc52-clipboard-sequence-format
  "osc52-clipboard-sequence wraps Base64 text in ESC ] 52 ; c ; ... ESC backslash —
   the outbound sequence that copies a selection to the host system clipboard."
  (let ((seq (cl-tmux/terminal/parser:osc52-clipboard-sequence "hi")))
    (is (char= #\Escape (char seq 0)) "starts with ESC")
    (is (search "]52;c;" seq) "carries the OSC 52 clipboard prefix")
    (is (search "aGk=" seq) "encodes the text as base64")
    (is (and (char= #\Escape (char seq (- (length seq) 2)))
             (char= #\\ (char seq (1- (length seq)))))
        "terminates with ST (ESC backslash)")))

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
