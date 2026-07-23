(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/buffer: paste-buffer ring operations.
;;;;
;;;; Uses with-empty-buffers from tests/helpers-session-fixtures.lisp (shared DSL) to isolate
;;;; *paste-buffers* state between tests.

(describe "buffer-suite"

  ;;; ── add + get round-trip ─────────────────────────────────────────────────────

  ;; add-paste-buffer then get-paste-buffer returns the added text.
  (it "add-and-get-buffer"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "hello")
      (expect (string= "hello" (cl-tmux/buffer:get-paste-buffer 0)))))

  ;; Inbound OSC 52 (an application writing the clipboard) is IGNORED when
  ;; set-clipboard is off — tmux drops application clipboard writes then.
  (it "osc52-inbound-respects-set-clipboard-off"
    (with-empty-buffers
      (with-fresh-options
        (cl-tmux/options:set-option "set-clipboard" "off")
        (cl-tmux/buffer::%osc52-inbound-clipboard "secret")
        (expect (null (cl-tmux/buffer:get-paste-buffer 0))))))

  ;; Inbound OSC 52 adds to the paste-buffer ring when set-clipboard is on (default).
  (it "osc52-inbound-accepted-when-set-clipboard-on"
    (with-empty-buffers
      (with-fresh-options
        (cl-tmux/options:set-option "set-clipboard" "on")
        (cl-tmux/buffer::%osc52-inbound-clipboard "copied")
        (expect (string= "copied" (cl-tmux/buffer:get-paste-buffer 0))))))

  ;; Most recently added buffer is returned as index 0.
  (it "buffer-lifo-order"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "first")
      (cl-tmux/buffer:add-paste-buffer "second")
      (check-table
       (list (list (cl-tmux/buffer:get-paste-buffer 0) "second"
                   "index 0 must be most recently added")
             (list (cl-tmux/buffer:get-paste-buffer 1) "first"
                   "index 1 must be previous buffer"))
       :test #'string=)))

  ;; get-paste-buffer returns NIL when *paste-buffers* is empty.
  (it "buffer-get-nil-when-empty"
    (with-empty-buffers
      (expect (null (cl-tmux/buffer:get-paste-buffer 0)))))

  ;; get-paste-buffer returns NIL for an index past the end.
  (it "buffer-get-nil-out-of-range"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "only")
      (expect (null (cl-tmux/buffer:get-paste-buffer 5)))))

  ;; delete-paste-buffer removes the buffer at the given index.
  (it "buffer-delete-removes-entry"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "a")
      (cl-tmux/buffer:add-paste-buffer "b")  ; b is index 0, a is index 1
      (cl-tmux/buffer:delete-paste-buffer 0)
      (expect (= 1 (length (cl-tmux/buffer:list-paste-buffers))))
      (expect (string= "a" (cl-tmux/buffer:get-paste-buffer 0)))))

  ;; delete-paste-buffer returns T when the index is valid.
  (it "buffer-delete-returns-t-on-success"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "x")
      (expect (cl-tmux/buffer:delete-paste-buffer 0) :to-be-truthy)))

  ;; delete-paste-buffer returns NIL when the index is out of range.
  (it "buffer-delete-returns-nil-out-of-range"
    (with-empty-buffers
      (expect (cl-tmux/buffer:delete-paste-buffer 0) :to-be-falsy)))

  ;; clear-paste-buffers sets *paste-buffers* to NIL.
  (it "buffer-clear-empties-ring"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "a")
      (cl-tmux/buffer:add-paste-buffer "b")
      (cl-tmux/buffer:clear-paste-buffers)
      (expect (null (cl-tmux/buffer:list-paste-buffers)))))

  ;; list-paste-buffers returns a fresh list, not the internal one.
  (it "buffer-list-returns-copy"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "x")
      (let ((lst (cl-tmux/buffer:list-paste-buffers)))
        (setf (car lst) "mutated")
        (expect (string= "x" (cl-tmux/buffer:get-paste-buffer 0))))))

  ;;; ── buffer-limit enforcement ─────────────────────────────────────────────────

  ;; add-paste-buffer trims the ring to the buffer-limit option.
  (it "buffer-limit-enforced-via-option"
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
               (expect (= 3 (length (cl-tmux/buffer:list-paste-buffers))))
               (check-table
                (loop for (idx expected desc)
                        in '((0 "d" "index 0 must be \"d\"")
                             (1 "c" "index 1 must be \"c\"")
                             (2 "b" "index 2 must be \"b\""))
                      collect (list (cl-tmux/buffer:get-paste-buffer idx)
                                    expected
                                    desc))
                :test #'string=)
               (expect (null (cl-tmux/buffer:get-paste-buffer 3))))
          (cl-tmux/options:set-option "buffer-limit" saved)))))

  ;; Without a configured limit, at most +default-buffer-limit+ (50) entries are kept.
  (it "buffer-limit-default-50"
    (with-empty-buffers
      (let ((limit cl-tmux/buffer:+default-buffer-limit+))
        (dotimes (i (+ limit 5))
          (cl-tmux/buffer:add-paste-buffer (format nil "buf~D" i)))
        (expect (<= (length (cl-tmux/buffer:list-paste-buffers)) limit)))))

  ;; delete-paste-buffer at a non-zero index removes the correct entry.
  (it "buffer-delete-middle-index"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "a")
      (cl-tmux/buffer:add-paste-buffer "b")
      (cl-tmux/buffer:add-paste-buffer "c")  ; c=0, b=1, a=2
      (cl-tmux/buffer:delete-paste-buffer 1)  ; remove b
      (check-table
       (list (list (length (cl-tmux/buffer:list-paste-buffers)) 2
                   "ring length after deleting middle entry"))
       :test #'=)
      (check-table
       (list (list (cl-tmux/buffer:get-paste-buffer 0) "c"
                   "newest entry remains at index 0")
             (list (cl-tmux/buffer:get-paste-buffer 1) "a"
                   "oldest entry shifts to index 1"))
       :test #'string=)))

  ;; add-paste-buffer returns the inserted text.
  (it "buffer-add-returns-text"
    (with-empty-buffers
      (expect (string= "hello" (cl-tmux/buffer:add-paste-buffer "hello")))))

  ;;; ── %buffer-limit fallback (ignore-errors path) ─────────────────────────────

  ;; When get-option signals a condition, %buffer-limit falls back to +default-buffer-limit+.
  (it "buffer-limit-fallback-when-options-error"
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
        (expect (= cl-tmux/buffer:+default-buffer-limit+ (cl-tmux/buffer::%buffer-limit))))))

  ;;; ── Table-driven add-paste-buffer cases ──────────────────────────────────────

  ;; Table-driven: add-paste-buffer handles varied text lengths correctly.
  (it "add-paste-buffer-table"
    (dolist (entry
             '(("" "empty string is stored as index 0")
               ("x" "single character is stored")
               ("hello world" "multi-word string is stored")
               ("line1\nline2" "multi-line string is stored")))
      (destructuring-bind (text desc) entry
        (declare (ignore desc))
        (with-empty-buffers
          (cl-tmux/buffer:add-paste-buffer text)
          (expect (string= text (cl-tmux/buffer:get-paste-buffer 0)))))))

  ;;; ── +default-buffer-limit+ constant ─────────────────────────────────────────

  ;; +default-buffer-limit+ is a positive integer.
  (it "default-buffer-limit-is-positive-integer"
    (expect (and (integerp cl-tmux/buffer:+default-buffer-limit+)
                 (plusp cl-tmux/buffer:+default-buffer-limit+))))

  ;;; ── list-paste-buffers order ─────────────────────────────────────────────────

  ;; list-paste-buffers returns buffers in most-recently-added order.
  (it "list-paste-buffers-most-recent-first"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "first")
      (cl-tmux/buffer:add-paste-buffer "second")
      (cl-tmux/buffer:add-paste-buffer "third")
      (let ((lst (cl-tmux/buffer:list-paste-buffers)))
        (check-table
         (loop for (idx expected desc)
                 in '((0 "third" "most recent first")
                      (1 "second" "second most recent")
                      (2 "first" "oldest last"))
               collect (list (nth idx lst) expected desc))
         :test #'string=))))

  ;;; ── delete-paste-buffer on non-empty then empty ──────────────────────────────

  ;; delete-paste-buffer on the only entry leaves an empty ring.
  (it "delete-paste-buffer-last-entry-empties-ring"
    (with-empty-buffers
      (cl-tmux/buffer:add-paste-buffer "only-one")
      (cl-tmux/buffer:delete-paste-buffer 0)
      (expect (null (cl-tmux/buffer:list-paste-buffers)))))

  ;;; ── clear-paste-buffers with empty ring is safe ──────────────────────────────

  ;; clear-paste-buffers on an already-empty ring is a no-op (does not signal).
  (it "clear-paste-buffers-idempotent-on-empty-ring"
    (with-empty-buffers
      (finishes (cl-tmux/buffer:clear-paste-buffers))
      (expect (null (cl-tmux/buffer:list-paste-buffers)))))

  ;;; ── OSC 52 end-to-end: handler wired to paste buffer ─────────────────────────

  ;; The *osc52-handler* is wired to add-paste-buffer at load time.
  (it "osc52-handler-is-set-to-add-paste-buffer"
    (expect cl-tmux/terminal/parser:*osc52-handler* :to-be-truthy)
    (expect (functionp cl-tmux/terminal/parser:*osc52-handler*)))

  ;; Calling *osc52-handler* with text pushes it onto the paste buffer ring.
  (it "osc52-handler-populates-paste-buffer"
    (with-empty-buffers
      (funcall cl-tmux/terminal/parser:*osc52-handler* "clipboard-text")
      (expect (string= "clipboard-text" (cl-tmux/buffer:get-paste-buffer 0)))))

  ;; An OSC 52 escape sequence processed by screen-process-bytes fills the paste buffer.
  ;; ESC ] 52 ; c ; SGVsbG8= ST  (Hello in base64).
  (it "osc52-screen-process-bytes-populates-paste-buffer"
    (with-empty-buffers
      (let ((s (make-screen 10 5)))
        ;; OSC 52: ESC ] 52 ; c ; SGVsbG8= ST
        ;; ST = ESC backslash (0x1b 0x5c)
        (let ((seq (babel:string-to-octets
                    (format nil "~C]52;c;SGVsbG8=~C\\" #\Escape #\Escape)
                    :encoding :latin-1)))
          (cl-tmux/terminal/emulator:screen-process-bytes s seq))
        (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
          (expect (and (stringp buf) (string= "Hello" buf))))))))
