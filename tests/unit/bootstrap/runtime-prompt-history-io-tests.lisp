(in-package #:cl-tmux/test)

;;;; Runtime prompt history persistence and stream parsing

(in-suite runtime-suite)

(test prompt-history-persists-to-history-file
  "add-prompt-history saves to history-file and load-prompt-history restores it
   (newest first)."
  (with-fresh-options
    (let ((path (format nil "~A/cl-tmux-hist-~D.txt"
                        (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                        (get-universal-time))))
      (unwind-protect
           (progn
             (let ((cl-tmux::*prompt-history* nil))
               (cl-tmux/options:set-option "history-file" path)
               (cl-tmux::add-prompt-history "first")
               (cl-tmux::add-prompt-history "second"))
             ;; Fresh in-memory history; loading from the file restores both.
             (let ((cl-tmux::*prompt-history* nil))
               (cl-tmux::load-prompt-history)
               (is (equal '("second" "first") cl-tmux::*prompt-history*)
                   "loaded history must be newest-first")))
        (ignore-errors (delete-file path))))))

(test prompt-history-no-file-is-in-memory-only
  "With history-file unset (default \"\"), history stays in memory and add does not error."
  (with-fresh-options
    (let ((cl-tmux::*prompt-history* nil))
      (is (null (cl-tmux::%prompt-history-path))
          "no history path when history-file is empty")
      (cl-tmux::add-prompt-history "x")
      (is (equal '("x") cl-tmux::*prompt-history*)
          "in-memory history still works without a file"))))

(test save-prompt-history-writes-oldest-first
  "save-prompt-history writes *prompt-history* (newest-first in memory) to the
   history-file oldest-first, so a later load-prompt-history restores the
   original newest-first order."
  (with-isolated-options ()
    (let ((path (format nil "~A/cl-tmux-save-hist-~D.txt"
                        (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                        (get-universal-time))))
      (unwind-protect
           (progn
             (cl-tmux/options:set-option "history-file" path)
             (let ((cl-tmux::*prompt-history* '("newest" "middle" "oldest")))
               (cl-tmux::save-prompt-history))
             (with-open-file (s path :direction :input)
               (is (string= "oldest" (read-line s))
                   "the first line written must be the oldest entry")
               (is (string= "middle" (read-line s))
                   "the second line written must be the middle entry")
               (is (string= "newest" (read-line s))
                   "the third line written must be the newest entry")))
        (ignore-errors (delete-file path))))))

(test save-prompt-history-no-op-when-history-file-unset
  "save-prompt-history is a no-op (does not error, does not create a file) when
   history-file is unset."
  (with-isolated-options ("history-file" "")
    (let ((cl-tmux::*prompt-history* '("a" "b")))
      (finishes (cl-tmux::save-prompt-history)
                "save-prompt-history must not error with history-file unset"))))

(test save-prompt-history-swallows-io-errors
  "save-prompt-history ignores I/O errors (e.g., an unwritable directory) rather
   than signalling."
  (with-isolated-options ("history-file" "/nonexistent-dir-xyz/hist.txt")
    (let ((cl-tmux::*prompt-history* '("a")))
      (finishes (cl-tmux::save-prompt-history)
                "save-prompt-history must swallow I/O errors from an invalid path"))))

(test read-history-lines-returns-lines-reversed
  "%read-history-lines reads non-empty lines from a stream and returns them newest-first."
  (let ((content (format nil "line1~%line2~%line3~%")))
    (with-input-from-string (stream content)
      (let ((result (cl-tmux::%read-history-lines stream)))
        (is (equal '("line3" "line2" "line1") result)
            "%read-history-lines must return lines reversed (newest first)")))))

(test read-history-lines-skips-empty-lines
  "%read-history-lines skips empty lines in the stream."
  (let ((content (format nil "first~%~%second~%")))
    (with-input-from-string (stream content)
      (let ((result (cl-tmux::%read-history-lines stream)))
        (is (= 2 (length result))
            "%read-history-lines must skip empty lines")
        (is (member "first" result :test #'string=) "first must appear")
        (is (member "second" result :test #'string=) "second must appear")))))

(test read-history-lines-returns-nil-for-empty-stream
  "%read-history-lines returns NIL when stream is empty."
  (with-input-from-string (stream "")
    (let ((result (cl-tmux::%read-history-lines stream)))
      (is (null result)
          "%read-history-lines must return NIL for empty stream"))))
