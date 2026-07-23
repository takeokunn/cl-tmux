(in-package #:cl-tmux/test)

;;;; Runtime prompt history persistence and stream parsing

(describe "runtime-suite"

  ;; add-prompt-history saves to history-file and load-prompt-history restores it
  ;; (newest first).
  (it "prompt-history-persists-to-history-file"
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
                 (expect (equal '("second" "first") cl-tmux::*prompt-history*))))
          (ignore-errors (delete-file path))))))

  ;; With history-file unset (default ""), history stays in memory and add does not error.
  (it "prompt-history-no-file-is-in-memory-only"
    (with-fresh-options
      (let ((cl-tmux::*prompt-history* nil))
        (expect (null (cl-tmux::%prompt-history-path)))
        (cl-tmux::add-prompt-history "x")
        (expect (equal '("x") cl-tmux::*prompt-history*)))))

  ;; save-prompt-history writes *prompt-history* (newest-first in memory) to the
  ;; history-file oldest-first, so a later load-prompt-history restores the
  ;; original newest-first order.
  (it "save-prompt-history-writes-oldest-first"
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
                 (expect (string= "oldest" (read-line s)))
                 (expect (string= "middle" (read-line s)))
                 (expect (string= "newest" (read-line s)))))
          (ignore-errors (delete-file path))))))

  ;; save-prompt-history is a no-op (does not error, does not create a file) when
  ;; history-file is unset.
  (it "save-prompt-history-no-op-when-history-file-unset"
    (with-isolated-options ("history-file" "")
      (let ((cl-tmux::*prompt-history* '("a" "b")))
        (finishes (cl-tmux::save-prompt-history)
                  "save-prompt-history must not error with history-file unset"))))

  ;; save-prompt-history ignores I/O errors (e.g., an unwritable directory) rather
  ;; than signalling.
  (it "save-prompt-history-swallows-io-errors"
    (with-isolated-options ("history-file" "/nonexistent-dir-xyz/hist.txt")
      (let ((cl-tmux::*prompt-history* '("a")))
        (finishes (cl-tmux::save-prompt-history)
                  "save-prompt-history must swallow I/O errors from an invalid path"))))

  ;; %read-history-lines reads non-empty lines from a stream and returns them newest-first.
  (it "read-history-lines-returns-lines-reversed"
    (let ((content (format nil "line1~%line2~%line3~%")))
      (with-input-from-string (stream content)
        (let ((result (cl-tmux::%read-history-lines stream)))
          (expect (equal '("line3" "line2" "line1") result))))))

  ;; %read-history-lines skips empty lines in the stream.
  (it "read-history-lines-skips-empty-lines"
    (let ((content (format nil "first~%~%second~%")))
      (with-input-from-string (stream content)
        (let ((result (cl-tmux::%read-history-lines stream)))
          (expect (= 2 (length result)))
          (expect (member "first" result :test #'string=))
          (expect (member "second" result :test #'string=))))))

  ;; %read-history-lines returns NIL when stream is empty.
  (it "read-history-lines-returns-nil-for-empty-stream"
    (with-input-from-string (stream "")
      (let ((result (cl-tmux::%read-history-lines stream)))
        (expect (null result))))))
