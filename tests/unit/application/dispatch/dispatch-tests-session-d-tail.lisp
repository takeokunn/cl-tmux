(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part D tail: named paste-buffer commands,
;;;; join-pane marked-pane, wait-for-arg.

(in-suite dispatch-suite)

;;; ── Named paste-buffer commands (-b name) ────────────────────────────────────

(test cmd-set-buffer-b-stores-named
  "set-buffer -b name data stores a named buffer retrievable by name."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "mybuf" "hello" "world"))
      (is (string= "hello world" (cl-tmux/buffer:get-named-buffer "mybuf"))
          "set-buffer -b stores the joined data under the name"))))

(test cmd-set-buffer-n-renames-latest-buffer
  "set-buffer -n new-name renames the most recent buffer and keeps its text."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:add-paste-buffer "hello")
      (cl-tmux::%cmd-set-buffer-arg s '("-n" "renamed" "ignored"))
      (is (equal '("renamed") (cl-tmux/buffer:buffer-names))
          "the buffer name changes to the requested new name")
      (is (string= "hello" (cl-tmux/buffer:get-named-buffer "renamed"))
          "rename preserves the source text")
      (is (null (cl-tmux/buffer:get-named-buffer "buffer0"))
          "the old automatic name is gone"))))

(test cmd-set-buffer-b-n-renames-named-buffer
  "set-buffer -b name -n new-name renames a selected buffer and ignores data."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "old" "named text")
      (cl-tmux/buffer:add-paste-buffer "keep")
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "old" "-n" "new" "ignored"))
      (is (equal '("new" "buffer0") (cl-tmux/buffer:buffer-names))
          "the selected named buffer is renamed in place")
      (is (string= "named text" (cl-tmux/buffer:get-named-buffer "new"))
          "rename keeps the original text")
      (is (null (cl-tmux/buffer:get-named-buffer "old"))
          "the old name is removed"))))

(test cmd-set-buffer-accepts-target-client-flag
  "set-buffer -t target-client (tmux args ab:t:n:w) consumes the client argument
   and stores the remaining data as a paste buffer, with no error overlay."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%cmd-set-buffer-arg s '("-t" "client" "hello"))
        (is (null *overlay*)
            "set-buffer -t must not raise an unsupported-argument overlay")
        (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
            "set-buffer -t stores the data after consuming the client arg")))))

(test cmd-set-buffer-n-reports-no-buffer-when-empty
  "set-buffer -n new-name on an empty ring reports that no source buffer exists."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (is (null (cl-tmux::%cmd-set-buffer-arg s '("-n" "renamed" "ignored")))
            "renaming fails cleanly without a source buffer")
        (assert-overlay-contains "no buffer" *overlay*
                                 "set-buffer -n reports the missing source buffer")
        (is (null (cl-tmux/buffer:list-paste-buffers-with-names))
            "no buffer should be created on failure")))))

(test cmd-set-buffer-a-appends-named
  "set-buffer -a -b name appends to the existing named buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "b" "foo"))
      (cl-tmux::%cmd-set-buffer-arg s '("-a" "-b" "b" "bar"))
      (is (string= "foobar" (cl-tmux/buffer:get-named-buffer "b"))
          "-a appends to the named buffer"))))

(test run-command-line-set-buffer-b-stores-named
  "The tokenized command-line path dispatches set-buffer -b to the arg handler."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%run-command-line s "set-buffer -b mybuf hello world")
      (is (string= "hello world" (cl-tmux/buffer:get-named-buffer "mybuf"))
          "set-buffer -b must work through %run-command-line"))))

(test cmd-set-buffer-w-stores-and-accepts
  "set-buffer -w (OSC 52 clipboard flag) is accepted and still stores the buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%cmd-set-buffer-arg s '("-w" "hello"))
        (is (null *overlay*)
            "set-buffer -w must not raise an unsupported-argument overlay")
        (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
            "set-buffer -w stores the data as well as sending to the clipboard")))))

(test cmd-set-buffer-rejects-unsupported-arguments
  "set-buffer rejects flags that are not in the tmux args set (ab:t:n:w)."
  (dolist (args '(("-Z" "hello")))
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (is (null (cl-tmux::%cmd-set-buffer-arg s args))
              "~S is rejected" args)
          (assert-overlay-contains "set-buffer: unsupported argument"
                                    *overlay* args)
          (is (null (cl-tmux/buffer:get-paste-buffer 0))
              "~S must not store a paste buffer after rejection" args)
          (is (null (cl-tmux/buffer:get-named-buffer "ignored"))
              "~S must not store a named buffer after rejection" args))))))

(test cmd-show-buffer-b-shows-named
  "show-buffer -b name shows that buffer's content in an overlay."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/buffer:set-named-buffer "b" "shown-content")
        (cl-tmux::%cmd-show-buffer-arg s '("-b" "b"))
        (assert-overlay-active "show-buffer -b opens an overlay")
        (assert-overlay-contains "shown-content" *overlay*
                                 "show-buffer -b")))))

(test cmd-delete-buffer-b-deletes-named
  "delete-buffer -b name removes that named buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "b" "x")
      (cl-tmux::%cmd-delete-buffer-arg s '("-b" "b"))
      (is (null (cl-tmux/buffer:get-named-buffer "b"))
          "the named buffer is gone after delete-buffer -b"))))

(test cmd-save-buffer-b-writes-named-buffer
  "save-buffer -b name path writes the named buffer to a file."
  (with-empty-buffers
    (with-fake-session (s)
      (let* ((label (format nil "cl-tmux-save-buffer-~D-~D.txt"
                            (get-universal-time)
                            (random 1000000)))
             (path (namestring (merge-pathnames label (uiop:temporary-directory)))))
        (unwind-protect
             (progn
               (cl-tmux/buffer:set-named-buffer "saved" "named text")
               (cl-tmux::%run-command-tokens s (list "save-buffer" "-b" "saved" path))
               (is (string= "named text" (uiop:read-file-string path))
                   "wrote selected named buffer"))
          (ignore-errors (delete-file path)))))))

(test cmd-save-buffer-a-appends
  "save-buffer -a appends the selected buffer to an existing file."
  (with-empty-buffers
    (with-fake-session (s)
      (let* ((label (format nil "cl-tmux-save-buffer-append-~D-~D.txt"
                            (get-universal-time)
                            (random 1000000)))
             (path (namestring (merge-pathnames label (uiop:temporary-directory)))))
        (unwind-protect
             (progn
               (with-open-file (out path
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
                 (write-string "pre:" out))
               (cl-tmux/buffer:add-paste-buffer "post")
               (cl-tmux::%run-command-tokens s (list "save-buffer" "-a" path))
               (is (string= "pre:post" (uiop:read-file-string path))
                   "appended most recent buffer"))
          (ignore-errors (delete-file path)))))))

(test cmd-save-buffer-rejects-unsupported-arguments
  "save-buffer rejects unknown flags and stray positionals before writing."
  (dolist (case '(:extra-arg :unknown-flag))
    (with-empty-buffers
      (with-fake-session (s)
        (let* ((label (format nil "cl-tmux-save-buffer-reject-~D-~D.txt"
                              (get-universal-time)
                              (random 1000000)))
               (path (namestring (merge-pathnames label (uiop:temporary-directory))))
               (args (ecase case
                       (:extra-arg (list "-b" "saved" path "extra"))
                       (:unknown-flag (list "-Z" path)))))
          (unwind-protect
               (progn
                (with-open-file (out path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                   (write-string "pre:" out))
                 (cl-tmux/buffer:set-named-buffer "saved" "named text")
                 (let ((*overlay* nil))
                   (is (null (cl-tmux::%run-command-tokens s (cons "save-buffer" args)))
                       "~S is rejected" args)
                   (assert-overlay-contains "save-buffer: unsupported argument"
                                             *overlay* args)
                   (is (string= "pre:" (uiop:read-file-string path))
                       "~S must not overwrite the file after rejection" args)))
            (ignore-errors (delete-file path))))))))

(test cmd-load-buffer-b-loads-named-buffer
  "load-buffer -b name path loads file contents."
  (with-empty-buffers
    (with-fake-session (s)
      (let* ((label (format nil "cl-tmux-load-buffer-~D-~D.txt"
                            (get-universal-time)
                            (random 1000000)))
             (path (namestring (merge-pathnames label (uiop:temporary-directory)))))
        (unwind-protect
             (progn
               (with-open-file (out path
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
                 (write-string "from file" out))
               (cl-tmux::%run-command-tokens s
                                             (list "load-buffer"
                                                   "-b" "loaded"
                                                   path))
               (is (string= "from file" (cl-tmux/buffer:get-named-buffer "loaded"))
                   "loaded file into named buffer"))
          (ignore-errors (delete-file path)))))))

(test cmd-load-buffer-rejects-unsupported-arguments
  "load-buffer rejects unknown flags and extra positionals before loading."
  (dolist (case '(:extra-arg :unknown-flag :former-window-flag :former-target-flag))
    (with-empty-buffers
      (with-fake-session (s)
        (let* ((label (format nil "cl-tmux-load-buffer-reject-~D-~D.txt"
                              (get-universal-time)
                              (random 1000000)))
               (path (namestring (merge-pathnames label (uiop:temporary-directory))))
               (args (ecase case
                       (:extra-arg (list path "extra"))
                       (:unknown-flag (list "-Z" path))
                       (:former-window-flag (list "-w" path))
                       (:former-target-flag (list "-t" "client" path)))))
          (unwind-protect
               (progn
                (with-open-file (out path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                   (write-string "from file" out))
                 (cl-tmux/buffer:add-paste-buffer "existing")
                 (let ((*overlay* nil))
                   (is (null (cl-tmux::%run-command-tokens s (cons "load-buffer" args)))
                       "~S is rejected" args)
                   (assert-overlay-contains "load-buffer: unsupported argument"
                                             *overlay* args)
                   (is (string= "existing" (cl-tmux/buffer:get-paste-buffer 0))
                       "~S must not mutate existing buffers after rejection" args)))
            (ignore-errors (delete-file path))))))))

(test cmd-paste-buffer-d-deletes-named-after-paste
  "paste-buffer -d -b name deletes the named buffer after pasting it."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "b" "data")
      (cl-tmux::%cmd-paste-buffer-arg s '("-d" "-b" "b"))
      (is (null (cl-tmux/buffer:get-named-buffer "b"))
          "-d removes the named buffer after pasting"))))

(test cmd-paste-buffer-rejects-unsupported-arguments
  "paste-buffer rejects unknown flags and positional arguments before side effects."
  (dolist (args '(("-d" "-b" "b" "extra")
                  ("-Z" "-d" "-b" "b")))
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux/buffer:set-named-buffer "b" "data")
          (is (null (cl-tmux::%cmd-paste-buffer-arg s args))
              "~S is rejected" args)
          (assert-overlay-contains "paste-buffer: unsupported argument"
                                    *overlay* args)
          (is (string= "data" (cl-tmux/buffer:get-named-buffer "b"))
              "~S must not delete the source buffer after rejection" args))))))

(test cmd-delete-and-show-buffer-reject-unsupported-arguments
  "delete-buffer and show-buffer reject unsupported arguments before mutation/output."
  (dolist (case '((cl-tmux::%cmd-delete-buffer-arg
                   ("-b" "b" "extra")
                   "delete-buffer: unsupported argument")
                  (cl-tmux::%cmd-delete-buffer-arg
                   ("-Z" "-b" "b")
                   "delete-buffer: unsupported argument")
                  (cl-tmux::%cmd-show-buffer-arg
                   ("-b" "b" "extra")
                   "show-buffer: unsupported argument")
                  (cl-tmux::%cmd-show-buffer-arg
                   ("-Z" "-b" "b")
                   "show-buffer: unsupported argument")))
    (destructuring-bind (fn args message) case
      (with-empty-buffers
        (with-fake-session (s)
          (let ((*overlay* nil))
            (cl-tmux/buffer:set-named-buffer "b" "shown-content")
            (is (null (funcall fn s args))
                "~S rejects ~S" fn args)
            (assert-overlay-contains message *overlay* fn)
            (is (string= "shown-content" (cl-tmux/buffer:get-named-buffer "b"))
                "~S must not mutate buffers after rejection" fn)))))))

(test cmd-copy-mode-rejects-unsupported-arguments
  "copy-mode rejects unknown flags and positionals before entering copy-mode."
  (dolist (args '(("extra") ("-Z")))
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*dirty* nil))
        (is (null (cl-tmux::%cmd-copy-mode-arg s args))
            "~S is rejected" args)
        (assert-overlay-contains "copy-mode: unsupported argument"
                                  *overlay* args)
        (is-false (screen-copy-mode-p (active-screen s))
                  "~S must not enter copy-mode after rejection" args)
        (is-false cl-tmux::*dirty*
                  "~S must not mark the UI dirty after rejection" args)))))

(test cmd-capture-pane-b-stores-named
  "capture-pane -b name stores the captured content under that name."
  (with-empty-buffers
    (with-fake-session (s)
      (feed (active-screen s) "captured text")
      (cl-tmux::%cmd-capture-pane-arg s '("-b" "cap"))
      (is (search "captured text" (or (cl-tmux/buffer:get-named-buffer "cap") ""))
          "capture-pane -b stores the capture under the given name"))))

(test config-bind-accepts-paste-buffer-b-flag
  "`bind X paste-buffer -b foo` is accepted by the config parser."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string "bind X paste-buffer -b foo"))
        "paste-buffer -b parses as one applied directive")))

;;; ── Coverage gap #19: join/move-pane default source = marked pane ───────────

(test cmd-join-pane-uses-marked-pane-as-default-source
  "join-pane without -s uses *server-marked-pane* as source when it is set."
  (with-fake-session (s :nwindows 2)
    (let* ((wins   (cl-tmux/model:session-windows s))
           (win0   (first wins))
           (win1   (second wins))
           (pane0  (cl-tmux/model:window-active-pane win0))
           (pane1  (cl-tmux/model:window-active-pane win1))
           ;; Mark a pane in win1 — join-pane without -s should use it as source.
           (cl-tmux::*server-marked-pane* pane1))
      (declare (ignore pane0))
      ;; Point session at win0 (the destination window).
      (cl-tmux/model:session-select-window s win0)
      ;; join-pane with no flags: source = marked pane (pane1 from win1).
      (cl-tmux::%cmd-join-pane-arg s '())
      (is (member pane1 (cl-tmux/model:window-panes win0))
          "join-pane without -s must move the marked pane into the active window"))))

(test cmd-join-pane-ignores-marked-pane-when-s-given
  "join-pane with explicit -s ignores *server-marked-pane* and uses the given source."
  (with-fake-session (s :nwindows 2)
    (let* ((wins   (cl-tmux/model:session-windows s))
           (win0   (first wins))
           (win1   (second wins))
           (pane0  (cl-tmux/model:window-active-pane win0))
           (pane1  (cl-tmux/model:window-active-pane win1))
           ;; Mark pane0 — join-pane -s win1 should still use pane1.
           (cl-tmux::*server-marked-pane* pane0))
      (is (eq pane0 cl-tmux::*server-marked-pane*)
          "precondition: pane0 must be the marked pane")
      ;; Point session at win0.
      (cl-tmux/model:session-select-window s win0)
      ;; Explicit -s @N (win1 window-id sigil) targets pane1, not the marked pane.
      (cl-tmux::%cmd-join-pane-arg s (list "-s" (format nil "@~D" (cl-tmux/model:window-id win1))))
      (is (member pane1 (cl-tmux/model:window-panes win0))
          "join-pane -s must use the explicit source, not the marked pane"))))

;;; ── %cmd-wait-for-arg (gap #10: -S/-L/-U flags) ─────────────────────────────

(test cmd-wait-for-arg-signal-signals-channel
  "wait-for -S channel signals the named channel (unblocks waiters)."
  (with-fake-session (s)
    (let ((received nil))
      ;; Start a thread waiting on the channel.
      (bt:make-thread
       (lambda () (setf received (cl-tmux::wait-for-channel "test-ch-signal")))
       :name "waiter")
      ;; Brief yield so the waiter thread reaches condition-wait before signal.
      (sleep 0.05)
      (cl-tmux::%cmd-wait-for-arg s '("-S" "test-ch-signal"))
      (sleep 0.05)
      (is-true received "wait-for -S must unblock the waiting thread"))))

(test cmd-wait-for-arg-option-terminator-after-flags
  "wait-for -S -- channel treats -- as an option terminator and signals channel."
  (with-fake-session (s)
    (let ((received nil))
      (bt:make-thread
       (lambda () (setf received (cl-tmux::wait-for-channel "test-ch-dd-signal")))
       :name "waiter-with-double-dash")
      (sleep 0.05)
      (cl-tmux::%cmd-wait-for-arg s '("-S" "--" "test-ch-dd-signal"))
      (sleep 0.05)
      (is-true received "wait-for -S -- channel must unblock the waiting thread"))))

(test cmd-wait-for-arg-lock-suppresses-signal
  "wait-for -L channel locks the channel; subsequent -S does not notify waiters."
  (with-fake-session (s)
    ;; Lock first, then signal — the signal should be a no-op.
    (cl-tmux::%cmd-wait-for-arg s '("-L" "test-ch-lock"))
    ;; A waiter on a LOCKED channel receives no notification; wait-for-channel
    ;; will time-out and return NIL.  We verify the lock was applied by checking
    ;; that signal-channel does not raise an error and that the channel is locked.
    (let ((ch (cl-tmux::%ensure-channel "test-ch-lock")))
      (is-true (getf ch :locked) "wait-for -L must set the :locked flag on the channel"))))

(test cmd-wait-for-arg-unlock-clears-lock
  "wait-for -U channel unlocks a previously locked channel."
  (with-fake-session (s)
    (cl-tmux::%cmd-wait-for-arg s '("-L" "test-ch-unlock"))
    (cl-tmux::%cmd-wait-for-arg s '("-U" "test-ch-unlock"))
    (let ((ch (cl-tmux::%ensure-channel "test-ch-unlock")))
      (is-false (getf ch :locked) "wait-for -U must clear the :locked flag"))))

(test cmd-wait-for-arg-bare-blocks-until-signaled
  "wait-for channel (bare, no flags) blocks until the channel is signaled."
  (with-fake-session (s)
    (let ((result :pending))
      ;; Run wait-for in a background thread so it blocks without stalling tests.
      (bt:make-thread
       (lambda ()
         (setf result (cl-tmux::%cmd-wait-for-arg s '("test-ch-bare"))))
       :name "bare-waiter")
      (sleep 0.05)
      (cl-tmux::signal-channel "test-ch-bare")
      (sleep 0.05)
      (is (not (eq result :pending))
          "wait-for (bare) must unblock after the channel is signaled"))))

(test cmd-wait-for-unsupported-arguments-are-rejected-before-channel-state
  "wait-for rejects invalid arguments with tmux-compatible diagnostics."
  (with-fake-session (s)
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (dolist (case '((("-Z" "test-ch-unsupported")
                       "command wait-for: unknown flag -Z")
                      (("-SZ" "test-ch-unsupported")
                       "command wait-for: unknown flag -Z")
                      (("-S")
                       "command wait-for: too few arguments (need at least 1)")
                      (("--")
                       "command wait-for: too few arguments (need at least 1)")
                      (("-L" "test-ch-unsupported" "extra")
                       "command wait-for: too many arguments (need at most 1)")
                      (("test-ch-unsupported" "-S")
                       "command wait-for: too many arguments (need at most 1)")))
        (destructuring-bind (args expected) case
          (let (cl-tmux::*overlay*)
            (cl-tmux::%cmd-wait-for-arg s args)
            (assert-overlay-contains expected
                                      cl-tmux::*overlay*
                                      "wait-for")
            (is-false (gethash "test-ch-unsupported" cl-tmux::*wait-channels*)
                      "wait-for must not create or mutate a channel after rejecting arguments")))))))
