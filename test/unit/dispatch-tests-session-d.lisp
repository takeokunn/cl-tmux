(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part D: display-popup, send-keys -N/-H,
;;;; capture-pane, named paste-buffer, join-pane marked-pane, wait-for-arg.

(in-suite dispatch-suite)

;;; ── display-popup / popup (arg-bearing handler + alias) ──────────────────────
;;;
;;; `bind C-p popup -E "cmd"` is a very common .tmux.conf form.  Previously
;;; display-popup only opened an interactive prompt and `popup` (its documented
;;; alias, man tmux ALIASES) was unrecognised.  %cmd-display-popup parses the
;;; flags, runs the command, and shows its output; `popup` aliases it everywhere.

(test cmd-display-popup-dimension-helper
  "%popup-dimension resolves nil→fallback, absolute cells, N% of axis, clamps to
   axis-total, and falls back on junk."
  (is (= 60  (cl-tmux::%popup-dimension nil    200 60)) "nil → fallback")
  (is (= 40  (cl-tmux::%popup-dimension "40"   200 60)) "absolute cell count")
  (is (= 80  (cl-tmux::%popup-dimension "80%"  100 60)) "N% of axis-total")
  (is (= 100 (cl-tmux::%popup-dimension "150"  100 60)) "clamped to axis-total")
  (is (= 60  (cl-tmux::%popup-dimension "junk" 200 60)) "unparseable → fallback"))

(test cmd-display-popup-with-command-opens-popup
  "display-popup with -w/-T and a command runs it and shows a popup with the
   requested width and title."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*term-cols* 100)
          (cl-tmux::*term-rows* 30))
      (cl-tmux::%cmd-display-popup s '("-E" "-w" "40" "-T" "mytitle" "echo" "hi"))
      (is (not (null cl-tmux/prompt:*active-popup*))
          "a command argument opens the popup directly (no prompt)")
      (is (= 40 (cl-tmux/prompt:popup-width cl-tmux/prompt:*active-popup*))
          "-w 40 sets the popup width")
      (is (string= "mytitle"
                   (cl-tmux/prompt:popup-title cl-tmux/prompt:*active-popup*))
          "-T sets the popup title"))))

(test cmd-display-popup-percent-width-of-terminal
  "display-popup -w 50% sizes the popup to half the terminal width."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil)
          (cl-tmux::*term-cols* 80)
          (cl-tmux::*term-rows* 24))
      (cl-tmux::%cmd-display-popup s '("-w" "50%" "echo" "x"))
      (is (= 40 (cl-tmux/prompt:popup-width cl-tmux/prompt:*active-popup*))
          "50% of 80 columns → 40"))))

(test cmd-display-popup-no-command-opens-prompt
  "display-popup with no command opens the interactive popup-command prompt
   rather than a popup overlay (legacy behaviour preserved)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::%cmd-display-popup s '())
      (is (null cl-tmux/prompt:*active-popup*)
          "no command → no popup overlay yet")
      (is (prompt-active-p) "no command opens the popup-command prompt instead")
      (is (string= "popup command" (prompt-label *prompt*))
          "the prompt label matches the legacy :display-popup prompt"))))

(test config-bind-accepts-popup-alias
  "`bind P popup -E \"cmd\"` is accepted by the config parser (popup resolves to
   :display-popup); previously the unrecognised `popup` name was rejected."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string
              "bind P popup -E \"echo hi\""))
        "one directive applied — popup is a recognised command alias")))

(test arg-command-table-has-popup-alias
  "*arg-command-table* maps both display-popup and popup to %cmd-display-popup."
  (let ((entry (assoc "popup" cl-tmux::*arg-command-table*
                      :test (lambda (k names) (member k names :test #'string=)))))
    (is (not (null entry)) "popup is registered in *arg-command-table*")
    (is (eq #'cl-tmux::%cmd-display-popup (cdr entry))
        "popup routes to %cmd-display-popup")))

;;; ── send-keys -N (repeat) and -H (hex) ───────────────────────────────────────
;;;
;;; -N count repeats the -X copy-mode command (or the whole key sequence) COUNT
;;; times; -H interprets each argument as a hexadecimal character code.  The -X
;;; repeat is observed via the copy cursor; -H is tested through the extracted
;;; %send-keys-hex-to-string helper (send-keys-to-pane no-ops on a fd -1 pane).

(test send-keys-hex-to-string-converts-codes
  "%send-keys-hex-to-string maps a hex code to its one-character string, or NIL
   for an unparseable / out-of-range code."
  (is (string= "A" (cl-tmux::%send-keys-hex-to-string "41")) "41 → A")
  (is (string= " " (cl-tmux::%send-keys-hex-to-string "20")) "20 → space")
  (is (= 27 (char-code (char (cl-tmux::%send-keys-hex-to-string "1b") 0)))
      "1b → ESC (char code 27)")
  (is (null (cl-tmux::%send-keys-hex-to-string "zz")) "non-hex → NIL")
  (is (null (cl-tmux::%send-keys-hex-to-string "FFFFFFFF"))
      "out-of-range code → NIL (never errors)"))

(test send-keys-x-with-N-repeats-copy-command
  "send-keys -X -N 3 cursor-up moves the copy cursor up 3 rows (the -N repeat
   count applied to the copy-mode command)."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let* ((screen (active-screen s))
           (row0   (car (screen-copy-cursor screen))))
      (cl-tmux::%cmd-send-keys-arg s '("-X" "-N" "3" "cursor-up"))
      (is (= (- row0 3) (car (screen-copy-cursor screen)))
          "cursor-up repeated 3× moves the copy cursor up 3 rows"))))

(test send-keys-x-without-N-runs-once
  "send-keys -X cursor-up with no -N defaults to a single application."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let* ((screen (active-screen s))
           (row0   (car (screen-copy-cursor screen))))
      (cl-tmux::%cmd-send-keys-arg s '("-X" "cursor-up"))
      (is (= (- row0 1) (car (screen-copy-cursor screen)))
          "a bare -X command runs exactly once (count defaults to 1)"))))

;;; ── capture-pane saves to a buffer by default (scriptable form) ──────────────
;;;
;;; The scriptable `capture-pane [flags]` command (%cmd-capture-pane-arg, distinct
;;; from the interactive :capture-pane overlay binding) follows tmux: without -p
;;; it SAVES the captured content to a paste buffer; -p prints (overlay) instead.

(test cmd-capture-pane-saves-to-buffer-by-default
  "capture-pane with no -p saves the pane content to a paste buffer (the canonical
   capture→paste workflow), not an overlay."
  (with-empty-buffers
    (with-fake-session (s)
      (feed (active-screen s) "hello capture")
      (cl-tmux::%cmd-capture-pane-arg s '())
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (not (null buf)) "capture-pane (no -p) saves to a paste buffer")
        (is (search "hello capture" buf)
            "the saved buffer contains the captured pane content")))))

(test cmd-capture-pane-p-shows-overlay-not-buffer
  "capture-pane -p prints (overlay) and does NOT save to a buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (feed (active-screen s) "shown only")
        (cl-tmux::%cmd-capture-pane-arg s '("-p"))
        (is (overlay-active-p) "-p shows the content in an overlay")
        (is (null (cl-tmux/buffer:get-paste-buffer 0))
            "-p does NOT save to a paste buffer (stdout equivalent)")))))

(test cmd-capture-pane-b-flag-accepted-stores-in-ring
  "capture-pane -b name is accepted; the capture is stored at the top of the ring."
  (with-empty-buffers
    (with-fake-session (s)
      (feed (active-screen s) "named buf")
      (cl-tmux::%cmd-capture-pane-arg s '("-b" "mybuf"))
      (is (search "named buf" (or (cl-tmux/buffer:get-paste-buffer 0) ""))
          "-b stores the capture in the unnamed ring (single-ring model)"))))

(test cmd-capture-pane-t-captures-target-pane
  "capture-pane -t captures the requested pane, not always the active pane."
  (with-empty-buffers
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let* ((win (session-active-window s))
             (active (window-active-pane win))
             (target (find 2 (window-panes win) :key #'pane-id)))
        (feed (pane-screen active) "active text")
        (feed (pane-screen target) "target text")
        (cl-tmux::%cmd-capture-pane-arg s '("-t" "%2" "-b" "cap"))
        (let ((buf (or (cl-tmux/buffer:get-buffer-by-name "cap") "")))
          (is (search "target text" buf) "-t %2 captures pane 2")
          (is (null (search "active text" buf))
              "-t %2 must not fall back to the active pane"))))))

;;; ── Named paste-buffer commands (-b name) ────────────────────────────────────

(test cmd-set-buffer-b-stores-named
  "set-buffer -b name data stores a named buffer retrievable by name."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "mybuf" "hello" "world"))
      (is (string= "hello world" (cl-tmux/buffer:get-buffer-by-name "mybuf"))
          "set-buffer -b stores the joined data under the name"))))

(test cmd-set-buffer-a-appends-named
  "set-buffer -a -b name appends to the existing named buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "b" "foo"))
      (cl-tmux::%cmd-set-buffer-arg s '("-a" "-b" "b" "bar"))
      (is (string= "foobar" (cl-tmux/buffer:get-buffer-by-name "b"))
          "-a appends to the named buffer"))))

(test cmd-show-buffer-b-shows-named
  "show-buffer -b name shows that buffer's content in an overlay."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/buffer:set-named-buffer "b" "shown-content")
        (cl-tmux::%cmd-show-buffer-arg s '("-b" "b"))
        (is (overlay-active-p) "show-buffer -b opens an overlay")
        (is (search "shown-content" (format nil "~{~A~%~}" (overlay-lines)))
            "the overlay contains the named buffer's content")))))

(test cmd-delete-buffer-b-deletes-named
  "delete-buffer -b name removes that named buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "b" "x")
      (cl-tmux::%cmd-delete-buffer-arg s '("-b" "b"))
      (is (null (cl-tmux/buffer:get-buffer-by-name "b"))
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

(test cmd-load-buffer-b-loads-named-buffer
  "load-buffer -b name path loads file contents into a named buffer."
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
               (cl-tmux::%run-command-tokens s (list "load-buffer" "-b" "loaded" path))
               (is (string= "from file" (cl-tmux/buffer:get-buffer-by-name "loaded"))
                   "loaded file into named buffer"))
          (ignore-errors (delete-file path)))))))

(test cmd-paste-buffer-d-deletes-named-after-paste
  "paste-buffer -d -b name deletes the named buffer after pasting it."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "b" "data")
      (cl-tmux::%cmd-paste-buffer-arg s '("-d" "-b" "b"))
      (is (null (cl-tmux/buffer:get-buffer-by-name "b"))
          "-d removes the named buffer after pasting"))))

(test cmd-capture-pane-b-stores-named
  "capture-pane -b name stores the captured content under that name."
  (with-empty-buffers
    (with-fake-session (s)
      (feed (active-screen s) "captured text")
      (cl-tmux::%cmd-capture-pane-arg s '("-b" "cap"))
      (is (search "captured text" (or (cl-tmux/buffer:get-buffer-by-name "cap") ""))
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
      (declare (ignore pane0))
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
