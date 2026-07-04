(in-package #:cl-tmux/test)

;;;; Dispatch session tests - control-mode notifications, active-change, and %output relay.

(in-suite dispatch-suite)

(test control-notifications-emit-on-hooks
  "Installed control-mode notifications write %window-add/-close/-renamed and
   %session-renamed to the output stream when the matching hooks fire."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win  (make-fake-window 5 "editor"))
                 (sess (make-fake-session)))
             (setf (cl-tmux::session-name sess) "work")
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+ win)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-renamed+ win)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-kill-window+ win)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-renamed+ sess)
             (let ((s (get-output-stream-string out)))
               (is (search "%window-add @5" s)         "%window-add emitted")
               (is (search "%window-renamed @5 editor" s) "%window-renamed emitted")
               (is (search "%window-close @5" s)       "%window-close emitted")
               (is (search (format nil "%session-renamed $~D work"
                                   (cl-tmux::session-id sess)) s)
                   "%session-renamed emitted")))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-removed-stop-emitting
  "After %remove-control-notifications, a hook no longer writes to the stream."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (cl-tmux::%remove-control-notifications handlers)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+
                               (make-fake-window 7 "x"))
      (is (string= "" (get-output-stream-string out))
          "no notification after the callbacks are removed"))))

(test control-notifications-removed-stop-emitting-every-hook-type
  "%remove-control-notifications unregisters ALL of the hook types installed by
   %install-control-notifications, not just +hook-after-new-window+ - fire every
   one of the 9 hooks it wires and confirm none produce output once removed."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out))
           (win      (make-fake-window 8 "y" :npanes 2))
           (pane     (first (cl-tmux/model:window-panes win)))
           (sess     (make-fake-session :nwindows 2)))
      (cl-tmux::%remove-control-notifications handlers)
      (dolist (fire (list
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-after-new-window+ win))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-after-kill-window+ win))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-window-renamed+ win))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-session-renamed+ sess))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-window-pane-changed+ win))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-session-window-changed+ sess))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-after-resize-pane+ win))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-after-split-window+ pane))
                     (lambda () (cl-tmux/hooks:run-hooks
                                 cl-tmux/hooks:+hook-pane-output+ pane
                                 (coerce '(104 105) '(vector (unsigned-byte 8)))))))
        (funcall fire))
      (is (string= "" (get-output-stream-string out))
          "none of the 9 installed hook types emit after removal"))))

(test control-notifications-layout-change-on-resize
  "after-resize-pane emits %layout-change @<window> with the window's layout string."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win (make-fake-window 3 "w")))
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
             (is (search "%layout-change @3" (get-output-stream-string out))
                 "%layout-change emitted on resize"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-layout-change-on-split-window-with-pane
  "after-split-window fires with a PANE (not a window) - %control-window-of must
   resolve it to its owning window, and %layout-change must carry that window's id."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let* ((win  (make-fake-window 4 "w" :npanes 2))
                  (pane (first (cl-tmux/model:window-panes win))))
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+ pane)
             (is (search "%layout-change @4" (get-output-stream-string out))
                 "%layout-change carries the pane's owning window id, not the pane id"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-layout-change-zoom-flag
  "%layout-change's raw-flags field is Z when the window is zoomed, * otherwise."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win (make-fake-window 6 "w")))
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
             (is (search "@6" (get-output-stream-string out))
                 "sanity: not-zoomed window still emits a layout-change")
             (setf (cl-tmux/model:window-zoom-p win) t)
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
             (let ((line (get-output-stream-string out)))
               (is (search (format nil "%layout-change @6 ~A ~A Z"
                                   (cl-tmux/model:layout->string win)
                                   (cl-tmux/model:layout->string win))
                           line)
                   "zoomed window's %layout-change ends with the Z flag")))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-window-pane-changed
  "+hook-window-pane-changed+ emits %window-pane-changed @<win> %<active-pane> to a
   control client."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((win (make-fake-window 5 "w")))   ; window id 5, active pane id 1
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-pane-changed+ win)
             (is (search "%window-pane-changed @5 %1" (get-output-stream-string out))
                 "emitted with the window id and its active pane id"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-session-window-changed
  "+hook-session-window-changed+ emits %session-window-changed $<sess> @<active-win>."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((sess (make-fake-session :nwindows 2)))  ; session id 1, active win id 0
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-window-changed+ sess)
             (is (search "%session-window-changed $1 @0" (get-output-stream-string out))
                 "emitted with the session id and its active window id"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-pane-output-emits-percent-output
  "+hook-pane-output+ emits %output %<pane-id> <escaped-data> to a control client."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((pane (%make-test-pane :id 7)))
             ;; Fire the hook with an octet vector (as runtime.lisp does).
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                      pane
                                      (coerce '(104 101 108 108 111) ; "hello"
                                              '(vector (unsigned-byte 8))))
             (is (search "%output %7 hello" (get-output-stream-string out))
                 "%output notification emitted with pane id and escaped bytes"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-notifications-pane-output-escapes-non-printable
  "+hook-pane-output+ escapes non-printable bytes in the %output notification."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((pane (%make-test-pane :id 3)))
             ;; ESC (27 = octal 033) followed by 'A' (65).
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                      pane
                                      (coerce '(27 65)
                                              '(vector (unsigned-byte 8))))
             (is (search "%output %3 \\033A" (get-output-stream-string out))
                 "ESC byte is escaped to \\033 in %output notification"))
        (cl-tmux::%remove-control-notifications handlers)))))

(test control-emit-serializes-concurrent-writers
  "%control-emit holds *control-output-lock*, so notifications emitted from
   multiple threads do not interleave a single write-line on the output stream.
   Each emitted line lands intact (a full %output line per call) and the count
   matches the number of emits - no torn/merged lines."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (cl-tmux::*control-output-lock* (cl-tmux::make-lock "test-control"))
           (n        200)
           (line     "%output %1 hello")
           (threads  (loop repeat 4
                           collect (cl-tmux::make-thread
                                    (lambda ()
                                      (dotimes (i n)
                                        (cl-tmux::%control-emit out line)))))))
      (mapc #'cl-tmux::join-thread threads)
      (let* ((s     (get-output-stream-string out))
             (lines (with-input-from-string (in s)
                      (loop for l = (read-line in nil nil)
                            while l collect l))))
        (is (= (* 4 n) (length lines))
            "every %control-emit produced exactly one whole line (no torn writes)")
        (is (every (lambda (l) (string= l line)) lines)
            "every line is the intact %output line (no interleaving)")))))

(test control-emit-respects-bound-lock
  "%control-emit acquires the dynamically-bound *control-output-lock*; emitting
   while that lock is already held by the current thread must still succeed
   (recursive lock is not required - this asserts the binding is the one used)."
  (with-isolated-hooks
    (let* ((out  (make-string-output-stream))
           (lock (cl-tmux::make-lock "bound-control"))
           (cl-tmux::*control-output-lock* lock))
      (cl-tmux::%control-emit out "%window-add @9")
      (is (search "%window-add @9" (get-output-stream-string out))
          "emit through the bound lock writes the line"))))

(test control-notifications-pane-output-noop-on-empty
  "+hook-pane-output+ does not emit when the byte vector is empty."
  (with-isolated-hooks
    (let* ((out      (make-string-output-stream))
           (handlers (cl-tmux::%install-control-notifications out)))
      (unwind-protect
           (let ((pane (%make-test-pane :id 2)))
             (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                      pane
                                      (make-array 0 :element-type '(unsigned-byte 8)))
             (is (string= "" (get-output-stream-string out))
                 "empty byte vector must not emit %output"))
        (cl-tmux::%remove-control-notifications handlers)))))
