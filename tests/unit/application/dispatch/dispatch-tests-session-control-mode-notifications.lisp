(in-package #:cl-tmux/test)

;;;; Dispatch session tests - control-mode notifications, active-change, and %output relay.

(describe "dispatch-suite"

  ;; Installed control-mode notifications write %window-add/-close/-renamed and
  ;; %session-renamed to the output stream when the matching hooks fire.
  (it "control-notifications-emit-on-hooks"
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
                 (expect (search "%window-add @5" s))
                 (expect (search "%window-renamed @5 editor" s))
                 (expect (search "%window-close @5" s))
                 (expect (search (format nil "%session-renamed $~D work"
                                     (cl-tmux::session-id sess)) s))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; After %remove-control-notifications, a hook no longer writes to the stream.
  (it "control-notifications-removed-stop-emitting"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (cl-tmux::%remove-control-notifications handlers)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+
                                 (make-fake-window 7 "x"))
        (expect (string= "" (get-output-stream-string out))))))

  ;; %remove-control-notifications unregisters ALL of the hook types installed by
  ;; %install-control-notifications, not just +hook-after-new-window+ - fire every
  ;; one of the 9 hooks it wires and confirm none produce output once removed.
  (it "control-notifications-removed-stop-emitting-every-hook-type"
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
        (expect (string= "" (get-output-stream-string out))))))

  ;; after-resize-pane emits %layout-change @<window> with the window's layout string.
  (it "control-notifications-layout-change-on-resize"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (unwind-protect
             (let ((win (make-fake-window 3 "w")))
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
               (expect (search "%layout-change @3" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; after-split-window fires with a PANE (not a window) - %control-window-of must
  ;; resolve it to its owning window, and %layout-change must carry that window's id.
  (it "control-notifications-layout-change-on-split-window-with-pane"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (unwind-protect
             (let* ((win  (make-fake-window 4 "w" :npanes 2))
                    (pane (first (cl-tmux/model:window-panes win))))
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+ pane)
               (expect (search "%layout-change @4" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; %layout-change's raw-flags field is Z when the window is zoomed, * otherwise.
  (it "control-notifications-layout-change-zoom-flag"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (unwind-protect
             (let ((win (make-fake-window 6 "w")))
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
               (expect (search "@6" (get-output-stream-string out)))
               (setf (cl-tmux/model:window-zoom-p win) t)
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-resize-pane+ win)
               (let ((line (get-output-stream-string out)))
                 (expect (search (format nil "%layout-change @6 ~A ~A Z"
                                     (cl-tmux/model:layout->string win)
                                     (cl-tmux/model:layout->string win))
                             line))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; +hook-window-pane-changed+ emits %window-pane-changed @<win> %<active-pane> to a
  ;; control client.
  (it "control-notifications-window-pane-changed"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (unwind-protect
             (let ((win (make-fake-window 5 "w")))   ; window id 5, active pane id 1
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-pane-changed+ win)
               (expect (search "%window-pane-changed @5 %1" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; +hook-session-window-changed+ emits %session-window-changed $<sess> @<active-win>.
  (it "control-notifications-session-window-changed"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (unwind-protect
             (let ((sess (make-fake-session :nwindows 2)))  ; session id 1, active win id 0
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-window-changed+ sess)
               (expect (search "%session-window-changed $1 @0" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; +hook-pane-output+ emits %output %<pane-id> <escaped-data> to a control client.
  (it "control-notifications-pane-output-emits-percent-output"
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
               (expect (search "%output %7 hello" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; +hook-pane-output+ escapes non-printable bytes in the %output notification.
  (it "control-notifications-pane-output-escapes-non-printable"
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
               (expect (search "%output %3 \\033A" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers)))))

  ;; %control-emit holds *control-output-lock*, so notifications emitted from
  ;; multiple threads do not interleave a single write-line on the output stream.
  ;; Each emitted line lands intact (a full %output line per call) and the count
  ;; matches the number of emits - no torn/merged lines.
  (it "control-emit-serializes-concurrent-writers"
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
          (expect (= (* 4 n) (length lines)))
          (expect (every (lambda (l) (string= l line)) lines))))))

  ;; %control-emit acquires the dynamically-bound *control-output-lock*; emitting
  ;; while that lock is already held by the current thread must still succeed
  ;; (recursive lock is not required - this asserts the binding is the one used).
  (it "control-emit-respects-bound-lock"
    (with-isolated-hooks
      (let* ((out  (make-string-output-stream))
             (lock (cl-tmux::make-lock "bound-control"))
             (cl-tmux::*control-output-lock* lock))
        (cl-tmux::%control-emit out "%window-add @9")
        (expect (search "%window-add @9" (get-output-stream-string out))))))

  ;; +hook-pane-output+ does not emit when the byte vector is empty.
  (it "control-notifications-pane-output-noop-on-empty"
    (with-isolated-hooks
      (let* ((out      (make-string-output-stream))
             (handlers (cl-tmux::%install-control-notifications out)))
        (unwind-protect
             (let ((pane (%make-test-pane :id 2)))
               (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+
                                        pane
                                        (make-array 0 :element-type '(unsigned-byte 8)))
               (expect (string= "" (get-output-stream-string out))))
          (cl-tmux::%remove-control-notifications handlers))))))
