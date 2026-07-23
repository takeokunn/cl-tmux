(in-package #:cl-tmux/test)

;;;; Cross-session window linking, unlinking, and grouped-session teardown.

(describe "server-suite"

  ;; %window-session-count returns the number of registered sessions holding a window.
  (it "window-session-count-counts-sessions-containing-window"
    (with-empty-registry
      (let* ((alpha (make-fake-session :nwindows 1))
             (beta  (make-fake-session :nwindows 1))
             (win   (first (cl-tmux/model:session-windows alpha))))
        (setf (cl-tmux/model:session-name alpha) "alpha"
              (cl-tmux/model:session-name beta)  "beta")
        (cl-tmux::server-add-session alpha)
        (cl-tmux::server-add-session beta)
        (expect (= 1 (cl-tmux::%window-session-count win)))
        (cl-tmux/model:session-insert-window beta win)
        (expect (= 2 (cl-tmux::%window-session-count win))))))

  ;; link-window -s src -t dst makes the source window appear in dst (no collision).
  (it "link-window-shares-window-into-destination"
    (with-empty-registry
      (let* ((alpha (make-fake-session :nwindows 1))
             (beta  (make-fake-session :nwindows 1))
             (alpha-win (first (cl-tmux/model:session-windows alpha))))
        (setf (cl-tmux/model:session-name alpha) "alpha"
              (cl-tmux/model:session-name beta)  "beta")
        (setf (cl-tmux/model:window-id (first (cl-tmux/model:session-windows beta))) 9)
        (cl-tmux::server-add-session alpha)
        (cl-tmux::server-add-session beta)
        (let ((cl-tmux/prompt:*overlay* nil))
          (cl-tmux::%cmd-link-window alpha '("-s" "alpha:0" "-t" "beta")))
        (expect (member alpha-win (cl-tmux/model:session-windows beta)) :to-be-truthy))))

  ;; unlink-window on a window shared by 2 sessions removes it from the target only.
  (it "unlink-window-shared-removes-from-one-session-only"
    (with-empty-registry
      (let* ((alpha (make-fake-session :nwindows 1))
             (beta  (make-fake-session :nwindows 1))
             (win   (first (cl-tmux/model:session-windows alpha))))
        (setf (cl-tmux/model:session-name alpha) "alpha"
              (cl-tmux/model:session-name beta)  "beta")
        (cl-tmux::server-add-session alpha)
        (cl-tmux::server-add-session beta)
        (cl-tmux/model:session-insert-window beta win)
        (cl-tmux/model:session-select-window beta win)
        (let ((cl-tmux/prompt:*overlay* nil))
          (cl-tmux::%cmd-unlink-window beta nil))
        (expect (member win (cl-tmux/model:session-windows beta)) :to-be-falsy)
        (expect (member win (cl-tmux/model:session-windows alpha)) :to-be-truthy))))

  ;; link-window fires +hook-window-linked+ when a window is linked in.
  (it "link-window-fires-window-linked-hook"
    (with-empty-registry
      (with-isolated-hooks
        (let* ((alpha (make-fake-session :nwindows 1))
               (beta  (make-fake-session :nwindows 1))
               (fired nil))
          (setf (cl-tmux/model:session-name alpha) "alpha"
                (cl-tmux/model:session-name beta)  "beta")
          (setf (cl-tmux/model:window-id (first (cl-tmux/model:session-windows beta))) 9)
          (cl-tmux::server-add-session alpha)
          (cl-tmux::server-add-session beta)
          (cl-tmux/hooks:add-hook "window-linked"
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (let ((cl-tmux/prompt:*overlay* nil))
            (cl-tmux::%cmd-link-window alpha '("-s" "alpha:0" "-t" "beta")))
          (expect fired :to-be-truthy)))))

  ;; unlink-window fires +hook-window-unlinked+ when a shared window is unlinked.
  (it "unlink-window-fires-window-unlinked-hook"
    (with-empty-registry
      (with-isolated-hooks
        (let* ((alpha (make-fake-session :nwindows 1))
               (beta  (make-fake-session :nwindows 1))
               (win   (first (cl-tmux/model:session-windows alpha)))
               (fired nil))
          (setf (cl-tmux/model:session-name alpha) "alpha"
                (cl-tmux/model:session-name beta)  "beta")
          (cl-tmux::server-add-session alpha)
          (cl-tmux::server-add-session beta)
          (cl-tmux/model:session-insert-window beta win)
          (cl-tmux/model:session-select-window beta win)
          (cl-tmux/hooks:add-hook "window-unlinked"
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (let ((cl-tmux/prompt:*overlay* nil))
            (cl-tmux::%cmd-unlink-window beta nil))
          (expect fired :to-be-truthy)))))

  ;; %destroy-session removes the session AND fires +hook-session-closed+.
  (it "destroy-session-fires-session-closed-hook"
    (with-empty-registry
      (with-isolated-hooks
        (let ((fired nil))
          (with-fake-session (s :nwindows 1)
            (cl-tmux::server-add-session s)
            (cl-tmux/hooks:add-hook "session-closed"
                                    (lambda (&rest _) (declare (ignore _)) (setf fired t)))
            (cl-tmux::%destroy-session s)
            (expect fired :to-be-truthy))))))

  ;; Destroying ONE session in a group must NOT close the PTYs of a window another
  ;; grouped session still shares.
  (it "destroy-grouped-session-keeps-shared-window-ptys-open"
    (with-empty-registry
      (let ((target  (make-fake-session :nwindows 1))
            (grouped (make-fake-session :nwindows 1))
            (closed  0))
        (setf (cl-tmux::session-name target)  "base"
              (cl-tmux::session-name grouped) "clone"
              (cl-tmux::session-windows grouped) (cl-tmux::session-windows target))
        (cl-tmux::server-add-session target)
        (cl-tmux::server-add-session grouped)
        (let ((orig (fdefinition 'cl-tmux/pty:pty-close)))
          (unwind-protect
               (progn
                 (setf (fdefinition 'cl-tmux/pty:pty-close)
                       (lambda (fd pid) (declare (ignore fd pid)) (incf closed)))
                 (cl-tmux::%destroy-session grouped))
            (setf (fdefinition 'cl-tmux/pty:pty-close) orig)))
        (expect (zerop closed))
        (expect (null (cl-tmux::server-find-session "clone")))
        (expect (not (null (cl-tmux::server-find-session "base")))))))

  ;; Regression guard: an ungrouped (single-reference) session's PTYs ARE still
  ;; closed on destroy - the reference-counted guard does not change the common case.
  (it "destroy-ungrouped-session-closes-its-ptys"
    (with-empty-registry
      (let ((sess   (make-fake-session :nwindows 1 :npanes 2))
            (closed  0))
        (setf (cl-tmux::session-name sess) "solo")
        (cl-tmux::server-add-session sess)
        (let ((orig (fdefinition 'cl-tmux/pty:pty-close)))
          (unwind-protect
               (progn
                 (setf (fdefinition 'cl-tmux/pty:pty-close)
                       (lambda (fd pid) (declare (ignore fd pid)) (incf closed)))
                 (cl-tmux::%destroy-session sess))
            (setf (fdefinition 'cl-tmux/pty:pty-close) orig)))
        (expect (= 2 closed)))))

  ;; rename-session removes+re-adds its registry entry but must NOT fire
  ;; session-closed (only actual destruction does).
  (it "rename-session-does-not-fire-session-closed"
    (with-empty-registry
      (with-isolated-hooks
        (let ((s (make-fake-session :nwindows 1))
              (fired nil))
          (cl-tmux::server-add-session s)
          (cl-tmux/hooks:add-hook "session-closed"
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%cmd-rename-session s '("renamed"))
          (expect fired :to-be-falsy)))))

  ;; unlink-window on a window present in only one session is refused without -k.
  (it "unlink-window-only-session-needs-k-flag"
    (with-empty-registry
      (let* ((alpha (make-fake-session :nwindows 2))
             (win   (first (cl-tmux/model:session-windows alpha))))
        (setf (cl-tmux/model:session-name alpha) "alpha")
        (cl-tmux::server-add-session alpha)
        (cl-tmux/model:session-select-window alpha win)
        (let ((cl-tmux/prompt:*overlay* nil))
          (cl-tmux::%cmd-unlink-window alpha nil))
        (expect (member win (cl-tmux/model:session-windows alpha)) :to-be-truthy)))))
