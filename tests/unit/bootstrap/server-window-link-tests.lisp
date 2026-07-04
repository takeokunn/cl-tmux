(in-package #:cl-tmux/test)

;;;; Cross-session window linking, unlinking, and grouped-session teardown.

(in-suite server-suite)

(test window-session-count-counts-sessions-containing-window
  "%window-session-count returns the number of registered sessions holding a window."
  (with-empty-registry
    (let* ((alpha (make-fake-session :nwindows 1))
           (beta  (make-fake-session :nwindows 1))
           (win   (first (cl-tmux/model:session-windows alpha))))
      (setf (cl-tmux/model:session-name alpha) "alpha"
            (cl-tmux/model:session-name beta)  "beta")
      (cl-tmux::server-add-session alpha)
      (cl-tmux::server-add-session beta)
      (is (= 1 (cl-tmux::%window-session-count win))
          "window initially in 1 session")
      (cl-tmux/model:session-insert-window beta win)
      (is (= 2 (cl-tmux::%window-session-count win))
          "after sharing, window is in 2 sessions"))))

(test link-window-shares-window-into-destination
  "link-window -s src -t dst makes the source window appear in dst (no collision)."
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
      (is-true (member alpha-win (cl-tmux/model:session-windows beta))
               "alpha's window must now appear in beta after link-window"))))

(test unlink-window-shared-removes-from-one-session-only
  "unlink-window on a window shared by 2 sessions removes it from the target only."
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
      (is-false (member win (cl-tmux/model:session-windows beta))
                "window unlinked from beta")
      (is-true (member win (cl-tmux/model:session-windows alpha))
               "window still present in alpha (not orphaned)"))))

(test link-window-fires-window-linked-hook
  "link-window fires +hook-window-linked+ when a window is linked in."
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
        (is-true fired "window-linked hook must fire on link-window")))))

(test unlink-window-fires-window-unlinked-hook
  "unlink-window fires +hook-window-unlinked+ when a shared window is unlinked."
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
        (is-true fired "window-unlinked hook must fire on unlink-window")))))

(test destroy-session-fires-session-closed-hook
  "%destroy-session removes the session AND fires +hook-session-closed+."
  (with-empty-registry
    (with-isolated-hooks
      (let ((fired nil))
        (with-fake-session (s :nwindows 1)
          (cl-tmux::server-add-session s)
          (cl-tmux/hooks:add-hook "session-closed"
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%destroy-session s)
          (is-true fired "session-closed hook must fire on destroy"))))))

(test destroy-grouped-session-keeps-shared-window-ptys-open
  "Destroying ONE session in a group must NOT close the PTYs of a window another
   grouped session still shares."
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
      (is (zerop closed)
          "shared window's PTYs must NOT be closed while 'base' still references them")
      (is (null (cl-tmux::server-find-session "clone"))
          "the destroyed grouped session is removed from the registry")
      (is (not (null (cl-tmux::server-find-session "base")))
          "the surviving grouped session remains"))))

(test destroy-ungrouped-session-closes-its-ptys
  "Regression guard: an ungrouped (single-reference) session's PTYs ARE still
   closed on destroy - the reference-counted guard does not change the common case."
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
      (is (= 2 closed)
          "both panes of the unshared window are closed (window-session-count = 1)"))))

(test rename-session-does-not-fire-session-closed
  "rename-session removes+re-adds its registry entry but must NOT fire
   session-closed (only actual destruction does)."
  (with-empty-registry
    (with-isolated-hooks
      (let ((s (make-fake-session :nwindows 1))
            (fired nil))
        (cl-tmux::server-add-session s)
        (cl-tmux/hooks:add-hook "session-closed"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-rename-session s '("renamed"))
        (is-false fired "rename-session must NOT fire session-closed")))))

(test unlink-window-only-session-needs-k-flag
  "unlink-window on a window present in only one session is refused without -k."
  (with-empty-registry
    (let* ((alpha (make-fake-session :nwindows 2))
           (win   (first (cl-tmux/model:session-windows alpha))))
      (setf (cl-tmux/model:session-name alpha) "alpha")
      (cl-tmux::server-add-session alpha)
      (cl-tmux/model:session-select-window alpha win)
      (let ((cl-tmux/prompt:*overlay* nil))
        (cl-tmux::%cmd-unlink-window alpha nil))
      (is-true (member win (cl-tmux/model:session-windows alpha))
               "window must remain without -k (would orphan otherwise)"))))
