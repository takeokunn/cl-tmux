(in-package #:cl-tmux/test)

;;;; event mouse dispatch, X10 sequences, middle-click paste, defaults

(in-suite events-suite)

;;; ── Mouse event dispatch tests ───────────────────────────────────────────────
;;;
;;; The with-two-pane-mouse-session macro (defined in tests/helpers-mouse-fixtures.lisp) builds
;;; the 2-pane h-split session, enables the 'mouse' option, and wraps the body
;;; in with-loop-state with appropriate *term-rows*/*term-cols* bindings.

(test dispatch-mouse-event-left-click-selects-pane
  "%dispatch-mouse-event with btn=0 release=NIL selects the pane at the given coordinates."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Click in the right pane (col 50, row 5)
    (cl-tmux::%dispatch-mouse-event sess 0 50 5 nil)
    (is (eq p1 (window-active-pane win))
        "left click in right half should focus p1")))

(test dispatch-mouse-event-release-does-not-select
  "%dispatch-mouse-event with release-p=T does not switch the active pane."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Release event (btn=0, release-p=T) — must not change active pane
    (cl-tmux::%dispatch-mouse-event sess 0 50 5 t)
    (is (eq p0 (window-active-pane win))
        "button release should not change the active pane")))

(test x10-mouse-sequence-via-process-byte
  "X10 mouse press ESC [ M <btn+32> <col+33> <row+33> fed one byte at a time
   selects the pane at the encoded coordinates."
  (with-two-pane-mouse-session (sess win p0 p1)
    ;; Enable per-screen mouse mode in addition to the session option.
    (setf (screen-mouse-mode (pane-screen p0)) 1)
    (with-input-state (input-state)
      ;; X10: btn=0 → 0+32=32; col=50 → 50+33=83; row=5 → 5+33=38
      ;; Sequence: ESC(27) [(91) M(77) 32 83 38
      (feed-bytes sess input-state '(27 91 77 32 83 38))
      (is (eq p1 (window-active-pane win))
          "X10 left-click in right pane must focus p1"))))

(test mouse-middle-click-pastes-top-buffer-into-pane
  "Middle-button press (btn 1) pastes the most recent paste-buffer into the pane
   under the pointer, writing it to that pane's PTY."
  (with-empty-buffers
    (with-pipe-fds (rfd wfd)
      (with-two-pane-mouse-session (sess win p0 p1)
        ;; Give the right pane (p1) a live PTY and stage a paste-buffer.
        (setf (pane-fd p1) wfd)
        (cl-tmux/buffer:add-paste-buffer "PASTE-ME")
        ;; Middle-click at col 50 (within p1, x=41..80), row 5.
        (cl-tmux::%dispatch-mouse-event sess 1 50 5 nil)
        (is (eq p1 (window-active-pane win))
            "middle-click must focus the pane under the pointer")
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
          (is-true ready "the pasted text must reach the pane's PTY")
          (when ready
            (cffi:with-foreign-object (buf :uint8 32)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 8
                                             :long)))
                (is (= 8 n) "all 8 bytes of PASTE-ME must arrive (got ~D)" n)
                (let ((str (make-string (max 0 n))))
                  (dotimes (i (max 0 n))
                    (setf (char str i) (code-char (cffi:mem-aref buf :uint8 i))))
                  (is (string= "PASTE-ME" str)
                      "pane must receive the buffer text (got ~S)" str))))))))))

(test mouse-middle-click-with-empty-buffer-writes-nothing
  "Middle-click with no paste-buffer is a safe no-op: the pane is focused but no
   bytes are written to its PTY."
  (with-empty-buffers
    (with-pipe-fds (rfd wfd)
      (with-two-pane-mouse-session (sess win p0 p1)
        (setf (pane-fd p1) wfd)
        ;; No add-paste-buffer → get-paste-buffer 0 is NIL.
        (cl-tmux::%dispatch-mouse-event sess 1 50 5 nil)
        (is (eq p1 (window-active-pane win))
            "middle-click still focuses the pane under the pointer")
        (is (null (cl-tmux/pty:select-fds (list rfd) 20000))
            "no paste-buffer → nothing is written (pipe stays idle)")))))

(test mouse-middle-click-release-does-not-paste
  "A middle-button RELEASE event must not paste (only the press does)."
  (with-empty-buffers
    (with-pipe-fds (rfd wfd)
      (with-two-pane-mouse-session (sess win p0 p1)
        (setf (pane-fd p1) wfd)
        (cl-tmux/buffer:add-paste-buffer "NOPE")
        ;; release-p = T → no paste
        (cl-tmux::%dispatch-mouse-event sess 1 50 5 t)
        (is (null (cl-tmux/pty:select-fds (list rfd) 20000))
            "middle-button release must not write any paste bytes")))))

(test mouse-mode-defaults-table
  "Fresh screen mouse defaults: mouse-mode=0, mouse-sgr-mode=nil."
  (dolist (c '((screen-mouse-mode     0   "mouse-mode defaults to 0")
               (screen-mouse-sgr-mode nil "mouse-sgr-mode defaults to nil")))
    (destructuring-bind (accessor expected desc) c
      (with-screen (s 20 5)
        (is (equal expected (funcall accessor s)) "~A" desc)))))
