(in-package #:cl-tmux/test)

;;;; Arg-taking target and window command dispatch tests

(describe "dispatch-suite"

  ;; ── kill-window / kill-pane with -t target ───────────────────────────────────

  ;; 'kill-window -t N' kills the window whose window-id is N.
  (it "run-command-line-kill-window-by-target"
    (with-fake-session (s :nwindows 3)
      (cl-tmux::%run-command-line s "kill-window -t 1")
      (expect (= 2 (length (session-windows s))))
      (expect (null (find 1 (session-windows s) :key #'window-id)))))

  ;; 'kill-pane -t N' kills the pane with pane-id N in the active window.
  (it "run-command-line-kill-pane-by-target"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s)))   ; pane-ids 1,2
        (cl-tmux::%run-command-line s "kill-pane -t 2")
        (expect (= 1 (length (window-panes win))))
        (expect (null (find 2 (window-panes win) :key #'pane-id))))))

  ;; 'kill-pane -t <nonexistent>' must NOT kill the active pane by accident.
  (it "run-command-line-kill-pane-invalid-target-is-noop"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s)))
        (cl-tmux::%run-command-line s "kill-pane -t 99")
        (expect (= 2 (length (window-panes win)))))))

  ;; 'kill-window' with no -t kills the active window (name-table fallthrough).
  (it "run-command-line-kill-window-no-arg-kills-active"
    (with-fake-session (s :nwindows 2)
      (let* ((active (session-active-window s)))
        (cl-tmux::%run-command-line s "kill-window")
        (expect (= 1 (length (session-windows s))))
        (expect (null (find active (session-windows s)))))))

  ;; kill-window and kill-pane reject unknown flags and extra positionals.
  (it "run-command-line-kill-commands-reject-unsupported-arguments"
    (dolist (case '(("kill-window extra" "kill-window: unsupported argument" :windows)
                    ("kill-window -Z" "kill-window: unsupported argument" :windows)
                    ("kill-pane extra" "kill-pane: unsupported argument" :panes)
                    ("kill-pane -Z" "kill-pane: unsupported argument" :panes)))
      (destructuring-bind (cmd message kind) case
        (ecase kind
          (:windows
           (with-fake-session (s :nwindows 2)
             (let ((before (copy-list (session-windows s)))
                   (*overlay* nil))
               (cl-tmux::%run-command-line s cmd)
               (assert-overlay-contains message *overlay* cmd)
               (expect (equal before (session-windows s))))))
          (:panes
           (with-fake-two-pane-session (s)
             (let* ((win (session-active-window s))
                    (before (copy-list (window-panes win)))
                    (*overlay* nil))
               (cl-tmux::%run-command-line s cmd)
               (assert-overlay-contains message *overlay* cmd)
               (expect (equal before (window-panes win))))))))))

  ;; link-window and unlink-window reject unknown flags and extra positionals.
  (it "run-command-line-link-commands-reject-unsupported-arguments"
    (dolist (cmd '("link-window -t dst extra" "link-window -Z -t dst"))
      (with-fake-session (src :nwindows 1)
        (with-fake-session (dst :nwindows 1)
          (setf (session-name src) "src"
                (session-name dst) "dst")
          (let ((src-before (copy-list (session-windows src)))
                (dst-before (copy-list (session-windows dst)))
                (cl-tmux::*server-sessions* (list (cons "src" src) (cons "dst" dst)))
                (*overlay* nil))
            (cl-tmux::%run-command-line src cmd)
            (assert-overlay-contains "link-window: unsupported argument"
                                      *overlay* cmd)
            (expect (equal src-before (session-windows src)))
            (expect (equal dst-before (session-windows dst))))))))

  ;; 'link-window -s 0 -t dst -k' (no -d) makes the linked window current in dst.
  (it "run-command-line-link-window-selects-linked-window-by-default"
    (with-fake-session (src :nwindows 1)
      (with-fake-session (dst :nwindows 1)
        (setf (session-name src) "src"
              (session-name dst) "dst")
        (let* ((src-win (session-active-window src))
               (cl-tmux::*server-sessions* (list (cons "src" src) (cons "dst" dst)))
               (*overlay* nil))
          (cl-tmux::%run-command-line src "link-window -s 0 -t dst -k")
          (expect (eq src-win (session-active-window dst)))))))

  ;; 'link-window -d -s 0 -t dst -k' leaves the destination's active window unchanged.
  (it "run-command-line-link-window-d-keeps-dst-active-window"
    (with-fake-session (src :nwindows 1)
      (with-fake-session (dst :nwindows 2)
        (setf (session-name src) "src"
              (session-name dst) "dst")
        (let* ((dst-active (session-active-window dst))
               (cl-tmux::*server-sessions* (list (cons "src" src) (cons "dst" dst)))
               (*overlay* nil))
          (cl-tmux::%run-command-line src "link-window -d -s 0 -t dst -k")
          (expect (eq dst-active (session-active-window dst)))))))

  ;; unlink-window rejects unknown flags and extra positionals before mutating the window list.
  (it "run-command-line-unlink-commands-reject-unsupported-arguments"
    (dolist (cmd '("unlink-window extra" "unlink-window -Z"))
      (with-fake-session (s :nwindows 2)
        (let ((before (copy-list (session-windows s)))
              (*overlay* nil))
          (cl-tmux::%run-command-line s cmd)
          (assert-overlay-contains "unlink-window: unsupported argument"
                                    *overlay* cmd)
          (expect (equal before (session-windows s)))))))

  ;; ── swap-window -s -t (two value flags) ──────────────────────────────────────

  ;; 'swap-window -s X -t Y' exchanges the two windows' INDEX NUMBERS (ids): the
  ;; content at X and Y trade indices, the list stays sorted by id.
  (it "run-command-line-swap-window-exchanges-indices"
    (with-fake-session (s :nwindows 3)
      (let* ((w0 (find 0 (session-windows s) :key #'window-id))
             (w1 (find 1 (session-windows s) :key #'window-id))
             (w2 (find 2 (session-windows s) :key #'window-id)))
        (cl-tmux::%run-command-line s "swap-window -s 0 -t 2")
        (dolist (row (list (list w0 2 "window formerly #0 now has index 2")
                           (list w2 0 "window formerly #2 now has index 0")
                           (list w1 1 "the middle window keeps index 1")))
          (destructuring-bind (win expected desc) row
            (declare (ignore desc))
            (expect (= expected (window-id win)))))
        (expect (equal '(0 1 2) (mapcar #'window-id (session-windows s)))))))

  ;; 'swap-window -t Y' uses the active window as the source.
  (it "run-command-line-swap-window-default-source-is-active"
    (with-fake-session (s :nwindows 2)
      (let* ((active (session-active-window s))
             (w1     (find 1 (session-windows s) :key #'window-id)))
        (cl-tmux::%run-command-line s "swap-window -t 1")
        (expect (= 1 (window-id active)))
        (expect (= 0 (window-id w1))))))

  ;; swap-window without -d selects the swapped source window; with -d the
  ;; pre-selected window stays active.
  ;; Each row: (pre-select-id command description).
  (it "run-command-line-swap-window-selects-source-or-keeps-active"
    (dolist (row '((0 "swap-window -s 0 -t 2"    "no -d: swapped source becomes active")
                   (1 "swap-window -d -s 0 -t 2" "-d: pre-selected window stays active")))
      (destructuring-bind (pre-id cmd desc) row
        (declare (ignore desc))
        (with-fake-session (s :nwindows 3)
          (let ((pre-win (find pre-id (session-windows s) :key #'window-id)))
            (session-select-window s pre-win)
            (cl-tmux::%run-command-line s cmd)
            (expect (eq pre-win (session-active-window s))))))))

  ;; 'swap-window -s 0 -t 99' (no such dst) leaves the window indices unchanged.
  (it "run-command-line-swap-window-unknown-target-is-noop"
    (with-fake-session (s :nwindows 3)
      (let* ((ids-before  (mapcar #'window-id (session-windows s))))
        (cl-tmux::%run-command-line s "swap-window -s 0 -t 99")
        (expect (equal ids-before (mapcar #'window-id (session-windows s)))))))

  ;; swap-window rejects unknown flags and extra positionals before swapping.
  (it "run-command-line-swap-window-rejects-unsupported-arguments"
    (dolist (command '("swap-window extra"
                       "swap-window -Z"
                       "swap-window -s 0 -t 2 extra"))
      (with-fake-session (s :nwindows 3)
        (let ((before (mapcar #'window-id (session-windows s)))
              (*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s command)))
          (expect (equal before (mapcar #'window-id (session-windows s))))
          (assert-overlay-contains "swap-window: unsupported argument"
                                    (overlay-lines) command)))))

  ;; ── arg-taking key bindings + source-file ────────────────────────────────────

  ;; A key bound to a command line runs it: bind X display-message hi, then prefix+X
  ;; shows 'hi' in an overlay (verifies dispatch-prefix-command's token-list path).
  (it "dispatch-prefix-bound-command-line-runs"
    (with-isolated-config
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux/config:apply-config-directive '("bind" "X" "display-message" "hi"))
          (cl-tmux::dispatch-prefix-command s (char-code #\X))
          (assert-overlay-contains "hi" (overlay-lines)
                                    "the bound display-message")))))

  ;; source-file <path> loads the file and applies its directives (end-to-end with
  ;; the set-option -g fix).
  (it "cmd-source-file-loads-config-file"
    (with-isolated-config
      (let ((path (format nil "/tmp/cl-tmux-srcfile-~D.conf" (get-universal-time))))
        (unwind-protect
             (progn
               (with-open-file (out path :direction :output :if-exists :supersede
                                         :if-does-not-exist :create)
                 (write-line "set-option -g status off" out))
               (cl-tmux::%run-command-line (make-fake-session)
                                           (format nil "source-file ~A" path))
               (expect (string= "off" (cl-tmux/options:get-option "status"))))
          (ignore-errors (delete-file path))))))

  ;; source-file on a non-existent path is a safe no-op (no error signalled).
  (it "cmd-source-file-missing-path-no-crash"
    (finishes (cl-tmux::%run-command-line (make-fake-session)
                                          "source-file /no/such/cl-tmux-file.conf")
              "source-file on a missing file must not signal"))

  ;; ── move-window -t <n> (renumber the active window) ──────────────────────────

  ;; 'move-window -t N' renumbers the active window to window-id N when N is free.
  (it "run-command-line-move-window-to-free-number"
    (with-fake-session (s :nwindows 2)
      (let* ((win (session-active-window s)))
        (cl-tmux::%run-command-line s "move-window -t 5")
        (expect (= 5 (window-id win))))))

  ;; 'move-window -t N' onto a taken index moves the window there and shifts the
  ;; occupant up (tmux winlink_shuffle_up) — not a silent no-op.
  (it "run-command-line-move-window-to-taken-number-shifts-up"
    (with-fake-session (s :nwindows 2)
      (let* ((win (session-active-window s))
             (w1  (find 1 (session-windows s) :key #'window-id)))
        (cl-tmux::%run-command-line s "move-window -t 1")   ; 1 is taken
        (expect (= 1 (window-id win)))
        (expect (= 2 (window-id w1))))))

  ;; move-window without -d selects the moved window as active; with -d the
  ;; original active window is preserved.
  ;; Each row: (command detach-p description).
  (it "run-command-line-move-window-selects-active-window-variants"
    (dolist (row '(("move-window -s 0 -t 5"    nil "no -d: moved window becomes active")
                   ("move-window -d -s 0 -t 5" t   "-d: original window stays active")))
      (destructuring-bind (cmd detach-p desc) row
        (declare (ignore desc))
        (with-fake-session (s :nwindows 2)
          (let* ((win   (session-active-window s))
                 (other (find 1 (session-windows s) :key #'window-id)))
            (session-select-window s other)
            (cl-tmux::%run-command-line s cmd)
            (expect (eq (if detach-p other win) (session-active-window s))))))))

  ;; move-window rejects unknown flags and extra positionals before moving.
  (it "run-command-line-move-window-rejects-unsupported-arguments"
    (dolist (command '("move-window extra"
                       "move-window -Z"
                       "move-window -s 0 -t 2 extra"))
      (with-fake-session (s :nwindows 3)
        (let ((before (mapcar #'window-id (session-windows s)))
              (active-before (session-active-window s))
              (*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s command)))
          (expect (equal before (mapcar #'window-id (session-windows s))))
          (expect (eq active-before (session-active-window s)))
          (assert-overlay-contains "move-window: unsupported argument"
                                    (overlay-lines) command))))))
