(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part D: display-popup, send-keys -N/-H, send-prefix,
;;;; capture-pane.

(describe "dispatch-suite"

  ;;; ── display-popup (arg-bearing handler) ──────────────────────────────────────
  ;;;
  ;;; %cmd-display-popup parses the flags, runs the command, and shows its output.

  ;; %popup-dimension resolves nil→fallback, absolute cells, N% of axis, clamps to
  ;; axis-total, and falls back on junk.
  (it "cmd-display-popup-dimension-helper"
    (expect (= 60  (cl-tmux::%popup-dimension nil    200 60)))
    (expect (= 40  (cl-tmux::%popup-dimension "40"   200 60)))
    (expect (= 80  (cl-tmux::%popup-dimension "80%"  100 60)))
    (expect (= 100 (cl-tmux::%popup-dimension "150"  100 60)))
    (expect (= 60  (cl-tmux::%popup-dimension "junk" 200 60))))

  ;; display-popup with -w/-T and a command runs it and shows a popup with the
  ;; requested width and title.
  (it "cmd-display-popup-with-command-opens-popup"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil)
            (cl-tmux::*term-cols* 100)
            (cl-tmux::*term-rows* 30))
        (cl-tmux::%cmd-display-popup s '("-E" "-w" "40" "-T" "mytitle" "echo" "hi"))
        (expect (not (null cl-tmux/prompt:*active-popup*)))
        (expect (= 40 (cl-tmux/prompt:popup-width cl-tmux/prompt:*active-popup*)))
        (expect (string= "mytitle"
                         (cl-tmux/prompt:popup-title cl-tmux/prompt:*active-popup*))))))

  ;; display-popup -w 50% sizes the popup to half the terminal width.
  (it "cmd-display-popup-percent-width-of-terminal"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil)
            (cl-tmux::*term-cols* 80)
            (cl-tmux::*term-rows* 24))
        (cl-tmux::%cmd-display-popup s '("-w" "50%" "echo" "x"))
        (expect (= 40 (cl-tmux/prompt:popup-width cl-tmux/prompt:*active-popup*))))))

  ;; display-popup with no command opens the interactive popup-command prompt
  ;; rather than a popup overlay.
  (it "cmd-display-popup-no-command-opens-prompt"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil))
        (cl-tmux::%cmd-display-popup s '())
        (expect (null cl-tmux/prompt:*active-popup*))
        (expect (prompt-active-p))
        (expect (string= "popup command" (prompt-label *prompt*))))))

  ;;; ── send-keys -N (repeat) and -H (hex) ───────────────────────────────────────
  ;;;
  ;;; -N count repeats the -X copy-mode command (or the whole key sequence) COUNT
  ;;; times; -H interprets each argument as a hexadecimal character code.  The -X
  ;;; repeat is observed via the copy cursor; -H is tested through the extracted
  ;;; %send-keys-hex-to-string helper (send-keys-to-pane no-ops on a fd -1 pane).

  ;; %send-keys-hex-to-string maps a hex code to its one-character string, or NIL
  ;; for an unparseable / out-of-range code.
  (it "send-keys-hex-to-string-converts-codes"
    (expect (string= "A" (cl-tmux::%send-keys-hex-to-string "41")))
    (expect (string= " " (cl-tmux::%send-keys-hex-to-string "20")))
    (expect (= 27 (char-code (char (cl-tmux::%send-keys-hex-to-string "1b") 0))))
    (expect (null (cl-tmux::%send-keys-hex-to-string "zz")))
    (expect (null (cl-tmux::%send-keys-hex-to-string "FFFFFFFF"))))

  ;; send-keys -X -N 3 cursor-up moves the copy cursor up 3 rows (the -N repeat
  ;; count applied to the copy-mode command).
  (it "send-keys-x-with-N-repeats-copy-command"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (let* ((screen (active-screen s))
             (row0   (car (screen-copy-cursor screen))))
        (cl-tmux::%cmd-send-keys-arg s '("-X" "-N" "3" "cursor-up"))
        (expect (= (- row0 3) (car (screen-copy-cursor screen)))))))

  ;; send-keys -X cursor-up with no -N defaults to a single application.
  (it "send-keys-x-without-N-runs-once"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (let* ((screen (active-screen s))
             (row0   (car (screen-copy-cursor screen))))
        (cl-tmux::%cmd-send-keys-arg s '("-X" "cursor-up"))
        (expect (= (- row0 1) (car (screen-copy-cursor screen)))))))

  ;; send-keys -M forwards the bound mouse event to the target pane as a mouse
  ;; escape sequence.
  (it "send-keys-m-forwards-current-mouse-event-to-target-pane"
    (with-isolated-config
      (with-fake-session (s :nwindows 1 :npanes 1)
        (let* ((pane (window-active-pane (session-active-window s)))
               (writes nil)
               (orig (fdefinition 'cl-tmux/pty:pty-write)))
          (setf (pane-fd pane) 2222)
          (unwind-protect
               (progn
                 (setf (fdefinition 'cl-tmux/pty:pty-write)
                       (lambda (fd bytes)
                         (push (list fd bytes) writes)))
                 (let ((cl-tmux::*current-mouse-event*
                         '(:btn 0 :col 0 :row 0 :release-p nil)))
                   (expect (cl-tmux::%cmd-send-keys-arg s '("-M"))))
                 (expect (= 1 (length writes)))
                 (let* ((write (first writes))
                        (data (second write)))
                   (expect (eql 2222 (first write)))
                   (expect (stringp data))
                   (expect (string= (format nil "~C[M~C~C~C" #\Escape #\Space #\! #\!)
                                    data))))
            (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

  ;; send-keys rejects unsupported client/key-table flags before writing to a pane.
  (it "send-keys-rejects-unsupported-client-key-table-flags"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (dolist (args '(("-c" "client-0" "Enter")
                      ("-K" "Enter")))
        (let ((cl-tmux::*overlay* nil))
          (expect (null (cl-tmux::%cmd-send-keys-arg s args)))
          (assert-overlay-contains "send-keys: unsupported argument"
                                   cl-tmux::*overlay*
                                   args)))))

  ;;; ── send-prefix -t / -2 ─────────────────────────────────────────────────────

  ;; send-prefix -t writes the literal primary prefix byte to the target pane.
  (it "cmd-send-prefix-t-targets-pane"
    (with-isolated-config
      (with-fake-two-pane-session (s)
        (let* ((win (session-active-window s))
               (active (window-active-pane win))
               (target (find 2 (window-panes win) :key #'pane-id))
               (writes nil)
               (orig (fdefinition 'cl-tmux/pty:pty-write)))
          (setf (pane-fd active) 1111
                (pane-fd target) 2222)
          (unwind-protect
               (progn
                 (setf (fdefinition 'cl-tmux/pty:pty-write)
                       (lambda (fd bytes)
                         (push (list fd (coerce bytes 'list)) writes)))
                 (cl-tmux::%run-command-line s "send-prefix -t %2")
                 (expect (equal '((2222 (2))) (reverse writes))))
            (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

  ;; send-prefix -2 writes the configured secondary prefix byte.
  (it "cmd-send-prefix-2-uses-secondary-prefix"
    (with-isolated-config
      (let ((cl-tmux/config:*prefix2-key-code* 1))
        (with-fake-session (s)
          (let* ((pane (window-active-pane (session-active-window s)))
                 (writes nil)
                 (orig (fdefinition 'cl-tmux/pty:pty-write)))
            (setf (pane-fd pane) 3333)
            (unwind-protect
                 (progn
                   (setf (fdefinition 'cl-tmux/pty:pty-write)
                         (lambda (fd bytes)
                           (push (list fd (coerce bytes 'list)) writes)))
                   (cl-tmux::%run-command-line s "send-prefix -2")
                   (expect (equal '((3333 (1))) (reverse writes))))
              (setf (fdefinition 'cl-tmux/pty:pty-write) orig)))))))

  ;; send-prefix suppresses the write for read-only clients and dead panes.
  ;; Each row: (fd-val read-only-p description).
  (it "cmd-send-prefix-no-write-cases-table"
    (dolist (row '((3333 t   "read-only client must suppress the write")
                   (-1   nil "dead pane (fd=-1) must suppress the write")))
      (destructuring-bind (fd-val read-only-p desc) row
        (declare (ignore desc))
        (with-isolated-config
          (with-fake-session (s)
            (let* ((pane (window-active-pane (session-active-window s)))
                   (writes nil)
                   (orig (fdefinition 'cl-tmux/pty:pty-write)))
              (setf (pane-fd pane) fd-val)
              (unwind-protect
                   (progn
                     (setf (fdefinition 'cl-tmux/pty:pty-write)
                           (lambda (fd bytes)
                             (push (list fd (coerce bytes 'list)) writes)))
                     (let ((cl-tmux::*client-read-only* read-only-p))
                       (cl-tmux::%run-command-line s "send-prefix"))
                     (expect (null writes)))
                (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))))

  ;; send-prefix rejects unsupported arguments before writing to a pane.
  (it "cmd-send-prefix-rejects-unsupported-arguments-before-writing"
    (with-isolated-config
      (dolist (args '(("-Z") ("extra")))
        (with-fake-session (s)
          (let* ((pane (window-active-pane (session-active-window s)))
                 (writes nil)
                 (orig (fdefinition 'cl-tmux/pty:pty-write)))
            (setf (pane-fd pane) 3333)
            (unwind-protect
                 (progn
                   (setf (fdefinition 'cl-tmux/pty:pty-write)
                         (lambda (fd bytes)
                           (push (list fd (coerce bytes 'list)) writes)))
                   (let ((cl-tmux::*overlay* nil))
                     (expect (null (cl-tmux::%cmd-send-prefix-arg s args)))
                     (expect (null writes))
                     (assert-overlay-contains "send-prefix: unsupported argument"
                                               cl-tmux::*overlay*
                                               (format nil "send-prefix reports an unsupported argument for ~S" args))))
              (setf (fdefinition 'cl-tmux/pty:pty-write) orig)))))))

  ;;; ── capture-pane saves to a buffer by default (scriptable form) ──────────────
  ;;;
  ;;; The scriptable `capture-pane [flags]` command (%cmd-capture-pane-arg, distinct
  ;;; from the interactive :capture-pane overlay binding) follows tmux: without -p
  ;;; it SAVES the captured content to a paste buffer; -p prints (overlay) instead.

  ;; capture-pane with no -p saves the pane content to a paste buffer (the canonical
  ;; capture→paste workflow), not an overlay.
  (it "cmd-capture-pane-saves-to-buffer-by-default"
    (with-empty-buffers
      (with-fake-session (s)
        (feed (active-screen s) "hello capture")
        (cl-tmux::%cmd-capture-pane-arg s '())
        (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
          (expect (not (null buf)))
          (expect (search "hello capture" buf))))))

  ;; capture-pane -p prints (overlay) and does NOT save to a buffer.
  (it "cmd-capture-pane-p-shows-overlay-not-buffer"
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (feed (active-screen s) "shown only")
          (cl-tmux::%cmd-capture-pane-arg s '("-p"))
          (assert-overlay-active "-p shows the content in an overlay")
          (expect (null (cl-tmux/buffer:get-paste-buffer 0)))))))

  ;; capture-pane -b name is accepted; the capture is stored at the top of the ring.
  (it "cmd-capture-pane-b-flag-accepted-stores-in-ring"
    (with-empty-buffers
      (with-fake-session (s)
        (feed (active-screen s) "named buf")
        (cl-tmux::%cmd-capture-pane-arg s '("-b" "mybuf"))
        (expect (search "named buf" (or (cl-tmux/buffer:get-paste-buffer 0) ""))))))

  ;; capture-pane -t captures the requested pane, not always the active pane.
  (it "cmd-capture-pane-t-captures-target-pane"
    (with-empty-buffers
      (with-fake-two-pane-session (s)
        (let* ((win (session-active-window s))
               (active (window-active-pane win))
               (target (find 2 (window-panes win) :key #'pane-id)))
          (feed (pane-screen active) "active text")
          (feed (pane-screen target) "target text")
          (cl-tmux::%cmd-capture-pane-arg s '("-t" "%2" "-b" "cap"))
          (let ((buf (or (cl-tmux/buffer:get-named-buffer "cap") "")))
            (expect (search "target text" buf))
            (expect (null (search "active text" buf))))))))

  ;; capture-pane rejects unknown flags and excess positional tokens.  (The tmux
  ;; flags -a/-C/-M/-P/-q/-T are accepted; args string "ab:CeE:JMNpPqS:Tt:".)
  (it "cmd-capture-pane-rejects-unsupported-arguments"
    (dolist (args '(("-z")
                    ("extra")
                    ("-b" "cap" "extra")))
      (with-empty-buffers
        (with-fake-session (s)
          (let ((*overlay* nil))
            (feed (active-screen s) "must not capture")
            (expect (null (cl-tmux::%cmd-capture-pane-arg s args)))
            (assert-overlay-contains "unsupported argument" *overlay* args)
            (expect (null (cl-tmux/buffer:get-paste-buffer 0))))))))

  ;; capture-pane accepts the tmux output-control flags -a/-C/-q and still captures.
  (it "cmd-capture-pane-accepts-tmux-flags"
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (feed (active-screen s) "captured text")
          (cl-tmux::%cmd-capture-pane-arg s '("-a" "-C" "-q"))
          (expect (null *overlay*))
          (expect (not (null (cl-tmux/buffer:get-paste-buffer 0)))))))))
