(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part D: display-popup, send-keys -N/-H, send-prefix,
;;;; capture-pane.

(in-suite dispatch-suite)

;;; ── display-popup (arg-bearing handler) ──────────────────────────────────────
;;;
;;; %cmd-display-popup parses the flags, runs the command, and shows its output.

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
   rather than a popup overlay."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::%cmd-display-popup s '())
      (is (null cl-tmux/prompt:*active-popup*)
          "no command → no popup overlay yet")
      (is (prompt-active-p) "no command opens the popup-command prompt instead")
      (is (string= "popup command" (prompt-label *prompt*))
          "the prompt label matches the popup prompt"))))

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

(test send-keys-m-forwards-current-mouse-event-to-target-pane
  "send-keys -M forwards the bound mouse event to the target pane as a mouse
   escape sequence."
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
                 (is (cl-tmux::%cmd-send-keys-arg s '("-M"))
                     "send-keys -M succeeds when a mouse event is bound"))
               (is (= 1 (length writes))
                   "send-keys -M writes exactly one mouse event to the pane")
               (let* ((write (first writes))
                      (data (second write)))
                 (is (eql 2222 (first write))
                     "send-keys -M writes to the target pane fd")
                 (is (stringp data)
                     "send-keys -M forwards a string payload through pty-write")
                 (is (string= (format nil "~C[M~C~C~C" #\Escape #\Space #\! #\!)
                              data)
                     "send-keys -M writes the encoded mouse event to the pane")))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

;;; ── send-prefix -t / -2 ─────────────────────────────────────────────────────

(test cmd-send-prefix-t-targets-pane
  "send-prefix -t writes the literal primary prefix byte to the target pane."
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
               (is (equal '((2222 (2))) (reverse writes))
                   "send-prefix -t %2 writes C-b to pane 2 only"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

(test cmd-send-prefix-2-uses-secondary-prefix
  "send-prefix -2 writes the configured secondary prefix byte."
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
                 (is (equal '((3333 (1))) (reverse writes))
                     "send-prefix -2 writes C-a when prefix2 is C-a"))
            (setf (fdefinition 'cl-tmux/pty:pty-write) orig)))))))

(test cmd-send-prefix-read-only-does-not-write
  "send-prefix command is suppressed when the client is read-only."
  (with-isolated-config
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
               (let ((cl-tmux::*client-read-only* t))
                 (cl-tmux::%run-command-line s "send-prefix"))
              (is (null writes)
                  "send-prefix must not write to a pane for read-only clients"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

(test cmd-send-prefix-dead-pane-does-not-write
  "send-prefix skips dead panes (pane-fd = -1) and does not attempt a write."
  (with-isolated-config
    (with-fake-session (s)
      (let* ((pane (window-active-pane (session-active-window s)))
             (writes nil)
             (orig (fdefinition 'cl-tmux/pty:pty-write)))
        (setf (pane-fd pane) -1)
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-write)
                     (lambda (fd bytes)
                       (push (list fd (coerce bytes 'list)) writes)))
               (cl-tmux::%run-command-line s "send-prefix")
               (is (null writes)
                   "send-prefix must not write when the target pane is dead"))
          (setf (fdefinition 'cl-tmux/pty:pty-write) orig))))))

(test cmd-send-prefix-rejects-unsupported-arguments-before-writing
  "send-prefix rejects unsupported arguments before writing to a pane."
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
                   (is (null (cl-tmux::%cmd-send-prefix-arg s args))
                       "send-prefix rejects ~S" args)
                   (is (null writes)
                       "send-prefix does not write for ~S" args)
                   (assert-overlay-contains "send-prefix: unsupported argument"
                                             cl-tmux::*overlay*
                                             (format nil "send-prefix reports an unsupported argument for ~S" args))))
            (setf (fdefinition 'cl-tmux/pty:pty-write) orig)))))))

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
        (assert-overlay-active "-p shows the content in an overlay")
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
    (with-fake-two-pane-session (s)
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

(test cmd-capture-pane-rejects-unsupported-arguments
  "capture-pane rejects unknown flags and excess positional tokens.  (The tmux
   flags -a/-C/-M/-P/-q/-T are accepted; args string \"ab:CeE:JMNpPqS:Tt:\".)"
  (dolist (args '(("-z")
                  ("extra")
                  ("-b" "cap" "extra")))
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (feed (active-screen s) "must not capture")
          (is (null (cl-tmux::%cmd-capture-pane-arg s args))
              "~S must be rejected instead of accepted as a no-op" args)
          (assert-overlay-contains "unsupported argument" *overlay* args)
          (is (null (cl-tmux/buffer:get-paste-buffer 0))
              "~S must not save a paste buffer after rejection" args))))))

(test cmd-capture-pane-accepts-tmux-flags
  "capture-pane accepts the tmux output-control flags -a/-C/-q and still captures."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (feed (active-screen s) "captured text")
        (cl-tmux::%cmd-capture-pane-arg s '("-a" "-C" "-q"))
        (is (null *overlay*)
            "capture-pane -a/-C/-q must not raise an unsupported-argument overlay")
        (is (not (null (cl-tmux/buffer:get-paste-buffer 0)))
            "capture-pane saves the capture to a paste buffer")))))
