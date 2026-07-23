(in-package #:cl-tmux/test)

;;;; application cursor keys, default bindings, mark-pane, and root key-table dispatch

(describe "events-suite"

  ;;; ── Application cursor keys — %arrow-final-to-ss3-bytes helper ──────────────

  ;; %arrow-final-to-ss3-bytes converts CSI arrow final bytes to SS3 sequences.
  (it "arrow-final-to-ss3-bytes-maps-arrows"
    ;; 65=A (up), 66=B (down), 67=C (right), 68=D (left)
    (dolist (final '(65 66 67 68))
      (let ((ss3 (cl-tmux::%arrow-final-to-ss3-bytes final)))
        (expect (and ss3
                     (= 3 (length ss3))
                     (= 27  (aref ss3 0))
                     (= 79  (aref ss3 1))
                     (= final (aref ss3 2)))))))

  ;; %arrow-final-to-ss3-bytes returns NIL for non-arrow final bytes.
  (it "arrow-final-to-ss3-bytes-returns-nil-for-non-arrows"
    (expect (null (cl-tmux::%arrow-final-to-ss3-bytes 72)))  ; H = home, not arrow
    (expect (null (cl-tmux::%arrow-final-to-ss3-bytes 109)))) ; m = SGR final

  ;;; ── New default key bindings ─────────────────────────────────────────────────

  ;; Default prefix bindings cover command-prompt, clock, info, messages, and layout/pane management.
  (it "key-bindings-table"
    (dolist (c '((58  :command-prompt       "C-b : → :command-prompt")
                 (116 :clock-mode           "C-b t → :clock-mode")
                 (105 :display-info         "C-b i → :display-info")
                 (126 :show-messages        "C-b ~ → :show-messages")
                 (109 :mark-pane            "C-b m → :mark-pane")
                 (77  :clear-mark           "C-b M → :clear-mark")
                 (69  :select-layout-spread "C-b E → :select-layout-spread")
                 (32  :next-layout          "C-b Space → :next-layout")
                 (46  :move-window-prompt   "C-b . → :move-window-prompt")
                 (68  :choose-client        "C-b D → :choose-client")))
      (destructuring-bind (code expected desc) c
        (declare (ignore desc))
        (expect (eq expected (lookup-key-binding (code-char code)))))))

  ;;; ── dispatch :mark-pane and :clear-mark ─────────────────────────────────────
  ;;; Build sessions manually (same pattern as dispatch-display-panes tests)
  ;;; to avoid any interaction with make-fake-session helpers.

  ;; :mark-pane command sets pane-marked on the active pane.
  (it "dispatch-mark-pane-marks-active-pane"
    (with-minimal-loop-session (p0 win sess)
      (let ((*overlay* nil))
        (expect (pane-marked p0) :to-be-falsy)
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (expect (pane-marked p0)))))

  ;; :mark-pane on an already-marked pane unmarks it (toggle).
  (it "dispatch-mark-pane-toggle-unmarks"
    (with-minimal-loop-session (p0 win sess)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (expect (pane-marked p0))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (expect (pane-marked p0) :to-be-falsy))))

  ;; :clear-mark clears the server-wide marked pane.
  (it "dispatch-clear-mark-unmarks-all-panes"
    (with-minimal-loop-session (p0 win sess)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :mark-pane nil)
        (expect (pane-marked p0))
        (cl-tmux::dispatch-command sess :clear-mark nil)
        (expect (pane-marked p0) :to-be-falsy))))

  ;;; ── Root key-table lookup ────────────────────────────────────────────────────

  ;; A key bound in the root table (bind -n) fires without the C-b prefix.
  (it "root-table-binding-fires-without-prefix"
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state)))
        ;; Bind 'Z' in root table so it selects the next window without C-b.
        (key-table-bind "root" #\Z :next-window)
        (unwind-protect
             (progn
               (cl-tmux::process-byte s (char-code #\Z) state)
               (expect (eq (second (session-windows s)) (session-active-window s))))
          ;; Clean up: remove the root binding we added.
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash #\Z tbl)))))))

  ;; A -n binding to a command LINE runs without the prefix: bind -n Z
  ;; display-message hi, then pressing Z (no C-b) shows 'hi' in an overlay
  ;; (verifies the root dispatch site's token-list path).
  (it "root-table-bound-command-line-runs-without-prefix"
    (with-isolated-config
      (with-fake-session (s)
        (let ((*overlay* nil)
              (state (cl-tmux::make-input-state)))
          (cl-tmux/config:apply-config-directive
           '("bind" "-n" "Z" "display-message" "hi"))
          (cl-tmux::process-byte s (char-code #\Z) state)
          (assert-overlay-active "a -n command-line binding must fire without C-b")
          (assert-overlay-contains "hi" *overlay*
                                   "overlay must contain the bound command's output 'hi'"))))))
