(in-package #:cl-tmux/test)

;;;; application cursor keys, default bindings, mark-pane, and root key-table dispatch

(in-suite events-suite)

;;; ── Application cursor keys — %arrow-final-to-ss3-bytes helper ──────────────

(test arrow-final-to-ss3-bytes-maps-arrows
  "%arrow-final-to-ss3-bytes converts CSI arrow final bytes to SS3 sequences."
  ;; 65=A (up), 66=B (down), 67=C (right), 68=D (left)
  (dolist (final '(65 66 67 68))
    (let ((ss3 (cl-tmux::%arrow-final-to-ss3-bytes final)))
      (is (and ss3
               (= 3 (length ss3))
               (= 27  (aref ss3 0))
               (= 79  (aref ss3 1))
               (= final (aref ss3 2)))
          "SS3 sequence for final byte ~D must be ESC O ~C" final (code-char final)))))

(test arrow-final-to-ss3-bytes-returns-nil-for-non-arrows
  "%arrow-final-to-ss3-bytes returns NIL for non-arrow final bytes."
  (is (null (cl-tmux::%arrow-final-to-ss3-bytes 72))  ; H = home, not arrow
      "Non-arrow byte must return NIL")
  (is (null (cl-tmux::%arrow-final-to-ss3-bytes 109)) ; m = SGR final
      "SGR final byte must return NIL"))

;;; ── New default key bindings ─────────────────────────────────────────────────

(test key-bindings-table
  "Default prefix bindings cover command-prompt, clock, info, messages, and layout/pane management."
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
      (is (eq expected (lookup-key-binding (code-char code)))
          "~A" desc))))

;;; ── dispatch :mark-pane and :clear-mark ─────────────────────────────────────
;;; Build sessions manually (same pattern as dispatch-display-panes tests)
;;; to avoid any interaction with make-fake-session helpers.

(test dispatch-mark-pane-marks-active-pane
  ":mark-pane command sets pane-marked on the active pane."
  (with-minimal-loop-session (p0 win sess)
    (let ((*overlay* nil))
      (is-false (pane-marked p0) "pane must not be marked initially")
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane must be marked after :mark-pane"))))

(test dispatch-mark-pane-toggle-unmarks
  ":mark-pane on an already-marked pane unmarks it (toggle)."
  (with-minimal-loop-session (p0 win sess)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane marked after first :mark-pane")
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is-false (pane-marked p0)
          "pane unmarked after :mark-pane on already-marked pane"))))

(test dispatch-clear-mark-unmarks-all-panes
  ":clear-mark clears the server-wide marked pane."
  (with-minimal-loop-session (p0 win sess)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :mark-pane nil)
      (is (pane-marked p0) "pane must be marked before :clear-mark")
      (cl-tmux::dispatch-command sess :clear-mark nil)
      (is-false (pane-marked p0) "pane must not be marked after :clear-mark"))))

;;; ── Root key-table lookup ────────────────────────────────────────────────────

(test root-table-binding-fires-without-prefix
  "A key bound in the root table (bind -n) fires without the C-b prefix."
  (with-fake-session (s :nwindows 2)
    (let ((state (cl-tmux::make-input-state)))
      ;; Bind 'Z' in root table so it selects the next window without C-b.
      (key-table-bind "root" #\Z :next-window)
      (unwind-protect
           (progn
             (cl-tmux::process-byte s (char-code #\Z) state)
             (is (eq (second (session-windows s)) (session-active-window s))
                 "root-table binding must fire without C-b prefix"))
        ;; Clean up: remove the root binding we added.
        (let ((tbl (gethash "root" *key-tables*)))
          (when tbl (remhash #\Z tbl)))))))

(test root-table-bound-command-line-runs-without-prefix
  "A -n binding to a command LINE runs without the prefix: bind -n Z
   display-message hi, then pressing Z (no C-b) shows 'hi' in an overlay
   (verifies the root dispatch site's token-list path)."
  (with-isolated-config
    (with-fake-session (s)
      (let ((*overlay* nil)
            (state (cl-tmux::make-input-state)))
        (cl-tmux/config:apply-config-directive
         '("bind" "-n" "Z" "display-message" "hi"))
        (cl-tmux::process-byte s (char-code #\Z) state)
        (assert-overlay-active "a -n command-line binding must fire without C-b")
        (assert-overlay-contains "hi" *overlay*
                                 "overlay must contain the bound command's output 'hi'")))))
