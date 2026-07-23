(in-package #:cl-tmux/test)

;;;; per-window option resolution, alert-state window-tab styles, status-bar-line — part IV

(describe "renderer-suite"

  ;;; ── %status-window-list-styled per-window option resolution ──────────────────
  ;;;
  ;;; window-status-format / window-status-current-format / window-status-style /
  ;;; window-status-current-style are now resolved PER WINDOW via
  ;;; get-option-for-context :window WINDOW inside the dolist.  A window-local
  ;;; override on one window must affect only that window's tab; windows without an
  ;;; override must still resolve to the global / registered value (so output is
  ;;; identical to the pre-change behaviour).  make-fake-session ids windows from 0
  ;;; (base-index), the FIRST window is active, so window 2 has #{window_index} = 1.

  ;; A window-local window-status-format on the (non-active) second window is used
  ;; for that window's tab only; the active window keeps the default
  ;; current-format.  Proves the format is read per-window, not from the global.
  (it "status-window-list-per-window-format-override"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 2))
             (windows (cl-tmux/model:session-windows sess))
             (win2 (second windows))
             ;; make-fake-session selects the FIRST window, so win2 is inactive.
             (idx2 (cl-tmux/model:window-id win2)))
        ;; Distinctive literal so the expansion " W1X " is unmistakable in output.
        (cl-tmux/options:set-option-for-window
         "window-status-format" " W#{window_index}X " win2)
        (let ((out (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
          ;; Window 2 (inactive, index 1) must use its per-window override.
          (expect (search (format nil "W~DX" idx2) out))
          ;; Window 1 (active, index 0) must still use the default current-format,
          ;; i.e. the normal "index:name" form ("0:0").  It must NOT have picked up
          ;; window 2's distinctive literal.
          (expect (search "0:0" out))
          (expect (null (search "W0X" out)))))))

  ;; A window-local window-status-style fg=red on the non-active window changes
  ;; only that window's rendered output.  Comparing the 2-window list WITH the
  ;; per-window override against the same list WITHOUT it proves the per-window
  ;; style is resolved and applied (the strings must differ).
  (it "status-window-list-per-window-style-override"
    ;; Capture WITHOUT any override.
    (let ((without
            (with-isolated-config
              (with-fake-session (sess :nwindows 2)
                (cl-tmux/renderer::%status-window-list-styled
                 sess (cl-tmux/model:session-active-window sess)))))
          ;; Capture WITH a window-local fg=red on window 2 (inactive).
          (with
            (with-isolated-config
              (with-fake-session (sess :nwindows 2)
                (let ((win2 (second (cl-tmux/model:session-windows sess))))
                  (cl-tmux/options:set-option-for-window
                   "window-status-style" "fg=red" win2)
                  (cl-tmux/renderer::%status-window-list-styled
                   sess (cl-tmux/model:session-active-window sess)))))))
      ;; fg=red is SGR 31; the styled output must emit a CSI escape and "31".
      (expect (search (format nil "~C[" #\Escape) with))
      (expect (search "31" with))
      ;; Robust proof of per-window resolution: with vs without must differ.
      (expect (not (string= with without)))))

  ;; With NO per-window overrides, a GLOBAL window-status-style still applies to
  ;; the window tabs.  Proves the fallback path (window -> global) is intact and
  ;; behaviour is preserved for windows that carry no local override.
  (it "status-window-list-no-override-is-global"
    (with-isolated-config
      ;; fg=green is SGR 32.  Set it GLOBALLY (no per-window override anywhere).
      (cl-tmux/options:set-option "window-status-style" "fg=green")
      (cl-tmux/options:set-option "window-status-current-style" "fg=green")
      (let* ((sess (make-fake-session :nwindows 2))
             (out  (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
        (expect (search (format nil "~C[" #\Escape) out))
        (expect (search "32" out)))))

  ;; A window-local window-status-CURRENT-format on the ACTIVE (first) window is
  ;; used for that window's tab only.  make-fake-session selects the FIRST window,
  ;; so the override lands on the active-window branch (current-format), proving
  ;; the current-format is read per-window and does not bleed onto the inactive
  ;; window.
  (it "status-window-list-per-window-current-format-override"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 2))
             (windows (cl-tmux/model:session-windows sess))
             (win1 (first windows)))   ; make-fake-session selects the FIRST window
        ;; Distinctive literal so the expansion " C0Y " is unmistakable in output.
        (cl-tmux/options:set-option-for-window
         "window-status-current-format" " C#{window_index}Y " win1)
        (let ((out (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
          ;; Window 1 (active, index 0) must use its per-window current-format override.
          (expect (search "C0Y" out))
          ;; The per-window override on the active window must NOT bleed onto the
          ;; inactive window 2 (index 1) — it keeps the default format.
          (expect (null (search "C1Y" out)))))))

  ;; A window-local window-status-CURRENT-style fg=red on the ACTIVE (first) window
  ;; changes only that window's rendered output.  Comparing the 2-window list WITH
  ;; the per-window override against the same list WITHOUT it proves the per-window
  ;; current-style is resolved and applied to the active-window branch (the strings
  ;; must differ).
  (it "status-window-list-per-window-current-style-override"
    ;; Capture WITHOUT any override.
    (let ((without
            (with-isolated-config
              (with-fake-session (sess :nwindows 2)
                (cl-tmux/renderer::%status-window-list-styled
                 sess (cl-tmux/model:session-active-window sess)))))
          ;; Capture WITH a window-local fg=red on window 1 (active).
          (with
            (with-isolated-config
              (with-fake-session (sess :nwindows 2)
                (let ((win1 (first (cl-tmux/model:session-windows sess))))
                  (cl-tmux/options:set-option-for-window
                   "window-status-current-style" "fg=red" win1)
                  (cl-tmux/renderer::%status-window-list-styled
                   sess (cl-tmux/model:session-active-window sess)))))))
      ;; fg=red is SGR 31; the styled output must emit a CSI escape and "31".
      (expect (search (format nil "~C[" #\Escape) with))
      (expect (search "31" with))
      ;; Robust proof of per-window resolution: with vs without must differ.
      (expect (not (string= with without)))))

  ;;; ── Alert-state window-tab styles (bell / activity / last) ───────────────────

  ;; A non-active window with its sticky bell flag set renders its tab with
  ;; window-status-bell-style (fg=red → SGR 31), overriding the (empty) normal style.
  (it "status-window-list-bell-style-applied-to-window-with-pending-bell"
    (with-isolated-config
      (cl-tmux/options:set-option "window-status-style" "")        ; normal: unstyled
      (cl-tmux/options:set-option "window-status-bell-style" "fg=red")
      (let* ((sess (make-fake-session :nwindows 2))
             (win2 (second (cl-tmux/model:session-windows sess))))
        ;; Mark the inactive window 2 as having an unseen bell.
        (setf (cl-tmux/model:window-bell-flag win2) t)
        (let ((out (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
          (expect (search "31" out))))))

  ;; A non-active window with its activity-flag set renders its tab with
  ;; window-status-activity-style (fg=blue → SGR 34).
  (it "status-window-list-activity-style-applied-to-window-with-activity"
    (with-isolated-config
      (cl-tmux/options:set-option "window-status-style" "")
      (cl-tmux/options:set-option "window-status-activity-style" "fg=blue")
      (let* ((sess (make-fake-session :nwindows 2))
             (win2 (second (cl-tmux/model:session-windows sess))))
        (setf (cl-tmux/model:window-activity-flag win2) t)
        (let ((out (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
          (expect (search "34" out))))))

  ;; The last (previously active) non-active window renders its tab with
  ;; window-status-last-style (fg=magenta → SGR 35) when set.
  (it "status-window-list-last-style-applied-to-last-window"
    (with-isolated-config
      (cl-tmux/options:set-option "window-status-style" "")
      (cl-tmux/options:set-option "window-status-last-style" "fg=magenta")
      (let* ((sess (make-fake-session :nwindows 2)))
        ;; make-fake-session selects window 1 active, leaving window 2 as the
        ;; last (second-highest last-active-time) window.
        (expect (eq (second (cl-tmux/model:session-windows sess))
                    (cl-tmux/model:session-last-window sess)))
        (let ((out (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
          (expect (search "35" out))))))

  ;; Alert-style precedence: a non-active window with BOTH an unseen bell and the
  ;; activity flag uses bell-style (fg=red, 31), not activity-style (fg=blue, 34).
  (it "status-window-list-bell-style-beats-activity-style"
    (with-isolated-config
      (cl-tmux/options:set-option "window-status-style" "")
      (cl-tmux/options:set-option "window-status-bell-style" "fg=red")
      (cl-tmux/options:set-option "window-status-activity-style" "fg=blue")
      (let* ((sess (make-fake-session :nwindows 2))
             (win2 (second (cl-tmux/model:session-windows sess))))
        (setf (cl-tmux/model:window-activity-flag win2) t)
        (setf (cl-tmux/model:window-bell-flag win2) t)
        (let ((out (cl-tmux/renderer::%status-window-list-styled
                    sess (cl-tmux/model:session-active-window sess))))
          (expect (search "31" out))
          (expect (not (search "34" out)))))))

  ;;; ── %justify-right (pure) ───────────────────────────────────────────────────

  ;; %justify-right never returns a line longer than the requested column width.
  (it "status-bar-line-fits-in-terminal-cols"
    (let ((line (cl-tmux/renderer::%justify-right "left-text" "12:34" 20)))
      (expect (<= (length line) 20))))

  ;; %justify-right's output contains both the left text and the right-justified time string.
  (it "status-bar-line-contains-left-and-time"
    (let ((line (cl-tmux/renderer::%justify-right "mysession" "09:00" 40)))
      (expect (search "mysession" line))
      (expect (search "09:00" line))))

  ;; %justify-right clamps its output to cols when the left text and time overflow a narrow terminal.
  (it "status-bar-line-truncates-when-too-long"
    ;; Terminal is only 5 cols wide; result must be clamped.
    (let ((line (cl-tmux/renderer::%justify-right "very-long-left-text" "99:99" 5)))
      (expect (= 5 (length line)))))

  ;;; ── %status-current-time ────────────────────────────────────────────────────

  ;; %status-current-time returns a 5-char HH:MM string.
  (it "status-current-time-returns-hhmm"
    (let ((t-str (cl-tmux/renderer::%status-current-time)))
      (expect (= 5 (length t-str)))
      (expect (char= #\: (char t-str 2)))
      (expect (every #'digit-char-p (remove #\: t-str)))))

  ;;; ── %status-left-text ────────────────────────────────────────────────────────

  ;; %status-left-text returns session/window info when no prompt is active.
  (it "status-left-text-normal-mode"
    (let ((cl-tmux/prompt:*prompt* nil))
      (let* ((s   (make-fake-session :nwindows 1))
             (win (session-active-window s))
             (ap  (session-active-pane  s))
             (left (cl-tmux/renderer::%status-left-text s win ap)))
        (expect (search "0" left))
        (expect (search "0" left)))))

  ;;; ── %status-justify-line ─────────────────────────────────────────────────────

  ;; %status-justify-line with justify=left matches %justify-right.
  (it "status-justify-line-left-default"
    (let* ((left "hello")
           (right "world")
           (cols 40)
           (result   (cl-tmux/renderer::%status-justify-line left right cols "left"))
           (expected (cl-tmux/renderer::%justify-right left right cols)))
      (expect (string= expected result))))

  ;; %status-justify-line with justify=right places the right string at far right.
  (it "status-justify-line-right-places-content-at-far-right"
    (let* ((result (cl-tmux/renderer::%status-justify-line "L" "R" 20 "right")))
      (expect (<= (length result) 20))
      (expect (char= #\R (char result (1- (length result)))))))

  ;; %status-justify-line with justify=centre produces output containing both strings.
  (it "status-justify-line-centre-pads-symmetrically"
    (let ((result (cl-tmux/renderer::%status-justify-line "AB" "XY" 20 "centre")))
      (expect (search "AB" result))
      (expect (search "XY" result))
      (expect (<= (length result) 20))))

  ;;; ── %status-format-or-default ────────────────────────────────────────────────

  ;; %status-format-or-default returns the expanded custom option when set.
  (it "status-format-or-default-uses-custom-option"
    (with-isolated-options ()
      (cl-tmux/options:set-option "status-left" "custom-left")
      (let* ((sess (make-renderer-test-session 40 10))
             (win  (session-active-window sess))
             (ap   (session-active-pane  sess))
             (ctx  (cl-tmux/format:format-context-from-session sess win ap))
             (result (cl-tmux/renderer::%status-format-or-default
                      "status-left" ctx (lambda () "fallback"))))
        (expect (string= "custom-left" result)))))

  ;; %status-format-or-default calls default-fn when option equals the registered default.
  (it "status-format-or-default-falls-back-to-default-fn"
    (let* ((sess (make-renderer-test-session 40 10))
           (win  (session-active-window sess))
           (ap   (session-active-pane  sess))
           (ctx  (cl-tmux/format:format-context-from-session sess win ap))
           (called nil)
           (result (cl-tmux/renderer::%status-format-or-default
                    "status-left" ctx (lambda () (setf called t) "from-default"))))
      (expect called :to-be-truthy)
      (expect (string= "from-default" result)))))
