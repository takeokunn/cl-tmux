(in-package #:cl-tmux/test)

;;;; per-window option resolution, alert-state window-tab styles, status-bar-line, overlay, DECTCEM — part IV

(in-suite renderer-suite)

;;; ── %status-window-list-styled per-window option resolution ──────────────────
;;;
;;; window-status-format / window-status-current-format / window-status-style /
;;; window-status-current-style are now resolved PER WINDOW via
;;; get-option-for-context :window WINDOW inside the dolist.  A window-local
;;; override on one window must affect only that window's tab; windows without an
;;; override must still resolve to the global / registered value (so output is
;;; identical to the pre-change behaviour).  make-fake-session ids windows from 0
;;; (base-index), the FIRST window is active, so window 2 has #{window_index} = 1.

(test status-window-list-per-window-format-override
  "A window-local window-status-format on the (non-active) second window is used
   for that window's tab only; the active window keeps the default
   current-format.  Proves the format is read per-window, not from the global."
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
        (is (search (format nil "W~DX" idx2) out)
            "non-active window 2 must use its per-window window-status-format ~
             (expected ~S in ~S)" (format nil "W~DX" idx2) out)
        ;; Window 1 (active, index 0) must still use the default current-format,
        ;; i.e. the normal "index:name" form ("0:0").  It must NOT have picked up
        ;; window 2's distinctive literal.
        (is (search "0:0" out)
            "active window 1 must still use the default current-format \"0:0\" ~
             (got ~S)" out)
        (is (null (search "W0X" out))
            "the per-window override on window 2 must NOT bleed onto window 1 ~
             (got ~S)" out)))))

(test status-window-list-per-window-style-override
  "A window-local window-status-style fg=red on the non-active window changes
   only that window's rendered output.  Comparing the 2-window list WITH the
   per-window override against the same list WITHOUT it proves the per-window
   style is resolved and applied (the strings must differ)."
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
    (is (search (format nil "~C[" #\Escape) with)
        "per-window fg=red must emit an SGR escape sequence (got ~S)" with)
    (is (search "31" with)
        "per-window fg=red must emit SGR code 31 for the window-2 tab (got ~S)"
        with)
    ;; Robust proof of per-window resolution: with vs without must differ.
    (is (not (string= with without))
        "per-window window-status-style override must change the output ~
         (with=~S without=~S)" with without)))

(test status-window-list-no-override-is-global
  "With NO per-window overrides, a GLOBAL window-status-style still applies to
   the window tabs.  Proves the fallback path (window -> global) is intact and
   behaviour is preserved for windows that carry no local override."
  (with-isolated-config
    ;; fg=green is SGR 32.  Set it GLOBALLY (no per-window override anywhere).
    (cl-tmux/options:set-option "window-status-style" "fg=green")
    (cl-tmux/options:set-option "window-status-current-style" "fg=green")
    (let* ((sess (make-fake-session :nwindows 2))
           (out  (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
      (is (search (format nil "~C[" #\Escape) out)
          "global window-status-style must emit an SGR escape (got ~S)" out)
      (is (search "32" out)
          "global window-status-style fg=green must emit SGR code 32 for the ~
           window tabs (got ~S)" out))))

(test status-window-list-per-window-current-format-override
  "A window-local window-status-CURRENT-format on the ACTIVE (first) window is
   used for that window's tab only.  make-fake-session selects the FIRST window,
   so the override lands on the active-window branch (current-format), proving
   the current-format is read per-window and does not bleed onto the inactive
   window."
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
        (is (search "C0Y" out)
            "active window 1 must use its per-window window-status-current-format ~
             (expected \"C0Y\" in ~S)" out)
        ;; The per-window override on the active window must NOT bleed onto the
        ;; inactive window 2 (index 1) — it keeps the default format.
        (is (null (search "C1Y" out))
            "the per-window current-format override on window 1 must NOT bleed ~
             onto window 2 (got ~S)" out)))))

(test status-window-list-per-window-current-style-override
  "A window-local window-status-CURRENT-style fg=red on the ACTIVE (first) window
   changes only that window's rendered output.  Comparing the 2-window list WITH
   the per-window override against the same list WITHOUT it proves the per-window
   current-style is resolved and applied to the active-window branch (the strings
   must differ)."
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
    (is (search (format nil "~C[" #\Escape) with)
        "per-window current-style fg=red must emit an SGR escape sequence (got ~S)" with)
    (is (search "31" with)
        "per-window current-style fg=red must emit SGR code 31 for the active ~
         window tab (got ~S)" with)
    ;; Robust proof of per-window resolution: with vs without must differ.
    (is (not (string= with without))
        "per-window window-status-current-style override must change the output ~
         (with=~S without=~S)" with without)))

;;; ── Alert-state window-tab styles (bell / activity / last) ───────────────────

(test status-window-list-bell-style-applied-to-window-with-pending-bell
  "A non-active window with a pane holding a pending BEL renders its tab with
   window-status-bell-style (fg=red → SGR 31), overriding the (empty) normal style."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")        ; normal: unstyled
    (cl-tmux/options:set-option "window-status-bell-style" "fg=red")
    (let* ((sess (make-fake-session :nwindows 2))
           (win2 (second (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win2))))
      ;; Mark a pane in the inactive window 2 as having a pending bell.
      (setf (cl-tmux/terminal/types:screen-bell-pending (cl-tmux/model:pane-screen pane)) t)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "31" out)
            "window with a pending bell must use window-status-bell-style (SGR 31): ~S" out)))))

(test status-window-list-activity-style-applied-to-window-with-activity
  "A non-active window with its activity-flag set renders its tab with
   window-status-activity-style (fg=blue → SGR 34)."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")
    (cl-tmux/options:set-option "window-status-activity-style" "fg=blue")
    (let* ((sess (make-fake-session :nwindows 2))
           (win2 (second (cl-tmux/model:session-windows sess))))
      (setf (cl-tmux/model:window-activity-flag win2) t)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "34" out)
            "window with activity must use window-status-activity-style (SGR 34): ~S" out)))))

(test status-window-list-last-style-applied-to-last-window
  "The last (previously active) non-active window renders its tab with
   window-status-last-style (fg=magenta → SGR 35) when set."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")
    (cl-tmux/options:set-option "window-status-last-style" "fg=magenta")
    (let* ((sess (make-fake-session :nwindows 2)))
      ;; make-fake-session selects window 1 active, leaving window 2 as the
      ;; last (second-highest last-active-time) window.
      (is (eq (second (cl-tmux/model:session-windows sess))
              (cl-tmux/model:session-last-window sess))
          "precondition: window 2 is the last window")
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "35" out)
            "last window must use window-status-last-style (SGR 35): ~S" out)))))

(test status-window-list-bell-style-beats-activity-style
  "Alert-style precedence: a non-active window with BOTH a pending bell and the
   activity flag uses bell-style (fg=red, 31), not activity-style (fg=blue, 34)."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")
    (cl-tmux/options:set-option "window-status-bell-style" "fg=red")
    (cl-tmux/options:set-option "window-status-activity-style" "fg=blue")
    (let* ((sess (make-fake-session :nwindows 2))
           (win2 (second (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win2))))
      (setf (cl-tmux/model:window-activity-flag win2) t)
      (setf (cl-tmux/terminal/types:screen-bell-pending (cl-tmux/model:pane-screen pane)) t)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "31" out)
            "bell must win over activity (expect SGR 31): ~S" out)
        (is (not (search "34" out))
            "activity style (SGR 34) must NOT appear when bell takes priority: ~S" out)))))

;;; ── %justify-right (pure) ───────────────────────────────────────────────────

(test status-bar-line-fits-in-terminal-cols
  (let ((line (cl-tmux/renderer::%justify-right "left-text" "12:34" 20)))
    (is (<= (length line) 20)
        "%justify-right output must fit within cols=20 (got ~D: ~S)"
        (length line) line)))

(test status-bar-line-contains-left-and-time
  (let ((line (cl-tmux/renderer::%justify-right "mysession" "09:00" 40)))
    (is (search "mysession" line)
        "%justify-right should contain left text (got ~S)" line)
    (is (search "09:00" line)
        "%justify-right should contain the time string (got ~S)" line)))

(test status-bar-line-truncates-when-too-long
  ;; Terminal is only 5 cols wide; result must be clamped.
  (let ((line (cl-tmux/renderer::%justify-right "very-long-left-text" "99:99" 5)))
    (is (= 5 (length line))
        "%justify-right should truncate to cols=5 (got ~D: ~S)"
        (length line) line)))

;;; ── %status-current-time ────────────────────────────────────────────────────

(test status-current-time-returns-hhmm
  "%status-current-time returns a 5-char HH:MM string."
  (let ((t-str (cl-tmux/renderer::%status-current-time)))
    (is (= 5 (length t-str))
        "time string must be 5 chars, got ~D: ~S" (length t-str) t-str)
    (is (char= #\: (char t-str 2))
        "colon must be at position 2, got ~C" (char t-str 2))
    (is (every #'digit-char-p (remove #\: t-str))
        "all non-colon chars must be digits")))

;;; ── %status-left-text ────────────────────────────────────────────────────────

(test status-left-text-normal-mode
  "%status-left-text returns session/window info when no prompt is active."
  (let ((cl-tmux/prompt:*prompt* nil))
    (let* ((s   (make-fake-session :nwindows 1))
           (win (session-active-window s))
           (ap  (session-active-pane  s))
           (left (cl-tmux/renderer::%status-left-text s win ap)))
      (is (search "0" left) "session name '0' must appear in left text")
      (is (search "0" left) "window name '0' must appear in left text"))))

;;; ── %status-justify-line ─────────────────────────────────────────────────────

(test status-justify-line-left-default
  "%status-justify-line with justify=left matches %justify-right."
  (let* ((left "hello")
         (right "world")
         (cols 40)
         (result   (cl-tmux/renderer::%status-justify-line left right cols "left"))
         (expected (cl-tmux/renderer::%justify-right left right cols)))
    (is (string= expected result)
        "left justify must match %justify-right (got ~S vs ~S)" result expected)))

(test status-justify-line-right-places-content-at-far-right
  "%status-justify-line with justify=right places the right string at far right."
  (let* ((result (cl-tmux/renderer::%status-justify-line "L" "R" 20 "right")))
    (is (<= (length result) 20)
        "right-justified result must fit in 20 cols (got ~D: ~S)"
        (length result) result)
    (is (char= #\R (char result (1- (length result))))
        "last character must be 'R' in right-justified mode (got ~S)" result)))

(test status-justify-line-centre-pads-symmetrically
  "%status-justify-line with justify=centre produces output containing both strings."
  (let ((result (cl-tmux/renderer::%status-justify-line "AB" "XY" 20 "centre")))
    (is (search "AB" result)
        "centre-justified must contain left 'AB' (got ~S)" result)
    (is (search "XY" result)
        "centre-justified must contain right 'XY' (got ~S)" result)
    (is (<= (length result) 20)
        "centre-justified result must fit in 20 cols (got ~D: ~S)"
        (length result) result)))

;;; ── %status-format-or-default ────────────────────────────────────────────────

(test status-format-or-default-uses-custom-option
  "%status-format-or-default returns the expanded custom option when set."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-left" "custom-left")
    (let* ((sess (make-test-session 40 10))
           (win  (session-active-window sess))
           (ap   (session-active-pane  sess))
           (ctx  (cl-tmux/format:format-context-from-session sess win ap))
           (result (cl-tmux/renderer::%status-format-or-default
                    "status-left" ctx (lambda () "fallback"))))
      (is (string= "custom-left" result)
          "%status-format-or-default must return the custom option (got ~S)" result))))

(test status-format-or-default-falls-back-to-default-fn
  "%status-format-or-default calls default-fn when option equals the registered default."
  (let* ((sess (make-test-session 40 10))
         (win  (session-active-window sess))
         (ap   (session-active-pane  sess))
         (ctx  (cl-tmux/format:format-context-from-session sess win ap))
         (called nil)
         (result (cl-tmux/renderer::%status-format-or-default
                  "status-left" ctx (lambda () (setf called t) "from-default"))))
    (is-true called
             "%status-format-or-default must invoke default-fn when option is unset")
    (is (string= "from-default" result)
        "%status-format-or-default must return the default-fn result (got ~S)" result)))

;;; ── render-overlay ───────────────────────────────────────────────────────────

(test render-session-draws-overlay
  "When an overlay is active, its text appears in the composed frame."
  (let ((*overlay* nil))
    (show-overlay "OVERLAY-HELP-LINE")
    (unwind-protect
         (let ((out (render-session-to-string
                     (make-test-session 30 6 :content "hi") 7 30)))
           (is (search "OVERLAY-HELP-LINE" out)
               "overlay text should be composited into the frame"))
      (clear-overlay))))

(test render-overlay-draws-overlay-lines
  "render-overlay writes each overlay line at the top of the screen."
  (let ((*overlay* nil))
    (show-overlay (format nil "line one~%line two"))
    (unwind-protect
         (let ((buf (make-string-output-stream)))
           (cl-tmux/renderer::render-overlay buf 20)
           (let ((out (get-output-stream-string buf)))
             (is (search "line" out) "overlay text must appear in output")))
      (clear-overlay))))

(test message-style-applied-to-overlay
  "render-overlay reads the message-style option for its SGR colour."
  (with-isolated-options ("message-style" "fg=white,bg=blue,bold")
    (let ((eff (cl-tmux/options:get-option "message-style" "")))
      (is (search "bold" eff)     "message-style bold preserved (got ~S)" eff)
      (is (search "fg=white" eff) "fg=white in message-style (got ~S)" eff)
      (is (search "bg=blue" eff)  "bg=blue in message-style (got ~S)" eff))))

(test render-overlay-wires-message-style
  "render-overlay applies the message-style option: the rendered overlay SGR
   differs from the unstyled overlay."
  (flet ((render ()
           (let ((*overlay* nil))
             (show-overlay "hello")
             (unwind-protect
                  (let ((buf (make-string-output-stream)))
                    (cl-tmux/renderer::render-overlay buf 20)
                    (get-output-stream-string buf))
               (clear-overlay)))))
    (let ((styled   (with-isolated-options ("message-style" "bg=red")
                      (render)))
          (unstyled (with-isolated-options ("message-style" "")
                      (render))))
      (is (not (string= styled unstyled))
          "message-style bg=red must change the overlay's rendered SGR (styled=~S unstyled=~S)"
          styled unstyled))))

;;; ── DECTCEM cursor-visibility in rendered output ────────────────────────────

(test render-session-hides-cursor-when-dectcem-off
  "When the active pane has screen-cursor-visible=NIL, ?25h must NOT appear in the frame."
  (let* ((sess (make-test-session 30 5))
         (ap   (session-active-pane sess))
         (screen (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-cursor-visible screen) nil)
    (let ((out (render-session-to-string sess 6 30)))
      (is (search (format nil "~C[?25l" #\Escape) out)
          "?25l must be emitted (cursor hidden at start)")
      (is-false (search (format nil "~C[?25h" #\Escape) out)
                "?25h must NOT be emitted when screen-cursor-visible is NIL"))))

(test render-session-shows-cursor-when-dectcem-on
  "When screen-cursor-visible=T (default), ?25h appears in the frame."
  (let* ((sess (make-test-session 30 5))
         (ap   (session-active-pane sess))
         (screen (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-cursor-visible screen) t)
    (let ((out (render-session-to-string sess 6 30)))
      (is (search (format nil "~C[?25h" #\Escape) out)
          "?25h must appear when screen-cursor-visible is T"))))

(test render-session-no-active-pane-shows-cursor
  "With no active pane (nil ap), the renderer always emits ?25h."
  (let* ((win  (make-window :id 1 :name "1" :width 20 :height 5 :panes nil))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (let ((out (render-session-to-string sess 6 20)))
      (is (search (format nil "~C[?25h" #\Escape) out)
          "?25h must be emitted when active pane is nil"))))

