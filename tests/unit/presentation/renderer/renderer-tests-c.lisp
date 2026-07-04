(in-package #:cl-tmux/test)

;;;; mouse/focus/keys sequences, lock-screen, justify, cursor-shape, overlay, inline-style — part III

(in-suite renderer-suite)

;;; ── enable-mouse-reporting / disable-mouse-reporting ─────────────────────────

(test mouse-reporting-toggle-table
  "enable/disable-mouse-reporting each write the correct set of DEC sequences."
  (dolist (c '((cl-tmux/renderer::enable-mouse-reporting
                ("?1000h" "?1002h" "?1006h")
                "enable-mouse-reporting")
               (cl-tmux/renderer::disable-mouse-reporting
                ("?1006l" "?1002l" "?1000l")
                "disable-mouse-reporting")))
    (destructuring-bind (fn expected-suffixes desc) c
      (let ((out (let ((*standard-output* (make-string-output-stream)))
                   (funcall fn)
                   (get-output-stream-string *standard-output*))))
        (dolist (suffix expected-suffixes)
          (is (search (format nil "~C[~A" #\Escape suffix) out)
              "~A must emit ~A (got ~S)" desc suffix out))))))

;;; ── enable/disable-extended-keys (CSI u / modifyOtherKeys) ───────────────────

(test extended-keys-level-mapping
  "extended-keys-level maps the option value to a modifyOtherKeys level or NIL."
  (is (= 1 (cl-tmux/renderer::extended-keys-level "on"))     "on → level 1")
  (is (= 2 (cl-tmux/renderer::extended-keys-level "always")) "always → level 2")
  (is (null (cl-tmux/renderer::extended-keys-level "off"))   "off → NIL")
  (is (null (cl-tmux/renderer::extended-keys-level nil))     "NIL → NIL"))

(test enable-extended-keys-table
  "enable-extended-keys maps option value to a level and the matching CSI sequence."
  (dolist (c '(("on"     1   ">4;1m" "on → level 1 + CSI >4;1m")
               ("always" 2   ">4;2m" "always → level 2 + CSI >4;2m")
               ("off"    nil nil     "off → nil + no output")))
    (destructuring-bind (value expected-level expected-suffix desc) c
      (let* ((level nil)
             (out (let ((*standard-output* (make-string-output-stream)))
                    (setf level (cl-tmux/renderer::enable-extended-keys value))
                    (get-output-stream-string *standard-output*))))
        (is (equal expected-level level) "~A: return level" desc)
        (if expected-suffix
            (is (search (format nil "~C[~A" #\Escape expected-suffix) out) "~A: sequence" desc)
            (is (string= "" out) "~A: no output" desc))))))

(test disable-extended-keys-emits-reset
  "disable-extended-keys writes CSI > 4 ; 0 m to reset the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-extended-keys)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[>4;0m" #\Escape) out)
        "disable-extended-keys must emit CSI > 4 ; 0 m (got ~S)" out)))

;;; ── enable/disable-focus-reporting (?1004) ───────────────────────────────────

(test focus-reporting-toggle-table
  "enable/disable-focus-reporting emit ?1004h and ?1004l respectively."
  (dolist (c '((cl-tmux/renderer::enable-focus-reporting  "?1004h" "enable")
               (cl-tmux/renderer::disable-focus-reporting "?1004l" "disable")))
    (destructuring-bind (fn suffix desc) c
      (let ((out (let ((*standard-output* (make-string-output-stream)))
                   (funcall fn)
                   (get-output-stream-string *standard-output*))))
        (is (search (format nil "~C[~A" #\Escape suffix) out)
            "~A-focus-reporting must emit ~A (got ~S)" desc suffix out)))))

;;; ── render-lock-screen ───────────────────────────────────────────────────────

(test render-lock-screen-fills-with-lock-message
  "render-lock-screen fills the terminal with a blue background and the 'locked' message."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::render-lock-screen s 24 80))))
    (is (plusp (length out))
        "render-lock-screen must produce non-empty output")
    (is (search "locked" out)
        "render-lock-screen must include 'locked' in the output (got ~S)" out)))

(test render-lock-screen-emits-blue-background
  "render-lock-screen emits the blue-background SGR sequence."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::render-lock-screen s 24 80))))
    (is (search (format nil "~C[44;97m" #\Escape) out)
        "render-lock-screen must emit ESC[44;97m (got ~S)" out)))

(test render-session-locked-shows-lock-overlay
  "When session-locked-p is T, render-session-to-string emits the lock overlay."
  (let* ((sess (make-renderer-test-session 40 10)))
    (setf (session-locked-p sess) t)
    (unwind-protect
         (let ((out (render-session-to-string sess 11 40)))
           (is (search "locked" out)
               "locked session must show 'locked' message in frame (got ~S)" out))
      ;; Restore so other tests are not affected.
      (setf (session-locked-p sess) nil))))

;;; ── %status-pane-indicator with non-nil pane ─────────────────────────────────

(test status-pane-indicator-formats-pane-id
  "%status-pane-indicator with pane id 99 returns a string containing '#99'."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 99 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (let ((out (cl-tmux/renderer::%status-pane-indicator pane)))
      (is (search "#99" out)
          "%status-pane-indicator must contain '#99' (got ~S)" out))))

;;; ── %status-left-text with copy mode ─────────────────────────────────────────

(test status-left-text-copy-mode-has-no-indicator
  "%status-left-text with copy mode active no longer includes the old copy indicator."
  (let ((cl-tmux/prompt:*prompt* nil))
    (let* ((sess   (make-fake-session :nwindows 1))
           (win    (session-active-window sess))
           (ap     (session-active-pane  sess))
           (screen (pane-screen ap)))
      ;; Enable copy mode with a non-zero offset.
      (setf (screen-copy-mode-p   screen) t
            (screen-copy-offset   screen) 2)
      (let ((left (cl-tmux/renderer::%status-left-text sess win ap)))
        (is (null (search "COPY" left))
            "%status-left-text in copy mode must not contain the old 'COPY' indicator (got ~S)" left)
        (is (null (search "+2" left))
            "%status-left-text in copy mode must not show the old offset '+2' (got ~S)" left)))))

;;; ── %render-mouse-sequences (internal — three-way dispatch) ──────────────────
;;;
;;; These tests exercise %render-mouse-sequences directly to cover all three
;;; branches of the mouse-mode case: X10 (1 → ?1000h), button-event (2 → ?1002h),
;;; and any-event (other → ?1003h).

(defun %mouse-seq-output (mouse-mode sgr-mode)
  "Run %render-mouse-sequences with a synthetic pane whose screen has MOUSE-MODE
   and SGR-MODE set.  The global 'mouse' option is isolated to NIL so only the
   pane-level branch fires.  Returns the emitted string."
  (with-isolated-options ("mouse" nil)
    (let* ((screen (make-screen 10 4))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 4
                              :fd -1 :screen screen)))
      (setf (cl-tmux/terminal/types:screen-mouse-mode     screen) mouse-mode
            (cl-tmux/terminal/types:screen-mouse-sgr-mode screen) sgr-mode)
      (with-output-to-string (s)
        (cl-tmux/renderer::%render-mouse-sequences s pane)))))

(test render-mouse-sequences-mode-table
  "%render-mouse-sequences emits the correct DEC sequence per mouse-mode and sgr-mode."
  (dolist (c '((1 nil "?1000h" "mouse-mode 1 (X10) → ?1000h")
               (2 nil "?1002h" "mouse-mode 2 (button-event) → ?1002h")
               (3 nil "?1003h" "mouse-mode 3 (any-event) → ?1003h")
               (1 t   "?1006h" "sgr-mode T → ?1006h")))
    (destructuring-bind (mode sgr expected desc) c
      (let ((out (%mouse-seq-output mode sgr)))
        (is (search (format nil "~C[~A" #\Escape expected) out) "~A (got ~S)" desc out)))))

(test render-mouse-sequences-zero-mode-emits-nothing
  "%render-mouse-sequences with mouse-mode 0 emits no sequences."
  (let ((out (%mouse-seq-output 0 nil)))
    (is (= 0 (length out))
        "mouse-mode 0 must emit nothing (got ~S)" out)))

(test render-mouse-sequences-session-global-overrides-pane
  "When the 'mouse' option is globally enabled, %render-mouse-sequences emits the
   global sequences (?1006h + ?1002h) regardless of pane mouse-mode."
  (with-isolated-options ("mouse" t)
    (let* ((screen (make-screen 10 4))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 4
                              :fd -1 :screen screen)))
      (setf (cl-tmux/terminal/types:screen-mouse-mode screen) 0)
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::%render-mouse-sequences s pane))))
        (is (search (format nil "~C[?1006h" #\Escape) out)
            "global mouse must emit ?1006h (got ~S)" out)
        (is (search (format nil "~C[?1002h" #\Escape) out)
            "global mouse must emit ?1002h (got ~S)" out)))))

;;; ── render-lock-screen edge cases (coverage gap) ────────────────────────────

(test render-lock-screen-narrow-terminal-fits-message
  "render-lock-screen clamps the message to terminal-cols when the terminal
   is narrower than the message."
  (let* ((narrow-cols 12)
         (out (with-output-to-string (s)
                (cl-tmux/renderer::render-lock-screen s 5 narrow-cols))))
    (is (plusp (length out))
        "narrow render-lock-screen must produce output")
    ;; The message is truncated to 12 chars; "Session lock" is the prefix.
    (is (search "Session lock" out)
        "narrow lock screen must include the beginning of the message (got ~S)" out)))

(test render-lock-screen-single-row-terminal
  "render-lock-screen with terminal-rows=1 produces output without error."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::render-lock-screen s 1 40))))
    (is (plusp (length out))
        "single-row lock screen must produce non-empty output")))

;;; ── %render-panes-and-borders zoom suppression (coverage gap) ───────────────

(test render-panes-borders-suppressed-when-zoomed
  "%render-panes-and-borders emits no border characters when window-zoom-p is T."
  ;; tl-window calls window-relayout which runs screen-resize on each pane,
  ;; so pane screens match the assigned geometry (fix for INVALID-ARRAY-INDEX-ERROR
  ;; that occurred when manual make-window with 1×1 screens met an 81×24 layout).
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (win (tl-window (make-layout-split :h l0 l1) 24 81))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (setf (cl-tmux/model:window-zoom-p win) t)
    (let ((buf (make-string-output-stream)))
      (cl-tmux/renderer::%render-panes-and-borders
       buf sess win (cl-tmux/model:window-panes win) (cl-tmux/model:window-active win) 81)
      (let ((out (get-output-stream-string buf)))
        (is (null (find #\│ out))
            "zoomed window must not emit vertical border │ (got ~S)" out)))))

;;; ── %justify-right and %justify-centre (coverage gap) ───────────────────────

(test justify-right-places-right-text-flush-right
  "%justify-right puts the right text at the far right of the line."
  (let* ((left  "left-text")
         (right "right")
         (cols  30)
         (line  (cl-tmux/renderer::%justify-right left right cols)))
    (is (<= (length line) cols)
        "%justify-right result must fit in ~D cols (got ~D: ~S)" cols (length line) line)
    (is (char= #\t (char line (1- (length line))))
        "last char must be the last char of right-text (got ~S)" line)
    (is (search left line)
        "%justify-right must include the left text (got ~S)" line)))

(test justify-right-short-cols-truncates
  "%justify-right truncates when cols is very small."
  (let ((line (cl-tmux/renderer::%justify-right "LLLL" "RRRR" 5)))
    (is (<= (length line) 5)
        "%justify-right must not exceed 5 cols (got ~D: ~S)" (length line) line)))

(test justify-centre-contains-both-strings
  "%justify-centre produces output containing both left and right strings."
  (let ((line (cl-tmux/renderer::%justify-centre "LEFT" "RIGHT" 30)))
    (is (search "LEFT"  line) "%justify-centre must contain 'LEFT' (got ~S)" line)
    (is (search "RIGHT" line) "%justify-centre must contain 'RIGHT' (got ~S)" line)
    (is (<= (length line) 30)
        "%justify-centre result must fit in 30 cols (got ~D: ~S)" (length line) line)))

(test justify-centre-short-cols-truncates
  "%justify-centre truncates when cols is smaller than the combined content."
  (let ((line (cl-tmux/renderer::%justify-centre "AAAA" "BBBB" 5)))
    (is (<= (length line) 5)
        "%justify-centre must not exceed 5 cols (got ~D: ~S)" (length line) line)))
