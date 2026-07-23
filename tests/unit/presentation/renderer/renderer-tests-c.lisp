(in-package #:cl-tmux/test)

;;;; mouse/focus/keys sequences, lock-screen, justify, cursor-shape, overlay, inline-style — part III

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

(describe "renderer-suite"

  ;;; ── enable-mouse-reporting / disable-mouse-reporting ─────────────────────────

  ;; enable/disable-mouse-reporting each write the correct set of DEC sequences.
  (it "mouse-reporting-toggle-table"
    (dolist (c '((cl-tmux/renderer::enable-mouse-reporting
                  ("?1000h" "?1002h" "?1006h")
                  "enable-mouse-reporting")
                 (cl-tmux/renderer::disable-mouse-reporting
                  ("?1006l" "?1002l" "?1000l")
                  "disable-mouse-reporting")))
      (destructuring-bind (fn expected-suffixes desc) c
        (declare (ignore desc))
        (let ((out (let ((*standard-output* (make-string-output-stream)))
                     (funcall fn)
                     (get-output-stream-string *standard-output*))))
          (dolist (suffix expected-suffixes)
            (expect (search (format nil "~C[~A" #\Escape suffix) out)))))))

  ;;; ── enable/disable-extended-keys (CSI u / modifyOtherKeys) ───────────────────

  ;; extended-keys-level maps the option value to a modifyOtherKeys level or NIL.
  (it "extended-keys-level-mapping"
    (expect (= 1 (cl-tmux/renderer::extended-keys-level "on")))
    (expect (= 2 (cl-tmux/renderer::extended-keys-level "always")))
    (expect (null (cl-tmux/renderer::extended-keys-level "off")))
    (expect (null (cl-tmux/renderer::extended-keys-level nil))))

  ;; enable-extended-keys maps option value to a level and the matching CSI sequence.
  (it "enable-extended-keys-table"
    (dolist (c '(("on"     1   ">4;1m" "on → level 1 + CSI >4;1m")
                 ("always" 2   ">4;2m" "always → level 2 + CSI >4;2m")
                 ("off"    nil nil     "off → nil + no output")))
      (destructuring-bind (value expected-level expected-suffix desc) c
        (declare (ignore desc))
        (let* ((level nil)
               (out (let ((*standard-output* (make-string-output-stream)))
                      (setf level (cl-tmux/renderer::enable-extended-keys value))
                      (get-output-stream-string *standard-output*))))
          (expect (equal expected-level level))
          (if expected-suffix
              (expect (search (format nil "~C[~A" #\Escape expected-suffix) out))
              (expect (string= "" out)))))))

  ;; disable-extended-keys writes CSI > 4 ; 0 m to reset the outer terminal.
  (it "disable-extended-keys-emits-reset"
    (let ((out (let ((*standard-output* (make-string-output-stream)))
                 (cl-tmux/renderer::disable-extended-keys)
                 (get-output-stream-string *standard-output*))))
      (expect (search (format nil "~C[>4;0m" #\Escape) out))))

  ;;; ── enable/disable-focus-reporting (?1004) ───────────────────────────────────

  ;; enable/disable-focus-reporting emit ?1004h and ?1004l respectively.
  (it "focus-reporting-toggle-table"
    (dolist (c '((cl-tmux/renderer::enable-focus-reporting  "?1004h" "enable")
                 (cl-tmux/renderer::disable-focus-reporting "?1004l" "disable")))
      (destructuring-bind (fn suffix desc) c
        (declare (ignore desc))
        (let ((out (let ((*standard-output* (make-string-output-stream)))
                     (funcall fn)
                     (get-output-stream-string *standard-output*))))
          (expect (search (format nil "~C[~A" #\Escape suffix) out))))))

  ;;; ── render-lock-screen ───────────────────────────────────────────────────────

  ;; render-lock-screen fills the terminal with a blue background and the 'locked' message.
  (it "render-lock-screen-fills-with-lock-message"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-lock-screen s 24 80))))
      (expect (plusp (length out)))
      (expect (search "locked" out))))

  ;; render-lock-screen emits the blue-background SGR sequence.
  (it "render-lock-screen-emits-blue-background"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-lock-screen s 24 80))))
      (expect (search (format nil "~C[44;97m" #\Escape) out))))

  ;; When session-locked-p is T, render-session-to-string emits the lock overlay.
  (it "render-session-locked-shows-lock-overlay"
    (let* ((sess (make-renderer-test-session 40 10)))
      (setf (session-locked-p sess) t)
      (unwind-protect
           (let ((out (render-session-to-string sess 11 40)))
             (expect (search "locked" out)))
        ;; Restore so other tests are not affected.
        (setf (session-locked-p sess) nil))))

  ;;; ── %status-pane-indicator with non-nil pane ─────────────────────────────────

  ;; %status-pane-indicator with pane id 99 returns a string containing '#99'.
  (it "status-pane-indicator-formats-pane-id"
    (let* ((screen (make-screen 10 5))
           (pane   (make-pane :id 99 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
      (let ((out (cl-tmux/renderer::%status-pane-indicator pane)))
        (expect (search "#99" out)))))

  ;;; ── %status-left-text with copy mode ─────────────────────────────────────────

  ;; %status-left-text with copy mode active no longer includes the old copy indicator.
  (it "status-left-text-copy-mode-has-no-indicator"
    (let ((cl-tmux/prompt:*prompt* nil))
      (let* ((sess   (make-fake-session :nwindows 1))
             (win    (session-active-window sess))
             (ap     (session-active-pane  sess))
             (screen (pane-screen ap)))
        ;; Enable copy mode with a non-zero offset.
        (setf (screen-copy-mode-p   screen) t
              (screen-copy-offset   screen) 2)
        (let ((left (cl-tmux/renderer::%status-left-text sess win ap)))
          (expect (null (search "COPY" left)))
          (expect (null (search "+2" left)))))))

  ;;; ── %render-mouse-sequences (internal — three-way dispatch) ──────────────────
  ;;;
  ;;; These tests exercise %render-mouse-sequences directly to cover all three
  ;;; branches of the mouse-mode case: X10 (1 → ?1000h), button-event (2 → ?1002h),
  ;;; and any-event (other → ?1003h).

  ;; %render-mouse-sequences emits the correct DEC sequence per mouse-mode and sgr-mode.
  (it "render-mouse-sequences-mode-table"
    (dolist (c '((1 nil "?1000h" "mouse-mode 1 (X10) → ?1000h")
                 (2 nil "?1002h" "mouse-mode 2 (button-event) → ?1002h")
                 (3 nil "?1003h" "mouse-mode 3 (any-event) → ?1003h")
                 (1 t   "?1006h" "sgr-mode T → ?1006h")))
      (destructuring-bind (mode sgr expected desc) c
        (declare (ignore desc))
        (let ((out (%mouse-seq-output mode sgr)))
          (expect (search (format nil "~C[~A" #\Escape expected) out))))))

  ;; %render-mouse-sequences with mouse-mode 0 emits no sequences.
  (it "render-mouse-sequences-zero-mode-emits-nothing"
    (let ((out (%mouse-seq-output 0 nil)))
      (expect (= 0 (length out)))))

  ;; When the 'mouse' option is globally enabled, %render-mouse-sequences emits the
  ;; global sequences (?1006h + ?1002h) regardless of pane mouse-mode.
  (it "render-mouse-sequences-session-global-overrides-pane"
    (with-isolated-options ("mouse" t)
      (let* ((screen (make-screen 10 4))
             (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 4
                                :fd -1 :screen screen)))
        (setf (cl-tmux/terminal/types:screen-mouse-mode screen) 0)
        (let ((out (with-output-to-string (s)
                     (cl-tmux/renderer::%render-mouse-sequences s pane))))
          (expect (search (format nil "~C[?1006h" #\Escape) out))
          (expect (search (format nil "~C[?1002h" #\Escape) out))))))

  ;;; ── render-lock-screen edge cases (coverage gap) ────────────────────────────

  ;; render-lock-screen clamps the message to terminal-cols when the terminal
  ;; is narrower than the message.
  (it "render-lock-screen-narrow-terminal-fits-message"
    (let* ((narrow-cols 12)
           (out (with-output-to-string (s)
                  (cl-tmux/renderer::render-lock-screen s 5 narrow-cols))))
      (expect (plusp (length out)))
      ;; The message is truncated to 12 chars; "Session lock" is the prefix.
      (expect (search "Session lock" out))))

  ;; render-lock-screen with terminal-rows=1 produces output without error.
  (it "render-lock-screen-single-row-terminal"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-lock-screen s 1 40))))
      (expect (plusp (length out)))))

  ;;; ── %render-panes-and-borders zoom suppression (coverage gap) ───────────────

  ;; %render-panes-and-borders emits no border characters when window-zoom-p is T.
  (it "render-panes-borders-suppressed-when-zoomed"
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
          (expect (null (find #\│ out)))))))

  ;;; ── %justify-right and %justify-centre (coverage gap) ───────────────────────

  ;; %justify-right puts the right text at the far right of the line.
  (it "justify-right-places-right-text-flush-right"
    (let* ((left  "left-text")
           (right "right")
           (cols  30)
           (line  (cl-tmux/renderer::%justify-right left right cols)))
      (expect (<= (length line) cols))
      (expect (char= #\t (char line (1- (length line)))))
      (expect (search left line))))

  ;; %justify-right truncates when cols is very small.
  (it "justify-right-short-cols-truncates"
    (let ((line (cl-tmux/renderer::%justify-right "LLLL" "RRRR" 5)))
      (expect (<= (length line) 5))))

  ;; %justify-centre produces output containing both left and right strings.
  (it "justify-centre-contains-both-strings"
    (let ((line (cl-tmux/renderer::%justify-centre "LEFT" "RIGHT" 30)))
      (expect (search "LEFT"  line))
      (expect (search "RIGHT" line))
      (expect (<= (length line) 30))))

  ;; %justify-centre truncates when cols is smaller than the combined content.
  (it "justify-centre-short-cols-truncates"
    (let ((line (cl-tmux/renderer::%justify-centre "AAAA" "BBBB" 5)))
      (expect (<= (length line) 5)))))
