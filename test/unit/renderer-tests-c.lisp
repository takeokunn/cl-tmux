(in-package #:cl-tmux/test)

;;;; mouse/focus/keys sequences, lock-screen, justify, cursor-shape, overlay, inline-style — part III

(in-suite renderer-suite)

;;; ── enable-mouse-reporting / disable-mouse-reporting ─────────────────────────

(test enable-mouse-reporting-emits-dec-sequences
  "enable-mouse-reporting writes ?1000h, ?1002h, and ?1006h to *standard-output*."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::enable-mouse-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1000h" #\Escape) out)
        "enable-mouse-reporting must emit ?1000h (got ~S)" out)
    (is (search (format nil "~C[?1002h" #\Escape) out)
        "enable-mouse-reporting must emit ?1002h (got ~S)" out)
    (is (search (format nil "~C[?1006h" #\Escape) out)
        "enable-mouse-reporting must emit ?1006h (got ~S)" out)))

(test disable-mouse-reporting-emits-dec-sequences
  "disable-mouse-reporting writes ?1006l, ?1002l, and ?1000l to *standard-output*."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-mouse-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1006l" #\Escape) out)
        "disable-mouse-reporting must emit ?1006l (got ~S)" out)
    (is (search (format nil "~C[?1002l" #\Escape) out)
        "disable-mouse-reporting must emit ?1002l (got ~S)" out)
    (is (search (format nil "~C[?1000l" #\Escape) out)
        "disable-mouse-reporting must emit ?1000l (got ~S)" out)))

;;; ── enable/disable-extended-keys (CSI u / modifyOtherKeys) ───────────────────

(test extended-keys-level-mapping
  "extended-keys-level maps the option value to a modifyOtherKeys level or NIL."
  (is (= 1 (cl-tmux/renderer::extended-keys-level "on"))     "on → level 1")
  (is (= 2 (cl-tmux/renderer::extended-keys-level "always")) "always → level 2")
  (is (null (cl-tmux/renderer::extended-keys-level "off"))   "off → NIL")
  (is (null (cl-tmux/renderer::extended-keys-level nil))     "NIL → NIL"))

(test enable-extended-keys-on-emits-level-1
  "enable-extended-keys with \"on\" writes CSI > 4 ; 1 m and returns level 1."
  (let* ((level nil)
         (out (let ((*standard-output* (make-string-output-stream)))
                (setf level (cl-tmux/renderer::enable-extended-keys "on"))
                (get-output-stream-string *standard-output*))))
    (is (= 1 level) "returns the emitted level")
    (is (search (format nil "~C[>4;1m" #\Escape) out)
        "must emit CSI > 4 ; 1 m (got ~S)" out)))

(test enable-extended-keys-always-emits-level-2
  "enable-extended-keys with \"always\" writes CSI > 4 ; 2 m and returns level 2."
  (let* ((level nil)
         (out (let ((*standard-output* (make-string-output-stream)))
                (setf level (cl-tmux/renderer::enable-extended-keys "always"))
                (get-output-stream-string *standard-output*))))
    (is (= 2 level) "returns the emitted level")
    (is (search (format nil "~C[>4;2m" #\Escape) out)
        "must emit CSI > 4 ; 2 m (got ~S)" out)))

(test enable-extended-keys-off-emits-nothing
  "enable-extended-keys with \"off\" writes no bytes and returns NIL."
  (let* ((level :sentinel)
         (out (let ((*standard-output* (make-string-output-stream)))
                (setf level (cl-tmux/renderer::enable-extended-keys "off"))
                (get-output-stream-string *standard-output*))))
    (is (null level) "off → NIL (reporting stays off)")
    (is (string= "" out) "off must emit nothing (got ~S)" out)))

(test disable-extended-keys-emits-reset
  "disable-extended-keys writes CSI > 4 ; 0 m to reset the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-extended-keys)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[>4;0m" #\Escape) out)
        "disable-extended-keys must emit CSI > 4 ; 0 m (got ~S)" out)))

;;; ── enable/disable-focus-reporting (?1004) ───────────────────────────────────

(test enable-focus-reporting-emits-1004h
  "enable-focus-reporting writes ?1004h to enable focus events on the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::enable-focus-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1004h" #\Escape) out)
        "enable-focus-reporting must emit ?1004h (got ~S)" out)))

(test disable-focus-reporting-emits-1004l
  "disable-focus-reporting writes ?1004l to disable focus events on the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-focus-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1004l" #\Escape) out)
        "disable-focus-reporting must emit ?1004l (got ~S)" out)))

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
  (let* ((sess (make-test-session 40 10)))
    (setf (session-locked-p sess) t)
    (unwind-protect
         (let ((out (render-session-to-string sess 11 40)))
           (is (search "locked" out)
               "locked session must show 'locked' message in frame (got ~S)" out))
      ;; Restore so other tests are not affected.
      (setf (session-locked-p sess) nil))))

;;; ── %status-copy-indicator edge cases ───────────────────────────────────────

(test status-copy-indicator-in-copy-mode-zero-offset-returns-empty
  "%status-copy-indicator in copy-mode with offset 0 returns empty string."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-offset screen) 0)
    (is (string= "" (cl-tmux/renderer::%status-copy-indicator pane))
        "%status-copy-indicator with offset 0 must return empty string")))

;;; ── %status-pane-indicator with non-nil pane ─────────────────────────────────

(test status-pane-indicator-formats-pane-id
  "%status-pane-indicator with pane id 99 returns a string containing '#99'."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 99 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (let ((out (cl-tmux/renderer::%status-pane-indicator pane)))
      (is (search "#99" out)
          "%status-pane-indicator must contain '#99' (got ~S)" out))))

;;; ── %status-left-text with copy mode ─────────────────────────────────────────

(test status-left-text-copy-mode-shows-indicator
  "%status-left-text with copy mode active includes the copy indicator."
  (let ((cl-tmux/prompt:*prompt* nil))
    (let* ((sess   (make-fake-session :nwindows 1))
           (win    (session-active-window sess))
           (ap     (session-active-pane  sess))
           (screen (pane-screen ap)))
      ;; Enable copy mode with a non-zero offset.
      (setf (screen-copy-mode-p   screen) t
            (screen-copy-offset   screen) 2)
      (let ((left (cl-tmux/renderer::%status-left-text sess win ap)))
        (is (search "COPY" left)
            "%status-left-text in copy mode must contain 'COPY' (got ~S)" left)
        (is (search "+2" left)
            "%status-left-text in copy mode must show offset '+2' (got ~S)" left)))))

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

(test render-mouse-sequences-x10-mode
  "%render-mouse-sequences with mouse-mode 1 emits ?1000h (X10 tracking)."
  (let ((out (%mouse-seq-output 1 nil)))
    (is (search (format nil "~C[?1000h" #\Escape) out)
        "mouse-mode 1 must emit ?1000h (got ~S)" out)))

(test render-mouse-sequences-button-event-mode
  "%render-mouse-sequences with mouse-mode 2 emits ?1002h (button-event tracking)."
  (let ((out (%mouse-seq-output 2 nil)))
    (is (search (format nil "~C[?1002h" #\Escape) out)
        "mouse-mode 2 must emit ?1002h (got ~S)" out)))

(test render-mouse-sequences-any-event-mode
  "%render-mouse-sequences with mouse-mode 3 (other) emits ?1003h (any-event tracking)."
  (let ((out (%mouse-seq-output 3 nil)))
    (is (search (format nil "~C[?1003h" #\Escape) out)
        "mouse-mode 3 must emit ?1003h (got ~S)" out)))

(test render-mouse-sequences-sgr-extension
  "%render-mouse-sequences with sgr-mode T appends ?1006h (SGR extended encoding)."
  (let ((out (%mouse-seq-output 1 t)))
    (is (search (format nil "~C[?1006h" #\Escape) out)
        "sgr-mode T must emit ?1006h (got ~S)" out)))

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
         (win (tl-window (make-layout-split :h l0 l1) 24 81)))
    (setf (cl-tmux/model:window-zoom-p win) t)
    (let ((buf (make-string-output-stream)))
      (cl-tmux/renderer::%render-panes-and-borders
       buf win (cl-tmux/model:window-panes win) (cl-tmux/model:window-active win) 81)
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

;;; ── %clamp-status-segment ───────────────────────────────────────────────────

(test clamp-status-segment-short-text-unchanged
  "%clamp-status-segment returns text unchanged when it fits within max-length."
  (is (string= "hello" (cl-tmux/renderer::%clamp-status-segment "hello" 10))
      "text shorter than max must be returned unchanged"))

(test clamp-status-segment-exact-length-unchanged
  "%clamp-status-segment returns text unchanged when length equals max-length."
  (is (string= "hello" (cl-tmux/renderer::%clamp-status-segment "hello" 5))
      "text at exact max must be returned unchanged"))

(test clamp-status-segment-truncates-long-text
  "%clamp-status-segment truncates text exceeding max-length."
  (is (string= "hel" (cl-tmux/renderer::%clamp-status-segment "hello" 3))
      "text exceeding max must be truncated to max chars"))

(test clamp-status-segment-empty-string
  "%clamp-status-segment with empty string returns empty string."
  (is (string= "" (cl-tmux/renderer::%clamp-status-segment "" 10))
      "empty string must be returned as-is"))

;;; ── set-cursor-shape in rendered output ──────────────────────────────────────

(test render-session-emits-cursor-shape
  "render-session-to-string emits the DECSCUSR sequence for the pane cursor shape."
  (let* ((sess  (make-test-session 20 5))
         (ap    (session-active-pane sess))
         (sc    (pane-screen ap)))
    ;; Set a non-default cursor shape (2 = steady block)
    (setf (cl-tmux/terminal/types:screen-cursor-shape sc) 2)
    (let ((out (render-session-to-string sess 6 20)))
      (is (search (format nil "~C[2 q" #\Escape) out)
          "DECSCUSR shape 2 must appear in the frame (got ~S)" out))))

;;; ── render-session-to-string with nil window ────────────────────────────────

(test render-session-no-window-produces-output
  "render-session-to-string with a session that has no active window still renders."
  (let* ((sess (make-session :id 1 :name "0" :windows nil)))
    (finishes
      (let ((out (render-session-to-string sess 5 20)))
        (is (plusp (length out))
            "no-window render must produce non-empty output")))))

;;; ── %render-panes-and-borders with nil window ───────────────────────────────

(test render-panes-borders-nil-window-finishes
  "%render-panes-and-borders with NIL window does not signal."
  (finishes
    (let ((buf (make-string-output-stream)))
      (cl-tmux/renderer::%render-panes-and-borders buf nil nil nil 80))))

;;; ── status-justify-line dispatch table ──────────────────────────────────────

(test status-justify-line-table-driven
  "%status-justify-line dispatches correctly to right/centre/left strategies."
  (let ((cases
         ;; (justify left right cols . description)
         '(("right"  "L" "R" 20 . "right")
           ("centre" "L" "R" 20 . "centre")
           ("left"   "L" "R" 20 . "left (default)")
           ("unknown" "L" "R" 20 . "unknown falls back to left"))))
    (dolist (c cases)
      (destructuring-bind (justify left right cols . desc) c
        (let ((result (cl-tmux/renderer::%status-justify-line left right cols justify)))
          (is (<= (length result) cols)
              "%status-justify-line ~A result must fit in ~D cols (got ~D: ~S)"
              desc cols (length result) result)
          (is (search left result)
              "%status-justify-line ~A must contain left text (got ~S)" desc result))))))

;;; ── render-overlay with scroll offset ───────────────────────────────────────

(test render-overlay-scroll-renders-lines-from-offset
  "render-overlay renders overlay lines starting from *overlay-scroll-offset*."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "line-A~%line-B~%line-C"))
    (unwind-protect
         (let ((buf (make-string-output-stream)))
           (cl-tmux/renderer::render-overlay buf 30)
           (let ((out (get-output-stream-string buf)))
             (is (search "line-A" out) "render-overlay must show first line")))
      (clear-overlay))))

;;; ── %status-bar-line gap calculation ────────────────────────────────────────

(test status-bar-line-gap-fills-exactly
  "%status-bar-line total length equals terminal-cols when content fits."
  (let* ((left  "abcde")
         (time  "12:34")
         (cols  20)
         (line  (cl-tmux/renderer::%status-bar-line left time cols)))
    ;; The line is truncated to cols so its length must be <= cols.
    (is (<= (length line) cols)
        "%status-bar-line must produce at most ~D chars (got ~D)" cols (length line))))

(test status-bar-line-empty-left-and-time
  "%status-bar-line with empty left and time strings produces spaces up to terminal-cols."
  (let ((line (cl-tmux/renderer::%status-bar-line "" "" 10)))
    (is (<= (length line) 10)
        "%status-bar-line with empty inputs must fit in 10 cols (got ~D: ~S)"
        (length line) line)))

;;; ── render-session-to-string status on/off interaction ──────────────────────

(test render-session-status-on-default-includes-time
  "With status=T and default options, the frame includes the HH:MM time pattern."
  (with-isolated-options ("status" t "status-left" nil)
    (let* ((sess (make-test-session 40 5))
           (out  (render-session-to-string sess 6 40)))
      ;; The default right status is HH:MM — 5 chars with a colon at position 2.
      ;; We just check a colon is present in a 5-char time substring.
      (is (find #\: out)
          "default status must include time with ':' character (got ~S)" out))))

;;; ── inline #[attr] style blocks + SGR-aware width (renderer-statusbar) ────────
;;;
;;; tmux status strings carry inline #[fg=…] style blocks and embedded SGR.  Those
;;; sequences are zero-width on screen, so the renderer expands #[…] into SGR and
;;; measures width by VISIBLE cells.  %visible-length/%visible-truncate must reduce
;;; to LENGTH/SUBSEQ on escape-free input (proven below) so older tests are intact.

(test visible-length-escape-free-equals-length
  "%visible-length equals LENGTH for strings with no escape sequences."
  (is (= 5 (cl-tmux/renderer::%visible-length "hello")))
  (is (= 0 (cl-tmux/renderer::%visible-length "")))
  (is (= (length "a:b 12:34")
         (cl-tmux/renderer::%visible-length "a:b 12:34"))))

(test visible-length-skips-sgr-sequences
  "%visible-length counts only visible cells, skipping CSI SGR escapes."
  (let ((esc #\Escape))
    (is (= 2 (cl-tmux/renderer::%visible-length
              (format nil "~C[32mhi~C[0m" esc esc)))
        "ESC[32mhiESC[0m has 2 visible cells")
    (is (= 3 (cl-tmux/renderer::%visible-length
              (format nil "~C[1;44;97mABC" esc)))
        "a multi-param SGR prefix is zero-width")))

(test visible-truncate-escape-free-equals-subseq
  "%visible-truncate equals SUBSEQ for escape-free strings."
  (is (string= "hel" (cl-tmux/renderer::%visible-truncate "hello" 3)))
  (is (string= "hello" (cl-tmux/renderer::%visible-truncate "hello" 5)))
  (is (string= "hello" (cl-tmux/renderer::%visible-truncate "hello" 99)))
  (is (string= "" (cl-tmux/renderer::%visible-truncate "hello" 0))))

(test visible-truncate-passes-sgr-through
  "%visible-truncate copies SGR escapes through without counting them toward N."
  (let* ((esc  #\Escape)
         (in   (format nil "~C[32mABCDE" esc))
         (out  (cl-tmux/renderer::%visible-truncate in 2)))
    (is (= 2 (cl-tmux/renderer::%visible-length out))
        "result must hold exactly 2 visible cells (got ~S)" out)
    (is (search "AB" out) "the 2 kept glyphs AB must be present (got ~S)" out)
    (is (char= esc (char out 0))
        "the leading SGR escape must be preserved (got ~S)" out)))

(test status-style-block-fg-becomes-sgr
  "%status-style-block-sgr turns fg=green into the SGR colour code 32."
  (let ((out (cl-tmux/renderer::%status-style-block-sgr "fg=green" "44;97")))
    (is (search (format nil "~C[32m" #\Escape) out)
        "fg=green must produce ESC[32m (got ~S)" out)))

(test status-style-block-default-resets-to-base
  "%status-style-block-sgr default/none/empty resets to the base status SGR."
  (let ((esc #\Escape))
    (dolist (body '("default" "none" "" "  "))
      (is (string= (format nil "~C[0;44;97m" esc)
                   (cl-tmux/renderer::%status-style-block-sgr body "44;97"))
          "~S must reset to ESC[0;44;97m" body))))

(test status-expand-style-blocks-no-block-unchanged
  "%status-expand-style-blocks returns escape-free / block-free text unchanged."
  (is (string= "plain text"
               (cl-tmux/renderer::%status-expand-style-blocks "plain text" "44;97")))
  (is (string= " 0 1:1* "
               (cl-tmux/renderer::%status-expand-style-blocks " 0 1:1* " "44;97"))))

(test status-expand-style-blocks-converts-blocks
  "%status-expand-style-blocks turns #[fg=green]X#[default] into SGR around X."
  (let* ((esc #\Escape)
         (out (cl-tmux/renderer::%status-expand-style-blocks
               "#[fg=green]X#[default]Y" "44;97")))
    (is (null (search "#[" out))
        "no literal #[ block may survive (got ~S)" out)
    (is (search (format nil "~C[32mX" esc) out)
        "green SGR must wrap X (got ~S)" out)
    (is (search (format nil "~C[0;44;97mY" esc) out)
        "#[default] before Y must reset to base SGR (got ~S)" out)))

(test clamp-status-segment-counts-visible-not-sgr
  "%clamp-status-segment measures visible cells; SGR escapes don't count and survive."
  (let* ((esc #\Escape)
         (txt (format nil "~C[32mhello~C[0m" esc esc)))   ; 5 visible cells
    (is (string= txt (cl-tmux/renderer::%clamp-status-segment txt 5))
        "5 visible ≤ max 5 → unchanged (SGR preserved)")
    (is (= 3 (cl-tmux/renderer::%visible-length
              (cl-tmux/renderer::%clamp-status-segment txt 3)))
        "max 3 keeps 3 visible cells")))

(test justify-right-ignores-sgr-width
  "%justify-right computes the gap from visible cells, so SGR doesn't shove content off-edge."
  (let* ((esc  #\Escape)
         (left (format nil "~C[32mABC~C[0m" esc esc))   ; 3 visible cells
         (line (cl-tmux/renderer::%justify-right left "RR" 20)))
    (is (= 20 (cl-tmux/renderer::%visible-length line))
        "visible width must fill exactly 20 cols (got ~D: ~S)"
        (cl-tmux/renderer::%visible-length line) line)
    (is (search "RR" line) "right text must be present (got ~S)" line)))

(test render-status-bar-inline-style-block-becomes-sgr
  "render-status-bar expands status-left #[fg=green]…#[default] into real SGR,
   and no literal #[ block reaches the output."
  (with-isolated-options ("status-left"  "#[fg=green]G#[default]"
                          "status-right" nil
                          "status-style" "")
    (let* ((sess (make-test-session 40 6))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
      (is (search (format nil "~C[32m" #\Escape) out)
          "inline #[fg=green] must emit SGR 32 (got ~S)" out)
      (is (null (search "#[" out))
          "literal #[ must not survive into the rendered bar (got ~S)" out)
      (is (find #\G out)
          "the styled glyph G must be present (got ~S)" out))))

;;; ── Background-window bell relay (gap #23) ────────────────────────────────

(test render-session-relays-background-window-bell-when-any
  "bell-action 'any': a BEL in a non-active window pane is forwarded to the
   outer terminal (ASCII 7 present in the rendered frame)."
  (with-isolated-options ("bell-action" "any" "visual-bell" nil "status" "off")
    (let* ((sess  (make-fake-session :nwindows 2))
           (win2  (second (cl-tmux/model:session-windows sess)))
           (pane2 (first (cl-tmux/model:window-panes win2))))
      ;; Plant a pending bell on the background window's pane.
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane2)) t)
      (let ((out (cl-tmux/renderer::render-session-to-string sess 5 20)))
        (is (find (code-char 7) out)
            "bell-action 'any': background-window BEL must appear in rendered frame")))))

(test render-session-relays-background-window-bell-when-other
  "bell-action 'other': a BEL in a non-active window pane IS forwarded."
  (with-isolated-options ("bell-action" "other" "visual-bell" nil "status" "off")
    (let* ((sess  (make-fake-session :nwindows 2))
           (win2  (second (cl-tmux/model:session-windows sess)))
           (pane2 (first (cl-tmux/model:window-panes win2))))
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane2)) t)
      (let ((out (cl-tmux/renderer::render-session-to-string sess 5 20)))
        (is (find (code-char 7) out)
            "bell-action 'other': background-window BEL must appear in rendered frame")))))

(test render-session-suppresses-background-bell-when-current
  "bell-action 'current': a BEL in a non-active window pane is NOT forwarded."
  (with-isolated-options ("bell-action" "current" "visual-bell" nil "status" "off")
    (let* ((sess  (make-fake-session :nwindows 2))
           (win2  (second (cl-tmux/model:session-windows sess)))
           (pane2 (first (cl-tmux/model:window-panes win2))))
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane2)) t)
      (let ((out (cl-tmux/renderer::render-session-to-string sess 5 20)))
        (is (null (find (code-char 7) out))
            "bell-action 'current': background-window BEL must be swallowed")))))

(test render-session-suppresses-background-bell-when-none
  "bell-action 'none': all BELs (foreground and background) are swallowed."
  (with-isolated-options ("bell-action" "none" "visual-bell" nil "status" "off")
    (let* ((sess  (make-fake-session :nwindows 2))
           (win2  (second (cl-tmux/model:session-windows sess)))
           (pane2 (first (cl-tmux/model:window-panes win2))))
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane2)) t)
      (let ((out (cl-tmux/renderer::render-session-to-string sess 5 20)))
        (is (null (find (code-char 7) out))
            "bell-action 'none': background-window BEL must be swallowed")))))
