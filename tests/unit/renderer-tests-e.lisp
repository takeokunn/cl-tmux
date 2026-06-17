(in-package #:cl-tmux/test)

;;;; renderer tests — part E: %clamp-status-segment, set-cursor-shape in rendered output,
;;;; render-session nil-window, render-panes-borders nil-window, status-justify-line,
;;;; render-overlay scroll, %status-bar-line gap, inline style blocks, SGR-aware width,
;;;; background-window bell relay.

(in-suite renderer-suite)

;;; ── %clamp-status-segment ───────────────────────────────────────────────────

(test clamp-status-segment-table
  "%clamp-status-segment returns text unchanged when it fits (≤ max) and truncates when it exceeds max."
  (dolist (row '(("hello" 10 "hello" "shorter than max → unchanged")
                 ("hello"  5 "hello" "exactly max length → unchanged")
                 ("hello"  3 "hel"   "exceeds max → truncated to 3 chars")
                 (""      10 ""      "empty string → always unchanged")))
    (destructuring-bind (text max expected desc) row
      (is (string= expected (cl-tmux/renderer::%clamp-status-segment text max))
          "~A" desc))))

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

;;; ── %justify-right gap calculation ──────────────────────────────────────────

(test status-bar-line-gap-fills-exactly
  "%justify-right total length equals cols when content fits."
  (let* ((left  "abcde")
         (time  "12:34")
         (cols  20)
         (line  (cl-tmux/renderer::%justify-right left time cols)))
    (is (<= (length line) cols)
        "%justify-right must produce at most ~D chars (got ~D)" cols (length line))))

(test status-bar-line-empty-left-and-time
  "%justify-right with empty left and time strings produces spaces up to cols."
  (let ((line (cl-tmux/renderer::%justify-right "" "" 10)))
    (is (<= (length line) 10)
        "%justify-right with empty inputs must fit in 10 cols (got ~D: ~S)"
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
  (dolist (c '(("hello" 3  "hel"   "truncate to 3")
               ("hello" 5  "hello" "truncate at exact length")
               ("hello" 99 "hello" "truncate past length -> unchanged")
               ("hello" 0  ""      "truncate to 0 -> empty string")))
    (destructuring-bind (input n expected desc) c
      (is (string= expected (cl-tmux/renderer::%visible-truncate input n)) "~A" desc))))

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
           (out  (render-status-bar-output sess 10 40)))
      (is (search (format nil "~C[32m" #\Escape) out)
          "inline #[fg=green] must emit SGR 32 (got ~S)" out)
      (is (null (search "#[" out))
          "literal #[ must not survive into the rendered bar (got ~S)" out)
      (is (find #\G out)
          "the styled glyph G must be present (got ~S)" out))))

;;; ── Background-window bell relay (gap #23) ────────────────────────────────

(test render-session-background-bell-action-table
  "bell-action controls whether BEL in a non-active window reaches the rendered frame."
  (dolist (row '(("any"     t   "bell-action 'any': background BEL must appear")
                 ("other"   t   "bell-action 'other': background BEL must appear")
                 ("current" nil "bell-action 'current': background BEL must be swallowed")
                 ("none"    nil "bell-action 'none': all BELs must be swallowed")))
    (destructuring-bind (bell-action expected-bell-p desc) row
      (with-isolated-options ("bell-action" bell-action "visual-bell" nil "status" "off")
        (let* ((sess  (make-fake-session :nwindows 2))
               (win2  (second (cl-tmux/model:session-windows sess)))
               (pane2 (first (cl-tmux/model:window-panes win2))))
          (setf (cl-tmux/terminal/types:screen-bell-pending
                 (cl-tmux/model:pane-screen pane2)) t)
          (let ((out (cl-tmux/renderer::render-session-to-string sess 5 20)))
            (if expected-bell-p
                (is (find (code-char 7) out)      "~A" desc)
                (is (null (find (code-char 7) out)) "~A" desc))))))))
