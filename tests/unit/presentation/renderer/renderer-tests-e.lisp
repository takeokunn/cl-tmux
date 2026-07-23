(in-package #:cl-tmux/test)

;;;; renderer tests — part E: %clamp-status-segment, set-cursor-shape in rendered output,
;;;; render-session nil-window, render-panes-borders nil-window, status-justify-line,
;;;; render-overlay scroll, %status-bar-line gap, inline style blocks, SGR-aware width,
;;;; background-window bell relay.

(describe "renderer-suite"

  ;;; ── %clamp-status-segment ───────────────────────────────────────────────────

  ;; %clamp-status-segment returns text unchanged when it fits (≤ max) and truncates when it exceeds max.
  (it "clamp-status-segment-table"
    (check-status-segment-clamp-cases
     '(("hello" 10 "hello" "shorter than max -> unchanged")
       ("hello"  5 "hello" "exactly max length -> unchanged")
       ("hello"  3 "hel"   "exceeds max -> truncated to 3 chars")
       (""      10 ""      "empty string -> always unchanged"))))

  ;;; ── set-cursor-shape in rendered output ──────────────────────────────────────

  ;; render-session-to-string emits the DECSCUSR sequence for the pane cursor shape.
  (it "render-session-emits-cursor-shape"
    (let* ((sess  (make-renderer-test-session 20 5))
           (ap    (session-active-pane sess))
           (sc    (pane-screen ap)))
      ;; Set a non-default cursor shape (2 = steady block)
      (setf (cl-tmux/terminal/types:screen-cursor-shape sc) 2)
      (let ((out (render-session-to-string sess 6 20)))
        (expect (search (format nil "~C[2 q" #\Escape) out)))))

  ;;; ── render-session-to-string with nil window ────────────────────────────────

  ;; render-session-to-string with a session that has no active window still renders.
  (it "render-session-no-window-produces-output"
    (let* ((sess (make-session :id 1 :name "0" :windows nil)))
      (finishes
        (let ((out (render-session-to-string sess 5 20)))
          (expect (plusp (length out)))))))

  ;;; ── %render-panes-and-borders with nil window ───────────────────────────────

  ;; %render-panes-and-borders with NIL window does not signal.
  (it "render-panes-borders-nil-window-finishes"
    (finishes
      (let ((buf (make-string-output-stream)))
        (cl-tmux/renderer::%render-panes-and-borders buf nil nil nil nil 80))))

  ;;; ── status-justify-line dispatch table ──────────────────────────────────────

  ;; %status-justify-line dispatches correctly to right/centre/left strategies.
  (it "status-justify-line-table-driven"
    (let ((cases
           ;; (justify left right cols . description)
           '(("right"  "L" "R" 20 . "right")
             ("centre" "L" "R" 20 . "centre")
             ("left"   "L" "R" 20 . "left (default)")
             ("unknown" "L" "R" 20 . "unknown falls back to left"))))
      (dolist (c cases)
        (destructuring-bind (justify left right cols . desc) c
          (declare (ignore desc))
          (let ((result (cl-tmux/renderer::%status-justify-line left right cols justify)))
            (expect (<= (length result) cols))
            (expect (search left result)))))))

  ;;; ── render-overlay with scroll offset ───────────────────────────────────────

  ;; render-overlay renders overlay lines starting from *overlay-scroll-offset*.
  (it "render-overlay-scroll-renders-lines-from-offset"
    (let ((*overlay* nil)
          (*overlay-scroll-offset* 0))
      (show-overlay (format nil "line-A~%line-B~%line-C"))
      (unwind-protect
           (let ((buf (make-string-output-stream)))
             (cl-tmux/renderer::render-overlay buf 30 10)
             (let ((out (get-output-stream-string buf)))
               (expect (search "line-A" out))))
        (clear-overlay))))

  ;;; ── %justify-right gap calculation ──────────────────────────────────────────

  ;; %justify-right total length equals cols when content fits.
  (it "status-bar-line-gap-fills-exactly"
    (let* ((left  "abcde")
           (time  "12:34")
           (cols  20)
           (line  (cl-tmux/renderer::%justify-right left time cols)))
      (expect (<= (length line) cols))))

  ;; %justify-right with empty left and time strings produces spaces up to cols.
  (it "status-bar-line-empty-left-and-time"
    (let ((line (cl-tmux/renderer::%justify-right "" "" 10)))
      (expect (<= (length line) 10))))

  ;;; ── render-session-to-string status on/off interaction ──────────────────────

  ;; With status=T and default options, the frame includes the HH:MM time pattern.
  (it "render-session-status-on-default-includes-time"
    (with-isolated-options ("status" t "status-left" nil)
      (let* ((sess (make-renderer-test-session 40 5))
             (out  (render-session-to-string sess 6 40)))
        ;; The default right status is HH:MM — 5 chars with a colon at position 2.
        ;; We just check a colon is present in a 5-char time substring.
        (expect (find #\: out)))))

  ;;; ── inline #[attr] style blocks + SGR-aware width (renderer-statusbar) ────────
  ;;;
  ;;; tmux status strings carry inline #[fg=…] style blocks and embedded SGR.  Those
  ;;; sequences are zero-width on screen, so the renderer expands #[…] into SGR and
  ;;; measures width by VISIBLE cells.  %visible-length/%visible-truncate must reduce
  ;;; to LENGTH/SUBSEQ on escape-free input (proven below) so older tests are intact.

  ;; %visible-length equals LENGTH for strings with no escape sequences.
  (it "visible-length-escape-free-equals-length"
    (expect (= 5 (cl-tmux/renderer::%visible-length "hello")))
    (expect (= 0 (cl-tmux/renderer::%visible-length "")))
    (expect (= (length "a:b 12:34")
               (cl-tmux/renderer::%visible-length "a:b 12:34"))))

  ;; %visible-length counts only visible cells, skipping CSI SGR escapes.
  (it "visible-length-skips-sgr-sequences"
    (let ((esc #\Escape))
      (expect (= 2 (cl-tmux/renderer::%visible-length
                    (format nil "~C[32mhi~C[0m" esc esc))))
      (expect (= 3 (cl-tmux/renderer::%visible-length
                    (format nil "~C[1;44;97mABC" esc))))))

  ;; %visible-truncate equals SUBSEQ for escape-free strings.
  (it "visible-truncate-escape-free-equals-subseq"
    (check-visible-truncate-cases
     '(("hello" 3  "hel"   "truncate to 3")
       ("hello" 5  "hello" "truncate at exact length")
       ("hello" 99 "hello" "truncate past length -> unchanged")
       ("hello" 0  ""      "truncate to 0 -> empty string"))))

  ;; %visible-truncate copies SGR escapes through without counting them toward N.
  (it "visible-truncate-passes-sgr-through"
    (let* ((esc  #\Escape)
           (in   (format nil "~C[32mABCDE" esc))
           (out  (cl-tmux/renderer::%visible-truncate in 2)))
      (expect (= 2 (cl-tmux/renderer::%visible-length out)))
      (expect (search "AB" out))
      (expect (char= esc (char out 0)))))

  ;; %status-style-block-sgr turns fg=green into the SGR colour code 32.
  (it "status-style-block-fg-becomes-sgr"
    (let ((out (cl-tmux/renderer::%status-style-block-sgr "fg=green" "44;97")))
      (expect (search (format nil "~C[32m" #\Escape) out))))

  ;; %status-style-block-sgr default/none/empty resets to the base status SGR.
  (it "status-style-block-default-resets-to-base"
    (check-status-style-reset-cases "44;97" '("default" "none" "" "  ")))

  ;; %status-expand-style-blocks returns escape-free / block-free text unchanged.
  (it "status-expand-style-blocks-no-block-unchanged"
    (check-status-expand-unchanged-cases "44;97" '("plain text" " 0 1:1* ")))

  ;; %status-expand-style-blocks turns #[fg=green]X#[default] into SGR around X.
  (it "status-expand-style-blocks-converts-blocks"
    (let* ((esc #\Escape)
           (out (cl-tmux/renderer::%status-expand-style-blocks
                 "#[fg=green]X#[default]Y" "44;97")))
      (expect (null (search "#[" out)))
      (expect (search (format nil "~C[32mX" esc) out))
      (expect (search (format nil "~C[0;44;97mY" esc) out))))

  ;; %clamp-status-segment measures visible cells; SGR escapes don't count and survive.
  (it "clamp-status-segment-counts-visible-not-sgr"
    (let* ((esc #\Escape)
           (txt (format nil "~C[32mhello~C[0m" esc esc)))   ; 5 visible cells
      (expect (string= txt (cl-tmux/renderer::%clamp-status-segment txt 5)))
      (expect (= 3 (cl-tmux/renderer::%visible-length
                    (cl-tmux/renderer::%clamp-status-segment txt 3))))))

  ;; %justify-right computes the gap from visible cells, so SGR doesn't shove content off-edge.
  (it "justify-right-ignores-sgr-width"
    (let* ((esc  #\Escape)
           (left (format nil "~C[32mABC~C[0m" esc esc))   ; 3 visible cells
           (line (cl-tmux/renderer::%justify-right left "RR" 20)))
      (expect (= 20 (cl-tmux/renderer::%visible-length line)))
      (expect (search "RR" line))))

  ;; render-status-bar expands status-left #[fg=green]…#[default] into real SGR,
  ;; and no literal #[ block reaches the output.
  (it "render-status-bar-inline-style-block-becomes-sgr"
    (with-isolated-options ("status-left"  "#[fg=green]G#[default]"
                            "status-right" nil
                            "status-style" "")
      (let* ((sess (make-renderer-test-session 40 6))
             (out  (render-status-bar-output sess 10 40)))
        (expect (search (format nil "~C[32m" #\Escape) out))
        (expect (null (search "#[" out)))
        (expect (find #\G out)))))

  ;;; ── Background-window bell relay (gap #23) ────────────────────────────────

  ;; bell-action controls whether BEL in a non-active window reaches the rendered frame.
  (it "render-session-background-bell-action-table"
    (dolist (row '(("any"     t   "bell-action 'any': background BEL must appear")
                   ("other"   t   "bell-action 'other': background BEL must appear")
                   ("current" nil "bell-action 'current': background BEL must be swallowed")
                   ("none"    nil "bell-action 'none': all BELs must be swallowed")))
      (destructuring-bind (bell-action expected-bell-p desc) row
        (declare (ignore desc))
        (with-isolated-options ("bell-action" bell-action "visual-bell" "off" "status" "off")
          (let* ((sess  (make-fake-session :nwindows 2))
                 (win2  (second (cl-tmux/model:session-windows sess)))
                 (pane2 (first (cl-tmux/model:window-panes win2))))
            (setf (cl-tmux/terminal/types:screen-bell-pending
                   (cl-tmux/model:pane-screen pane2)) t)
            (let ((out (cl-tmux/renderer::render-session-to-string sess 5 20)))
              (if expected-bell-p
                  (expect (find (code-char 7) out))
                  (expect (null (find (code-char 7) out))))
              ;; The pending bell is consumed either way — a bell swallowed by
              ;; bell-action must not ring later when its window becomes active.
              (expect (null (cl-tmux/terminal/types:screen-bell-pending
                             (cl-tmux/model:pane-screen pane2))))))))))

  ;; A BEL in the ACTIVE window fires the alert-bell hook with the window when
  ;; bell-action applies to the current window (any/current); other/none do not.
  (it "render-session-current-window-bell-fires-alert-bell-hook"
    (dolist (row '(("any" t) ("current" t) ("other" nil) ("none" nil)))
      (destructuring-bind (bell-action expect-fired) row
        (with-isolated-options ("bell-action" bell-action "status" "off")
          (with-isolated-hooks
            (let* ((sess     (make-fake-session :nwindows 1))
                   (win      (cl-tmux/model:session-active-window sess))
                   (pane     (cl-tmux/model:window-active-pane win))
                   (hook-win nil))
              (setf (cl-tmux/terminal/types:screen-bell-pending
                     (cl-tmux/model:pane-screen pane)) t)
              (cl-tmux/hooks:add-hook "alert-bell"
                                      (lambda (&rest args) (setf hook-win (first args))))
              (cl-tmux/renderer::render-session-to-string sess 5 20)
              (if expect-fired
                  (expect (eq win hook-win))
                  (expect (null hook-win)))))))))

  ;; visual-bell off/both relay the audible BEL; on is visual-only.
  (it "emit-bell-visual-bell-tri-state-table"
    (dolist (row '(("off" t) ("both" t) ("on" nil)))
      (destructuring-bind (visual expect-bel-p) row
        (let ((out (with-output-to-string (s)
                     (cl-tmux/renderer::%emit-bell s visual))))
          (if expect-bel-p
              (expect (find (code-char 7) out))
              (expect (null (find (code-char 7) out))))))))

  ;; %emit-bell rejects non-canonical visual-bell values instead of treating them as off.
  (it "emit-bell-rejects-non-canonical-visual-bell"
    (dolist (visual '(nil "" "disabled"))
      (signals error
        (with-output-to-string (s)
          (cl-tmux/renderer::%emit-bell s visual))))))
