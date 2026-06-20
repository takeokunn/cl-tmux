(in-package #:cl-tmux/renderer)

;;;; Session-frame compositing for the cl-tmux renderer.
;;;;
;;;; This file owns the full-frame pipeline: lock-screen overlay, pane/border
;;;; rendering, overlay dispatch, mouse sequences, bell emission, cursor
;;;; restoration, and the render-session / render-session-to-string entry points.
;;;;
;;;; Status-bar composition lives in renderer-statusbar.lisp (loaded just before
;;;; this file).
;;;;
;;;; Load order: renderer-format → renderer-style → renderer-pane
;;;;             → renderer-overlay → renderer-statusbar
;;;;             → renderer-compose-protocols → renderer-compose-overlay
;;;;             → renderer-compose-effects → renderer-compose

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((source (or *load-truename* *compile-file-truename*))
         (base (and source
                    (make-pathname :name nil :type nil :defaults source))))
    (dolist (name '("renderer-compose-protocols.lisp"
                    "renderer-compose-overlay.lisp"
                    "renderer-compose-effects.lisp"))
      (let ((path (and base (merge-pathnames name base))))
        (when (and path (probe-file path))
          (load path))))))

;;; ── Lock-screen overlay ─────────────────────────────────────────────────────

(defun render-lock-screen (stream terminal-rows terminal-cols)
  "Render a full-screen lock overlay.  Fills the screen with a solid colour
   and centres a 'Session locked' message."
  (reset-attrs stream)
  (%emit-sgr stream +sgr-default-status+)
  ;; Fill all rows with spaces.
  (let ((blank-row (make-string terminal-cols :initial-element #\Space)))
    (loop for row below (1- terminal-rows)
          do (move-to stream row 0)
             (write-string blank-row stream)))
  ;; Centre the lock message.
  (let* ((msg     "Session locked — press any key to unlock")
         (mlen    (min (length msg) terminal-cols))
         (mid-row (floor terminal-rows 2))
         (mid-col (%center-coord terminal-cols mlen)))
    (move-to stream mid-row mid-col)
    (write-string (subseq msg 0 mlen) stream))
  (reset-attrs stream))

(defun %render-panes-and-borders (buffer session window panes active-pane terminal-cols)
  "Render all panes and split-tree borders for WINDOW into BUFFER.
   Snapshots zoom state under the window lock to avoid a race with
   window-zoom-toggle running on the main thread."
  (let ((zoomed nil) (tree nil))
    (when window
      (with-lock-held ((window-lock window))
        (setf zoomed (window-zoom-p window)
              tree   (window-tree   window))))
          (dolist (pane panes) (render-pane buffer session pane))
    (when (and tree (not zoomed))
      (render-tree-borders buffer tree active-pane terminal-cols))))

(defun render-session-to-string (session terminal-rows terminal-cols)
  "Compose a full frame for SESSION as an escape-sequence string.
   Does not touch *standard-output*; suitable for unit-testing without a TTY."
  (let* ((buffer      (make-string-output-stream))
         (window      (session-active-window session))
         (panes       (when window (window-panes window)))
         (active-pane (session-active-pane session))
         ;; Status row count from the `status` option (0..5).  The pane layout
         ;; reserves the matching count via cl-tmux/config:*status-height*, kept
         ;; in sync by the `status` option's side-effect — so the bar and the
         ;; pane area stay in lockstep in normal use.
         (status-lines (status-line-count))
         (status-on   (> status-lines 0))
         (status-pos  (cl-tmux/options:get-option "status-position" "bottom")))
    (cursor-invisible buffer)
    (if (session-locked-p session)
        (render-lock-screen buffer terminal-rows terminal-cols)
        (progn
          (%render-panes-and-borders buffer session window panes active-pane terminal-cols)
          ;; pane-border-status title lines (drawn after borders so they overwrite border cells)
          (when (and window panes
                     (string/= (cl-tmux/options:get-option "pane-border-status" "off") "off"))
            (dolist (pane panes)
              (%render-pane-border-status buffer pane session window)))
          ;; display-panes (C-b q): big per-pane numbers while the display-panes overlay
          ;; is active, coloured by display-panes-(active-)colour.  Drawn after borders so
          ;; the numbers overlay the pane content, before the top overlay layer.
          (when (and cl-tmux/prompt:*display-panes-active* (overlay-active-p) window panes)
            (dolist (pane panes)
              (%draw-pane-number-to-screen buffer (pane-x pane) (pane-y pane)
                                           (pane-width pane) (pane-height pane)
                                           (pane-id pane) (eq pane active-pane))))
          ;; copy-mode search-match highlighting on the active pane (it is the one that
          ;; can be in copy mode), overdrawn after panes/borders.
          (when (and active-pane (pane-screen active-pane)
                     (screen-copy-mode-p (pane-screen active-pane)))
            (%render-copy-search-matches buffer active-pane))
          (%render-overlay-layer buffer active-pane terminal-rows terminal-cols)
          (when status-on
            (render-status-region buffer session terminal-rows terminal-cols
                                  status-lines status-pos))
          (%render-mouse-sequences buffer active-pane)
          ;; allow-passthrough: emit any DCS-passthrough sequences (images, nested tmux).
          (when panes (%render-passthrough buffer panes))
          (when panes (%render-clipboard buffer panes))
          (%render-bell-and-cursor buffer active-pane)
          ;; Relay bells from background windows (bell-action 'any'/'other').
          (%render-background-bells buffer session window)
          ;; set-titles: emit OSC 0 to set the outer terminal window title.
          (when (cl-tmux/options:get-option "set-titles")
            (let* ((title-fmt (cl-tmux/options:get-option "set-titles-string" "#W"))
                   (win        (session-active-window session))
                   (pane       (session-active-pane session))
                   (ctx        (cl-tmux/format:format-context-from-session session win pane))
                   (title      (cl-tmux/format:expand-format title-fmt ctx)))
              (format buffer "~C]0;~A~C" +esc+ title (code-char 7))))))
    (get-output-stream-string buffer)))

(defun render-session (session terminal-rows terminal-cols)
  "Repaint all panes and the status bar; flush to *standard-output* in one write."
  (write-string (render-session-to-string session terminal-rows terminal-cols))
  (force-output))
