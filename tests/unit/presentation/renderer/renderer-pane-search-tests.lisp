(in-package #:cl-tmux/test)

;;;; Direct unit tests for renderer-pane-search.lisp's %render-copy-search-matches.
;;;; Existing tests (renderer-pane-tests-b.lisp) already cover the base
;;;; match-style highlighting and the no-search-term no-op case via
;;;; render-session-to-string, but nothing previously distinguished the
;;;; "current match" (the span under the copy-mode cursor, styled with
;;;; copy-mode-current-match-style) from the other, non-current matches
;;;; (styled with copy-mode-match-style) — the more subtle branch of
;;;; %render-copy-search-matches's per-range current-p test.

(describe "renderer-suite/pane-search"

  ;; When the copy-mode cursor sits inside a match span, that span uses
  ;; copy-mode-current-match-style; other matches still use copy-mode-match-style.
  (it "copy-search-current-match-uses-current-style"
    (with-fake-session (s)
      (feed (active-screen s) "hello world hello")
      (cl-tmux/commands::copy-mode-enter (active-screen s))
      (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) "hello")
      ;; "hello world hello" -> matches at columns [0,5) and [12,17); put the
      ;; cursor inside the second match.
      (setf (cl-tmux/terminal/types:screen-copy-cursor (active-screen s)) (cons 0 13))
      (let* ((match-sgr   (cl-tmux/renderer:style-to-sgr
                           (cl-tmux/renderer:parse-style-string "bg=green")))
             (current-sgr (cl-tmux/renderer:style-to-sgr
                           (cl-tmux/renderer:parse-style-string "bg=magenta")))
             (frame       (cl-tmux/renderer:render-session-to-string s 24 81)))
        (expect frame :to-contain-sgr match-sgr)
        (expect frame :to-contain-sgr current-sgr))))

  ;; When the copy-mode cursor is outside every match span, all matches use
  ;; copy-mode-match-style and copy-mode-current-match-style never appears.
  (it "copy-search-cursor-off-match-uses-only-plain-style"
    (with-fake-session (s)
      (feed (active-screen s) "hello world hello")
      (cl-tmux/commands::copy-mode-enter (active-screen s))
      (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) "hello")
      ;; Column 7 is inside "world", not a match.
      (setf (cl-tmux/terminal/types:screen-copy-cursor (active-screen s)) (cons 0 7))
      (let* ((match-sgr   (cl-tmux/renderer:style-to-sgr
                           (cl-tmux/renderer:parse-style-string "bg=green")))
             (current-sgr (cl-tmux/renderer:style-to-sgr
                           (cl-tmux/renderer:parse-style-string "bg=magenta")))
             (frame       (cl-tmux/renderer:render-session-to-string s 24 81)))
        (expect frame :to-contain-sgr match-sgr)
        (expect frame :not :to-contain-sgr current-sgr))))

  ;; With copy-mode-current-match-style cleared, the cursor's own match falls
  ;; back to the plain match style rather than emitting no SGR at all.
  (it "copy-search-current-match-falls-back-when-current-style-empty"
    (with-isolated-options ("copy-mode-current-match-style" "")
      (with-fake-session (s)
        (feed (active-screen s) "hello world hello")
        (cl-tmux/commands::copy-mode-enter (active-screen s))
        (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) "hello")
        (setf (cl-tmux/terminal/types:screen-copy-cursor (active-screen s)) (cons 0 13))
        (let* ((match-sgr (cl-tmux/renderer:style-to-sgr
                           (cl-tmux/renderer:parse-style-string "bg=green")))
               (frame     (cl-tmux/renderer:render-session-to-string s 24 81)))
          (expect frame :to-contain-sgr match-sgr))))))
