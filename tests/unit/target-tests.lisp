(in-package #:cl-tmux/test)

;;;; Tests for src/target.lisp — session/window/pane target resolution.
;;;;
;;;; Tests: target-suite — %parse-target, find-session-by-target,
;;;; find-window-by-target, find-pane-by-target, resolve-target.

(def-suite target-suite :description "Session/window/pane target resolution")
(in-suite target-suite)

;;; ── %parse-session-component direct tests ───────────────────────────────────
;;;
;;; %parse-session-component has non-trivial logic for the colon/dot cases.
;;; Testing it directly gives coverage independent of %parse-target's full path.

(test parse-session-component-table
  "%parse-session-component extracts text before the colon or dot, returning NIL for empty."
  (dolist (c '(("sess:win"   4   nil "sess"      "text before colon")
               ("sess.2"     nil 4   "sess"      "text before dot (no colon)")
               ("mysession"  nil nil "mysession" "whole string (no colon or dot)")
               (":win"       0   nil nil         "NIL when empty before colon")
               (""           nil nil nil         "NIL for empty string")))
    (destructuring-bind (input colon-pos dot-pos expected desc) c
      (is (equal expected (cl-tmux::%parse-session-component input colon-pos dot-pos))
          "~A" desc))))

;;; ── %parse-target ────────────────────────────────────────────────────────────

(test parse-target-nil-and-empty-return-all-nil
  "%parse-target with NIL or empty string returns (nil nil nil) for all three components."
  (dolist (input '(nil ""))
    (multiple-value-bind (s w p) (cl-tmux::%parse-target input)
      (is (null s) "session must be NIL for input ~S" input)
      (is (null w) "window must be NIL for input ~S" input)
      (is (null p) "pane must be NIL for input ~S" input))))

(test parse-target-table
  "%parse-target decomposes target strings into (session window pane) components."
  (dolist (c '(("mysession"  "mysession" nil   nil   "plain name → session only")
               ("sess:win"   "sess"      "win" nil   "sess:win → session+window")
               ("sess:win.3" "sess"      "win" "3"   "sess:win.pane → all three")
               ("sess.2"     "sess"      nil   "2"   "sess.N (no colon) → session+pane")
               (":win"       nil         "win" nil   ":win → window only")
               ("$1:@2.%3"  "$1"        "@2"  "%3"  "sigil forms → session+window+pane")
               ("%2"         nil         nil   "%2"  "bare %N → pane id")
               ("@3"         nil         "@3"  nil   "bare @N → window id")
               ("$1"         "$1"        nil   nil   "bare $N → session id")
               ("work"       "work"      nil   nil   "plain name stays session")))
    (destructuring-bind (input expected-s expected-w expected-p desc) c
      (multiple-value-bind (s w p) (cl-tmux::%parse-target input)
        (is (equal expected-s s) "~A: session must be ~S (got ~S)" desc expected-s s)
        (is (equal expected-w w) "~A: window must be ~S (got ~S)"  desc expected-w w)
        (is (equal expected-p p) "~A: pane must be ~S (got ~S)"    desc expected-p p)))))

;;; ── find-session-by-target ───────────────────────────────────────────────────

(test find-session-by-target-nil-target-returns-nil
  "find-session-by-target returns NIL when target-str is NIL."
  (let ((sess (make-session :id 1 :name "main" :windows nil)))
    (is (null (cl-tmux::find-session-by-target
               (list (cons "main" sess)) nil)))))

(test find-session-by-target-exact-name
  "find-session-by-target finds a session by exact name match."
  (let* ((s1 (make-session :id 1 :name "alpha" :windows nil))
         (s2 (make-session :id 2 :name "beta"  :windows nil))
         (registry (list (cons "alpha" s1) (cons "beta" s2))))
    (is (eq s1 (cl-tmux::find-session-by-target registry "alpha")))
    (is (eq s2 (cl-tmux::find-session-by-target registry "beta")))))

(test find-session-by-target-dollar-id
  "find-session-by-target matches $N by session-id."
  (let* ((s1 (make-session :id 1 :name "first"  :windows nil))
         (s2 (make-session :id 2 :name "second" :windows nil))
         (registry (list (cons "first" s1) (cons "second" s2))))
    (is (eq s1 (cl-tmux::find-session-by-target registry "$1")))
    (is (eq s2 (cl-tmux::find-session-by-target registry "$2")))))

(test find-session-by-target-prefix-match
  "find-session-by-target matches by name prefix when no exact match."
  (let* ((s1 (make-session :id 1 :name "longname" :windows nil))
         (registry (list (cons "longname" s1))))
    (is (eq s1 (cl-tmux::find-session-by-target registry "long")))))

(test find-session-by-target-no-match-returns-nil
  "find-session-by-target returns NIL when no session matches."
  (let* ((s1 (make-session :id 1 :name "alpha" :windows nil))
         (registry (list (cons "alpha" s1))))
    (is (null (cl-tmux::find-session-by-target registry "beta")))))

;;; ── find-window-by-target ────────────────────────────────────────────────────

(test find-window-by-target-nil-inputs-return-nil
  "find-window-by-target returns NIL when session or target is NIL."
  (let* ((w1 (make-window :id 1 :name "w1" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1))))
    (is (null (cl-tmux::find-window-by-target nil "w1")))
    (is (null (cl-tmux::find-window-by-target sess nil)))))

(test find-window-by-target-exact-name
  "find-window-by-target finds by exact window name."
  (let* ((w1 (make-window :id 1 :name "editor" :width 80 :height 24))
         (w2 (make-window :id 2 :name "shell"  :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1 w2))))
    (is (eq w1 (cl-tmux::find-window-by-target sess "editor")))
    (is (eq w2 (cl-tmux::find-window-by-target sess "shell")))))

(test find-window-by-target-at-id
  "find-window-by-target finds by @N notation."
  (let* ((w1 (make-window :id 1 :name "win1" :width 80 :height 24))
         (w2 (make-window :id 2 :name "win2" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1 w2))))
    (is (eq w1 (cl-tmux::find-window-by-target sess "@1")))
    (is (eq w2 (cl-tmux::find-window-by-target sess "@2")))))

(test find-window-by-target-numeric-index
  "find-window-by-target finds by 0-based numeric index."
  (let* ((w1 (make-window :id 1 :name "win1" :width 80 :height 24))
         (w2 (make-window :id 2 :name "win2" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1 w2))))
    (is (eq w1 (cl-tmux::find-window-by-target sess "0")))
    (is (eq w2 (cl-tmux::find-window-by-target sess "1")))))

(test find-window-by-target-prefix-match
  "find-window-by-target falls back to name prefix."
  (let* ((w1 (make-window :id 1 :name "editwin" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1))))
    (is (eq w1 (cl-tmux::find-window-by-target sess "edit")))))

(test find-window-by-target-no-match-returns-nil
  "find-window-by-target returns NIL when no window matches."
  (let* ((w1 (make-window :id 1 :name "alpha" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1))))
    (is (null (cl-tmux::find-window-by-target sess "beta")))))

;;; ── find-pane-by-target ──────────────────────────────────────────────────────

(test find-pane-by-target-nil-inputs-return-nil
  "find-pane-by-target returns NIL when window or target is NIL."
  (let* ((p1  (make-no-pty-pane 1 0 0 40 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p1))))
    (is (null (cl-tmux::find-pane-by-target nil "%1")))
    (is (null (cl-tmux::find-pane-by-target win nil)))))

(test find-pane-by-target-percent-id
  "find-pane-by-target finds by %N notation."
  (let* ((p1  (make-no-pty-pane 1  0 0 40 24))
         (p2  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p1 p2))))
    (is (eq p1 (cl-tmux::find-pane-by-target win "%1")))
    (is (eq p2 (cl-tmux::find-pane-by-target win "%2")))))

(test find-pane-by-target-numeric-index
  "find-pane-by-target finds by 0-based numeric index."
  (let* ((p1  (make-no-pty-pane 1  0 0 40 24))
         (p2  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p1 p2))))
    (is (eq p1 (cl-tmux::find-pane-by-target win "0")))
    (is (eq p2 (cl-tmux::find-pane-by-target win "1")))))

(test find-pane-by-target-no-match-returns-nil
  "find-pane-by-target returns NIL when pane id does not exist."
  (let* ((p1  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p1))))
    (is (null (cl-tmux::find-pane-by-target win "%99")))
    (is (null (cl-tmux::find-pane-by-target win "5")))))

;;; ── resolve-target ───────────────────────────────────────────────────────────

(test resolve-target-nil-returns-current-defaults
  "resolve-target with NIL target returns the current-* defaults."
  (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list p1)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (multiple-value-bind (rs rw rp)
        (cl-tmux::resolve-target nil nil
                                 :current-session sess
                                 :current-window  win
                                 :current-pane    p1)
      (is (eq sess rs) "session must default to current-session")
      (is (eq win  rw) "window  must default to current-window")
      (is (eq p1   rp) "pane    must default to current-pane"))))

(test resolve-target-session-by-name
  "resolve-target resolves a named session from the registry."
  (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list p1)))
         (sess (make-session :id 1 :name "mysess" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (let ((registry (list (cons "mysess" sess))))
      (multiple-value-bind (rs rw rp)
          (cl-tmux::resolve-target registry "mysess")
        (is (eq sess rs))
        (is (eq win  rw))
        (is (eq p1   rp))))))

(test resolve-target-session-colon-window
  "resolve-target resolves 'sess:win' to the named session and window."
  (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
         (w1   (make-window :id 1 :name "editor" :width 80 :height 24
                            :panes (list p1)))
         (w2   (make-window :id 2 :name "shell" :width 80 :height 24
                            :panes (list (make-no-pty-pane 2 0 0 80 24))))
         (sess (make-session :id 1 :name "work" :windows (list w1 w2))))
    (window-select-pane w1 p1)
    (session-select-window sess w1)
    (let ((registry (list (cons "work" sess))))
      (multiple-value-bind (rs rw _rp)
          (cl-tmux::resolve-target registry "work:shell")
        (declare (ignore _rp))
        (is (eq sess rs))
        (is (eq w2   rw))))))

(test resolve-target-full-path
  "resolve-target resolves 'sess:win.pane' fully."
  (let* ((p1   (make-no-pty-pane 1  0 0 40 24))
         (p2   (make-no-pty-pane 2 41 0 40 24))
         (win  (make-window :id 1 :name "w" :width 81 :height 24
                            :panes (list p1 p2)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (let ((registry (list (cons "s" sess))))
      (multiple-value-bind (_rs _rw rp)
          (cl-tmux::resolve-target registry "s:w.%2")
        (declare (ignore _rs _rw))
        (is (eq p2 rp) "pane must be resolved via %2 notation")))))

(test resolve-target-bare-pane-sigil-resolves-pane
  "resolve-target with a BARE %N resolves that pane in the current window —
   tmux's position-independent pane id, not a session fallback."
  (let* ((p1   (make-no-pty-pane 1  0 0 40 24))
         (p2   (make-no-pty-pane 2 41 0 40 24))
         (win  (make-window :id 1 :name "w" :width 81 :height 24
                            :panes (list p1 p2)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (multiple-value-bind (_rs _rw rp)
        (cl-tmux::resolve-target nil "%2"
                                 :current-session sess :current-window win)
      (declare (ignore _rs _rw))
      (is (eq p2 rp)
          "bare %2 must resolve to pane-id 2, not fall back to the active pane"))))

(test resolve-target-unknown-session-falls-back-to-current
  "resolve-target falls back to current-session when target session is unknown."
  (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list p1)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (multiple-value-bind (rs _rw _rp)
        (cl-tmux::resolve-target nil "nonexistent"
                                 :current-session sess)
      (declare (ignore _rw _rp))
      ;; When the named session is not found, current-session is used.
      (is (eq sess rs)))))
