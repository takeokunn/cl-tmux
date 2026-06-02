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

(test parse-session-component-with-colon
  "%parse-session-component returns text before the colon when colon is present."
  (is (string= "sess"
               (cl-tmux::%parse-session-component "sess:win" 4 nil))
      "session component must be 'sess' when colon is at position 4"))

(test parse-session-component-no-colon-with-dot
  "%parse-session-component returns text before the dot when no colon is present."
  (is (string= "sess"
               (cl-tmux::%parse-session-component "sess.2" nil 4))
      "session component must be 'sess' when no colon but dot is at position 4"))

(test parse-session-component-no-colon-no-dot
  "%parse-session-component returns the whole string when neither colon nor dot."
  (is (string= "mysession"
               (cl-tmux::%parse-session-component "mysession" nil nil))
      "session component must be the whole string when no colon or dot"))

(test parse-session-component-empty-before-colon-returns-nil
  "%parse-session-component returns NIL when the text before the colon is empty."
  (is (null (cl-tmux::%parse-session-component ":win" 0 nil))
      "session component must be NIL when colon is at position 0"))

(test parse-session-component-empty-string-returns-nil
  "%parse-session-component returns NIL for the empty string."
  (is (null (cl-tmux::%parse-session-component "" nil nil))
      "session component must be NIL for empty input"))

;;; ── %parse-target ────────────────────────────────────────────────────────────

(test parse-target-nil-returns-all-nil
  "%parse-target with NIL returns (nil nil nil)."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target nil)
    (is (null s))
    (is (null w))
    (is (null p))))

(test parse-target-empty-string-returns-all-nil
  "%parse-target with empty string returns (nil nil nil)."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target "")
    (is (null s))
    (is (null w))
    (is (null p))))

(test parse-target-session-only
  "%parse-target with plain name extracts only the session component."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target "mysession")
    (is (string= "mysession" s))
    (is (null w))
    (is (null p))))

(test parse-target-session-colon-window
  "%parse-target with 'sess:win' extracts session and window."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target "sess:win")
    (is (string= "sess" s))
    (is (string= "win"  w))
    (is (null p))))

(test parse-target-session-colon-window-dot-pane
  "%parse-target with 'sess:win.pane' extracts all three components."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target "sess:win.3")
    (is (string= "sess" s))
    (is (string= "win"  w))
    (is (string= "3"    p))))

(test parse-target-session-dot-pane-no-window
  "%parse-target with 'sess.2' (no colon) extracts session and pane, no window."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target "sess.2")
    (is (string= "sess" s))
    (is (null w))
    (is (string= "2" p))))

(test parse-target-colon-window-only
  "%parse-target with ':win' gives nil session, window set."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target ":win")
    (is (null s))
    (is (string= "win" w))
    (is (null p))))

(test parse-target-dollar-session-id
  "%parse-target handles $N session ids."
  (multiple-value-bind (s w p) (cl-tmux::%parse-target "$1:@2.%3")
    (is (string= "$1" s))
    (is (string= "@2" w))
    (is (string= "%3" p))))

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

;;; ── %sigil-id (pure helper) ──────────────────────────────────────────────────

(test sigil-id-dollar-sign
  "%sigil-id parses $N with sigil $ and returns the integer N."
  (is (= 1  (cl-tmux::%sigil-id "$1"  #\$)))
  (is (= 42 (cl-tmux::%sigil-id "$42" #\$))))

(test sigil-id-at-sign
  "%sigil-id parses @N with sigil @ and returns the integer N."
  (is (= 3 (cl-tmux::%sigil-id "@3" #\@))))

(test sigil-id-percent-sign
  "%sigil-id parses %N with sigil % and returns the integer N."
  (is (= 7 (cl-tmux::%sigil-id "%7" #\%))))

(test sigil-id-wrong-sigil-returns-nil
  "%sigil-id returns NIL when the string begins with the wrong sigil char."
  (is (null (cl-tmux::%sigil-id "$5" #\@))
      "%sigil-id with wrong sigil must return NIL"))

(test sigil-id-non-sigil-string-returns-nil
  "%sigil-id returns NIL when the string does not begin with the sigil char."
  (is (null (cl-tmux::%sigil-id "main" #\$))
      "%sigil-id on plain name must return NIL"))

(test sigil-id-empty-string-returns-nil
  "%sigil-id returns NIL for an empty string."
  (is (null (cl-tmux::%sigil-id "" #\$))
      "%sigil-id on empty string must return NIL"))

;;; ── %name-prefix-p (pure helper) ─────────────────────────────────────────────

(test name-prefix-p-exact-match
  "%name-prefix-p returns T when PREFIX equals NAME."
  (is-true (cl-tmux::%name-prefix-p "foo" "foo")
           "%name-prefix-p must return T for exact match"))

(test name-prefix-p-prefix-match
  "%name-prefix-p returns T when NAME starts with PREFIX."
  (is-true (cl-tmux::%name-prefix-p "fo" "foobar")
           "%name-prefix-p must return T when name starts with prefix"))

(test name-prefix-p-no-match
  "%name-prefix-p returns NIL when PREFIX is longer than NAME."
  (is-false (cl-tmux::%name-prefix-p "foobar" "fo")
            "%name-prefix-p must return NIL when prefix is longer than name"))

(test name-prefix-p-different-strings
  "%name-prefix-p returns NIL when PREFIX does not match the start of NAME."
  (is-false (cl-tmux::%name-prefix-p "bar" "foobar")
            "%name-prefix-p must return NIL when strings diverge"))

(test name-prefix-p-empty-prefix
  "%name-prefix-p with an empty prefix matches any name."
  (is-true (cl-tmux::%name-prefix-p "" "anything")
           "%name-prefix-p with empty prefix must return T for any name"))

;;; ── find-session-by-target edge cases ────────────────────────────────────────

(test find-session-by-target-empty-registry-returns-nil
  "find-session-by-target returns NIL when the registry is empty."
  (is (null (cl-tmux::find-session-by-target nil "alpha"))
      "find-session-by-target on nil registry must return NIL"))

;;; ── find-window-by-target edge cases ─────────────────────────────────────────

(test find-window-by-target-empty-windows-returns-nil
  "find-window-by-target returns NIL when the session has no windows."
  (let ((sess (make-session :id 1 :name "s" :windows nil)))
    (is (null (cl-tmux::find-window-by-target sess "any"))
        "find-window-by-target with empty windows list must return NIL")))

(test find-window-by-target-index-out-of-range-returns-nil
  "find-window-by-target returns NIL when numeric index exceeds window count."
  (let* ((w1   (make-window :id 1 :name "w1" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1))))
    (is (null (cl-tmux::find-window-by-target sess "5"))
        "find-window-by-target with out-of-range index must return NIL")))

;;; ── find-pane-by-target edge cases ───────────────────────────────────────────

(test find-pane-by-target-empty-panes-returns-nil
  "find-pane-by-target returns NIL when the window has no panes."
  (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :panes nil)))
    (is (null (cl-tmux::find-pane-by-target win "%1"))
        "find-pane-by-target with empty pane list must return NIL")))

(test find-pane-by-target-index-out-of-range-returns-nil
  "find-pane-by-target returns NIL when numeric index exceeds pane count."
  (let* ((p1  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p1))))
    (is (null (cl-tmux::find-pane-by-target win "10"))
        "find-pane-by-target with out-of-range index must return NIL")))

;;; ── resolve-target: window-only target ───────────────────────────────────────

(test resolve-target-colon-window-only
  "resolve-target with ':win' resolves to the named window in current session."
  (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
         (w1   (make-window :id 1 :name "alpha" :width 80 :height 24
                            :panes (list p1)))
         (w2   (make-window :id 2 :name "beta" :width 80 :height 24
                            :panes (list (make-no-pty-pane 2 0 0 80 24))))
         (sess (make-session :id 1 :name "s" :windows (list w1 w2))))
    (window-select-pane w1 p1)
    (session-select-window sess w1)
    (let ((registry (list (cons "s" sess))))
      (multiple-value-bind (_rs rw _rp)
          (cl-tmux::resolve-target registry ":beta"
                                   :current-session sess
                                   :current-window  w1)
        (declare (ignore _rs _rp))
        (is (eq w2 rw)
            "':beta' must resolve to window w2 (got ~S)" rw)))))

;;; ── resolve-target: multiple sessions in registry ────────────────────────────

(test resolve-target-multiple-sessions-selects-correct-one
  "resolve-target selects the correct session from a multi-entry registry."
  (let* ((p1a  (make-no-pty-pane 1 0 0 80 24))
         (w1   (make-window :id 1 :name "w" :width 80 :height 24 :panes (list p1a)))
         (s1   (make-session :id 1 :name "one" :windows (list w1)))
         (p2a  (make-no-pty-pane 2 0 0 80 24))
         (w2   (make-window :id 2 :name "w" :width 80 :height 24 :panes (list p2a)))
         (s2   (make-session :id 2 :name "two" :windows (list w2)))
         (registry (list (cons "one" s1) (cons "two" s2))))
    (window-select-pane w1 p1a)
    (window-select-pane w2 p2a)
    (session-select-window s1 w1)
    (session-select-window s2 w2)
    (multiple-value-bind (rs _rw _rp)
        (cl-tmux::resolve-target registry "two")
      (declare (ignore _rw _rp))
      (is (eq s2 rs)
          "resolve-target 'two' must select session s2 (got ~S)" rs))))

;;; ── %non-empty pure helper ───────────────────────────────────────────────────

(test non-empty-returns-non-empty-string
  "%non-empty returns the string when it is non-empty."
  (is (string= "hello" (cl-tmux::%non-empty "hello"))
      "%non-empty must return the string when non-empty"))

(test non-empty-returns-nil-for-empty-string
  "%non-empty returns NIL when the string is empty."
  (is (null (cl-tmux::%non-empty ""))
      "%non-empty must return NIL for an empty string"))

(test non-empty-returns-nil-for-nil
  "%non-empty returns NIL when the input is NIL."
  (is (null (cl-tmux::%non-empty nil))
      "%non-empty must return NIL for NIL input"))

;;; ── Table-driven %parse-target cases ────────────────────────────────────────

(test parse-target-table
  "Table-driven: %parse-target decomposes various target strings correctly."
  ;; Each entry: (target-string expected-sess expected-win expected-pane description)
  (dolist (entry
           '(("sess:win.3" "sess" "win" "3" "full path")
             ("sess:win"   "sess" "win" nil "session and window only")
             ("sess.2"     "sess" nil   "2" "session and pane no window")
             (":win"       nil    "win" nil "window only via colon prefix")
             ("mysess"     "mysess" nil nil "bare session name")))
    (destructuring-bind (target-str exp-sess exp-win exp-pane desc) entry
      (multiple-value-bind (s w p) (cl-tmux::%parse-target target-str)
        (is (equal exp-sess s) "session component of ~S: ~S" target-str desc)
        (is (equal exp-win  w) "window  component of ~S: ~S" target-str desc)
        (is (equal exp-pane p) "pane    component of ~S: ~S" target-str desc)))))

;;; ── resolve-target with pane specified as numeric index ─────────────────────

(test resolve-target-pane-by-numeric-index
  "resolve-target resolves a pane by its 0-based numeric index."
  (let* ((p1   (make-no-pty-pane 1  0 0 40 24))
         (p2   (make-no-pty-pane 2 41 0 40 24))
         (win  (make-window :id 1 :name "w" :width 81 :height 24
                            :panes (list p1 p2)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (let ((registry (list (cons "s" sess))))
      (multiple-value-bind (_rs _rw rp)
          (cl-tmux::resolve-target registry "s:w.1")
        (declare (ignore _rs _rw))
        (is (eq p2 rp) "pane at index 1 must be p2")))))

;;; ── find-session-by-target: id higher than 9 ────────────────────────────────

(test find-session-by-target-multi-digit-id
  "find-session-by-target parses $N with multi-digit N correctly."
  (let* ((sess (make-session :id 42 :name "big" :windows nil))
         (registry (list (cons "big" sess))))
    (is (eq sess (cl-tmux::find-session-by-target registry "$42"))
        "find-session-by-target must resolve $42 to the session with id 42")))

;;; ── find-window-by-target: @N with multi-digit id ────────────────────────────

(test find-window-by-target-multi-digit-at-id
  "find-window-by-target parses @N with multi-digit N correctly."
  (let* ((w1   (make-window :id 99 :name "bigwin" :width 80 :height 24))
         (sess (make-session :id 1 :name "s" :windows (list w1))))
    (is (eq w1 (cl-tmux::find-window-by-target sess "@99"))
        "find-window-by-target must resolve @99 to the window with id 99")))

;;; ── find-pane-by-target: %N with multi-digit id ──────────────────────────────

(test find-pane-by-target-multi-digit-percent-id
  "find-pane-by-target parses %N with multi-digit N correctly."
  (let* ((p1  (make-no-pty-pane 15 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p1))))
    (is (eq p1 (cl-tmux::find-pane-by-target win "%15"))
        "find-pane-by-target must resolve %15 to the pane with id 15")))

;;; ── resolve-target: empty string is same as NIL ─────────────────────────────

(test resolve-target-empty-string-uses-current-defaults
  "resolve-target with an empty string behaves identically to NIL target."
  (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list p1)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (window-select-pane win p1)
    (session-select-window sess win)
    (multiple-value-bind (rs rw rp)
        (cl-tmux::resolve-target nil ""
                                 :current-session sess
                                 :current-window  win
                                 :current-pane    p1)
      (is (eq sess rs) "empty-string target must use current-session")
      (is (eq win  rw) "empty-string target must use current-window")
      (is (eq p1   rp) "empty-string target must use current-pane"))))
