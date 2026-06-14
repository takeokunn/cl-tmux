(in-package #:cl-tmux/test)

;;;; target tests — part B: %sigil-id, %name-prefix-p, edge cases for
;;;; find-session/window/pane-by-target, resolve-target window-only / multi-session,
;;;; %non-empty, parse-target table-driven, pane-by-numeric-index,
;;;; multi-digit ids, resolve-target empty string.

(in-suite target-suite)

;;; ── %sigil-id (pure helper) ──────────────────────────────────────────────────

(test sigil-id-table
  "%sigil-id parses sigil+N strings and returns the integer N, or NIL on mismatch."
  (dolist (row '(("$1"   #\$  1   "dollar single-digit")
                 ("$42"  #\$  42  "dollar multi-digit")
                 ("@3"   #\@  3   "at-sign")
                 ("%7"   #\%  7   "percent-sign")
                 ("$5"   #\@  nil "wrong sigil")
                 ("main" #\$  nil "non-sigil string")
                 (""     #\$  nil "empty string")))
    (destructuring-bind (input sigil expected desc) row
      (is (equal expected (cl-tmux::%sigil-id input sigil)) "~A" desc))))

;;; ── %name-prefix-p (pure helper) ─────────────────────────────────────────────

(test name-prefix-p-table
  "%name-prefix-p returns T when PREFIX equals or is a prefix of NAME, NIL otherwise."
  (dolist (row '((t   "foo"    "foo"     "exact match")
                 (t   "fo"     "foobar"  "prefix of longer name")
                 (nil "foobar" "fo"      "prefix longer than name")
                 (nil "bar"    "foobar"  "strings diverge")
                 (t   ""       "anything" "empty prefix matches anything")))
    (destructuring-bind (expected prefix name desc) row
      (is (eq expected (cl-tmux::%name-prefix-p prefix name)) "~A" desc))))

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

(test non-empty-table
  "%non-empty returns the string for non-empty input; NIL for empty string or NIL input."
  (dolist (row '(("hello" "hello" "%non-empty of \"hello\" must return itself")
                 (""      nil     "%non-empty of empty string must return NIL")
                 (nil     nil     "%non-empty of NIL input must return NIL")))
    (destructuring-bind (input expected desc) row
      (is (equal expected (cl-tmux::%non-empty input)) "~A" desc))))

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
