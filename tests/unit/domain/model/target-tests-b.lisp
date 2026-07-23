(in-package #:cl-tmux/test)

;;;; target tests — part B: %sigil-id, %name-prefix-p, edge cases for
;;;; find-session/window/pane-by-target, resolve-target window-only / multi-session,
;;;; %non-empty, parse-target table-driven, pane-by-numeric-index,
;;;; multi-digit ids, resolve-target empty string.

(describe "target-suite"

  ;;; ── %sigil-id (pure helper) ──────────────────────────────────────────────────

  ;; %sigil-id parses sigil+N strings and returns the integer N, or NIL on mismatch.
  (it "sigil-id-table"
    (dolist (row '(("$1"   #\$  1   "dollar single-digit")
                   ("$42"  #\$  42  "dollar multi-digit")
                   ("@3"   #\@  3   "at-sign")
                   ("%7"   #\%  7   "percent-sign")
                   ("$5"   #\@  nil "wrong sigil")
                   ("main" #\$  nil "non-sigil string")
                   (""     #\$  nil "empty string")))
      (destructuring-bind (input sigil expected desc) row
        (declare (ignore desc))
        (expect (equal expected (cl-tmux::%sigil-id input sigil))))))

  ;;; ── %name-prefix-p (pure helper) ─────────────────────────────────────────────

  ;; %name-prefix-p returns T when PREFIX equals or is a prefix of NAME, NIL otherwise.
  (it "name-prefix-p-table"
    (dolist (row '((t   "foo"    "foo"     "exact match")
                   (t   "fo"     "foobar"  "prefix of longer name")
                   (nil "foobar" "fo"      "prefix longer than name")
                   (nil "bar"    "foobar"  "strings diverge")
                   (t   ""       "anything" "empty prefix matches anything")))
      (destructuring-bind (expected prefix name desc) row
        (declare (ignore desc))
        (expect (eq expected (cl-tmux::%name-prefix-p prefix name))))))

  ;;; ── find-session-by-target edge cases ────────────────────────────────────────

  ;; find-session-by-target returns NIL when the registry is empty.
  (it "find-session-by-target-empty-registry-returns-nil"
    (expect (null (cl-tmux::find-session-by-target nil "alpha"))))

  ;;; ── find-window-by-target edge cases ─────────────────────────────────────────

  ;; find-window-by-target returns NIL when the session has no windows.
  (it "find-window-by-target-empty-windows-returns-nil"
    (let ((sess (make-session :id 1 :name "s" :windows nil)))
      (expect (null (cl-tmux::find-window-by-target sess "any")))))

  ;; find-window-by-target returns NIL when numeric index exceeds window count.
  (it "find-window-by-target-index-out-of-range-returns-nil"
    (let* ((w1   (make-window :id 1 :name "w1" :width 80 :height 24))
           (sess (make-session :id 1 :name "s" :windows (list w1))))
      (expect (null (cl-tmux::find-window-by-target sess "5")))))

  ;;; ── find-pane-by-target edge cases ───────────────────────────────────────────

  ;; find-pane-by-target returns NIL when the window has no panes.
  (it "find-pane-by-target-empty-panes-returns-nil"
    (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :panes nil)))
      (expect (null (cl-tmux::find-pane-by-target win "%1")))))

  ;; find-pane-by-target returns NIL when numeric index exceeds pane count.
  (it "find-pane-by-target-index-out-of-range-returns-nil"
    (let* ((p1  (make-no-pty-pane 1 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p1))))
      (expect (null (cl-tmux::find-pane-by-target win "10")))))

  ;;; ── resolve-target: window-only target ───────────────────────────────────────

  ;; resolve-target with ':win' resolves to the named window in current session.
  (it "resolve-target-colon-window-only"
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
          (expect (eq w2 rw))))))

  ;;; ── resolve-target: multiple sessions in registry ────────────────────────────

  ;; resolve-target selects the correct session from a multi-entry registry.
  (it "resolve-target-multiple-sessions-selects-correct-one"
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
        (expect (eq s2 rs)))))

  ;;; ── %non-empty pure helper ───────────────────────────────────────────────────

  ;; %non-empty returns the string for non-empty input; NIL for empty string or NIL input.
  (it "non-empty-table"
    (dolist (row '(("hello" "hello" "%non-empty of \"hello\" must return itself")
                   (""      nil     "%non-empty of empty string must return NIL")
                   (nil     nil     "%non-empty of NIL input must return NIL")))
      (destructuring-bind (input expected desc) row
        (declare (ignore desc))
        (expect (equal expected (cl-tmux::%non-empty input))))))

  ;;; ── %parse-integer-or-nil pure helper ────────────────────────────────────────

  ;; %parse-integer-or-nil parses numeric strings, supports parse-integer keywords, and returns NIL for invalid input.
  (it "parse-integer-or-nil-table"
    (dolist (row '(("0"    0   nil                         "zero parses")
                   ("42"   42  nil                         "multi-digit parses")
                   ("-7"   -7  nil                         "signed integers parse")
                   ("1f"   31  (:radix 16)                "hexadecimal parsing works")
                   ("123x" 123 (:end 3)                   "substring parsing works")
                   ("abc"  nil nil                        "alphabetic input returns NIL")
                   (""     nil nil                        "empty string returns NIL")
                   (nil    nil nil                        "NIL input returns NIL")))
      (destructuring-bind (input expected args desc) row
        (declare (ignore desc))
        (expect (eql expected (apply #'cl-tmux::%parse-integer-or-nil input args))))))

  ;;; ── resolve-target with pane specified as numeric index ─────────────────────

  ;; resolve-target resolves a pane by its 0-based numeric index.
  (it "resolve-target-pane-by-numeric-index"
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
          (expect (eq p2 rp))))))

  ;;; ── find-session-by-target: id higher than 9 ────────────────────────────────

  ;; find-session-by-target parses $N with multi-digit N correctly.
  (it "find-session-by-target-multi-digit-id"
    (let* ((sess (make-session :id 42 :name "big" :windows nil))
           (registry (list (cons "big" sess))))
      (expect (eq sess (cl-tmux::find-session-by-target registry "$42")))))

  ;;; ── find-window-by-target: @N with multi-digit id ────────────────────────────

  ;; find-window-by-target parses @N with multi-digit N correctly.
  (it "find-window-by-target-multi-digit-at-id"
    (let* ((w1   (make-window :id 99 :name "bigwin" :width 80 :height 24))
           (sess (make-session :id 1 :name "s" :windows (list w1))))
      (expect (eq w1 (cl-tmux::find-window-by-target sess "@99")))))

  ;;; ── find-pane-by-target: %N with multi-digit id ──────────────────────────────

  ;; find-pane-by-target parses %N with multi-digit N correctly.
  (it "find-pane-by-target-multi-digit-percent-id"
    (let* ((p1  (make-no-pty-pane 15 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p1))))
      (expect (eq p1 (cl-tmux::find-pane-by-target win "%15")))))

  ;;; ── resolve-target: empty string is same as NIL ─────────────────────────────

  ;; resolve-target with an empty string behaves identically to NIL target.
  (it "resolve-target-empty-string-uses-current-defaults"
    (multiple-value-bind (sess win p1) (make-single-pane-session)
      (multiple-value-bind (rs rw rp)
          (cl-tmux::resolve-target nil ""
                                   :current-session sess
                                   :current-window  win
                                   :current-pane    p1)
        (expect (eq sess rs))
        (expect (eq win  rw))
        (expect (eq p1   rp)))))

  ;;; ── resolve-target-context ───────────────────────────────────────────────────
  ;;;
  ;;; resolve-target-context is the public entry point dispatch-core.lisp uses for
  ;;; -t flag resolution; it derives current-session/window/pane defaults from
  ;;; SESSION rather than requiring callers to pass them explicitly.

  ;; resolve-target-context with a NIL target-string resolves to SESSION's own
  ;; active window and active pane, derived internally rather than passed in.
  (it "resolve-target-context-nil-target-defaults-to-session-active-objects"
    (multiple-value-bind (sess win pane) (make-single-pane-session)
      (multiple-value-bind (rs rw rp)
          (cl-tmux::resolve-target-context nil sess nil)
        (expect (eq sess rs))
        (expect (eq win  rw))
        (expect (eq pane rp)))))

  ;; resolve-target-context resolves a bare window-name target against SESSION,
  ;; without requiring the caller to pass current-window/current-pane explicitly.
  (it "resolve-target-context-resolves-window-within-session"
    (let* ((p1   (make-no-pty-pane 1 0 0 80 24))
           (w1   (make-window :id 1 :name "editor" :width 80 :height 24
                              :panes (list p1)))
           (w2   (make-window :id 2 :name "shell" :width 80 :height 24
                              :panes (list (make-no-pty-pane 2 0 0 80 24))))
           (sess (make-session :id 1 :name "work" :windows (list w1 w2))))
      (window-select-pane w1 p1)
      (session-select-window sess w1)
      (multiple-value-bind (rs rw _rp)
          (cl-tmux::resolve-target-context (list (cons "work" sess)) sess ":shell")
        (declare (ignore _rp))
        (expect (eq sess rs))
        (expect (eq w2   rw)))))

  ;; resolve-target-context still resolves relative to SESSION even when SERVER
  ;; does not contain SESSION under any registry key (e.g. a not-yet-registered
  ;; session), since the target-string does not name a session component.
  (it "resolve-target-context-falls-back-when-server-omits-session"
    (multiple-value-bind (sess win pane) (make-single-pane-session)
      (multiple-value-bind (rs rw rp)
          (cl-tmux::resolve-target-context nil sess "0")
        (expect (eq sess rs))
        (expect (eq win  rw))
        (expect (eq pane rp))))))
