(in-package #:cl-tmux/test)

;;;; format tests — part E: content-search #{C:term}, %glob-match-p,
;;;; %pane-visible-lines, %apply-pad-modifier, %lsof-extract-cwd, %window-raw-flags.

(defun %content-search-pane (&rest lines)
  "A no-PTY pane (width 20) whose visible screen holds LINES, one per row."
  (let* ((p   (make-no-pty-pane 1 0 0 20 (max 3 (length lines))))
         (scr (pane-screen p)))
    (apply #'feed-lines scr lines)
    p))

(describe "format-suite"

  ;;; ── Content search #{C:term} / #{C/r:} / #{C/i:} ─────────────────────────────
  ;;;
  ;;; tmux's #{C:term} searches the VISIBLE pane content line by line and yields the
  ;;; 1-based line number of the first match (or "0").  Default = glob (*term*);
  ;;; /r = regex; /i = case-insensitive.  These fixtures feed known text into a
  ;;; no-PTY pane's virtual screen and assert the returned line number.

  ;; #{C:term} returns the 1-based line number of the first line containing term.
  (it "format-content-search-glob-returns-line-number"
    (let* ((p   (%content-search-pane "hello world" "foo bar" "baz qux"))
           (ctx (cl-tmux/format:format-context-from-session nil nil p)))
      (dolist (c '(("#{C:hello}" "1" "term on the first line → 1")
                   ("#{C:bar}"   "2" "term on the second line → 2")
                   ("#{C:qux}"   "3" "term on the third line → 3")))
        (destructuring-bind (spec expected desc) c
          (declare (ignore desc))
          (expect (string= expected (cl-tmux/format:expand-format spec ctx)))))))

  ;; #{C:term} returns "0" when no visible line contains term.
  (it "format-content-search-no-match-returns-zero"
    (let* ((p   (%content-search-pane "hello world" "foo bar"))
           (ctx (cl-tmux/format:format-context-from-session nil nil p)))
      (expect (string= "0" (cl-tmux/format:expand-format "#{C:nomatch}" ctx)))))

  ;; #{C:term} with no pane in context returns "0" (never errors).
  (it "format-content-search-no-pane-returns-zero"
    (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
      (expect (string= "0" (cl-tmux/format:expand-format "#{C:anything}" ctx)))))

  ;; #{C:h?llo} glob (?) matches 'hello'; the term is wrapped *term* so it is a
  ;; contains-with-globbing search, matching tmux's window_pane_search.
  (it "format-content-search-glob-metachars"
    (let* ((p   (%content-search-pane "say hello there"))
           (ctx (cl-tmux/format:format-context-from-session nil nil p)))
      (expect (string= "1" (cl-tmux/format:expand-format "#{C:h?llo}" ctx)))))

  ;; #{C/r:term} treats term as a regex (anchors honoured: ^ matches line start).
  (it "format-content-search-regex"
    (let* ((p   (%content-search-pane "alpha line" "foo bar" "baz"))
           (ctx (cl-tmux/format:format-context-from-session nil nil p)))
      (dolist (c '(("#{C/r:b.r}"  "2" "b.r regex matches 'bar' on line 2")
                   ("#{C/r:^foo}" "2" "^foo anchors to the start of line 2")
                   ("#{C/r:^bar}" "0" "^bar matches no line start → 0 (regex, not substring)")))
        (destructuring-bind (spec expected desc) c
          (declare (ignore desc))
          (expect (string= expected (cl-tmux/format:expand-format spec ctx)))))))

  ;; #{C/i:term} folds case; bare #{C:term} is case-sensitive.
  (it "format-content-search-case-insensitive"
    (let* ((p   (%content-search-pane "Hello World"))
           (ctx (cl-tmux/format:format-context-from-session nil nil p)))
      (dolist (c '(("#{C:hello}"   "0" "case-sensitive glob does not match 'Hello'")
                   ("#{C/i:hello}" "1" "case-insensitive glob matches 'Hello'")
                   ("#{C/ri:HELLO}" "1" "case-insensitive regex matches 'Hello'")))
        (destructuring-bind (spec expected desc) c
          (declare (ignore desc))
          (expect (string= expected (cl-tmux/format:expand-format spec ctx)))))))

  ;; #{C:#{var}} expands the term as a format string before searching.
  (it "format-content-search-term-is-expanded"
    (let* ((p   (%content-search-pane "alpha" "beta"))
           (ctx (list :%c-search-pane p :target "beta")))
      (expect (string= "2" (cl-tmux/format:expand-format "#{C:#{target}}" ctx)))))

  ;;; ── %glob-match-p direct unit tests ─────────────────────────────────────────
  ;;;
  ;;; These exercise %glob-match-p in isolation, covering:
  ;;;   - exact match, prefix/suffix stars, interior stars, ? wildcard
  ;;;   - consecutive *s (the optimisation in the skip-loop)
  ;;;   - trailing ** / pattern longer than string
  ;;;   - empty pattern vs empty string

  ;; %glob-match-p handles exact matches, prefix/suffix/infix *, and ? wildcard.
  (it "glob-match-p-table"
    (dolist (c '(("hello" "hello" t   "exact match")
                 ("hello" "Hello" nil "exact: case-sensitive mismatch")
                 ("hello" "hell"  nil "exact: length mismatch")
                 ("*sh"   "bash"  t   "prefix-star matches bash")
                 ("*sh"   "sh"    t   "prefix-star matches sh alone")
                 ("*sh"   "bash-old" nil "prefix-star: trailing mismatch")
                 ("ba*"   "bash"  t   "suffix-star matches bash")
                 ("ba*"   "ba"    t   "suffix-star: empty suffix")
                 ("ba*"   "zsh"   nil "suffix-star: mismatch")
                 ("a*b"   "ab"    t   "infix-star: empty middle")
                 ("a*b"   "axyzb" t   "infix-star: non-empty middle")
                 ("a*b"   "axyzc" nil "infix-star: mismatch")
                 ("b?sh"  "bash"  t   "? matches one char")
                 ("b?sh"  "bzsh"  t   "? matches alt char")
                 ("b?sh"  "bsh"   nil "? requires exactly one char")
                 ("b?sh"  "baxsh" nil "? not greedy")))
      (destructuring-bind (pattern string expected desc) c
        (declare (ignore desc))
        (expect (eq expected (cl-tmux/format::%glob-match-p pattern string))))))

  ;; Consecutive ** are collapsed — **a matches any string ending in a.
  (it "glob-match-consecutive-stars-optimisation"
    (expect (cl-tmux/format::%glob-match-p "**a" "ba") :to-be-truthy)
    (expect (cl-tmux/format::%glob-match-p "**a" "a") :to-be-truthy)
    (expect (cl-tmux/format::%glob-match-p "**a" "ab") :to-be-falsy))

  ;; A pattern that ends with * (including **) matches any remaining suffix.
  (it "glob-match-trailing-stars"
    (expect (cl-tmux/format::%glob-match-p "*"  "anything") :to-be-truthy)
    (expect (cl-tmux/format::%glob-match-p "*"  "") :to-be-truthy)
    (expect (cl-tmux/format::%glob-match-p "**" "anything") :to-be-truthy)
    (expect (cl-tmux/format::%glob-match-p "a*" "a") :to-be-truthy))

  ;; A non-wildcard pattern longer than the string never matches.
  (it "glob-match-pattern-longer-than-string"
    (expect (cl-tmux/format::%glob-match-p "abcdef" "abc") :to-be-falsy)
    (expect (cl-tmux/format::%glob-match-p "???" "ab") :to-be-falsy))

  ;; Empty pattern matches only the empty string.
  (it "glob-match-empty-pattern-vs-empty-string"
    (expect (cl-tmux/format::%glob-match-p "" "") :to-be-truthy)
    (expect (cl-tmux/format::%glob-match-p "" "a") :to-be-falsy))

  ;;; ── %pane-visible-lines direct unit tests ────────────────────────────────────

  ;; %pane-visible-lines returns one string per visible row with trailing spaces trimmed.
  (it "pane-visible-lines-returns-content-rows"
    (let* ((pane (make-no-pty-pane 1 0 0 10 3))
           (scr  (pane-screen pane)))
      (feed scr "hi")
      (let ((lines (cl-tmux/format::%pane-visible-lines pane)))
        (expect (= 3 (length lines)))
        (expect (string= "hi" (first lines)))
        (expect (string= "" (second lines))))))

  ;; %pane-visible-lines with NIL pane returns NIL without error.
  (it "pane-visible-lines-nil-pane-returns-nil"
    (expect (null (cl-tmux/format::%pane-visible-lines nil))))

  ;; %pane-visible-lines with a pane whose screen is NIL returns NIL.
  (it "pane-visible-lines-pane-no-screen-returns-nil"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen nil)))
      (expect (null (cl-tmux/format::%pane-visible-lines pane)))))

  ;;; ── %apply-pad-modifier direct unit tests ────────────────────────────────────

  ;; %apply-pad-modifier with positive N pads VALUE on the right with spaces.
  (it "apply-pad-modifier-right-pad-extends-short-value"
    (let ((result (cl-tmux/format::%apply-pad-modifier "p8" "abc")))
      (expect (= 8 (length result)))
      (expect (string= "abc" (subseq result 0 3)))
      (expect (every (lambda (c) (char= c #\Space)) (subseq result 3)))))

  ;; %apply-pad-modifier with negative N pads VALUE on the left with spaces.
  (it "apply-pad-modifier-left-pad-extends-short-value"
    (let ((result (cl-tmux/format::%apply-pad-modifier "p-8" "abc")))
      (expect (= 8 (length result)))
      (expect (string= "abc" (subseq result 5)))
      (expect (every (lambda (c) (char= c #\Space)) (subseq result 0 5)))))

  ;; %apply-pad-modifier returns the value unchanged when it already meets the field width.
  (it "apply-pad-modifier-value-already-at-width-unchanged"
    (expect (string= "hello" (cl-tmux/format::%apply-pad-modifier "p5" "hello")))
    (expect (string= "hello" (cl-tmux/format::%apply-pad-modifier "p-5" "hello"))))

  ;; %apply-pad-modifier does not truncate values wider than the requested field.
  (it "apply-pad-modifier-value-wider-than-field-returned-as-is"
    (expect (string= "hello" (cl-tmux/format::%apply-pad-modifier "p3" "hello")))
    (expect (string= "hello" (cl-tmux/format::%apply-pad-modifier "p-3" "hello"))))

  ;; %apply-pad-modifier returns NIL when MOD does not start with 'p'.
  (it "apply-pad-modifier-not-a-pad-modifier-returns-nil"
    (expect (null (cl-tmux/format::%apply-pad-modifier "b" "hello")))
    (expect (null (cl-tmux/format::%apply-pad-modifier "=5" "hello"))))

  ;;; ── %lsof-extract-cwd direct unit tests ─────────────────────────────────────

  ;; %lsof-extract-cwd returns the PATH part of the first 'nPATH' line.
  (it "lsof-extract-cwd-finds-n-line"
    (let ((output (format nil "p1234~%f~%n/home/user/project~%")))
      (expect (string= "/home/user/project"
                       (cl-tmux/format::%lsof-extract-cwd output)))))

  ;; %lsof-extract-cwd ignores lines that do not start with 'n'.
  (it "lsof-extract-cwd-skips-non-n-lines"
    (let ((output (format nil "p999~%fcwd~%n/tmp~%")))
      (expect (string= "/tmp" (cl-tmux/format::%lsof-extract-cwd output)))))

  ;; %lsof-extract-cwd returns NIL when no 'n' line is present.
  (it "lsof-extract-cwd-empty-output-returns-nil"
    (expect (null (cl-tmux/format::%lsof-extract-cwd "")))
    (expect (null (cl-tmux/format::%lsof-extract-cwd (format nil "p123~%f~%")))))

  ;;; ── %window-raw-flags direct unit tests ──────────────────────────────────────

  ;; %window-raw-flags returns "*" when window is the session-active-window.
  (it "window-raw-flags-active-sets-star"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess)))
      (expect (string= "*" (cl-tmux/format::%window-raw-flags win win sess)))))

  ;; %window-raw-flags returns "" for an inactive window that has never been active.
  (it "window-raw-flags-inactive-no-history-is-empty"
    (let* ((sess (make-fake-session :nwindows 2))
           (wins (cl-tmux/model:session-windows sess))
           (inactive (second wins)))
      ;; make-fake-session selects the first window; the second was never active.
      (setf (cl-tmux/model:window-last-active-time inactive) 0)
      (expect (string= "" (cl-tmux/format::%window-raw-flags
                           inactive (cl-tmux/model:session-active-window sess) sess)))))

  ;; %window-raw-flags returns "*Z" for a zoomed active window.
  (it "window-raw-flags-zoomed-active-has-star-and-z"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess)))
      (setf (cl-tmux/model:window-zoom-p win) t)
      (let ((flags (cl-tmux/format::%window-raw-flags win win sess)))
        (expect (search "*" flags))
        (expect (search "Z" flags)))
      (setf (cl-tmux/model:window-zoom-p win) nil)))

  ;; %window-raw-flags with a NIL window returns "".
  (it "window-raw-flags-nil-window-is-empty"
    (expect (string= "" (cl-tmux/format::%window-raw-flags nil nil nil)))))
