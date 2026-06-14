(in-package #:cl-tmux/test)

;;;; format tests — part E: content-search #{C:term}, %glob-match-p,
;;;; %pane-visible-lines, %apply-pad-modifier, %lsof-extract-cwd, %window-raw-flags.

(in-suite format-suite)

;;; ── Content search #{C:term} / #{C/r:} / #{C/i:} ─────────────────────────────
;;;
;;; tmux's #{C:term} searches the VISIBLE pane content line by line and yields the
;;; 1-based line number of the first match (or "0").  Default = glob (*term*);
;;; /r = regex; /i = case-insensitive.  These fixtures feed known text into a
;;; no-PTY pane's virtual screen and assert the returned line number.

(defun %content-search-pane (&rest lines)
  "A no-PTY pane (width 20) whose visible screen holds LINES, one per row."
  (let* ((p   (make-no-pty-pane 1 0 0 20 (max 3 (length lines))))
         (scr (pane-screen p)))
    (apply #'feed-lines scr lines)
    p))

(test format-content-search-glob-returns-line-number
  "#{C:term} returns the 1-based line number of the first line containing term."
  (let* ((p   (%content-search-pane "hello world" "foo bar" "baz qux"))
         (ctx (cl-tmux/format:format-context-from-session nil nil p)))
    (dolist (c '(("#{C:hello}" "1" "term on the first line → 1")
                 ("#{C:bar}"   "2" "term on the second line → 2")
                 ("#{C:qux}"   "3" "term on the third line → 3")))
      (destructuring-bind (spec expected desc) c
        (is (string= expected (cl-tmux/format:expand-format spec ctx)) "~A" desc)))))

(test format-content-search-no-match-returns-zero
  "#{C:term} returns \"0\" when no visible line contains term."
  (let* ((p   (%content-search-pane "hello world" "foo bar"))
         (ctx (cl-tmux/format:format-context-from-session nil nil p)))
    (is (string= "0" (cl-tmux/format:expand-format "#{C:nomatch}" ctx)))))

(test format-content-search-no-pane-returns-zero
  "#{C:term} with no pane in context returns \"0\" (never errors)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "0" (cl-tmux/format:expand-format "#{C:anything}" ctx)))))

(test format-content-search-glob-metachars
  "#{C:h?llo} glob (?) matches 'hello'; the term is wrapped *term* so it is a
   contains-with-globbing search, matching tmux's window_pane_search."
  (let* ((p   (%content-search-pane "say hello there"))
         (ctx (cl-tmux/format:format-context-from-session nil nil p)))
    (is (string= "1" (cl-tmux/format:expand-format "#{C:h?llo}" ctx))
        "? matches one char inside an embedded word")))

(test format-content-search-regex
  "#{C/r:term} treats term as a regex (anchors honoured: ^ matches line start)."
  (let* ((p   (%content-search-pane "alpha line" "foo bar" "baz"))
         (ctx (cl-tmux/format:format-context-from-session nil nil p)))
    (dolist (c '(("#{C/r:b.r}"  "2" "b.r regex matches 'bar' on line 2")
                 ("#{C/r:^foo}" "2" "^foo anchors to the start of line 2")
                 ("#{C/r:^bar}" "0" "^bar matches no line start → 0 (regex, not substring)")))
      (destructuring-bind (spec expected desc) c
        (is (string= expected (cl-tmux/format:expand-format spec ctx)) "~A" desc)))))

(test format-content-search-case-insensitive
  "#{C/i:term} folds case; bare #{C:term} is case-sensitive."
  (let* ((p   (%content-search-pane "Hello World"))
         (ctx (cl-tmux/format:format-context-from-session nil nil p)))
    (dolist (c '(("#{C:hello}"   "0" "case-sensitive glob does not match 'Hello'")
                 ("#{C/i:hello}" "1" "case-insensitive glob matches 'Hello'")
                 ("#{C/ri:HELLO}" "1" "case-insensitive regex matches 'Hello'")))
      (destructuring-bind (spec expected desc) c
        (is (string= expected (cl-tmux/format:expand-format spec ctx)) "~A" desc)))))

(test format-content-search-term-is-expanded
  "#{C:#{var}} expands the term as a format string before searching."
  (let* ((p   (%content-search-pane "alpha" "beta"))
         (ctx (list :%c-search-pane p :target "beta")))
    (is (string= "2" (cl-tmux/format:expand-format "#{C:#{target}}" ctx))
        "the nested #{target} resolves to 'beta' → line 2")))

;;; ── %glob-match-p direct unit tests ─────────────────────────────────────────
;;;
;;; These exercise %glob-match-p in isolation, covering:
;;;   - exact match, prefix/suffix stars, interior stars, ? wildcard
;;;   - consecutive *s (the optimisation in the skip-loop)
;;;   - trailing ** / pattern longer than string
;;;   - empty pattern vs empty string

(test glob-match-p-table
  "%glob-match-p handles exact matches, prefix/suffix/infix *, and ? wildcard."
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
      (is (eq expected (cl-tmux/format::%glob-match-p pattern string)) "~A" desc))))

(test glob-match-consecutive-stars-optimisation
  "Consecutive ** are collapsed — **a matches any string ending in a."
  (is-true  (cl-tmux/format::%glob-match-p "**a" "ba"))
  (is-true  (cl-tmux/format::%glob-match-p "**a" "a"))
  (is-false (cl-tmux/format::%glob-match-p "**a" "ab")))

(test glob-match-trailing-stars
  "A pattern that ends with * (including **) matches any remaining suffix."
  (is-true  (cl-tmux/format::%glob-match-p "*"  "anything"))
  (is-true  (cl-tmux/format::%glob-match-p "*"  ""))
  (is-true  (cl-tmux/format::%glob-match-p "**" "anything"))
  (is-true  (cl-tmux/format::%glob-match-p "a*" "a")))

(test glob-match-pattern-longer-than-string
  "A non-wildcard pattern longer than the string never matches."
  (is-false (cl-tmux/format::%glob-match-p "abcdef" "abc"))
  (is-false (cl-tmux/format::%glob-match-p "???" "ab")))

(test glob-match-empty-pattern-vs-empty-string
  "Empty pattern matches only the empty string."
  (is-true  (cl-tmux/format::%glob-match-p "" ""))
  (is-false (cl-tmux/format::%glob-match-p "" "a")))

;;; ── %pane-visible-lines direct unit tests ────────────────────────────────────

(test pane-visible-lines-returns-content-rows
  "%pane-visible-lines returns one string per visible row with trailing spaces trimmed."
  (let* ((pane (make-no-pty-pane 1 0 0 10 3))
         (scr  (pane-screen pane)))
    (feed scr "hi")
    (let ((lines (cl-tmux/format::%pane-visible-lines pane)))
      (is (= 3 (length lines)) "must return exactly height rows")
      (is (string= "hi" (first lines))
          "first row must be 'hi' (trailing spaces trimmed)")
      (is (string= "" (second lines)) "empty rows must be empty strings"))))

(test pane-visible-lines-nil-pane-returns-nil
  "%pane-visible-lines with NIL pane returns NIL without error."
  (is (null (cl-tmux/format::%pane-visible-lines nil))))

(test pane-visible-lines-pane-no-screen-returns-nil
  "%pane-visible-lines with a pane whose screen is NIL returns NIL."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen nil)))
    (is (null (cl-tmux/format::%pane-visible-lines pane)))))

;;; ── %apply-pad-modifier direct unit tests ────────────────────────────────────

(test apply-pad-modifier-right-pad-extends-short-value
  "%apply-pad-modifier with positive N pads VALUE on the right with spaces."
  (let ((result (cl-tmux/format::%apply-pad-modifier "p8" "abc")))
    (is (= 8 (length result))
        "right-padded result must be 8 chars wide (got ~D: ~S)" (length result) result)
    (is (string= "abc" (subseq result 0 3))
        "value must be at the start in right-padded mode")
    (is (every (lambda (c) (char= c #\Space)) (subseq result 3))
        "padding must be spaces")))

(test apply-pad-modifier-left-pad-extends-short-value
  "%apply-pad-modifier with negative N pads VALUE on the left with spaces."
  (let ((result (cl-tmux/format::%apply-pad-modifier "p-8" "abc")))
    (is (= 8 (length result))
        "left-padded result must be 8 chars wide (got ~D: ~S)" (length result) result)
    (is (string= "abc" (subseq result 5))
        "value must be at the end in left-padded mode")
    (is (every (lambda (c) (char= c #\Space)) (subseq result 0 5))
        "padding must be spaces")))

(test apply-pad-modifier-value-already-at-width-unchanged
  "%apply-pad-modifier returns the value unchanged when it already meets the field width."
  (is (string= "hello" (cl-tmux/format::%apply-pad-modifier "p5" "hello"))
      "value exactly at width must be returned unchanged")
  (is (string= "hello" (cl-tmux/format::%apply-pad-modifier "p-5" "hello"))
      "value exactly at width (left pad) must be returned unchanged"))

(test apply-pad-modifier-value-wider-than-field-returned-as-is
  "%apply-pad-modifier does not truncate values wider than the requested field."
  (is (string= "hello" (cl-tmux/format::%apply-pad-modifier "p3" "hello"))
      "value wider than right-pad field must be returned as-is")
  (is (string= "hello" (cl-tmux/format::%apply-pad-modifier "p-3" "hello"))
      "value wider than left-pad field must be returned as-is"))

(test apply-pad-modifier-not-a-pad-modifier-returns-nil
  "%apply-pad-modifier returns NIL when MOD does not start with 'p'."
  (is (null (cl-tmux/format::%apply-pad-modifier "b" "hello"))
      "non-pad modifier must return NIL")
  (is (null (cl-tmux/format::%apply-pad-modifier "=5" "hello"))
      "truncate modifier must return NIL from apply-pad-modifier"))

;;; ── %lsof-extract-cwd direct unit tests ─────────────────────────────────────

(test lsof-extract-cwd-finds-n-line
  "%lsof-extract-cwd returns the PATH part of the first 'nPATH' line."
  (let ((output (format nil "p1234~%f~%n/home/user/project~%")))
    (is (string= "/home/user/project"
                 (cl-tmux/format::%lsof-extract-cwd output))
        "must extract /home/user/project from lsof -Fn output")))

(test lsof-extract-cwd-skips-non-n-lines
  "%lsof-extract-cwd ignores lines that do not start with 'n'."
  (let ((output (format nil "p999~%fcwd~%n/tmp~%")))
    (is (string= "/tmp" (cl-tmux/format::%lsof-extract-cwd output)))))

(test lsof-extract-cwd-empty-output-returns-nil
  "%lsof-extract-cwd returns NIL when no 'n' line is present."
  (is (null (cl-tmux/format::%lsof-extract-cwd "")))
  (is (null (cl-tmux/format::%lsof-extract-cwd (format nil "p123~%f~%")))))

;;; ── %window-raw-flags direct unit tests ──────────────────────────────────────

(test window-raw-flags-active-sets-star
  "%window-raw-flags returns \"*\" when window is the session-active-window."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (cl-tmux/model:session-active-window sess)))
    (is (string= "*" (cl-tmux/format::%window-raw-flags win win sess))
        "active window must get * flag")))

(test window-raw-flags-inactive-no-history-is-empty
  "%window-raw-flags returns \"\" for an inactive window that has never been active."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (inactive (second wins)))
    ;; make-fake-session selects the first window; the second was never active.
    (setf (cl-tmux/model:window-last-active-time inactive) 0)
    (is (string= "" (cl-tmux/format::%window-raw-flags
                     inactive (cl-tmux/model:session-active-window sess) sess))
        "never-active inactive window must have empty raw flags")))

(test window-raw-flags-zoomed-active-has-star-and-z
  "%window-raw-flags returns \"*Z\" for a zoomed active window."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (cl-tmux/model:session-active-window sess)))
    (setf (cl-tmux/model:window-zoom-p win) t)
    (let ((flags (cl-tmux/format::%window-raw-flags win win sess)))
      (is (search "*" flags) "active zoomed window must have * (got ~S)" flags)
      (is (search "Z" flags) "active zoomed window must have Z (got ~S)" flags))
    (setf (cl-tmux/model:window-zoom-p win) nil)))

(test window-raw-flags-nil-window-is-empty
  "%window-raw-flags with a NIL window returns \"\"."
  (is (string= "" (cl-tmux/format::%window-raw-flags nil nil nil))))
