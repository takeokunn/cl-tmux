(in-package #:cl-tmux/test)

;;;; path-modifier, substitute, nested-braces, pane_current_path, strftime, context keys, glob, regex — part II

(describe "format-suite"

  ;;; ── Path-modifier helpers (direct unit tests for edge cases) ──────────────────

  ;; %path-basename handles roots, trailing slashes, and bare names.
  (it "path-basename-edge-cases"
    (dolist (c '(("/a/b/c" "c") ("/a/b/" "b") ("foo" "foo") ("/" "/")))
      (destructuring-bind (input expected) c
        (expect (string= expected (cl-tmux/format::%path-basename input))))))

  ;; %path-dirname handles roots, trailing slashes, and bare names.
  (it "path-dirname-edge-cases"
    (dolist (c '(("/a/b/c" "/a/b") ("/foo" "/") ("foo" ".")))
      (destructuring-bind (input expected) c
        (expect (string= expected (cl-tmux/format::%path-dirname input))))))

  ;;; ── Substitute modifier: #{s/PAT/REP/[i]:var} ────────────────────────────────

  ;; #{s/PAT/REP/:var} replaces all matches; 'i' for case-insensitive; empty pattern is safe.
  (it "format-modifier-substitute-table"
    (dolist (c '(("#{s/foo/bar/:window_name}" :window-name "foofoo" "barbar" "replaces all occurrences")
                 ("#{s/o/0/:p}"               :p           "moon"   "m00n"   "replaces every occurrence")
                 ("#{s/xyz/Q/:p}"             :p           "abc"    "abc"    "no match → unchanged")
                 ("#{s/abc/x/i:p}"            :p           "abcABC" "xx"     "case-insensitive flag")
                 ("#{s/abc/x/:p}"             :p           "abcABC" "xABC"   "case-sensitive by default")
                 ("#{s///:p}"                 :p           "abc"    "abc"    "empty pattern → unchanged")))
      (destructuring-bind (spec key val expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec key val))))))

  ;; #{s/PAT/REP/:var} treats PAT as an extended regular expression (tmux regsub),
  ;; supporting character classes, anchors, quantifiers and \N backreferences.
  (it "format-modifier-substitute-regex"
    (dolist (c '(("#{s/[0-9]+/N/:p}"        :p "a12b345"   "aNbN"   "digit class + quantifier")
                 ("#{s/a.c/X/:p}"          :p "abc-aXc"   "X-X"    ". matches any char")
                 ("#{s/^foo/BAR/:p}"       :p "foofoo"    "BARfoo" "^ anchors to start only")
                 ("#{s/(a)(b)/\\2\\1/:p}"   :p "ab"        "ba"     "\\N backreferences in REP")
                 ("#{s/[A-Z]+/x/i:p}"      :p "abcABC"    "x"      "i flag folds case in the class")))
      (destructuring-bind (spec key val expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec key val))))))

  ;; %regex-replace-all returns the input unchanged on a malformed regex and on an
  ;; empty pattern (never signals, never inserts per-position).
  (it "format-regex-replace-all-malformed-unit"
    (expect (string= "a(b" (cl-tmux/format::%regex-replace-all "a(b" "(" "X" nil)))
    (expect (string= "abc" (cl-tmux/format::%regex-replace-all "abc" "" "Z" nil))))

  ;;; ── Nested #{...} (balanced braces) + comparison operators ───────────────────

  ;; %matching-close-brace returns the OUTER close, skipping nested #{...}.
  (it "format-matching-close-brace-balances-nesting"
    (flet ((mc (s) (cl-tmux/format::%matching-close-brace s 2)))  ; start past '#{'
      ;; "#{=5:#{w}}" → content is "=5:#{w}", outer } at index 9
      (expect (= 9 (mc "#{=5:#{w}}")))
      ;; no nesting: first } (index 4) for "#{abc}"
      (expect (= 5 (mc "#{abc}")))))

  ;; A modifier operand may itself be a nested #{...}, expanded before the modifier.
  (it "format-modifier-nested-operand"
    (expect (string= "veryl" (fmt "#{=5:#{window_name}}" :window-name "verylongname")))
    (expect (string= "project"
                     (fmt "#{b:#{pane_current_path}}" :pane-current-path "/home/u/project"))))

  ;; A bare (non-nested) modifier operand is still a variable lookup (unchanged).
  (it "format-modifier-bare-operand-still-lookup"
    (expect (string= "veryl" (fmt "#{=5:window_name}" :window-name "verylongname"))))

  ;; #{==:a,b} → 1 when equal else 0; #{!=:a,b} is its negation.
  (it "format-comparison-equal-and-not-equal"
    (dolist (c '(("#{==:foo,foo}" "1") ("#{==:foo,bar}" "0")
                 ("#{!=:foo,bar}" "1") ("#{!=:foo,foo}" "0")))
      (destructuring-bind (spec expected) c
        (expect (string= expected (fmt spec))))))

  ;; #{==:#{var},literal} expands the nested side before comparing.
  (it "format-comparison-expands-nested-sides"
    (expect (string= "1" (fmt "#{==:#{session_name},main}" :session-name "main")))
    (expect (string= "0" (fmt "#{==:#{session_name},main}" :session-name "other"))))

  ;; #{?#{==:#{x},y},A,B} — a comparison used as a conditional test (the if-shell -F
  ;; pattern), end-to-end.
  (it "format-comparison-drives-conditional"
    (expect (string= "A" (fmt "#{?#{==:#{session_name},main},A,B}" :session-name "main")))
    (expect (string= "B" (fmt "#{?#{==:#{session_name},main},A,B}" :session-name "nope"))))

  ;; Bare comparison expands nested sides and compares lexicographically (strcmp).
  (it "format-comparison-lexicographic-nested-and-nonnumeric"
    (expect (string= "1" (fmt "#{>:#{window_index},0}" :window-index "2")))
    (expect (string= "0" (fmt "#{<:foo,5}"))))

  ;; A bare comparison as a conditional test (single-digit operands, so the
  ;; lexicographic result coincides with the numeric one).
  (it "format-comparison-lexicographic-drives-conditional"
    (expect (string= "pos"
                     (fmt "#{?#{>:#{window_index},0},pos,nonpos}" :window-index "1")))
    (expect (string= "nonpos"
                     (fmt "#{?#{>:#{window_index},0},pos,nonpos}" :window-index "0"))))

  ;; Bare </>/<=/>= use strcmp (lexicographic), matching tmux's bare operators:
  ;; multi-digit and non-numeric pairs compare by character order, not value.
  (it "format-comparison-lexicographic-strcmp-semantics"
    (dolist (c '(("#{<:10,9}"        "1" "\"10\" sorts before \"9\"")
                 ("#{>:10,9}"        "0" "\"10\" does not sort after \"9\"")
                 ("#{<:apple,banana}" "1" "\"apple\" < \"banana\"")
                 ("#{>:apple,banana}" "0" "\"apple\" not > \"banana\"")
                 ("#{<=:abc,abc}"    "1" "equal strings satisfy <=")
                 ("#{>=:abc,abc}"    "1" "equal strings satisfy >=")
                 ("#{<:abc,abc}"     "0" "equal strings are not <")
                 ("#{>:abc,abc}"     "0" "equal strings are not >")))
      (destructuring-bind (spec expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec))))))

  ;; #{?#{var},t,f} expands the nested condition before testing truthiness.
  (it "format-conditional-nested-condition"
    (expect (string= "yes" (fmt "#{?#{window_active},yes,no}" :window-active "1")))
    (expect (string= "no"  (fmt "#{?#{window_active},yes,no}" :window-active "0"))))

  ;; #{?cond,#{var},f} expands the chosen branch (nested #{...} resolves).
  (it "format-conditional-nested-branch"
    (expect (string= "win"  (fmt "#{?1,#{window_name},none}" :window-name "win")))
    (expect (string= "none" (fmt "#{?0,#{window_name},none}" :window-name "win"))))

  ;; A #{?cond,YES,NO} with literal branches is unchanged.
  (it "format-conditional-literal-branch-still-works"
    (expect (string= "YES" (fmt "#{?window_active,YES,NO}" :window-active "1")))
    (expect (string= "NO"  (fmt "#{?window_active,YES,NO}" :window-active "0"))))

  ;;; ── #{pane_current_path} (from the OSC 7 cwd) ────────────────────────────────

  ;; format-context-from-session exposes #{pane_current_path} from the pane's screen
  ;; cwd, and #{b:pane_current_path} gives its basename.
  (it "format-context-pane-current-path-from-osc7"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win))))
      (setf (cl-tmux/terminal/types:screen-cwd (cl-tmux/model:pane-screen pane))
            "/home/user/project")
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (expect (string= "/home/user/project"
                         (cl-tmux/format:expand-format "#{pane_current_path}" ctx)))
        (expect (string= "project"
                         (cl-tmux/format:expand-format "#{b:pane_current_path}" ctx))))))

  ;; #{pane_current_path} is empty when no OSC 7 cwd has been reported (nil pane).
  (it "format-context-pane-current-path-defaults-empty"
    (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
      (expect (string= "" (cl-tmux/format:expand-format "#{pane_current_path}" ctx)))))

  ;;; ── New modifiers: #{t:timestamp-var}, #{pN:var}, #{U:var}, #{L:var}, #{n:var}, #{l:var} ──

  ;; #{t:...} is a timestamp-variable modifier only. Strftime syntax must not
  ;; route through the public format expander.
  (it "format-t-modifier-rejects-current-time-strftime"
    (dolist (spec '("#{t:%H:%M}" "#{t:%Y-%m-%d}" "#{t:}"))
      (expect (string= "" (fmt spec)))))

  ;; %strftime-format-at decodes a CL universal-time and formats it (round-trips
  ;; through the local timezone, so encode then format returns the same wall clock).
  (it "strftime-format-at-formats-given-timestamp"
    (let ((ts (encode-universal-time 5 30 14 15 6 2021)))   ; 2021-06-15 14:30:05 local
      (expect (string= "2021-06-15 14:30:05"
                       (cl-tmux/format::%strftime-format-at "%Y-%m-%d %H:%M:%S" ts)))))

  ;; %strftime-format-at returns the empty string for NIL / zero / non-positive.
  (it "strftime-format-at-empty-for-non-timestamp"
    (dolist (ts '(nil 0 -1))
      (expect (string= "" (cl-tmux/format::%strftime-format-at "%Y" ts)))))

  ;; %days-in-month returns the correct day count for fixed-length months and
  ;; handles the Feb leap-year boundary (divisible-by-4, century, and
  ;; divisible-by-400 rules).
  (it "days-in-month-table"
    (dolist (c '((1  2023 31 "January is always 31")
                 (3  2023 31 "March is always 31")
                 (5  2023 31 "May is always 31")
                 (7  2023 31 "July is always 31")
                 (8  2023 31 "August is always 31")
                 (10 2023 31 "October is always 31")
                 (12 2023 31 "December is always 31")
                 (4  2023 30 "April is always 30")
                 (6  2023 30 "June is always 30")
                 (9  2023 30 "September is always 30")
                 (11 2023 30 "November is always 30")
                 (2  2023 28 "2023 is not a leap year (not divisible by 4)")
                 (2  2024 29 "2024 is a leap year (divisible by 4, not 100)")
                 (2  1900 28 "1900 is divisible by 100 but not 400 → not a leap year")
                 (2  2000 29 "2000 is divisible by 400 → a leap year")))
      (destructuring-bind (month year expected desc) c
        (declare (ignore desc))
        (expect (= expected (cl-tmux/format::%days-in-month month year))))))

  ;; #{t:VAR} (bare variable, no %) formats VAR's value as a timestamp via the
  ;; default format - tmux semantics, e.g. #{t:session_last_attached}.
  (it "format-t-modifier-formats-timestamp-variable"
    (let* ((ts       (encode-universal-time 0 0 12 1 1 2020))
           (expected (cl-tmux/format::%strftime-format-at "" ts)))
      (expect (plusp (length expected)))
      (expect (string= expected (fmt "#{t:my_time}" :my-time (princ-to-string ts))))))

  ;; #{t:VAR} only formats positive integer timestamps. Missing, empty, or
  ;; non-timestamp operands expand to the empty string.
  (it "format-t-modifier-non-timestamp-is-empty"
    (expect (string= "" (fmt "#{t:window_name}" :window-name "bash")))
    (expect (string= "" (fmt "#{t:missing_var}")))
    (expect (string= "" (fmt "#{t:%Y}"))))

  ;; #{p5:var} pads right; #{p-5:var} pads left; at-width values are unchanged; longer pass through.
  (it "format-modifier-pad-table"
    (dolist (c '(("#{p5:v}"  :v "ab"      "ab   "   "right pad: 2 chars to 5")
                 ("#{p5:v}"  :v "hello"   "hello"   "right pad: at width — no change")
                 ("#{p5:v}"  :v "toolong" "toolong" "right pad: longer than width — pass through")
                 ("#{p-5:v}" :v "ab"      "   ab"   "left pad: 2 chars to 5")
                 ("#{p-5:v}" :v "hello"   "hello"   "left pad: at width — no change")))
      (destructuring-bind (spec key val expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec key val))))))

  ;; #{U:var} uppercases and #{L:var} lowercases the resolved value.
  (it "format-modifier-case-table"
    (dolist (c '(("#{U:v}"            :v            "hello" "HELLO" "uppercase literal")
                 ("#{U:window_name}"  :window-name  "bash"  "BASH"  "uppercase via variable")
                 ("#{L:v}"            :v            "HELLO" "hello" "lowercase literal")
                 ("#{L:session_name}" :session-name "MAIN"  "main"  "lowercase via variable")))
      (destructuring-bind (spec key val expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec key val))))))

  ;; #{n:var} returns the character length of the value as a string (tmux FORMAT_LENGTH;
  ;; #{l:...} is now the literal/unescape modifier).
  (it "format-modifier-length"
    (dolist (c '(("#{n:v}"            :v            "hello" "5" "hello is 5 chars")
                 ("#{n:v}"            :v            ""      "0" "empty string is 0")
                 ("#{n:session_name}" :session-name "abc"   "3" "resolves via var name")))
      (destructuring-bind (spec key val expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec key val))))))

  ;; %strftime-format internal helpers produce correct output.
  (it "format-modifier-strftime-unit-tests"

    ;; #{l:rest} emits REST literally, bypassing #{...} expansion (tmux FORMAT_LITERAL).
    (it "format-modifier-literal"
      ;; Plain text passes through unchanged.
      (expect (string= "hello" (fmt "#{l:hello}")))
      ;; A nested #{...} operand is NOT expanded under l.
      (expect (string= "#{pane_in_mode}" (fmt "#{l:#{pane_in_mode}}" :pane-in-mode "1")))
      ;; A bare variable name under l is also literal (not looked up).
      (expect (string= "session_name" (fmt "#{l:session_name}" :session-name "main"))))
    ;; Month abbreviations
    (expect (plusp (length (cl-tmux/format::%strftime-format "%b"))))
    ;; Hour is in 0-23 range
    (let ((h (parse-integer (cl-tmux/format::%strftime-format "%H") :junk-allowed t)))
      (expect (and h (>= h 0) (< h 24))))
    ;; %F is YYYY-MM-DD (10 chars)
    (expect (= 10 (length (cl-tmux/format::%strftime-format "%F"))))))
