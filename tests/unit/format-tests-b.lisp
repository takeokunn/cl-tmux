(in-package #:cl-tmux/test)

;;;; path-modifier, substitute, nested-braces, pane_current_path, strftime, context keys, glob, regex — part II

(in-suite format-suite)

;;; ── Path-modifier helpers (direct unit tests for edge cases) ──────────────────

(test path-basename-edge-cases
  "%path-basename handles roots, trailing slashes, and bare names."
  (dolist (c '(("/a/b/c" "c") ("/a/b/" "b") ("foo" "foo") ("/" "/")))
    (destructuring-bind (input expected) c
      (is (string= expected (cl-tmux/format::%path-basename input))
          "%path-basename ~S → ~S" input expected))))

(test path-dirname-edge-cases
  "%path-dirname handles roots, trailing slashes, and bare names."
  (dolist (c '(("/a/b/c" "/a/b") ("/foo" "/") ("foo" ".")))
    (destructuring-bind (input expected) c
      (is (string= expected (cl-tmux/format::%path-dirname input))
          "%path-dirname ~S → ~S" input expected))))

;;; ── Substitute modifier: #{s/PAT/REP/[i]:var} ────────────────────────────────

(test format-modifier-substitute-table
  "#{s/PAT/REP/:var} replaces all matches; 'i' for case-insensitive; empty pattern is safe."
  (dolist (c '(("#{s/foo/bar/:window_name}" :window-name "foofoo" "barbar" "replaces all occurrences")
               ("#{s/o/0/:p}"               :p           "moon"   "m00n"   "replaces every occurrence")
               ("#{s/xyz/Q/:p}"             :p           "abc"    "abc"    "no match → unchanged")
               ("#{s/abc/x/i:p}"            :p           "abcABC" "xx"     "case-insensitive flag")
               ("#{s/abc/x/:p}"             :p           "abcABC" "xABC"   "case-sensitive by default")
               ("#{s///:p}"                 :p           "abc"    "abc"    "empty pattern → unchanged")))
    (destructuring-bind (spec key val expected desc) c
      (is (string= expected (fmt spec key val))
          "~A" desc))))

(test string-replace-all-unit
  "%string-replace-all replaces all occurrences; case-insensitive on request;
   empty pattern returns the input unchanged."
  (dolist (c '(("axbxc" "x" "-" nil "a-b-c" "basic replacement")
               ("aAa"   "a" "X" t   "XXX"   "case-insensitive replaces all three a/A")
               ("aAa"   "a" "X" nil "XAX"   "case-sensitive replaces only the two lowercase a")
               ("abc"   ""  "Z" nil "abc"    "empty pattern returns the input unchanged")))
    (destructuring-bind (str pat rep ic expected desc) c
      (is (string= expected (cl-tmux/format::%string-replace-all str pat rep ic)) "~A" desc))))

;;; ── Nested #{...} (balanced braces) + comparison operators ───────────────────

(test format-matching-close-brace-balances-nesting
  "%matching-close-brace returns the OUTER close, skipping nested #{...}."
  (flet ((mc (s) (cl-tmux/format::%matching-close-brace s 2)))  ; start past '#{'
    ;; "#{=5:#{w}}" → content is "=5:#{w}", outer } at index 9
    (is (= 9 (mc "#{=5:#{w}}")))
    ;; no nesting: first } (index 4) for "#{abc}"
    (is (= 5 (mc "#{abc}")))))

(test format-modifier-nested-operand
  "A modifier operand may itself be a nested #{...}, expanded before the modifier."
  (is (string= "veryl" (fmt "#{=5:#{window_name}}" :window-name "verylongname"))
      "truncate the expansion of a nested #{window_name}")
  (is (string= "project"
               (fmt "#{b:#{pane_current_path}}" :pane-current-path "/home/u/project"))
      "basename of a nested path expansion"))

(test format-modifier-bare-operand-still-lookup
  "A bare (non-nested) modifier operand is still a variable lookup (unchanged)."
  (is (string= "veryl" (fmt "#{=5:window_name}" :window-name "verylongname"))))

(test format-comparison-equal-and-not-equal
  "#{==:a,b} → 1 when equal else 0; #{!=:a,b} is its negation."
  (dolist (c '(("#{==:foo,foo}" "1") ("#{==:foo,bar}" "0")
               ("#{!=:foo,bar}" "1") ("#{!=:foo,foo}" "0")))
    (destructuring-bind (spec expected) c
      (is (string= expected (fmt spec)) "~S → ~S" spec expected))))

(test format-comparison-expands-nested-sides
  "#{==:#{var},literal} expands the nested side before comparing."
  (is (string= "1" (fmt "#{==:#{session_name},main}" :session-name "main")))
  (is (string= "0" (fmt "#{==:#{session_name},main}" :session-name "other"))))

(test format-comparison-drives-conditional
  "#{?#{==:#{x},y},A,B} — a comparison used as a conditional test (the if-shell -F
   pattern), end-to-end."
  (is (string= "A" (fmt "#{?#{==:#{session_name},main},A,B}" :session-name "main")))
  (is (string= "B" (fmt "#{?#{==:#{session_name},main},A,B}" :session-name "nope"))))

(test format-comparison-numeric-operators
  "#{<:a,b} #{>:a,b} #{<=:a,b} #{>=:a,b} compare the sides numerically."
  (dolist (c '(("#{<:5,10}"  "1" "5 < 10")
               ("#{<:10,5}"  "0" "not 10 < 5")
               ("#{>:10,5}"  "1" "10 > 5")
               ("#{>:5,10}"  "0" "not 5 > 10")
               ("#{<=:5,5}"  "1" "5 <= 5")
               ("#{<=:6,5}"  "0" "not 6 <= 5")
               ("#{>=:5,5}"  "1" "5 >= 5")
               ("#{>=:4,5}"  "0" "not 4 >= 5")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec)) "~A" desc))))

(test format-comparison-numeric-nested-and-nonnumeric
  "Numeric comparison expands nested sides; a non-numeric side parses as 0."
  (is (string= "1" (fmt "#{>:#{window_index},0}" :window-index "2"))
      "#{window_index}=2 > 0")
  (is (string= "1" (fmt "#{<:foo,5}"))
      "a non-numeric side parses as 0, so 0 < 5"))

(test format-comparison-numeric-drives-conditional
  "A numeric comparison as a conditional test (e.g. wide-vs-narrow on width)."
  (is (string= "pos"
               (fmt "#{?#{>:#{window_index},0},pos,nonpos}" :window-index "1")))
  (is (string= "nonpos"
               (fmt "#{?#{>:#{window_index},0},pos,nonpos}" :window-index "0"))))

(test format-conditional-nested-condition
  "#{?#{var},t,f} expands the nested condition before testing truthiness."
  (is (string= "yes" (fmt "#{?#{window_active},yes,no}" :window-active "1")))
  (is (string= "no"  (fmt "#{?#{window_active},yes,no}" :window-active "0"))))

(test format-conditional-nested-branch
  "#{?cond,#{var},f} expands the chosen branch (nested #{...} resolves)."
  (is (string= "win"  (fmt "#{?1,#{window_name},none}" :window-name "win")))
  (is (string= "none" (fmt "#{?0,#{window_name},none}" :window-name "win"))))

(test format-conditional-literal-branch-still-works
  "A #{?cond,YES,NO} with literal branches is unchanged."
  (is (string= "YES" (fmt "#{?window_active,YES,NO}" :window-active "1")))
  (is (string= "NO"  (fmt "#{?window_active,YES,NO}" :window-active "0"))))

;;; ── #{pane_current_path} (from the OSC 7 cwd) ────────────────────────────────

(test format-context-pane-current-path-from-osc7
  "format-context-from-session exposes #{pane_current_path} from the pane's screen
   cwd, and #{b:pane_current_path} gives its basename."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win))))
    (setf (cl-tmux/terminal/types:screen-cwd (cl-tmux/model:pane-screen pane))
          "/home/user/project")
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "/home/user/project"
                   (cl-tmux/format:expand-format "#{pane_current_path}" ctx))
          "pane_current_path must be the screen cwd")
      (is (string= "project"
                   (cl-tmux/format:expand-format "#{b:pane_current_path}" ctx))
          "#{b:pane_current_path} must be the basename"))))

(test format-context-pane-current-path-defaults-empty
  "#{pane_current_path} is empty when no OSC 7 cwd has been reported (nil pane)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "" (cl-tmux/format:expand-format "#{pane_current_path}" ctx)))))

;;; ── New modifiers: #{t:strftime}, #{pN:var}, #{U:var}, #{L:var}, #{l:var} ──

(test format-modifier-strftime-table
  "#{t:FORMAT} expands to the current time using the given strftime format."
  (dolist (row '(("#{t:%H:%M}"     5  #\: 2 "HH:MM - 5 chars, colon at pos 2")
                 ("#{t:%Y-%m-%d}"  10 #\- 4 "YYYY-MM-DD - 10 chars, dash at pos 4")))
    (destructuring-bind (fmt-str expected-len sep-char sep-pos desc) row
      (let ((result (fmt fmt-str)))
        (is (= expected-len (length result))          "~A: length"    desc)
        (is (char= sep-char (char result sep-pos))    "~A: separator" desc)))))

(test format-modifier-strftime-default-empty-format
  "#{t:} with empty format string uses the default strftime format."
  (let ((result (fmt "#{t:}")))
    ;; Default format is "%a %b %e %H:%M:%S %Z %Y" — reasonably long
    (is (plusp (length result))
        "#{t:} default format must produce a non-empty string, got ~S" result)))

(test strftime-format-at-formats-given-timestamp
  "%strftime-format-at decodes a CL universal-time and formats it (round-trips
   through the local timezone, so encode then format returns the same wall clock)."
  (let ((ts (encode-universal-time 5 30 14 15 6 2021)))   ; 2021-06-15 14:30:05 local
    (is (string= "2021-06-15 14:30:05"
                 (cl-tmux/format::%strftime-format-at "%Y-%m-%d %H:%M:%S" ts)))))

(test strftime-format-at-empty-for-non-timestamp
  "%strftime-format-at returns the empty string for NIL / zero / non-positive."
  (dolist (ts '(nil 0 -1))
    (is (string= "" (cl-tmux/format::%strftime-format-at "%Y" ts))
        "must return empty for ~S" ts)))

(test format-t-modifier-formats-timestamp-variable
  "#{t:VAR} (bare variable, no %) formats VAR's value as a timestamp via the
   default format — tmux semantics, e.g. #{t:session_last_attached}."
  (let* ((ts       (encode-universal-time 0 0 12 1 1 2020))
         (expected (cl-tmux/format::%strftime-format-at "" ts)))
    (is (plusp (length expected)) "sanity: default format produces output")
    (is (string= expected (fmt "#{t:my_time}" :my-time (princ-to-string ts)))
        "#{t:my_time} formats the timestamp held by the variable")))

(test format-t-modifier-legacy-percent-uses-current-time
  "#{t:%Y} (operand contains %) still formats the CURRENT time, not a variable."
  (let ((r (fmt "#{t:%Y}")))
    (is (= 4 (length r)) "current year is 4 digits, got ~S" r)
    (is (every #'digit-char-p r) "all digits, got ~S" r)))

(test format-t-modifier-non-timestamp-falls-back-to-strftime
  "#{t:VAR} where VAR does not resolve to an integer timestamp falls back to the
   legacy strftime path (REST treated as a format string), preserving literal
   pass-through."
  (is (string= "window_name" (fmt "#{t:window_name}" :window-name "bash"))
      "a non-timestamp variable operand passes through as literal strftime text")
  (is (string= "missing_var" (fmt "#{t:missing_var}"))
      "an unknown operand passes through literally (legacy strftime)"))

(test format-modifier-strftime-literals-pass-through
  "Non-% characters in the strftime format are passed through unchanged."
  (let ((result (fmt "#{t:TIME:}")))
    (is (string= "TIME:" result)
        "Literal text with no %codes passes through, got ~S" result)))

(test format-modifier-strftime-percent-escape
  "%% in the strftime format produces a literal percent."
  (let ((result (fmt "#{t:100%%}")))
    (is (string= "100%" result)
        "#{t:100%%} must produce '100%%', got ~S" result)))

(test format-modifier-pad-table
  "#{p5:var} pads right; #{p-5:var} pads left; at-width values are unchanged; longer pass through."
  (dolist (c '(("#{p5:v}"  :v "ab"      "ab   "   "right pad: 2 chars to 5")
               ("#{p5:v}"  :v "hello"   "hello"   "right pad: at width — no change")
               ("#{p5:v}"  :v "toolong" "toolong" "right pad: longer than width — pass through")
               ("#{p-5:v}" :v "ab"      "   ab"   "left pad: 2 chars to 5")
               ("#{p-5:v}" :v "hello"   "hello"   "left pad: at width — no change")))
    (destructuring-bind (spec key val expected desc) c
      (is (string= expected (fmt spec key val))
          "~A" desc))))

(test format-modifier-case-table
  "#{U:var} uppercases and #{L:var} lowercases the resolved value."
  (dolist (c '(("#{U:v}"            :v            "hello" "HELLO" "uppercase literal")
               ("#{U:window_name}"  :window-name  "bash"  "BASH"  "uppercase via variable")
               ("#{L:v}"            :v            "HELLO" "hello" "lowercase literal")
               ("#{L:session_name}" :session-name "MAIN"  "main"  "lowercase via variable")))
    (destructuring-bind (spec key val expected desc) c
      (is (string= expected (fmt spec key val))
          "~A" desc))))

(test format-modifier-length
  "#{l:var} returns the character length of the value as a string."
  (dolist (c '(("#{l:v}"            :v            "hello" "5" "hello is 5 chars")
               ("#{l:v}"            :v            ""      "0" "empty string is 0")
               ("#{l:session_name}" :session-name "abc"   "3" "resolves via var name")))
    (destructuring-bind (spec key val expected desc) c
      (is (string= expected (fmt spec key val)) "~A" desc))))

(test format-modifier-strftime-unit-tests
  "%strftime-format internal helpers produce correct output."
  ;; Month abbreviations
  (is (plusp (length (cl-tmux/format::%strftime-format "%b")))
      "%b produces a non-empty abbreviation")
  ;; Hour is in 0-23 range
  (let ((h (parse-integer (cl-tmux/format::%strftime-format "%H") :junk-allowed t)))
    (is (and h (>= h 0) (< h 24))
        "%H must be in 0-23, got ~A" (cl-tmux/format::%strftime-format "%H")))
  ;; %F is YYYY-MM-DD (10 chars)
  (is (= 10 (length (cl-tmux/format::%strftime-format "%F")))
      "%F must produce 10-char YYYY-MM-DD"))
