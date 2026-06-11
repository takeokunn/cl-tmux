(in-package #:cl-tmux/test)

;;;; path-modifier, substitute, nested-braces, pane_current_path, strftime, context keys, glob, regex — part II

(in-suite format-suite)

;;; ── Path-modifier helpers (direct unit tests for edge cases) ──────────────────

(test path-basename-edge-cases
  "%path-basename handles roots, trailing slashes, and bare names."
  (is (string= "c"   (cl-tmux/format::%path-basename "/a/b/c")))
  (is (string= "b"   (cl-tmux/format::%path-basename "/a/b/")))
  (is (string= "foo" (cl-tmux/format::%path-basename "foo")))
  (is (string= "/"   (cl-tmux/format::%path-basename "/"))))

(test path-dirname-edge-cases
  "%path-dirname handles roots, trailing slashes, and bare names."
  (is (string= "/a/b" (cl-tmux/format::%path-dirname "/a/b/c")))
  (is (string= "/"    (cl-tmux/format::%path-dirname "/foo")))
  (is (string= "."    (cl-tmux/format::%path-dirname "foo"))))

;;; ── Substitute modifier: #{s/PAT/REP/[i]:var} ────────────────────────────────

(test format-modifier-substitute-replaces-all
  "#{s/PAT/REP/:var} replaces every occurrence of PAT in the resolved value."
  (is (string= "barbar" (fmt "#{s/foo/bar/:window_name}" :window-name "foofoo")))
  (is (string= "m00n"   (fmt "#{s/o/0/:p}" :p "moon"))))

(test format-modifier-substitute-no-match-unchanged
  "#{s/PAT/REP/:var} returns the value unchanged when PAT does not occur."
  (is (string= "abc" (fmt "#{s/xyz/Q/:p}" :p "abc"))))

(test format-modifier-substitute-case-insensitive-flag
  "The trailing 'i' flag makes the substitution case-insensitive."
  (is (string= "xx" (fmt "#{s/abc/x/i:p}" :p "abcABC"))))

(test format-modifier-substitute-case-sensitive-by-default
  "Without the 'i' flag, the substitution is case-sensitive."
  (is (string= "xABC" (fmt "#{s/abc/x/:p}" :p "abcABC"))))

(test format-modifier-substitute-empty-pattern-is-safe
  "An empty pattern leaves the value unchanged (no infinite loop)."
  (is (string= "abc" (fmt "#{s///:p}" :p "abc"))))

(test string-replace-all-unit
  "%string-replace-all replaces all occurrences; case-insensitive on request;
   empty pattern returns the input unchanged."
  (is (string= "a-b-c" (cl-tmux/format::%string-replace-all "axbxc" "x" "-")))
  (is (string= "XXX"   (cl-tmux/format::%string-replace-all "aAa" "a" "X" t))
      "case-insensitive replaces all three a/A")
  (is (string= "XAX"   (cl-tmux/format::%string-replace-all "aAa" "a" "X"))
      "case-sensitive replaces only the two lowercase a")
  (is (string= "abc"   (cl-tmux/format::%string-replace-all "abc" "" "Z"))
      "empty pattern returns the input unchanged"))

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
  (is (string= "1" (fmt "#{==:foo,foo}")))
  (is (string= "0" (fmt "#{==:foo,bar}")))
  (is (string= "1" (fmt "#{!=:foo,bar}")))
  (is (string= "0" (fmt "#{!=:foo,foo}"))))

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
  (is (string= "1" (fmt "#{<:5,10}"))  "5 < 10")
  (is (string= "0" (fmt "#{<:10,5}"))  "not 10 < 5")
  (is (string= "1" (fmt "#{>:10,5}"))  "10 > 5")
  (is (string= "0" (fmt "#{>:5,10}"))  "not 5 > 10")
  (is (string= "1" (fmt "#{<=:5,5}"))  "5 <= 5")
  (is (string= "0" (fmt "#{<=:6,5}"))  "not 6 <= 5")
  (is (string= "1" (fmt "#{>=:5,5}"))  "5 >= 5")
  (is (string= "0" (fmt "#{>=:4,5}"))  "not 4 >= 5"))

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
  "Backward compat: a #{?cond,YES,NO} with literal branches is unchanged."
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

(test format-modifier-strftime-hhmm
  "#{t:%H:%M} formats the current hour and minute as HH:MM."
  (let ((result (fmt "#{t:%H:%M}")))
    ;; Result must be exactly 5 chars HH:MM
    (is (= 5 (length result))
        "#{t:%H:%M} must be 5 chars, got ~S" result)
    (is (char= #\: (char result 2))
        "#{t:%H:%M} must have colon at position 2, got ~S" result)))

(test format-modifier-strftime-date
  "#{t:%Y-%m-%d} formats the current date as YYYY-MM-DD."
  (let ((result (fmt "#{t:%Y-%m-%d}")))
    (is (= 10 (length result))
        "#{t:%Y-%m-%d} must be 10 chars, got ~S" result)
    (is (char= #\- (char result 4))
        "#{t:%Y-%m-%d} must have dash at position 4, got ~S" result)))

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
  (is (string= "" (cl-tmux/format::%strftime-format-at "%Y" nil)))
  (is (string= "" (cl-tmux/format::%strftime-format-at "%Y" 0)))
  (is (string= "" (cl-tmux/format::%strftime-format-at "%Y" -1))))

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
   pass-through and backward compatibility."
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

(test format-modifier-pad-right
  "#{p5:var} pads value to 5 chars on the right (left-align)."
  (is (string= "ab   " (fmt "#{p5:v}" :v "ab"))
      "2-char value padded to 5 should be 'ab   '")
  (is (string= "hello" (fmt "#{p5:v}" :v "hello"))
      "5-char value matches width exactly — no change")
  (is (string= "toolong" (fmt "#{p5:v}" :v "toolong"))
      "value longer than width passes through unchanged"))

(test format-modifier-pad-left
  "#{p-5:var} pads value to 5 chars on the left (right-align)."
  (is (string= "   ab" (fmt "#{p-5:v}" :v "ab"))
      "2-char value right-aligned to 5 should be '   ab'")
  (is (string= "hello" (fmt "#{p-5:v}" :v "hello"))
      "5-char value matches width exactly — no change"))

(test format-modifier-uppercase
  "#{U:var} uppercases the value."
  (is (string= "HELLO" (fmt "#{U:v}" :v "hello")))
  (is (string= "BASH"  (fmt "#{U:window_name}" :window-name "bash"))))

(test format-modifier-lowercase
  "#{L:var} lowercases the value."
  (is (string= "hello" (fmt "#{L:v}" :v "HELLO")))
  (is (string= "main"  (fmt "#{L:session_name}" :session-name "MAIN"))))

(test format-modifier-length
  "#{l:var} returns the character length of the value as a string."
  (is (string= "5" (fmt "#{l:v}" :v "hello")))
  (is (string= "0" (fmt "#{l:v}" :v "")))
  (is (string= "3" (fmt "#{l:session_name}" :session-name "abc"))))

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

;;; ── New context keys: cursor_x, cursor_y, pane_in_mode, window_layout ────────

(test format-context-cursor-xy-defaults
  "format-context-from-session :cursor-x and :cursor-y default to 0 when pane is NIL."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (= 0 (getf ctx :cursor-x))  ":cursor-x must default to 0")
    (is (= 0 (getf ctx :cursor-y))  ":cursor-y must default to 0")))

(test format-context-cursor-character-empty-when-pane-nil
  "#{cursor_character} is empty when there is no pane (out-of-grid safe)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "" (cl-tmux/format:expand-format "#{cursor_character}" ctx))
        "#{cursor_character} must be empty with no pane")))

(test format-context-cursor-character-reads-glyph-under-cursor
  "#{cursor_character} expands to the glyph in the cell under the cursor."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (scr  (cl-tmux/model:pane-screen pane)))
    ;; make-fake-window panes are 20x5; place 'Z' at (2,1) and move there.
    (setf (cl-tmux/terminal/types:screen-cell scr 2 1)
          (cl-tmux/terminal/types:make-cell :char #\Z))
    (setf (cl-tmux/terminal/types:screen-cursor-x scr) 2
          (cl-tmux/terminal/types:screen-cursor-y scr) 1)
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "Z" (cl-tmux/format:expand-format "#{cursor_character}" ctx))
          "#{cursor_character} must be the glyph under the cursor"))))

(test format-context-pane-in-mode-not-in-copy-mode
  "format-context-from-session :pane-in-mode is \"0\" when pane is not in copy mode."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "0" (getf ctx :pane-in-mode))
        ":pane-in-mode must be \"0\" when pane is not in copy mode")))

(test format-context-pane-in-mode-in-copy-mode
  "format-context-from-session :pane-in-mode is \"1\" when pane is in copy mode."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (scr  (cl-tmux/model:pane-screen pane)))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) t)
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "1" (getf ctx :pane-in-mode))
          ":pane-in-mode must be \"1\" when copy mode is active"))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) nil)))

(test format-context-copy-cursor-empty-outside-copy-mode
  "#{copy_cursor_x}/#{copy_cursor_y} are empty and #{selection_present} is 0 when
   the pane is not in copy mode."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "" (cl-tmux/format:expand-format "#{copy_cursor_x}" ctx)))
    (is (string= "" (cl-tmux/format:expand-format "#{copy_cursor_y}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{selection_present}" ctx)))))

(test format-context-copy-cursor-reports-position-in-copy-mode
  "In copy mode #{copy_cursor_x}/#{copy_cursor_y} report the copy cursor (row . col)
   and #{selection_present} is 1 once a selection is being made."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (scr  (cl-tmux/model:pane-screen pane)))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) t
          (cl-tmux/terminal/types:screen-copy-cursor scr) (cons 7 3)
          (cl-tmux/terminal/types:screen-copy-selecting scr) t)
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "3" (cl-tmux/format:expand-format "#{copy_cursor_x}" ctx))
          "#{copy_cursor_x} must be the copy cursor column (cdr of (row . col))")
      (is (string= "7" (cl-tmux/format:expand-format "#{copy_cursor_y}" ctx))
          "#{copy_cursor_y} must be the copy cursor row (car of (row . col))")
      (is (string= "1" (cl-tmux/format:expand-format "#{selection_present}" ctx))
          "#{selection_present} must be 1 while selecting"))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) nil
          (cl-tmux/terminal/types:screen-copy-selecting scr) nil)))

(test format-context-window-layout-non-empty-for-window-with-panes
  "format-context-from-session :window-layout is a non-empty string for a window with a tree."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 2))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (let ((layout (getf ctx :window-layout)))
      (is (stringp layout) ":window-layout must be a string")
      (is (plusp (length layout)) ":window-layout must be non-empty for a window with panes"))))

(test format-context-window-last-flag-correct
  "#{window_last_flag} is \"1\" for the previously active window and \"0\" for all others.
   Regression: a duplicate plist key shadowed the conditional, always returning \"0\"."
  (let* ((sess (make-fake-session :nwindows 2))
         (win0 (first  (cl-tmux/model:session-windows sess)))
         (win1 (second (cl-tmux/model:session-windows sess))))
    ;; Simulate: win0 was active first (lower time), win1 is now active (higher time).
    (setf (cl-tmux/model:window-last-active-time win0) 100
          (cl-tmux/model:window-last-active-time win1) 200)
    (cl-tmux/model:session-select-window sess win1)
    (let ((ctx0 (cl-tmux/format:format-context-from-session sess win0 nil))
          (ctx1 (cl-tmux/format:format-context-from-session sess win1 nil)))
      (is (string= "1" (getf ctx0 :window-last-flag))
          "the previously active window must have window_last_flag = \"1\"")
      (is (string= "0" (getf ctx1 :window-last-flag))
          "the currently active window must have window_last_flag = \"0\""))))

;;; ── Modifier chaining ────────────────────────────────────────────────────────

(test format-modifier-chain-b-of-d
  "#{b:d:var} chains dirname then basename: b(d('/a/b/c')) = b('/a/b') = 'b'."
  (is (string= "b" (fmt "#{b:d:x}" :x "/a/b/c"))))

(test format-modifier-chain-U-of-b
  "#{U:b:var} chains basename then uppercase."
  (is (string= "FOO" (fmt "#{U:b:x}" :x "/some/path/foo"))))

(test format-modifier-chain-three
  "#{U:b:d:var} chains dirname, basename, uppercase."
  (is (string= "B" (fmt "#{U:b:d:x}" :x "/a/b/c"))))

;;; ── Glob match #{m:pattern,string} ──────────────────────────────────────────

(test format-glob-match-star-matches-prefix
  "#{m:*bash,bash} → '1'."
  (is (string= "1" (fmt "#{m:*bash,bash}"))))

(test format-glob-match-star-suffix
  "#{m:bash*,bash-5.1} → '1'."
  (is (string= "1" (fmt "#{m:bash*,bash-5.1}"))))

(test format-glob-match-no-match
  "#{m:*zsh*,bash} → '0'."
  (is (string= "0" (fmt "#{m:*zsh*,bash}"))))

(test format-glob-match-question
  "#{m:ba?h,bash} → '1'."
  (is (string= "1" (fmt "#{m:ba?h,bash}"))))

(test format-glob-match-with-context-var
  "#{m:*bash,#{x}} with x='fish' → '0'."
  (is (string= "0" (fmt "#{m:*bash,#{x}}" :x "fish"))))

(test format-glob-match-in-conditional
  "#{?#{m:*bash,bash},yes,no} → 'yes'."
  (is (string= "yes" (fmt "#{?#{m:*bash,bash},yes,no}"))))

;;; ── Regex match #{m/r:pattern,string} (cl-ppcre) ─────────────────────────────

(test format-regex-match-anchored
  "#{m/r:^h.*o$,hello} → '1' (anchored regex matches the whole string)."
  (is (string= "1" (fmt "#{m/r:^h.*o$,hello}"))))

(test format-regex-match-substring
  "#{m/r:ell,hello} → '1' (regex matches a substring, unlike anchored glob)."
  (is (string= "1" (fmt "#{m/r:ell,hello}"))))

(test format-regex-match-no-match
  "#{m/r:^x,hello} → '0'."
  (is (string= "0" (fmt "#{m/r:^x,hello}"))))

(test format-regex-match-case-insensitive
  "#{m/ri:HELLO,hello} → '1' (the i flag makes the regex case-insensitive)."
  (is (string= "1" (fmt "#{m/ri:HELLO,hello}")))
  (is (string= "0" (fmt "#{m/r:HELLO,hello}"))
      "without i, the regex is case-sensitive → no match"))

(test format-regex-match-malformed-pattern-is-zero
  "A malformed regex yields '0' (no match), never an error."
  (is (string= "0" (fmt "#{m/r:[,hello}"))))

(test format-regex-match-character-class
  "#{m/r:[0-9]+,abc123} → '1' (a regex feature glob cannot express)."
  (is (string= "1" (fmt "#{m/r:[0-9]+,abc123}")))
  (is (string= "0" (fmt "#{m/r:[0-9]+,abcdef}"))))

(test format-regex-match-with-context-vars
  "#{m/r:#{pat},#{val}} expands both operands before matching."
  (is (string= "1" (fmt "#{m/r:#{pat},#{val}}" :pat "^a.c$" :val "abc"))))

(test format-glob-still-works-after-regex-addition
  "Plain #{m:...} glob is unaffected by the m/r regex branch."
  (is (string= "1" (fmt "#{m:*bash,bash}")))
  (is (string= "0" (fmt "#{m:*zsh*,bash}"))))

;;; ── New format context variables ─────────────────────────────────────────────

(test format-context-session-id
  "#{session_id} is available and is an integer (via pane context)."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (integerp (getf ctx :session-id))
        ":session-id must be an integer")))

(test format-context-window-id
  "#{window_id} is available and is an integer."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (integerp (getf ctx :window-id))
        ":window-id must be an integer")))

(test format-context-pane-current-command-is-string
  "#{pane_current_command} is available as a non-empty string."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (stringp (getf ctx :pane-current-command))
        ":pane-current-command must be a string")
    (is (plusp (length (getf ctx :pane-current-command)))
        ":pane-current-command must be non-empty")))

(test format-expand-session-id-and-window-id
  "#{session_id} and #{window_id} expand to numeric strings."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (plusp (length (cl-tmux/format:expand-format "#{session_id}" ctx)))
        "#{session_id} must expand to a non-empty string")
    (is (plusp (length (cl-tmux/format:expand-format "#{window_id}" ctx)))
        "#{window_id} must expand to a non-empty string")))
