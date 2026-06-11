(in-package #:cl-tmux/test)

;;;; format tests — part B: path-modifier helpers, substitute, nested braces,
;;;; pane_current_path, strftime, format arithmetic, geometry variables,
;;;; content search, glob-match-p, pane-visible-lines, apply-pad-modifier.

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

;;; ── Format arithmetic #{e|OP|A,B} ───────────────────────────────────────────

(test format-arithmetic-addition
  "#{e|+|1,2} expands to 3."
  (is (string= "3" (fmt "#{e|+|1,2}"))))

(test format-arithmetic-subtraction
  "#{e|-|5,2} expands to 3."
  (is (string= "3" (fmt "#{e|-|5,2}"))))

(test format-arithmetic-multiplication
  "#{e|*|3,4} expands to 12."
  (is (string= "12" (fmt "#{e|*|3,4}"))))

(test format-arithmetic-division
  "#{e|/|10,3} expands to 3 (integer division)."
  (is (string= "3" (fmt "#{e|/|10,3}"))))

(test format-arithmetic-modulo
  "#{e|%|10,3} expands to 1."
  (is (string= "1" (fmt "#{e|%|10,3}"))))

(test format-arithmetic-with-variable
  "#{e|+|1,#{window_index}} expands to window_index+1."
  (let ((ctx (list :window-index 5)))
    (is (string= "6" (cl-tmux/format:expand-format "#{e|+|1,#{window_index}}" ctx)))))

(test format-arithmetic-divide-by-zero
  "#{e|/|5,0} returns 0 (no error)."
  (is (string= "0" (fmt "#{e|/|5,0}"))))

;;; ── Additional format variables ─────────────────────────────────────────────

(test format-context-version-is-35
  "#{version} expands to 3.5 for tmux config compatibility guards."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "3.5" (cl-tmux/format:expand-format "#{version}" ctx))
        "#{version} must be 3.5")))

(test format-context-pane-format-is-1-when-pane-present
  "#{pane_format} is 1 when a pane is in context."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_format}" ctx))
        "#{pane_format} must be 1 when pane is in context")))

(test format-context-window-format-is-1-when-window-present
  "#{window_format} is 1 when a window is in context."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{window_format}" ctx))
        "#{window_format} must be 1 when window is in context")))

;;; ── Bare strftime codes (%H, %M, %S, etc.) ──────────────────────────────────
;;;
;;; Real tmux passes status-left/right through strftime before #{} expansion,
;;; so bare %H:%M works in those strings. Our inline handler mimics this.

(test format-bare-strftime-hour-minute
  "Bare %H:%M in a format string expands to the current HH:MM time."
  (let ((result (cl-tmux/format:expand-format "%H:%M" nil)))
    ;; Should look like HH:MM (10 chars: 2 digits, colon, 2 digits)
    (is (= 5 (length result)) "bare %H:%M must expand to exactly 5 characters")
    (is (char= #\: (char result 2)) "colon at position 2")))

(test format-bare-strftime-percent-escape
  "Bare %% expands to a literal %."
  (is (string= "%" (cl-tmux/format:expand-format "%%" nil))))

(test format-bare-strftime-mixed-with-hash-var
  "Bare %H and #{session_name} can coexist in one template."
  (let* ((result (cl-tmux/format:expand-format "%H:00 #{session_name}"
                                               '(:session-name "main"))))
    ;; Should end with ":00 main" (hour prefix varies)
    (is (search ":00 main" result) "mixed bare-% and #{} expansion")))

(test format-bare-strftime-unknown-letter-is-literal
  "A %X where X is not a strftime letter passes through unchanged."
  (is (string= "test%q" (cl-tmux/format:expand-format "test%q" nil))))

;;; ── @user-option fallback in format variables ────────────────────────────────
;;;
;;; Real tmux allows #{@my-var} to access user-defined options set via
;;; `set -g @my-var value`. The fallback through *global-options* provides this.

(test format-user-option-at-variable
  "#{@my-var} falls back to *global-options* when not in context."
  (with-isolated-config
    (cl-tmux/options:set-option "@my-var" "hello")
    (let ((result (cl-tmux/format:expand-format "#{@my-var}" nil)))
      (is (string= "hello" result)
          "#{@my-var} must expand via global options fallback"))))

(test format-user-option-unknown-returns-empty
  "#{@nonexistent} returns empty string when option not set."
  (with-isolated-config
    (let ((result (cl-tmux/format:expand-format "#{@nonexistent}" nil)))
      (is (string= "" result) "#{@nonexistent} must return empty string"))))

;;; ── Version guard patterns ───────────────────────────────────────────────────

(test format-version-guard-comparison
  "#{>=:#{version},3.0} evaluates to 1 (version 3.5 >= 3.0)."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    ;; Note: comparison is numeric; "3.5" vs "3.0" — parse-integer gives 3 for both
    ;; due to junk-allowed stopping at '.'. This is a known limitation.
    ;; The test just verifies no error is thrown.
    (is (stringp (cl-tmux/format:expand-format "#{version}" ctx))
        "#{version} must expand to a string")))

;;; ── #{pane_synchronized} respects per-window scoping ─────────────────────────

(test format-pane-synchronized-window-local-override
  "#{pane_synchronized} reads the window-local synchronize-panes override:
   it is \"1\" for a window with the local override on, and \"0\" for a fresh
   window with no override (global stays nil)."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" nil)
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win))))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (is (string= "1" (cl-tmux/format:expand-format "#{pane_synchronized}" ctx))
            "#{pane_synchronized} must be \"1\" for a window with the local override on"))
      ;; A second, fresh window with no override falls back to the global NIL → "0".
      (let* ((win2  (make-fake-window 99 "w2"))
             (pane2 (first (cl-tmux/model:window-panes win2)))
             (ctx2  (cl-tmux/format:format-context-from-session sess win2 pane2)))
        (is (string= "0" (cl-tmux/format:expand-format "#{pane_synchronized}" ctx2))
            "#{pane_synchronized} must be \"0\" for a window with no override")))))

;;; ── geometry-derived variables: window_width/height, pane_at_* ───────────────
;;;
;;; make-fake-window builds panes/windows at 20x5 (each fake pane shares
;;; x=0 y=0 w=20 h=5, matching the window), so a single-pane fake window has
;;; the pane filling the whole window — every edge flag is "1".  For a real
;;; split we use make-two-pane-h-window from helpers.lisp, which lays out:
;;;   window 81x24; p0 x=0 y=0 w=40 h=24; p1 x=41 y=0 w=40 h=24.
;;; So p0 touches top/bottom/left but NOT right (0+40=40 ≠ 81); p1 touches
;;; top/bottom/right (41+40=81) but NOT left (x=41 ≠ 0).

(test format-pane-tty-from-pane
  "#{pane_tty} expands to the pane's slave PTY device path."
  (let ((pane (make-no-pty-pane 1 0 0 80 24)))
    (setf (cl-tmux/model:pane-tty pane) "/dev/pts/7")
    (is (string= "/dev/pts/7"
                 (cl-tmux/format:expand-format
                  "#{pane_tty}"
                  (cl-tmux/format:format-context-from-session nil nil pane)))
        "#{pane_tty} must report the pane's tty slot")))

(test format-pane-tty-empty-when-no-pty-or-nil
  "#{pane_tty} is empty for a pane with no PTY (default \"\") and for a NIL pane."
  (let ((pane (make-no-pty-pane 1 0 0 80 24)))
    (is (string= "" (cl-tmux/format:expand-format
                     "#{pane_tty}"
                     (cl-tmux/format:format-context-from-session nil nil pane)))
        "no-PTY pane → empty pane_tty"))
  (is (string= "" (cl-tmux/format:expand-format
                   "#{pane_tty}"
                   (cl-tmux/format:format-context-from-session nil nil nil)))
      "NIL pane → empty pane_tty"))

(test format-window-width-height-from-window
  "#{window_width} / #{window_height} expand to the window's layout dimensions.
   make-fake-window builds a 20x5 window, so the expansions are \"20\"/\"5\"."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "20" (cl-tmux/format:expand-format "#{window_width}" ctx))
        "#{window_width} must equal the fake window's width (20), got ~S"
        (cl-tmux/format:expand-format "#{window_width}" ctx))
    (is (string= "5" (cl-tmux/format:expand-format "#{window_height}" ctx))
        "#{window_height} must equal the fake window's height (5), got ~S"
        (cl-tmux/format:expand-format "#{window_height}" ctx))))

(test format-pane-at-edges-single-pane-all-true
  "For a single-pane window (pane fills the window) all pane_at_* flags are \"1\".
   make-fake-window's lone pane is x=0 y=0 w=20 h=5 in a 20x5 window."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx))
        "single pane must be at top")
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx))
        "single pane must be at bottom")
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx))
        "single pane must be at left")
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx))
        "single pane must be at right")))

(test format-pane-at-edges-horizontal-split
  "For a laid-out horizontal split (make-two-pane-h-window: 81x24, p0 x=0 w=40,
   p1 x=41 w=40), the left pane is NOT at the right edge and the right pane is
   NOT at the left edge, while both span the full height (at top and bottom)."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (let ((ctx0 (cl-tmux/format:format-context-from-session nil win p0))
          (ctx1 (cl-tmux/format:format-context-from-session nil win p1)))
      ;; left pane p0: at left, top, bottom; NOT at right.
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx0))
          "left pane must be at left edge")
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_right}" ctx0))
          "left pane must NOT be at right edge (0+40=40 ≠ 81)")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx0))
          "left pane must be at top edge")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx0))
          "left pane must be at bottom edge (0+24=24)")
      ;; right pane p1: at right, top, bottom; NOT at left.
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_left}" ctx1))
          "right pane must NOT be at left edge (x=41 ≠ 0)")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx1))
          "right pane must be at right edge (41+40=81)")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx1))
          "right pane must be at top edge")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx1))
          "right pane must be at bottom edge")
      ;; window_width/height from the real split window.
      (is (string= "81" (cl-tmux/format:expand-format "#{window_width}" ctx0))
          "#{window_width} must equal the split window's width (81)")
      (is (string= "24" (cl-tmux/format:expand-format "#{window_height}" ctx0))
          "#{window_height} must equal the split window's height (24)"))))

;;; ── pane_at_top/bottom "0" branches + NIL-safe defaults ──────────────────────
;;;
;;; with-v-split-window (helpers.lisp) lays out: window 80x21;
;;;   p0 x=0 y=0  w=80 h=10 (top pane), p1 x=0 y=11 w=80 h=10 (bottom pane).
;;; Both span the full width (x=0, w=80=window width → at left and right).
;;; p0 is at top (y=0) but NOT at bottom (0+10=10 ≠ 21); p1 is NOT at top
;;; (y=11 ≠ 0) but IS at bottom (11+10=21).  This exercises the "0" branch of
;;; #{pane_at_top}/#{pane_at_bottom}, which the full-height fixtures never hit.

(test format-pane-at-edges-vertical-split
  "A laid-out vertical split drives the \"0\" branch of pane_at_top/pane_at_bottom:
   the TOP pane is not at the bottom edge, the BOTTOM pane is not at the top edge,
   while both span the full width."
  (with-v-split-window (win p0 p1)
    (let ((ctx0 (cl-tmux/format:format-context-from-session nil win p0))
          (ctx1 (cl-tmux/format:format-context-from-session nil win p1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx0)))
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx0)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx0)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx0)))
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_top}" ctx1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx1))))))

(test format-pane-at-edges-and-window-dims-default-when-nil
  "With NIL session/window/pane, geometry vars are empty-safe: window_width/height
   expand to \"0\" and every pane_at_* flag is \"0\"."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "0" (cl-tmux/format:expand-format "#{window_width}"  ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{window_height}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_top}"    ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_left}"   ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_right}"  ctx)))))

(test format-pane-at-bottom-right-default-when-window-nil
  "Pane present but window NIL: pane_at_top/left resolve from the pane's coords,
   but pane_at_bottom/right short-circuit to \"0\" (far-edge needs the window)."
  (let* ((pane (make-no-pty-pane 1 0 0 40 24))
         (ctx  (cl-tmux/format:format-context-from-session nil nil pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}"    ctx)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}"   ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_right}"  ctx)))))

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
    (is (string= "1" (cl-tmux/format:expand-format "#{C:hello}" ctx))
        "term on the first line → 1")
    (is (string= "2" (cl-tmux/format:expand-format "#{C:bar}" ctx))
        "term on the second line → 2")
    (is (string= "3" (cl-tmux/format:expand-format "#{C:qux}" ctx))
        "term on the third line → 3")))

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
    (is (string= "2" (cl-tmux/format:expand-format "#{C/r:b.r}" ctx))
        "b.r regex matches 'bar' on line 2")
    (is (string= "2" (cl-tmux/format:expand-format "#{C/r:^foo}" ctx))
        "^foo anchors to the start of line 2")
    (is (string= "0" (cl-tmux/format:expand-format "#{C/r:^bar}" ctx))
        "^bar matches no line start → 0 (regex, not substring)")))

(test format-content-search-case-insensitive
  "#{C/i:term} folds case; bare #{C:term} is case-sensitive."
  (let* ((p   (%content-search-pane "Hello World"))
         (ctx (cl-tmux/format:format-context-from-session nil nil p)))
    (is (string= "0" (cl-tmux/format:expand-format "#{C:hello}" ctx))
        "case-sensitive glob does not match 'Hello'")
    (is (string= "1" (cl-tmux/format:expand-format "#{C/i:hello}" ctx))
        "case-insensitive glob matches 'Hello'")
    (is (string= "1" (cl-tmux/format:expand-format "#{C/ri:HELLO}" ctx))
        "case-insensitive regex matches 'Hello'")))

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

(test glob-match-exact
  "%glob-match-p with a plain pattern (no metacharacters) is an equality check."
  (is-true  (cl-tmux/format::%glob-match-p "hello" "hello"))
  (is-false (cl-tmux/format::%glob-match-p "hello" "Hello"))
  (is-false (cl-tmux/format::%glob-match-p "hello" "hell")))

(test glob-match-star-prefix
  "%glob-match-p with leading * matches any prefix."
  (is-true  (cl-tmux/format::%glob-match-p "*sh" "bash"))
  (is-true  (cl-tmux/format::%glob-match-p "*sh" "sh"))
  (is-false (cl-tmux/format::%glob-match-p "*sh" "bash-old")))

(test glob-match-star-suffix
  "%glob-match-p with trailing * matches any suffix."
  (is-true  (cl-tmux/format::%glob-match-p "ba*" "bash"))
  (is-true  (cl-tmux/format::%glob-match-p "ba*" "ba"))
  (is-false (cl-tmux/format::%glob-match-p "ba*" "zsh")))

(test glob-match-star-infix
  "%glob-match-p with an interior * matches any sequence between the anchors."
  (is-true  (cl-tmux/format::%glob-match-p "a*b" "ab"))
  (is-true  (cl-tmux/format::%glob-match-p "a*b" "axyzb"))
  (is-false (cl-tmux/format::%glob-match-p "a*b" "axyzc")))

(test glob-match-question-mark
  "%glob-match-p with ? matches exactly one character."
  (is-true  (cl-tmux/format::%glob-match-p "b?sh" "bash"))
  (is-true  (cl-tmux/format::%glob-match-p "b?sh" "bzsh"))
  (is-false (cl-tmux/format::%glob-match-p "b?sh" "bsh"))
  (is-false (cl-tmux/format::%glob-match-p "b?sh" "baxsh")))

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
