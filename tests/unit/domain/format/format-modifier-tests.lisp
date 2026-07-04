;;;; format modifier expansion

(in-package #:cl-tmux/test)
(in-suite format-suite)

(test format-modifier-truncate-table
  "#{=N:var} keeps the first N chars; #{=-N:var} keeps the last N; shorter values pass through."
  (dolist (c '(("#{=5:window_name}"  "verylongname" "veryl" "left truncation: first 5 chars")
               ("#{=-5:window_name}" "verylongname" "gname" "right truncation: last 5 chars")
               ("#{=20:window_name}" "short"        "short" "shorter than limit → unchanged")))
    (destructuring-bind (spec input expected desc) c
      (is (string= expected (fmt spec :window-name input))
          "~A: ~S → ~S" desc spec expected))))

(test format-modifier-logical-or
  "#{||:a,b} returns 1 when either operand is truthy, else 0."
  (dolist (c '(("#{||:1,0}"       ()           "1" "1 || 0 → 1")
               ("#{||:0,1}"       ()           "1" "0 || 1 → 1")
               ("#{||:0,0}"       ()           "0" "0 || 0 → 0")
               ("#{||:,}"         ()           "0" "empty || empty → 0")
               ("#{||:#{a},#{b}}" (:a "" :b "x") "1" "operands expand before the truthiness test")))
    (destructuring-bind (spec ctx expected desc) c
      (is (string= expected (cl-tmux/format:expand-format spec ctx)) "~A" desc))))

(test format-modifier-logical-and
  "#{&&:a,b} returns 1 only when both operands are truthy."
  (dolist (c '(("#{&&:1,1}" "1" "1 && 1 → 1")
               ("#{&&:1,0}" "0" "1 && 0 → 0")
               ("#{&&:0,1}" "0" "0 && 1 → 0")
               ("#{&&:0,0}" "0" "0 && 0 → 0")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec)) "~A" desc))))

(test format-modifier-logical-nested-in-conditional
  "#{?#{||:cond1,cond2},yes,no} chooses the branch by the logical result."
  (is (string= "yes" (fmt "#{?#{||:#{a},#{b}},yes,no}" :a "" :b "1")))
  (is (string= "no"  (fmt "#{?#{&&:#{a},#{b}},yes,no}" :a "" :b "1"))))

(test format-modifier-quote
  "#{q:var} backslash-escapes shell-special characters in the resolved value."
  (dolist (c '(("a b"   "a\\ b"  "space is escaped")
               ("a;b"   "a\\;b"  "semicolon is escaped")
               ("plain" "plain"  "ordinary text is unchanged")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (fmt "#{q:p}" :p input)) "~A" desc))))

(test format-modifier-char-from-code-table
  "#{a:N} yields the single character at code point N; nested format operands work."
  (dolist (c (list (list "#{a:35}"           "#"                    "code 35 is '#'")
                   (list "#{a:65}"           "A"                    "code 65 is 'A'")
                   (list "#{a:97}"           "a"                    "code 97 is 'a'")
                   (list "#{a:0}"            (string (code-char 0)) "code 0 is NUL")
                   (list "#{a:955}"          (string (code-char 955)) "code 955 is lambda")
                   (list "#{a:#{code}}"      "B"                    "nested format to code 66")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec :code "66")) "~A" desc))))

(test format-modifier-char-from-code-invalid-operands-yield-empty
  "Non-numeric, negative, out-of-range, empty, and nested-empty operands all yield empty."
  (dolist (spec '("#{a:notanumber}" "#{a:-1}" "#{a:9999999}" "#{a:}" "#{a:#{missing}}"))
    (is (string= "" (fmt spec)) "~S must yield the empty string" spec)))

(test format-modifier-basename-dirname-table
  "#{b:var} yields the basename; #{d:var} yields the dirname."
  (dolist (c '(("#{b:p}" :p "/home/user/project" "project"    "basename of deep path")
               ("#{b:p}" :p "/a/b/"              "b"          "trailing slash stripped")
               ("#{b:p}" :p "foo"                "foo"        "bare name is its own basename")
               ("#{d:p}" :p "/home/user/project" "/home/user" "dirname of deep path")
               ("#{d:p}" :p "foo"                "."          "no slash → current dir")
               ("#{d:p}" :p "/foo"               "/"          "top-level → root")))
    (destructuring-bind (spec key val expected desc) c
      (is (string= expected (fmt spec key val)) "~A" desc))))

(test format-modifier-unrecognized-falls-back-to-lookup
  "An unrecognised modifier prefix falls back to a plain variable lookup of the
   whole #{...} content (an unknown key yields empty string), never an error."
  (is (string= "" (fmt "#{zz:window_name}" :window-name "x"))))

(test format-conditional-with-colon-in-branch-is-not-a-modifier
  "A ':' inside a #{?...} conditional branch must NOT be mistaken for a modifier
   separator — the conditional is matched first."
  (is (string= "a:b" (fmt "#{?on,a:b,c}" :on "1"))
      "true branch 'a:b' (containing a colon) must survive intact")
  (is (string= "c"   (fmt "#{?on,a:b,c}" :on "0"))
      "false branch still selected when condition is false"))
