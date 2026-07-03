(in-package #:cl-tmux/test)

;;;; shorthand table, %expand-brace edge cases, %truthy-p, %variable-to-keyword, pane/client vars, #(shell), structural vars, modifiers — part IV

(in-suite format-suite)

;;; ── Table-driven shorthand expansion ─────────────────────────────────────────

(test expand-format-all-shorthands-table
  "All single-character shorthands expand to the correct context value."
  (let ((cases '(("#S" :session-name "sess1" "sess1")
                 ("#I" :window-index "3"     "3")
                 ("#W" :window-name  "bash"  "bash")
                 ("#P" :pane-index   "2"     "2")
                 ("#H" :hostname     "box"   "box")
                 ("##" nil           nil     "#"))))
    (dolist (c cases)
      (destructuring-bind (tmpl key val expected) c
        (let ((ctx (if key (list key val) '())))
          (is (string= expected (cl-tmux/format:expand-format tmpl ctx))
              "shorthand ~S: expected ~S" tmpl expected))))))

;;; ── %expand-brace edge cases ──────────────────────────────────────────────────

(test expand-format-unclosed-delimiter-emits-literal-hash
  "#{, #[, and #( without a closing delimiter each emit a literal '#' and do not crash."
  (dolist (c '(("#{no_close" "brace without closing }")
               ("#[no_close" "bracket without closing ]")
               ("#(no_close" "paren without closing )")))
    (destructuring-bind (input desc) c
      (let ((result (cl-tmux/format:expand-format input '())))
        (is (char= #\# (char result 0))
            "~A: first char must be '#' (got ~S)" desc result)))))

(test expand-format-bracket-passthrough-preserves-content
  "#[attrs] passes the full bracketed text through unchanged."
  (is (string= "#[fg=red,bold]" (fmt "#[fg=red,bold]"))
      "#[fg=red,bold] must pass through unchanged"))

(test expand-format-conditional-context-variable-resolution
  "#{?window_active,YES,NO} resolves window_active from the context plist."
  ;; When :window-active is \"1\" in the context, the true branch is returned.
  (is (string= "YES"
               (cl-tmux/format:expand-format "#{?window_active,YES,NO}"
                                             '(:window-active "1")))
      "truthy context variable must select true branch")
  (is (string= "NO"
               (cl-tmux/format:expand-format "#{?window_active,YES,NO}"
                                             '(:window-active "0")))
      "falsy context variable must select false branch"))

(test expand-format-conditional-literal-true-zero
  "#{?1,yes,no} / #{?0,yes,no} work without any context."
  (is (string= "yes" (fmt "#{?1,yes,no}")) "literal 1 → yes")
  (is (string= "no"  (fmt "#{?0,yes,no}")) "literal 0 → no"))

(test expand-format-brace-missing-key-uses-empty
  "#{window_name} with no matching context key emits an empty string."
  (is (string= "" (cl-tmux/format:expand-format "#{window_name}" '()))
      "missing context key must produce empty string"))

(test expand-format-mixed-template
  "A template mixing shorthands, brace vars, and plain text expands correctly."
  (let ((ctx '(:session-name "main" :window-name "bash")))
    (is (string= "session=main window=bash"
                 (cl-tmux/format:expand-format "session=#{session_name} window=#W" ctx))
        "mixed template must expand all specifiers")))

;;; ── %expand-shorthand unknown returns nil ────────────────────────────────────

(test expand-shorthand-unknown-char-emits-both-chars
  "An unknown shorthand #X emits both '#' and 'X' literally."
  (dolist (spec '("#Z" "#?"))
    (is (string= spec (fmt spec)) "~S must pass through as two literal chars" spec)))

;;; ── format-context-from-window nil window ────────────────────────────────────

(test format-context-from-window-nil-window
  "format-context-from-window with NIL window returns safe defaults."
  (let* ((sess (make-fake-session :nwindows 1))
         (ctx  (cl-tmux/format:format-context-from-window sess nil)))
    (is (stringp (getf ctx :session-name))
        ":session-name must be a string when window is nil")
    (is (string= "" (getf ctx :window-name))
        ":window-name must be empty when window is nil")
    (is (= 0 (getf ctx :window-index))
        ":window-index must be 0 when window is nil")))

;;; ── %lookup integer value ────────────────────────────────────────────────────

(test lookup-integer-value-converted-to-string
  "%lookup converts integer values to strings via princ-to-string."
  (is (string= "42" (cl-tmux/format::%lookup (list :count 42) :count))
      ":count 42 must stringify to \"42\""))

;;; ── Table-driven %truthy-p boundary tests ────────────────────────────────────

(test truthy-p-table-driven
  "%truthy-p boundary table: various inputs and expected truthiness."
  ;; tmux format_true: only the empty string and exactly "0" are false; every
  ;; other non-empty string (including "false") is truthy.
  (let ((cases '(("1"     . t)
                 ("yes"   . t)
                 ("true"  . t)
                 (""      . nil)
                 ("0"     . nil)
                 ("false" . t)
                 ("FALSE" . t))))
    (dolist (c cases)
      (let ((input    (car c))
            (expected (cdr c)))
        (if expected
            (is-true  (cl-tmux/format::%truthy-p input)
                      "%truthy-p ~S must be truthy" input)
            (is-false (cl-tmux/format::%truthy-p input)
                      "%truthy-p ~S must be falsy" input))))))

;;; ── %variable-to-keyword table-driven ────────────────────────────────────────

(test variable-to-keyword-table-driven
  "%variable-to-keyword converts multiple names correctly."
  (let ((cases '(("session_name"  . :session-name)
                 ("window_index"  . :window-index)
                 ("pane_index"    . :pane-index)
                 ("host_short"    . :host-short)
                 ("window_active" . :window-active)
                 ("time"          . :time))))
    (dolist (c cases)
      (is (eq (cdr c) (cl-tmux/format::%variable-to-keyword (car c)))
          "%variable-to-keyword ~S → ~S" (car c) (cdr c)))))

;;; ── #{pane_title} expansion ───────────────────────────────────────────────────

(test expand-format-pane-title-table
  "#{pane_title} expands to :pane-title when present, or empty when absent."
  (is (string= "mytitle" (fmt "#{pane_title}" :pane-title "mytitle"))
      "#{pane_title} must expand to the :pane-title value")
  (is (string= "" (fmt "#{pane_title}"))
      "#{pane_title} with no context must return empty string"))

(test format-context-pane-title-from-pane-slot
  "format-context-from-session :pane-title uses the pane's title slot."
  (let* ((sess  (make-fake-session :nwindows 1 :npanes 1))
         (win   (first (cl-tmux/model:session-windows sess)))
         (pane  (first (cl-tmux/model:window-panes win))))
    (setf (cl-tmux/model:pane-title pane) "my-pane-title")
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "my-pane-title" (getf ctx :pane-title))
          ":pane-title mismatch: expected ~S got ~S"
          "my-pane-title" (getf ctx :pane-title)))))

(test format-context-pane-title-nil-pane-returns-empty
  "format-context-from-session :pane-title is empty when pane is NIL."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (ctx  (cl-tmux/format:format-context-from-session sess win nil)))
    (is (string= "" (getf ctx :pane-title))
        ":pane-title must be empty when pane is NIL, got ~S"
        (getf ctx :pane-title))))

(test expand-format-conditional-pane-title
  "#{?pane_title,has-title,no-title} branches correctly on :pane-title."
  (is (string= "has-title"
               (cl-tmux/format:expand-format "#{?pane_title,has-title,no-title}"
                                             '(:pane-title "vim")))
      "non-empty pane_title must select true branch")
  (is (string= "no-title"
               (cl-tmux/format:expand-format "#{?pane_title,has-title,no-title}"
                                             '(:pane-title "")))
      "empty pane_title must select false branch"))

;;; ── #{client_width}, #{client_height}, #{client_tty} ─────────────────────────

(test expand-format-client-fields-table
  "#{client_width}, #{client_height}, #{client_tty} each expand to the matching context value."
  (dolist (c '(("#{client_width}"  :client-width  220    "220")
               ("#{client_height}" :client-height 55     "55")
               ("#{client_tty}"    :client-tty    "/dev/pts/0" "/dev/pts/0")))
    (destructuring-bind (spec key value expected) c
      (is (string= expected (fmt spec key value))
          "~S with ~S=~S must expand to ~S" spec key value expected))))

(test format-context-client-defaults-are-zero
  "format-context-from-session :client-width and :client-height default to 0."
  (with-format-context (sess win pane ctx) ()
    (is (eql 0 (getf ctx :client-width))
        ":client-width must default to 0, got ~S" (getf ctx :client-width))
    (is (eql 0 (getf ctx :client-height))
        ":client-height must default to 0, got ~S" (getf ctx :client-height))
    (is (string= "" (getf ctx :client-tty))
        ":client-tty must default to empty string, got ~S" (getf ctx :client-tty))))

;;; ── #{client_name}, #{client_session}, #{client_pid}, #{client_termname} ─────

(test format-context-client-name-mirrors-tty
  "#{client_name} defaults to the client tty path (tmux's default client name)."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session
                sess win pane :client-tty "/dev/ttys004")))
    (is (string= "/dev/ttys004"
                 (cl-tmux/format:expand-format "#{client_name}" ctx))
        "#{client_name} must mirror the client tty")))

(test format-context-client-session-is-session-name
  "#{client_session} expands to the name of the session the client is viewing."
  (with-format-context (sess win pane ctx) ()
    ;; make-fake-session names the session "0".
    (is (string= (cl-tmux/model:session-name sess)
                 (cl-tmux/format:expand-format "#{client_session}" ctx))
        "#{client_session} must be the session name")))

(test format-context-client-pid-is-numeric
  "#{client_pid} expands to a non-empty numeric PID string (single-process model)."
  (with-format-context (sess win pane ctx) ()
    (let ((pid (cl-tmux/format:expand-format "#{client_pid}" ctx)))
      (is (plusp (length pid)) "#{client_pid} must be non-empty")
      (is (every #'digit-char-p pid) "#{client_pid} must be all digits, got ~S" pid))))

(test format-context-client-termname-is-string
  "#{client_termname} expands to a string (the TERM env value or empty)."
  (with-format-context (sess win pane ctx) ()
    (is (stringp (cl-tmux/format:expand-format "#{client_termname}" ctx))
        "#{client_termname} must expand to a string")))

(test format-context-client-keyword-args-propagated
  "format-context-from-session forwards :client-width/:client-height/:client-tty."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane
                                                           :client-width  200
                                                           :client-height 50
                                                           :client-tty    "/dev/pts/3")))
    (is (eql 200 (getf ctx :client-width))
        ":client-width expected 200 got ~S" (getf ctx :client-width))
    (is (eql 50 (getf ctx :client-height))
        ":client-height expected 50 got ~S" (getf ctx :client-height))
    (is (string= "/dev/pts/3" (getf ctx :client-tty))
        ":client-tty expected \"/dev/pts/3\" got ~S" (getf ctx :client-tty))))

;;; ── #(shell-cmd) expansion ───────────────────────────────────────────────────

(test expand-format-shell-cmd-table
  "#(cmd) expands to command output; errors return empty string; embeds inline."
  (dolist (c '(("#(echo hello)"           "hello"          "echo output")
               ("#(printf '%s' foo)"      "foo"            "no trailing newline")
               ("#(false)"               ""               "error → empty string")
               ("status: #(echo ok) done" "status: ok done" "embedded in text")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec)) "~A" desc))))

;;; ── #[attr] style directive — no crash guarantee ─────────────────────────────

(test expand-format-sgr-no-crash-complex-attr
  "#[fg=colour231,bold] passes through without crashing."
  (let ((result (fmt "#[fg=colour231,bold]")))
    (is (stringp result) "#[...] result must be a string (no crash)")
    (is (string= "#[fg=colour231,bold]" result)
        "#[fg=colour231,bold] must pass through literally, got ~S" result)))

;;; ── structural pane / aggregate format variables ─────────────────────────────
;;;
;;; These are pure functions of the session/window/pane structs, wired into
;;; format-context-from-session and exercised end-to-end through expand-format.

(test format-context-exposes-structural-pane-variables
  "format-context-from-session populates pane geometry/id/pid variables that
   expand-format resolves (#{pane_width} #{pane_height} #{pane_id} #{pane_left}
   #{pane_top} #{pane_pid}).
   make-fake-window panes are 20x5 at (0,0), id 1, pid -1.
   Inclusive far-edge: right = 0+20-1 = 19, bottom = 0+5-1 = 4."
  (with-format-context (sess win pane ctx) ()
    (dolist (c '(("#{pane_width}"  "20") ("#{pane_height}" "5")
                 ("#{pane_id}"     "1")  ("#{pane_left}"   "0")
                 ("#{pane_top}"    "0")  ("#{pane_right}"  "19")
                 ("#{pane_bottom}" "4")  ("#{pane_pid}"    "-1")))
      (destructuring-bind (spec expected) c
        (is (string= expected (cl-tmux/format:expand-format spec ctx))
            "~S must expand to ~S" spec expected)))))

(test format-context-pane-variables-default-when-pane-nil
  "With a NIL pane, structural pane variables default to 0 (empty-safe)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (dolist (spec '("#{pane_width}" "#{pane_id}" "#{pane_right}"
                    "#{pane_bottom}" "#{pane_active}"))
      (is (string= "0" (cl-tmux/format:expand-format spec ctx))
          "~S must default to 0 with nil pane" spec))))

(test window-bell-flag-respects-monitor-bell
  "#{window_bell_flag} shows ! only when monitor-bell is on (default); monitor-bell
   off suppresses the bell alert even with the sticky window bell flag set."
  (with-fresh-options
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win))))
      (setf (cl-tmux/model:window-bell-flag win) t)
      (cl-tmux/options:set-option "monitor-bell" t)
      (is (string= "!" (cl-tmux/format:expand-format
                        "#{window_bell_flag}"
                        (cl-tmux/format:format-context-from-session sess win pane)))
          "monitor-bell on must show the bell flag")
      (cl-tmux/options:set-option "monitor-bell" nil)
      (is (not (string= "!" (cl-tmux/format:expand-format
                             "#{window_bell_flag}"
                             (cl-tmux/format:format-context-from-session sess win pane))))
          "monitor-bell off must suppress the bell flag"))))

(test format-context-pane-active-distinguishes-active-pane
  "#{pane_active} is 1 for the window's active pane, 0 otherwise — and drives
   the #{?pane_active,t,f} conditional, the common real-world usage."
  (let* ((sess       (make-fake-session :nwindows 1 :npanes 2))
         (win        (first (cl-tmux/model:session-windows sess)))
         (panes      (cl-tmux/model:window-panes win))
         (p-active   (cl-tmux/model:window-active-pane win))
         (p-inactive (find-if-not (lambda (p) (eq p p-active)) panes)))
    (let ((ctx-a (cl-tmux/format:format-context-from-session sess win p-active))
          (ctx-i (cl-tmux/format:format-context-from-session sess win p-inactive)))
      (dolist (c `((,ctx-a "#{pane_active}"            "1"    "active pane → #{pane_active} 1")
                   (,ctx-i "#{pane_active}"            "0"    "inactive pane → #{pane_active} 0")
                   (,ctx-a "#{?pane_active,HERE,away}" "HERE" "conditional picks the true branch")
                   (,ctx-i "#{?pane_active,HERE,away}" "away" "conditional picks the false branch")))
        (destructuring-bind (ctx spec expected desc) c
          (is (string= expected (cl-tmux/format:expand-format spec ctx)) "~A" desc))))))

(test format-context-window-panes-and-session-windows-counts
  "#{window_panes} is the pane count; #{session_windows} is the window count."
  (with-format-context (sess win pane ctx) (:nwindows 3 :npanes 2)
    (is (string= "2" (cl-tmux/format:expand-format "#{window_panes}" ctx))
        "window has 2 panes")
    (is (string= "3" (cl-tmux/format:expand-format "#{session_windows}" ctx))
        "session has 3 windows")))

(test format-context-session-count-is-numeric
  "#{session_count} expands to a non-empty numeric string (server session total,
   minimum 1 in the single-process model)."
  (with-format-context (sess win pane ctx) ()
    (let ((count (cl-tmux/format:expand-format "#{session_count}" ctx)))
      (is (plusp (length count)) "#{session_count} must be non-empty")
      (is (every #'digit-char-p count) "#{session_count} must be all digits, got ~S" count)
      (is (>= (parse-integer count) 1) "#{session_count} must be at least 1, got ~S" count))))

;;; ── Format modifiers: #{=N:var} #{=-N:var} #{b:var} #{d:var} ─────────────────

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


(test pane-dead-status-format-vars-table
  "#{pane_dead_status}/#{pane_dead_signal}/#{pane_dead_time} expand from the
   pane's death record and are empty for a live pane (tmux empty defaults)."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win))))
    (flet ((expand (spec)
             (cl-tmux/format:expand-format
              spec (cl-tmux/format:format-context-from-session sess win pane))))
      (dolist (spec '("#{pane_dead_status}" "#{pane_dead_signal}" "#{pane_dead_time}"))
        (is (string= "" (expand spec))
            "~A must be empty while the pane is alive" spec))
      (setf (cl-tmux/model:pane-dead-status pane) 1
            (cl-tmux/model:pane-dead-time pane) 3927584461)
      (is (string= "1" (expand "#{pane_dead_status}"))
          "pane_dead_status must expand to the recorded exit code")
      (is (string= "3927584461" (expand "#{pane_dead_time}"))
          "pane_dead_time must expand to the recorded universal-time")
      (is (string= "" (expand "#{pane_dead_signal}"))
          "pane_dead_signal must stay empty for a normally-exited pane"))))

(test format-terminal-state-vars-table
  "The terminal-state / selection / key-table format variables added from the
   tmux inventory diff expand from live screen state.
   Each row: (spec expected description) against a fresh fake session."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (h    (cl-tmux/terminal/types:screen-height
                (cl-tmux/model:pane-screen pane))))
    (flet ((expand (spec)
             (cl-tmux/format:expand-format
              spec (cl-tmux/format:format-context-from-session sess win pane))))
      (dolist (row `(("#{cursor_flag}"         "1"  "cursor visible by default")
                     ("#{insert_flag}"         "0"  "IRM off by default")
                     ("#{wrap_flag}"           "1"  "autowrap on by default")
                     ("#{origin_flag}"         "0"  "origin mode off")
                     ("#{alternate_on}"        "0"  "primary screen")
                     ("#{scroll_region_upper}" "0"  "scroll region top")
                     ("#{scroll_region_lower}" ,(format nil "~D" (1- h))
                                                    "scroll region bottom")
                     ("#{mouse_any_flag}"      "0"  "mouse reporting off")
                     ("#{rectangle_toggle}"    "0"  "rect select off")
                     ("#{client_key_table}"    "root" "root key table at rest")
                     ("#{window_marked_flag}"  "0"  "no marked pane")
                     ("#{pane_last}"           "0"  "no last pane yet")))
        (destructuring-bind (spec expected desc) row
          (is (string= expected (expand spec)) "~A: ~A" spec desc)))
      (is (plusp (parse-integer (expand "#{session_created}")))
          "session_created must be a construction timestamp")
      (is (string/= "" (expand "#{session_activity}"))
          "session_activity must expand")
      ;; A marked pane and copy-mode key table flip their flags.
      (setf (cl-tmux/model:pane-marked pane) t)
      (is (string= "1" (expand "#{window_marked_flag}"))
          "marking a pane must set window_marked_flag")
      (cl-tmux/commands:copy-mode-enter (cl-tmux/model:pane-screen pane))
      (is (string= "copy-mode" (expand "#{client_key_table}"))
          "copy mode must report the copy-mode key table"))))
