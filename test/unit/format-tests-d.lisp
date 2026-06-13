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

(test expand-format-brace-no-closing-brace-emits-literal-hash
  "%expand-brace emits a literal '#' when there is no closing brace."
  ;; The '#' at the end of the template has no following character → plain char.
  ;; The string "#{no_close" has '#' + '{' but no '}'; the function returns '#' literally.
  (let ((result (cl-tmux/format:expand-format "#{no_close" '())))
    (is (char= #\# (char result 0))
        "missing closing brace must emit literal '#' (got ~S)" result)))

(test expand-format-bracket-no-closing-bracket-emits-literal-hash
  "%expand-bracket emits a literal '#' when there is no closing bracket."
  (let ((result (cl-tmux/format:expand-format "#[no_close" '())))
    (is (char= #\# (char result 0))
        "missing closing bracket must emit literal '#' (got ~S)" result)))

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
  (is (string= "#Z" (fmt "#Z")) "#Z must pass through as two literal chars")
  (is (string= "#?" (fmt "#?")) "#? must pass through as two literal chars"))

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
  (let ((cases '(("1"     . t)
                 ("yes"   . t)
                 ("true"  . t)
                 (""      . nil)
                 ("0"     . nil)
                 ("false" . nil)
                 ("FALSE" . nil))))
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

(test expand-format-pane-title-from-context
  "#{pane_title} expands to :pane-title from context."
  (is (string= "mytitle"
               (fmt "#{pane_title}" :pane-title "mytitle"))
      "#{pane_title} must expand to the :pane-title value"))

(test expand-format-pane-title-missing-returns-empty
  "#{pane_title} returns empty string when :pane-title is absent."
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

(test expand-format-shell-cmd-echo
  "#(echo hello) expands to the command output."
  (let ((result (fmt "#(echo hello)")))
    (is (string= "hello" result)
        "#(echo hello) must expand to \"hello\", got ~S" result)))

(test expand-format-shell-cmd-no-trailing-newline
  "#(printf foo) does not add a trailing newline."
  (let ((result (fmt "#(printf '%s' foo)")))
    (is (string= "foo" result)
        "#(printf '%%s' foo) must expand to \"foo\" without newline, got ~S" result)))

(test expand-format-shell-cmd-error-returns-empty
  "#(false) (failing command) returns empty string without signalling."
  (let ((result (fmt "#(false)")))
    (is (stringp result) "#(false) result must be a string")
    (is (string= "" result) "#(false) must return empty string on non-zero exit")))

(test expand-format-shell-cmd-no-close-paren-emits-literal-hash
  "#( with no closing paren emits a literal '#' and does not crash."
  (let ((result (cl-tmux/format:expand-format "#(no_close" '())))
    (is (char= #\# (char result 0))
        "missing closing paren must emit literal '#' (got ~S)" result)))

(test expand-format-shell-cmd-mixed-with-text
  "#(echo ok) embedded in a longer format string expands inline."
  (let ((result (fmt "status: #(echo ok) done")))
    (is (string= "status: ok done" result)
        "shell cmd must expand inline, got ~S" result)))

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
   #{pane_top} #{pane_pid})."
  (with-format-context (sess win pane ctx) ()
    ;; make-fake-window panes are 20x5 at (0,0), id 1, pid -1.
    (is (string= "20" (cl-tmux/format:expand-format "#{pane_width}"  ctx)))
    (is (string= "5"  (cl-tmux/format:expand-format "#{pane_height}" ctx)))
    (is (string= "1"  (cl-tmux/format:expand-format "#{pane_id}"     ctx)))
    (is (string= "0"  (cl-tmux/format:expand-format "#{pane_left}"   ctx)))
    (is (string= "0"  (cl-tmux/format:expand-format "#{pane_top}"    ctx)))
    ;; #{pane_right}/#{pane_bottom}: inclusive far edge = origin + size - 1.
    ;; A 20x5 pane at (0,0) → right column 19, bottom row 4.
    (is (string= "19" (cl-tmux/format:expand-format "#{pane_right}"  ctx)))
    (is (string= "4"  (cl-tmux/format:expand-format "#{pane_bottom}" ctx)))
    (is (string= "-1" (cl-tmux/format:expand-format "#{pane_pid}"    ctx)))))

(test format-context-pane-variables-default-when-pane-nil
  "With a NIL pane, structural pane variables default to 0 (empty-safe)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_width}"  ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_id}"     ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_right}"  ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_bottom}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_active}" ctx)))))

(test window-bell-flag-respects-monitor-bell
  "#{window_bell_flag} shows ! only when monitor-bell is on (default); monitor-bell
   off suppresses the bell alert even with a pending bell."
  (with-fresh-options
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win))))
      (setf (cl-tmux/terminal/types:screen-bell-pending
             (cl-tmux/model:pane-screen pane)) t)
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
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_active}" ctx-a))
          "active pane → #{pane_active} 1")
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_active}" ctx-i))
          "inactive pane → #{pane_active} 0")
      (is (string= "HERE" (cl-tmux/format:expand-format "#{?pane_active,HERE,away}" ctx-a))
          "conditional picks the true branch for the active pane")
      (is (string= "away" (cl-tmux/format:expand-format "#{?pane_active,HERE,away}" ctx-i))
          "conditional picks the false branch for an inactive pane"))))

(test format-context-window-panes-and-session-windows-counts
  "#{window_panes} is the pane count; #{session_windows} is the window count."
  (with-format-context (sess win pane ctx) (:nwindows 3 :npanes 2)
    (is (string= "2" (cl-tmux/format:expand-format "#{window_panes}" ctx))
        "window has 2 panes")
    (is (string= "3" (cl-tmux/format:expand-format "#{session_windows}" ctx))
        "session has 3 windows")))

;;; ── Format modifiers: #{=N:var} #{=-N:var} #{b:var} #{d:var} ─────────────────

(test format-modifier-truncate-left
  "#{=N:var} keeps the first N characters of the resolved value."
  (is (string= "veryl" (fmt "#{=5:window_name}" :window-name "verylongname"))))

(test format-modifier-truncate-right
  "#{=-N:var} keeps the last N characters of the resolved value."
  (is (string= "gname" (fmt "#{=-5:window_name}" :window-name "verylongname"))))

(test format-modifier-truncate-shorter-than-limit-unchanged
  "#{=N:var} leaves a value shorter than N untouched."
  (is (string= "short" (fmt "#{=20:window_name}" :window-name "short"))))

(test format-modifier-logical-or
  "#{||:a,b} returns 1 when either operand is truthy, else 0."
  (is (string= "1" (fmt "#{||:1,0}")) "1 || 0 → 1")
  (is (string= "1" (fmt "#{||:0,1}")) "0 || 1 → 1")
  (is (string= "0" (fmt "#{||:0,0}")) "0 || 0 → 0")
  (is (string= "0" (fmt "#{||:,}"))   "empty || empty → 0")
  (is (string= "1" (fmt "#{||:#{a},#{b}}" :a "" :b "x"))
      "operands expand as formats before the truthiness test"))

(test format-modifier-logical-and
  "#{&&:a,b} returns 1 only when both operands are truthy."
  (is (string= "1" (fmt "#{&&:1,1}")) "1 && 1 → 1")
  (is (string= "0" (fmt "#{&&:1,0}")) "1 && 0 → 0")
  (is (string= "0" (fmt "#{&&:0,1}")) "0 && 1 → 0")
  (is (string= "0" (fmt "#{&&:0,0}")) "0 && 0 → 0"))

(test format-modifier-logical-nested-in-conditional
  "#{?#{||:cond1,cond2},yes,no} chooses the branch by the logical result."
  (is (string= "yes" (fmt "#{?#{||:#{a},#{b}},yes,no}" :a "" :b "1")))
  (is (string= "no"  (fmt "#{?#{&&:#{a},#{b}},yes,no}" :a "" :b "1"))))

(test format-modifier-quote
  "#{q:var} backslash-escapes shell-special characters in the resolved value."
  (is (string= "a\\ b"  (fmt "#{q:p}" :p "a b"))   "space is escaped")
  (is (string= "a\\;b"  (fmt "#{q:p}" :p "a;b"))   "semicolon is escaped")
  (is (string= "plain"  (fmt "#{q:p}" :p "plain")) "ordinary text is unchanged"))

(test format-modifier-char-from-code
  "#{a:N} yields the single character whose character code is N."
  (is (string= "#" (fmt "#{a:35}"))  "code 35 is '#'")
  (is (string= "A" (fmt "#{a:65}"))  "code 65 is 'A'")
  (is (string= "a" (fmt "#{a:97}"))  "code 97 is 'a'")
  (is (string= "B" (fmt "#{a:#{code}}" :code "66"))
      "operand may be a nested format resolving to a number")
  (is (string= "" (fmt "#{a:notanumber}"))
      "a non-numeric operand yields the empty string"))

(test format-modifier-char-from-code-zero-is-nul
  "#{a:0} yields a length-1 string whose char is #\\Nul."
  (let ((result (fmt "#{a:0}")))
    (is (= 1 (length result)))
    (is (char= #\Nul (char result 0)))))

(test format-modifier-char-from-code-invalid-operands-yield-empty
  "Non-numeric, negative, out-of-range, empty, and nested-empty operands all yield empty."
  (dolist (spec '("#{a:-1}" "#{a:9999999}" "#{a:}" "#{a:#{missing}}"))
    (is (string= "" (fmt spec)) "~S must yield the empty string" spec)))

(test format-modifier-char-from-code-large-valid-unicode
  "#{a:955} yields the Greek small letter lambda."
  (is (string= (string (code-char 955)) (fmt "#{a:955}"))))

(test format-modifier-basename
  "#{b:var} yields the final path component of the resolved value."
  (is (string= "project"
               (fmt "#{b:pane_current_path}" :pane-current-path "/home/user/project")))
  (is (string= "b" (fmt "#{b:p}" :p "/a/b/"))   "trailing slash is stripped first")
  (is (string= "foo" (fmt "#{b:p}" :p "foo"))   "a bare name is its own basename"))

(test format-modifier-dirname
  "#{d:var} yields the directory part of the resolved value."
  (is (string= "/home/user"
               (fmt "#{d:pane_current_path}" :pane-current-path "/home/user/project")))
  (is (string= "." (fmt "#{d:p}" :p "foo"))   "no slash → current dir")
  (is (string= "/" (fmt "#{d:p}" :p "/foo"))  "top-level → root"))

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

