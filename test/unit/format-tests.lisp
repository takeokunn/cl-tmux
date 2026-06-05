(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/format: expand-format and format-context-from-session.

(def-suite format-suite :description "Format string expansion")
(in-suite format-suite)

;;; ── Helper ───────────────────────────────────────────────────────────────────

(defun fmt (template &rest ctx-pairs)
  "Expand TEMPLATE against a plist context built from CTX-PAIRS."
  (cl-tmux/format:expand-format template (apply #'list ctx-pairs)))

;;; ── Single-character shorthands ─────────────────────────────────────────────

(test expand-format-hash-s
  "#S expands to :session-name from context."
  (is (string= "mysession" (fmt "#S" :session-name "mysession"))))

(test expand-format-hash-i
  "#I expands to :window-index from context."
  (is (string= "2" (fmt "#I" :window-index "2"))))

(test expand-format-hash-w
  "#W expands to :window-name from context."
  (is (string= "bash" (fmt "#W" :window-name "bash"))))

(test expand-format-hash-p
  "#P expands to :pane-index from context."
  (is (string= "1" (fmt "#P" :pane-index "1"))))

(test expand-format-hash-h
  "#H expands to :hostname from context."
  (is (string= "box" (fmt "#H" :hostname "box"))))

(test expand-format-hash-hash
  "## expands to a single literal #."
  (is (string= "#" (fmt "##"))))

;;; ── Brace variable form ──────────────────────────────────────────────────────

(test expand-format-brace-variable
  "#{session_name} expands via keyword lookup."
  (is (string= "main" (fmt "#{session_name}" :session-name "main"))))

(test expand-format-brace-missing-key-returns-empty
  "#{unknown} returns empty string when key is absent from context."
  (is (string= "" (fmt "#{no_such_key}"))))

;;; ── Conditional form ─────────────────────────────────────────────────────────

(test expand-format-conditional-true
  "#{?1,yes,no} returns the true branch."
  (is (string= "yes" (fmt "#{?1,yes,no}"))))

(test expand-format-conditional-false
  "#{?0,yes,no} returns the false branch."
  (is (string= "no" (fmt "#{?0,yes,no}"))))

(test expand-format-conditional-empty-is-false
  "#{?,yes,no} with empty cond returns the false branch."
  (is (string= "no" (fmt "#{?,yes,no}"))))

;;; ── Plain text and unknown specifiers ────────────────────────────────────────

(test expand-format-plain-text
  "Plain text without specifiers passes through unchanged."
  (is (string= "hello world" (fmt "hello world"))))

(test expand-format-unknown-specifier-kept-literally
  "An unrecognized #X sequence is kept as two literal characters."
  (is (string= "#Z" (fmt "#Z"))))

;;; ── SGR attribute passthrough ────────────────────────────────────────────────

(test expand-format-sgr-passthrough
  "#[fg=red] is passed through literally."
  (is (string= "#[fg=red]" (fmt "#[fg=red]"))))

;;; ── format-context-from-session ──────────────────────────────────────────────

(test format-context-nil-session-returns-defaults
  "format-context-from-session with all NIL args returns safe defaults."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "" (getf ctx :session-name)))
    (is (= 0 (getf ctx :window-index)))
    (is (string= "" (getf ctx :window-name)))
    (is (= 0 (getf ctx :pane-index)))))

;;; ── format-context-from-session with real objects ────────────────────────────

(test format-context-window-count-reflects-session-windows
  "format-context-from-session :window-count equals the number of windows in the session."
  (let* ((sess (make-fake-session :nwindows 3))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (= 3 (getf ctx :window-count))
        ":window-count expected 3 got ~D" (getf ctx :window-count))))

(test format-context-window-index-is-1-based
  "format-context-from-session :window-index is 1-based (first window → 1)."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win1 (first wins))
         (win2 (second wins))
         (pane (first (cl-tmux/model:window-panes win1))))
    (let ((ctx1 (cl-tmux/format:format-context-from-session sess win1 pane)))
      (is (= 1 (getf ctx1 :window-index))
          "first window: expected :window-index 1 got ~D" (getf ctx1 :window-index)))
    (let* ((pane2 (first (cl-tmux/model:window-panes win2)))
           (ctx2  (cl-tmux/format:format-context-from-session sess win2 pane2)))
      (is (= 2 (getf ctx2 :window-index))
          "second window: expected :window-index 2 got ~D" (getf ctx2 :window-index)))))

(test format-context-pane-index-is-1-based
  "format-context-from-session :pane-index is 1-based (first pane → 1)."
  (let* ((sess  (make-fake-session :nwindows 1 :npanes 2))
         (win   (first (cl-tmux/model:session-windows sess)))
         (panes (cl-tmux/model:window-panes win))
         (ctx1  (cl-tmux/format:format-context-from-session sess win (first panes)))
         (ctx2  (cl-tmux/format:format-context-from-session sess win (second panes))))
    (is (= 1 (getf ctx1 :pane-index))
        "first pane: expected :pane-index 1 got ~D" (getf ctx1 :pane-index))
    (is (= 2 (getf ctx2 :pane-index))
        "second pane: expected :pane-index 2 got ~D" (getf ctx2 :pane-index))))

(test format-context-session-name-propagated
  "format-context-from-session :session-name matches the session's name field."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= (cl-tmux/model:session-name sess) (getf ctx :session-name))
        ":session-name mismatch: expected ~S got ~S"
        (cl-tmux/model:session-name sess) (getf ctx :session-name))))

(test format-context-window-name-propagated
  "format-context-from-session :window-name matches the window's name field."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= (cl-tmux/model:window-name win) (getf ctx :window-name))
        ":window-name mismatch: expected ~S got ~S"
        (cl-tmux/model:window-name win) (getf ctx :window-name))))

;;; ── expand-format round-trips through format-context-from-session ────────────

(test expand-format-uses-window-count-from-context
  "#{window_count} expands to the window count injected via format-context-from-session."
  (let* ((sess (make-fake-session :nwindows 4))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "4" (cl-tmux/format:expand-format "#{window_count}" ctx))
        "#{window_count}: expected \"4\" got ~S"
        (cl-tmux/format:expand-format "#{window_count}" ctx))))

;;; ── New context keys: time, host, host_short, window_flags, window_active ────

(test format-context-time-is-hhmm
  "format-context-from-session :time is a HH:MM-format 5-char string."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane))
         (t-str (getf ctx :time)))
    (is (= 5 (length t-str))
        ":time must be 5 chars (HH:MM), got ~D: ~S" (length t-str) t-str)
    (is (char= #\: (char t-str 2))
        ":time must have colon at position 2, got ~C" (char t-str 2))))

(test format-context-host-is-non-empty
  "format-context-from-session :host is a non-empty string."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (stringp (getf ctx :host))   ":host must be a string")
    (is (plusp (length (getf ctx :host))) ":host must be non-empty")))

(test format-context-host-short-no-dot
  "format-context-from-session :host-short contains no dot."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (null (find #\. (getf ctx :host-short)))
        ":host-short must not contain a dot, got ~S" (getf ctx :host-short))))

(test format-context-window-active-for-active-window
  "format-context-from-session :window-active is \"1\" for the session's active window."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win  (first wins))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    ;; make-fake-session selects the first window
    (is (string= "1" (getf ctx :window-active))
        ":window-active expected \"1\" for active window, got ~S"
        (getf ctx :window-active))))

(test format-context-window-active-for-inactive-window
  "format-context-from-session :window-active is \"0\" for a non-active window."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win2 (second wins))
         (pane (first (cl-tmux/model:window-panes win2)))
         (ctx  (cl-tmux/format:format-context-from-session sess win2 pane)))
    (is (string= "0" (getf ctx :window-active))
        ":window-active expected \"0\" for inactive window, got ~S"
        (getf ctx :window-active))))

(test format-context-window-flags-active
  "format-context-from-session :window-flags is \"*\" for the active window."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "*" (getf ctx :window-flags))
        ":window-flags expected \"*\" for active window, got ~S"
        (getf ctx :window-flags))))

(test format-context-window-flags-inactive
  "format-context-from-session :window-flags is \" \" for an inactive window."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win2 (second wins))
         (pane (first (cl-tmux/model:window-panes win2)))
         (ctx  (cl-tmux/format:format-context-from-session sess win2 pane)))
    (is (string= " " (getf ctx :window-flags))
        ":window-flags expected \" \" for inactive window, got ~S"
        (getf ctx :window-flags))))

(test expand-format-time-expands
  "#{time} expands to the :time value from context."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane))
         (result (cl-tmux/format:expand-format "#{time}" ctx)))
    (is (= 5 (length result))
        "#{time} should expand to 5-char HH:MM, got ~S" result)))

(test format-context-from-window-works
  "format-context-from-window returns the same keys as format-context-from-session."
  (let* ((sess (make-fake-session :nwindows 2))
         (win  (first (cl-tmux/model:session-windows sess)))
         (ctx  (cl-tmux/format:format-context-from-window sess win)))
    (is (stringp (getf ctx :session-name))
        ":session-name must be a string")
    (is (stringp (getf ctx :window-name))
        ":window-name must be a string")
    (is (member (getf ctx :window-active) '("0" "1") :test #'string=)
        ":window-active must be \"0\" or \"1\"")))

;;; ── Internal helper unit tests ───────────────────────────────────────────────

(test truthy-p-non-empty-string-is-true
  "%truthy-p returns T for any non-empty, non-zero, non-false string."
  (is-true  (cl-tmux/format::%truthy-p "1")     "\"1\" is truthy")
  (is-true  (cl-tmux/format::%truthy-p "yes")   "\"yes\" is truthy")
  (is-true  (cl-tmux/format::%truthy-p "hello") "arbitrary non-empty string is truthy"))

(test truthy-p-falsy-strings
  "%truthy-p returns NIL for empty string, \"0\", and \"false\" (case-insensitive)."
  (is-false (cl-tmux/format::%truthy-p "")      "empty string is not truthy")
  (is-false (cl-tmux/format::%truthy-p "0")     "\"0\" is not truthy")
  (is-false (cl-tmux/format::%truthy-p "false") "\"false\" is not truthy")
  (is-false (cl-tmux/format::%truthy-p "FALSE") "\"FALSE\" is not truthy (case-insensitive)"))

(test variable-to-keyword-converts-underscores
  "%variable-to-keyword converts underscored names to hyphenated keywords."
  (is (eq :session-name (cl-tmux/format::%variable-to-keyword "session_name"))
      "session_name → :session-name")
  (is (eq :window-index (cl-tmux/format::%variable-to-keyword "window_index"))
      "window_index → :window-index")
  (is (eq :pane-index   (cl-tmux/format::%variable-to-keyword "pane_index"))
      "pane_index → :pane-index"))

(test variable-to-keyword-no-underscore-passes-through
  "%variable-to-keyword upcases a name with no underscores into a plain keyword."
  (is (eq :time (cl-tmux/format::%variable-to-keyword "time"))
      "time → :time")
  (is (eq :host (cl-tmux/format::%variable-to-keyword "host"))
      "host → :host"))

(test split-conditional-both-branches
  "%split-conditional parses ?cond,true,false into three parts."
  (multiple-value-bind (cond-str true-str false-str)
      (cl-tmux/format::%split-conditional "1,yes,no")
    (is (string= "1"   cond-str)  "condition part")
    (is (string= "yes" true-str)  "true branch")
    (is (string= "no"  false-str) "false branch")))

(test split-conditional-missing-false-branch
  "%split-conditional returns empty string for missing false branch."
  (multiple-value-bind (cond-str true-str false-str)
      (cl-tmux/format::%split-conditional "1,yes")
    (is (string= "1"   cond-str)  "condition part")
    (is (string= "yes" true-str)  "true branch")
    (is (string= ""    false-str) "false branch defaults to empty string")))

(test split-conditional-no-commas-returns-condition-only
  "%split-conditional with no commas returns the whole string as condition."
  (multiple-value-bind (cond-str true-str false-str)
      (cl-tmux/format::%split-conditional "something")
    (is (string= "something" cond-str) "whole input is condition")
    (is (string= ""          true-str) "true branch empty")
    (is (string= ""          false-str) "false branch empty")))

(test lookup-returns-empty-for-missing-key
  "%lookup returns an empty string when the key is absent from the context."
  (is (string= "" (cl-tmux/format::%lookup '() :missing-key))
      "%lookup must return \"\" when key is absent"))

(test lookup-returns-string-value
  "%lookup returns the princ-to-string representation of the stored value."
  (let ((ctx (list :count 42 :label "hello")))
    (is (string= "42"    (cl-tmux/format::%lookup ctx :count)) ":count → \"42\"")
    (is (string= "hello" (cl-tmux/format::%lookup ctx :label)) ":label → \"hello\"")))

(test short-hostname-strips-domain
  "%short-hostname returns the part before the first dot."
  (is (string= "myhost" (cl-tmux/format::%short-hostname "myhost.example.com"))
      "full FQDN → short host part"))

(test short-hostname-no-dot-returns-full-string
  "%short-hostname returns the full string when there is no dot."
  (is (string= "myhost" (cl-tmux/format::%short-hostname "myhost"))
      "no dot → full string unchanged"))

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

(test expand-format-client-width-from-context
  "#{client_width} expands to :client-width from context."
  (is (string= "220"
               (fmt "#{client_width}" :client-width 220))
      "#{client_width} must expand to the :client-width value"))

(test expand-format-client-height-from-context
  "#{client_height} expands to :client-height from context."
  (is (string= "55"
               (fmt "#{client_height}" :client-height 55))
      "#{client_height} must expand to the :client-height value"))

(test expand-format-client-tty-from-context
  "#{client_tty} expands to :client-tty from context."
  (is (string= "/dev/pts/0"
               (fmt "#{client_tty}" :client-tty "/dev/pts/0"))
      "#{client_tty} must expand to the :client-tty value"))

(test format-context-client-defaults-are-zero
  "format-context-from-session :client-width and :client-height default to 0."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (eql 0 (getf ctx :client-width))
        ":client-width must default to 0, got ~S" (getf ctx :client-width))
    (is (eql 0 (getf ctx :client-height))
        ":client-height must default to 0, got ~S" (getf ctx :client-height))
    (is (string= "" (getf ctx :client-tty))
        ":client-tty must default to empty string, got ~S" (getf ctx :client-tty))))

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
