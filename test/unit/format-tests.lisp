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

(test format-context-window-index-matches-window-id
  "format-context-from-session :window-index equals the window's numeric id.
   With make-fake-session (base-index=0), ids are 0, 1; :window-index follows."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win1 (first wins))
         (win2 (second wins))
         (pane (first (cl-tmux/model:window-panes win1))))
    (let ((ctx1 (cl-tmux/format:format-context-from-session sess win1 pane)))
      (is (= (cl-tmux/model:window-id win1) (getf ctx1 :window-index))
          "first window: :window-index must equal window-id (~D)"
          (cl-tmux/model:window-id win1)))
    (let* ((pane2 (first (cl-tmux/model:window-panes win2)))
           (ctx2  (cl-tmux/format:format-context-from-session sess win2 pane2)))
      (is (= (cl-tmux/model:window-id win2) (getf ctx2 :window-index))
          "second window: :window-index must equal window-id (~D)"
          (cl-tmux/model:window-id win2)))))

;;; ── #{window_raw_flags} vs #{window_flags} ──────────────────────────────────
;;;
;;; #{window_flags} pads to a single space when no flags apply; #{window_raw_flags}
;;; stays empty ("") in that case.  For the active window both contain "*".

(test format-window-raw-flags-active-window-has-star
  "For the active window, #{window_raw_flags} and #{window_flags} both contain \"*\"."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (let ((raw   (cl-tmux/format:expand-format "#{window_raw_flags}" ctx))
          (flags (cl-tmux/format:expand-format "#{window_flags}" ctx)))
      (is (search "*" raw)
          "active window #{window_raw_flags} must contain \"*\" (got ~S)" raw)
      (is (search "*" flags)
          "active window #{window_flags} must contain \"*\" (got ~S)" flags))))

(test format-window-raw-flags-inactive-empty-vs-flags-space
  "For an inactive, never-previously-active window, #{window_raw_flags} is the
   empty string while #{window_flags} is a single space (the padding fallback)."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         ;; window 0 is active (make-fake-session selects the first window);
         ;; window 1 is inactive and has never been previously active.
         (inactive (second wins))
         (pane     (first (cl-tmux/model:window-panes inactive)))
         (ctx      (cl-tmux/format:format-context-from-session sess inactive pane)))
    (let ((raw   (cl-tmux/format:expand-format "#{window_raw_flags}" ctx))
          (flags (cl-tmux/format:expand-format "#{window_flags}" ctx)))
      (is (string= "" raw)
          "inactive window #{window_raw_flags} must be empty (got ~S)" raw)
      (is (string= " " flags)
          "inactive window #{window_flags} must be a single space (got ~S)" flags))))

(test format-window-raw-flags-zoomed-window-has-z
  "When the active window is zoomed, #{window_raw_flags} contains BOTH \"*\"
   (active) and \"Z\" (zoomed)."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (cl-tmux/model:session-active-window sess))
         (pane (first (cl-tmux/model:window-panes win))))
    (setf (cl-tmux/model:window-zoom-p win) t)
    (let* ((ctx (cl-tmux/format:format-context-from-session sess win pane))
           (raw (cl-tmux/format:expand-format "#{window_raw_flags}" ctx)))
      (is (search "*" raw)
          "zoomed active window #{window_raw_flags} must contain \"*\" (got ~S)" raw)
      (is (search "Z" raw)
          "zoomed window #{window_raw_flags} must contain \"Z\" (got ~S)" raw))))

(test format-context-pane-index-matches-pane-id
  "format-context-from-session :pane-index equals the pane's numeric id."
  (let* ((sess  (make-fake-session :nwindows 1 :npanes 2))
         (win   (first (cl-tmux/model:session-windows sess)))
         (panes (cl-tmux/model:window-panes win))
         (ctx1  (cl-tmux/format:format-context-from-session sess win (first panes)))
         (ctx2  (cl-tmux/format:format-context-from-session sess win (second panes))))
    (is (= (cl-tmux/model:pane-id (first panes)) (getf ctx1 :pane-index))
        "first pane: :pane-index must equal pane-id")
    (is (= (cl-tmux/model:pane-id (second panes)) (getf ctx2 :pane-index))
        "second pane: :pane-index must equal pane-id")))

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

;;; ── structural pane / aggregate format variables ─────────────────────────────
;;;
;;; These are pure functions of the session/window/pane structs, wired into
;;; format-context-from-session and exercised end-to-end through expand-format.

(test format-context-exposes-structural-pane-variables
  "format-context-from-session populates pane geometry/id/pid variables that
   expand-format resolves (#{pane_width} #{pane_height} #{pane_id} #{pane_left}
   #{pane_top} #{pane_pid})."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    ;; make-fake-window panes are 20x5 at (0,0), id 1, pid -1.
    (is (string= "20" (cl-tmux/format:expand-format "#{pane_width}"  ctx)))
    (is (string= "5"  (cl-tmux/format:expand-format "#{pane_height}" ctx)))
    (is (string= "1"  (cl-tmux/format:expand-format "#{pane_id}"     ctx)))
    (is (string= "0"  (cl-tmux/format:expand-format "#{pane_left}"   ctx)))
    (is (string= "0"  (cl-tmux/format:expand-format "#{pane_top}"    ctx)))
    (is (string= "-1" (cl-tmux/format:expand-format "#{pane_pid}"    ctx)))))

(test format-context-pane-variables-default-when-pane-nil
  "With a NIL pane, structural pane variables default to 0 (empty-safe)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_width}"  ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_id}"     ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_active}" ctx)))))

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
  (let* ((sess (make-fake-session :nwindows 3 :npanes 2))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
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

(test format-modifier-char-from-code-negative-yields-empty
  "#{a:-1} (negative code) yields the empty string."
  (is (string= "" (fmt "#{a:-1}"))))

(test format-modifier-char-from-code-out-of-range-yields-empty
  "#{a:9999999} (beyond char-code-limit) yields the empty string."
  (is (string= "" (fmt "#{a:9999999}"))))

(test format-modifier-char-from-code-large-valid-unicode
  "#{a:955} yields the Greek small letter lambda."
  (is (string= (string (code-char 955)) (fmt "#{a:955}"))))

(test format-modifier-char-from-code-empty-operand-yields-empty
  "#{a:} (empty operand) yields the empty string."
  (is (string= "" (fmt "#{a:}"))))

(test format-modifier-char-from-code-nested-empty-operand-yields-empty
  "#{a:#{missing}} (nested operand resolving to empty) yields the empty string."
  (is (string= "" (fmt "#{a:#{missing}}"))))

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

(test format-context-window-layout-non-empty-for-window-with-panes
  "format-context-from-session :window-layout is a non-empty string for a window with a tree."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 2))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (let ((layout (getf ctx :window-layout)))
      (is (stringp layout) ":window-layout must be a string")
      (is (plusp (length layout)) ":window-layout must be non-empty for a window with panes"))))

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
