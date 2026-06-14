(in-package #:cl-tmux/test)

;;;; shorthands, brace/conditional, format-context-from-session, window_flags, context keys, internal helpers — part I

(def-suite format-suite :description "Format string expansion")
(in-suite format-suite)

;;; ── Helper ───────────────────────────────────────────────────────────────────

(defun fmt (template &rest ctx-pairs)
  "Expand TEMPLATE against a plist context built from CTX-PAIRS."
  (cl-tmux/format:expand-format template (apply #'list ctx-pairs)))

;;; ── Single-character shorthands ─────────────────────────────────────────────

(test expand-format-hash-shorthands
  "Each #X shorthand expands to the correct context key value; ## yields a literal #."
  (dolist (c '(("#S" :session-name "mysession")
               ("#I" :window-index "2")
               ("#W" :window-name  "bash")
               ("#P" :pane-index   "1")
               ("#H" :hostname     "box")))
    (destructuring-bind (spec key val) c
      (is (string= val (fmt spec key val))
          "~S must expand to ~S (context ~S)" spec val key)))
  (is (string= "#" (fmt "##")) "## must expand to a literal #"))

;;; ── Brace variable form ──────────────────────────────────────────────────────

(test expand-format-brace-variable
  "#{session_name} expands via keyword lookup."
  (is (string= "main" (fmt "#{session_name}" :session-name "main"))))

(test expand-format-brace-missing-key-returns-empty
  "#{unknown} returns empty string when key is absent from context."
  (is (string= "" (fmt "#{no_such_key}"))))

;;; ── Conditional form ─────────────────────────────────────────────────────────

(test expand-format-conditional-table
  "#{?cond,true,false}: truthy condition → true branch; zero/empty → false branch."
  (dolist (c '(("#{?1,yes,no}" "yes" "truthy condition → true branch")
               ("#{?0,yes,no}" "no"  "zero condition → false branch")
               ("#{?,yes,no}"  "no"  "empty condition → false branch")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (fmt input)) "~A" desc))))

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
    (dolist (row (list (list (getf ctx :session-name) "" "session-name defaults to empty string")
                       (list (getf ctx :window-index)  0 "window-index defaults to 0")
                       (list (getf ctx :window-name)  "" "window-name defaults to empty string")
                       (list (getf ctx :pane-index)    0 "pane-index defaults to 0")))
      (destructuring-bind (actual expected desc) row
        (is (equal expected actual) "~A" desc)))))

;;; ── format-context-from-session with real objects ────────────────────────────

(test format-context-window-count-reflects-session-windows
  "format-context-from-session :window-count equals the number of windows in the session."
  (with-format-context (sess win pane ctx) (:nwindows 3)
    (is (= 3 (getf ctx :window-count))
        ":window-count expected 3 got ~D" (getf ctx :window-count))))

(test format-W-modifier-iterates-windows
  "#{W:fmt} expands fmt once per session window, joined by window-status-separator;
   the inner format sees a per-window context."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 3))
           (win  (cl-tmux/model:session-active-window sess))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      ;; Single format: repeated per window, joined by the default separator " ".
      (is (string= "X X X" (cl-tmux/format:expand-format "#{W:X}" ctx))
          "#{W:X} must repeat once per window")
      ;; Per-window context: each window's own index is expanded — 3 single-digit
      ;; indices joined by 2 separator spaces.
      (let ((out (cl-tmux/format:expand-format "#{W:#{window_index}}" ctx)))
        (is (= 2 (count #\Space out))
            "#{W:#{window_index}} must join 3 per-window values with 2 separators (got ~S)"
            out)))))

(test format-W-modifier-active-vs-inactive
  "#{W:active,inactive} applies the active format to the current window and the
   inactive format to the others."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 3))
           (win  (cl-tmux/model:session-active-window sess))
           (ctx  (cl-tmux/format:format-context-from-session sess win nil))
           (out  (cl-tmux/format:expand-format "#{W:A,I}" ctx)))
      (is (= 1 (count #\A out)) "exactly one (current) window is active (got ~S)" out)
      (is (= 2 (count #\I out)) "the other two windows are inactive (got ~S)" out))))

(test format-W-modifier-no-session-is-empty
  "#{W:...} with no session in the context yields the empty string (no error)."
  (is (string= "" (cl-tmux/format:expand-format
                   "#{W:X}" (cl-tmux/format:format-context-from-session nil nil nil)))))

(test format-P-modifier-iterates-panes
  "#{P:fmt} expands fmt once per pane of the current window, concatenated with no
   auto-separator (tmux's P: loop behaviour)."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 1 :npanes 3))
           (win  (cl-tmux/model:session-active-window sess))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "XXX" (cl-tmux/format:expand-format "#{P:X}" ctx))
          "#{P:X} must repeat once per pane, concatenated")
      (is (= 3 (length (cl-tmux/format:expand-format "#{P:#{pane_index}}" ctx)))
          "#{P:#{pane_index}} must emit one (single-digit) index per pane"))))

(test format-P-modifier-active-vs-inactive
  "#{P:active,inactive} applies the active format to the window's active pane and
   the inactive format to the others."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 1 :npanes 3))
           (win  (cl-tmux/model:session-active-window sess))
           (ctx  (cl-tmux/format:format-context-from-session sess win nil))
           (out  (cl-tmux/format:expand-format "#{P:A,I}" ctx)))
      (is (= 1 (count #\A out)) "exactly one (active) pane (got ~S)" out)
      (is (= 2 (count #\I out)) "the other two panes are inactive (got ~S)" out))))

(test format-P-modifier-no-window-is-empty
  "#{P:...} with no session/window in the context yields the empty string."
  (is (string= "" (cl-tmux/format:expand-format
                   "#{P:X}" (cl-tmux/format:format-context-from-session nil nil nil)))))

(test format-S-modifier-iterates-server-sessions
  "#{S:fmt} expands fmt once per server session; the context's current session is
   marked active.  Concatenated with no auto-separator."
  (with-isolated-config
    (let* ((s1  (make-fake-session :nwindows 1))
           (s2  (make-fake-session :nwindows 1))
           (cl-tmux::*server-sessions* (list (cons "a" s1) (cons "b" s2)))
           (ctx (cl-tmux/format:format-context-from-session
                 s1 (cl-tmux/model:session-active-window s1) nil)))
      (is (string= "XX" (cl-tmux/format:expand-format "#{S:X}" ctx))
          "#{S:X} must repeat once per server session")
      (let ((out (cl-tmux/format:expand-format "#{S:A,I}" ctx)))
        (is (= 1 (count #\A out)) "exactly the current session is active (got ~S)" out)
        (is (= 1 (count #\I out)) "the other session is inactive (got ~S)" out)))))

(test format-pane-pipe-reflects-pipe-state
  "#{pane_pipe} is '1' when the pane is being piped (pipe-pane active), else '0'."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (pane (cl-tmux/model:window-active-pane win)))
      (is (string= "0" (cl-tmux/format:expand-format
                        "#{pane_pipe}"
                        (cl-tmux/format:format-context-from-session sess win pane)))
          "#{pane_pipe} must be 0 with no pipe active")
      (setf (cl-tmux/model:pane-pipe-fd pane) (make-string-output-stream))
      (is (string= "1" (cl-tmux/format:expand-format
                        "#{pane_pipe}"
                        (cl-tmux/format:format-context-from-session sess win pane)))
          "#{pane_pipe} must be 1 when pipe-pane output is active")
      (setf (cl-tmux/model:pane-pipe-fd pane) nil))))

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
  (with-format-context (sess win pane ctx) ()
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

(test format-context-names-propagated
  "format-context-from-session propagates :session-name and :window-name correctly."
  (with-format-context (sess win pane ctx) ()
    (is (string= (cl-tmux/model:session-name sess) (getf ctx :session-name))
        ":session-name mismatch: expected ~S got ~S"
        (cl-tmux/model:session-name sess) (getf ctx :session-name))
    (is (string= (cl-tmux/model:window-name win) (getf ctx :window-name))
        ":window-name mismatch: expected ~S got ~S"
        (cl-tmux/model:window-name win) (getf ctx :window-name))))

;;; ── expand-format round-trips through format-context-from-session ────────────

(test expand-format-uses-window-count-from-context
  "#{window_count} expands to the window count injected via format-context-from-session."
  (with-format-context (sess win pane ctx) (:nwindows 4)
    (is (string= "4" (cl-tmux/format:expand-format "#{window_count}" ctx))
        "#{window_count}: expected \"4\" got ~S"
        (cl-tmux/format:expand-format "#{window_count}" ctx))))

;;; ── New context keys: time, host, host_short, window_flags, window_active ────

(test format-context-time-is-hhmm
  "format-context-from-session :time is a HH:MM-format 5-char string."
  (with-format-context (sess win pane ctx) ()
    (let ((t-str (getf ctx :time)))
      (is (= 5 (length t-str))
          ":time must be 5 chars (HH:MM), got ~D: ~S" (length t-str) t-str)
      (is (char= #\: (char t-str 2))
          ":time must have colon at position 2, got ~C" (char t-str 2)))))

(test format-context-host-fields
  ":host is a non-empty string; :host-short contains no dot."
  (with-format-context (sess win pane ctx) ()
    (is (plusp (length (getf ctx :host))) ":host must be non-empty")
    (is (null (find #\. (getf ctx :host-short)))
        ":host-short must not contain a dot, got ~S" (getf ctx :host-short))))

(test format-context-window-active-table
  ":window-active is \"1\" for the active window and \"0\" for an inactive window."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win1 (first wins))
         (win2 (second wins)))
    (is (string= "1" (getf (cl-tmux/format:format-context-from-session
                             sess win1 (first (cl-tmux/model:window-panes win1)))
                            :window-active))
        ":window-active must be \"1\" for active window")
    (is (string= "0" (getf (cl-tmux/format:format-context-from-session
                             sess win2 (first (cl-tmux/model:window-panes win2)))
                            :window-active))
        ":window-active must be \"0\" for inactive window")))

(test format-context-window-flags-table
  ":window-flags is \"*\" for the active window and \" \" for an inactive window."
  (let* ((sess (make-fake-session :nwindows 2))
         (wins (cl-tmux/model:session-windows sess))
         (win1 (first wins))
         (win2 (second wins)))
    (is (string= "*" (getf (cl-tmux/format:format-context-from-session
                             sess win1 (first (cl-tmux/model:window-panes win1)))
                            :window-flags))
        ":window-flags must be \"*\" for active window")
    (is (string= " " (getf (cl-tmux/format:format-context-from-session
                             sess win2 (first (cl-tmux/model:window-panes win2)))
                            :window-flags))
        ":window-flags must be \" \" for inactive window")))

(test expand-format-time-expands
  "#{time} expands to the :time value from context."
  (with-format-context (sess win pane ctx) ()
    (let ((result (cl-tmux/format:expand-format "#{time}" ctx)))
      (is (= 5 (length result))
          "#{time} should expand to 5-char HH:MM, got ~S" result))))

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

(test truthy-p-table
  "%truthy-p: non-empty/non-zero/non-false strings are truthy; empty, \"0\", \"false\" are not."
  (dolist (c '(("1"     t   "\"1\" is truthy")
               ("yes"   t   "\"yes\" is truthy")
               ("hello" t   "arbitrary non-empty string is truthy")
               (""      nil "empty string is not truthy")
               ("0"     nil "\"0\" is not truthy")
               ("false" nil "\"false\" is not truthy")
               ("FALSE" nil "\"FALSE\" is not truthy (case-insensitive)")))
    (destructuring-bind (input expected desc) c
      (if expected
          (is-true  (cl-tmux/format::%truthy-p input) "~A" desc)
          (is-false (cl-tmux/format::%truthy-p input) "~A" desc)))))

(test variable-to-keyword-table
  "%variable-to-keyword converts underscored names to hyphenated keywords and plain names to keywords."
  (dolist (c '(("session_name" :session-name "session_name → :session-name")
               ("window_index" :window-index "window_index → :window-index")
               ("pane_index"   :pane-index   "pane_index → :pane-index")
               ("time"         :time         "time → :time")
               ("host"         :host         "host → :host")))
    (destructuring-bind (input expected desc) c
      (is (eq expected (cl-tmux/format::%variable-to-keyword input))
          "~A" desc))))

(test split-conditional-table
  "%split-conditional parses the condition/true/false triple; missing branches default to empty."
  (dolist (c '(("1,yes,no"  "1"         "yes" "no"  "both branches")
               ("1,yes"     "1"         "yes" ""    "missing false branch → empty")
               ("something" "something" ""    ""    "no commas → condition only")))
    (destructuring-bind (input ec et ef desc) c
      (multiple-value-bind (cond-str true-str false-str)
          (cl-tmux/format::%split-conditional input)
        (is (string= ec cond-str)  "~A: condition" desc)
        (is (string= et true-str)  "~A: true branch" desc)
        (is (string= ef false-str) "~A: false branch" desc)))))

(test lookup-returns-empty-for-missing-key
  "%lookup returns an empty string when the key is absent from the context."
  (is (string= "" (cl-tmux/format::%lookup '() :missing-key))
      "%lookup must return \"\" when key is absent"))

(test lookup-returns-string-value
  "%lookup returns the princ-to-string representation of the stored value."
  (let ((ctx (list :count 42 :label "hello")))
    (dolist (c '((:count "42") (:label "hello")))
      (destructuring-bind (key expected) c
        (is (string= expected (cl-tmux/format::%lookup ctx key)) "~S" key)))))

(test short-hostname-table
  "%short-hostname strips the domain suffix, or passes through unchanged when there is no dot."
  (dolist (c '(("myhost.example.com" "myhost" "FQDN → short hostname")
               ("myhost"             "myhost" "no dot → full string unchanged")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/format::%short-hostname input))
          "~A: ~S → ~S" desc input expected))))
