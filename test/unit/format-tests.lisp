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

(test format-context-session-name-propagated
  "format-context-from-session :session-name matches the session's name field."
  (with-format-context (sess win pane ctx) ()
    (is (string= (cl-tmux/model:session-name sess) (getf ctx :session-name))
        ":session-name mismatch: expected ~S got ~S"
        (cl-tmux/model:session-name sess) (getf ctx :session-name))))

(test format-context-window-name-propagated
  "format-context-from-session :window-name matches the window's name field."
  (with-format-context (sess win pane ctx) ()
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

(test format-context-host-is-non-empty
  "format-context-from-session :host is a non-empty string."
  (with-format-context (sess win pane ctx) ()
    (is (stringp (getf ctx :host))   ":host must be a string")
    (is (plusp (length (getf ctx :host))) ":host must be non-empty")))

(test format-context-host-short-no-dot
  "format-context-from-session :host-short contains no dot."
  (with-format-context (sess win pane ctx) ()
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
  (with-format-context (sess win pane ctx) ()
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
