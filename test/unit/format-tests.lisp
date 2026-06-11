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
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    ;; make-fake-session names the session "0".
    (is (string= (cl-tmux/model:session-name sess)
                 (cl-tmux/format:expand-format "#{client_session}" ctx))
        "#{client_session} must be the session name")))

(test format-context-client-pid-is-numeric
  "#{client_pid} expands to a non-empty numeric PID string (single-process model)."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane))
         (pid  (cl-tmux/format:expand-format "#{client_pid}" ctx)))
    (is (plusp (length pid)) "#{client_pid} must be non-empty")
    (is (every #'digit-char-p pid) "#{client_pid} must be all digits, got ~S" pid)))

(test format-context-client-termname-is-string
  "#{client_termname} expands to a string (the TERM env value or empty)."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
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

