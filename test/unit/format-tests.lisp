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
