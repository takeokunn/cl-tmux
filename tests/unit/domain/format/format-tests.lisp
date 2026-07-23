(in-package #:cl-tmux/test)

;;;; shorthands, brace/conditional, format-context-from-session, window_flags, context keys, internal helpers — part I

(describe "format-suite"

  ;; ── Helper ───────────────────────────────────────────────────────────────────

  (defun fmt (template &rest ctx-pairs)
    "Expand TEMPLATE against a plist context built from CTX-PAIRS."
    (cl-tmux/format:expand-format template (apply #'list ctx-pairs)))

  ;; ── Single-character shorthands ─────────────────────────────────────────────

  ;; Each #X shorthand expands to the correct context key value; ## yields a literal #.
  (it "expand-format-hash-shorthands"
    (dolist (c '(("#S" :session-name "mysession")
                 ("#I" :window-index "2")
                 ("#W" :window-name  "bash")
                 ("#P" :pane-index   "1")
                 ("#H" :hostname     "box")))
      (destructuring-bind (spec key val) c
        (expect (string= val (fmt spec key val)))))
    (expect (string= "#" (fmt "##"))))

  ;; ── Brace variable form ──────────────────────────────────────────────────────

  ;; #{session_name} expands via keyword lookup.
  (it "expand-format-brace-variable"
    (expect (string= "main" (fmt "#{session_name}" :session-name "main"))))

  ;; #{unknown} returns empty string when key is absent from context.
  (it "expand-format-brace-missing-key-returns-empty"
    (expect (string= "" (fmt "#{no_such_key}"))))

  ;; ── Conditional form ─────────────────────────────────────────────────────────

  ;; #{?cond,true,false}: truthy condition → true branch; zero/empty → false branch.
  (it "expand-format-conditional-table"
    (dolist (c '(("#{?1,yes,no}" "yes" "truthy condition → true branch")
                 ("#{?0,yes,no}" "no"  "zero condition → false branch")
                 ("#{?,yes,no}"  "no"  "empty condition → false branch")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt input))))))

  ;; ── Plain text and unknown specifiers ────────────────────────────────────────

  ;; Plain text without specifiers passes through unchanged.
  (it "expand-format-plain-text"
    (expect (string= "hello world" (fmt "hello world"))))

  ;; An unrecognized #X sequence is kept as two literal characters.
  (it "expand-format-unknown-specifier-kept-literally"
    (expect (string= "#Z" (fmt "#Z"))))

  ;; ── SGR attribute passthrough ────────────────────────────────────────────────

  ;; #[fg=red] is passed through literally.
  (it "expand-format-sgr-passthrough"
    (expect (string= "#[fg=red]" (fmt "#[fg=red]"))))

  ;; ── format-context-from-session ──────────────────────────────────────────────

  ;; format-context-from-session with all NIL args returns safe defaults.
  (it "format-context-nil-session-returns-defaults"
    (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
      (check-table (list (list (getf ctx :session-name) "" "session-name defaults to empty string")
                         (list (getf ctx :window-index)  0 "window-index defaults to 0")
                         (list (getf ctx :window-name)  "" "window-name defaults to empty string")
                         (list (getf ctx :pane-index)    0 "pane-index defaults to 0"))
                   :test #'equal)))

  ;; ── format-context-from-session with real objects ────────────────────────────

  ;; format-context-from-session :window-count equals the number of windows in the session.
  (it "format-context-window-count-reflects-session-windows"
    (with-format-context (sess win pane ctx) (:nwindows 3)
      (expect (= 3 (getf ctx :window-count)))))

  ;; #{W:fmt} expands fmt once per session window, joined by window-status-separator;
  ;; the inner format sees a per-window context.
  (it "format-W-modifier-iterates-windows"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 3))
             (win  (cl-tmux/model:session-active-window sess))
             (pane (first (cl-tmux/model:window-panes win)))
             (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
        ;; Single format: repeated per window, joined by the default separator " ".
        (expect (string= "X X X" (cl-tmux/format:expand-format "#{W:X}" ctx)))
        ;; Per-window context: each window's own index is expanded — 3 single-digit
        ;; indices joined by 2 separator spaces.
        (let ((out (cl-tmux/format:expand-format "#{W:#{window_index}}" ctx)))
          (expect (= 2 (count #\Space out)))))))

  ;; #{W:active,inactive} applies the active format to the current window and the
  ;; inactive format to the others.
  (it "format-W-modifier-active-vs-inactive"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 3))
             (win  (cl-tmux/model:session-active-window sess))
             (ctx  (cl-tmux/format:format-context-from-session sess win nil))
             (out  (cl-tmux/format:expand-format "#{W:A,I}" ctx)))
        (expect (= 1 (count #\A out)))
        (expect (= 2 (count #\I out))))))

  ;; #{W:...} with no session in the context yields the empty string (no error).
  (it "format-W-modifier-no-session-is-empty"
    (expect (string= "" (cl-tmux/format:expand-format
                         "#{W:X}" (cl-tmux/format:format-context-from-session nil nil nil)))))

  ;; #{P:fmt} expands fmt once per pane of the current window, concatenated with no
  ;; auto-separator (tmux's P: loop behaviour).
  (it "format-P-modifier-iterates-panes"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 1 :npanes 3))
             (win  (cl-tmux/model:session-active-window sess))
             (pane (first (cl-tmux/model:window-panes win)))
             (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
        (expect (string= "XXX" (cl-tmux/format:expand-format "#{P:X}" ctx)))
        (expect (= 3 (length (cl-tmux/format:expand-format "#{P:#{pane_index}}" ctx)))))))

  ;; #{P:active,inactive} applies the active format to the window's active pane and
  ;; the inactive format to the others.
  (it "format-P-modifier-active-vs-inactive"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 1 :npanes 3))
             (win  (cl-tmux/model:session-active-window sess))
             (ctx  (cl-tmux/format:format-context-from-session sess win nil))
             (out  (cl-tmux/format:expand-format "#{P:A,I}" ctx)))
        (expect (= 1 (count #\A out)))
        (expect (= 2 (count #\I out))))))

  ;; #{P:...} with no session/window in the context yields the empty string.
  (it "format-P-modifier-no-window-is-empty"
    (expect (string= "" (cl-tmux/format:expand-format
                         "#{P:X}" (cl-tmux/format:format-context-from-session nil nil nil)))))

  ;; #{S:fmt} expands fmt once per server session; the context's current session is
  ;; marked active.  Concatenated with no auto-separator.
  (it "format-S-modifier-iterates-server-sessions"
    (with-isolated-config
      (let* ((s1  (make-fake-session :nwindows 1))
             (s2  (make-fake-session :nwindows 1))
             (cl-tmux::*server-sessions* (list (cons "a" s1) (cons "b" s2)))
             (ctx (cl-tmux/format:format-context-from-session
                   s1 (cl-tmux/model:session-active-window s1) nil)))
        (expect (string= "XX" (cl-tmux/format:expand-format "#{S:X}" ctx)))
        (let ((out (cl-tmux/format:expand-format "#{S:A,I}" ctx)))
          (expect (= 1 (count #\A out)))
          (expect (= 1 (count #\I out)))))))

  ;; #{pane_pipe} is '1' when the pane is being piped (pipe-pane active), else '0'.
  (it "format-pane-pipe-reflects-pipe-state"
    (with-isolated-config
      (let* ((sess (make-fake-session :nwindows 1))
             (win  (cl-tmux/model:session-active-window sess))
             (pane (cl-tmux/model:window-active-pane win)))
        (expect (string= "0" (cl-tmux/format:expand-format
                              "#{pane_pipe}"
                              (cl-tmux/format:format-context-from-session sess win pane))))
        (setf (cl-tmux/model:pane-pipe-output-stream pane) (make-string-output-stream))
        (expect (string= "1" (cl-tmux/format:expand-format
                              "#{pane_pipe}"
                              (cl-tmux/format:format-context-from-session sess win pane))))
        (setf (cl-tmux/model:pane-pipe-output-stream pane) nil))))

  ;; format-context-from-session :window-index equals the window's numeric id.
  ;; With make-fake-session (base-index=0), ids are 0, 1; :window-index follows.
  (it "format-context-window-index-matches-window-id"
    (let* ((sess (make-fake-session :nwindows 2))
           (wins (cl-tmux/model:session-windows sess))
           (win1 (first wins))
           (win2 (second wins))
           (pane (first (cl-tmux/model:window-panes win1))))
      (let ((ctx1 (cl-tmux/format:format-context-from-session sess win1 pane)))
        (expect (= (cl-tmux/model:window-id win1) (getf ctx1 :window-index))))
      (let* ((pane2 (first (cl-tmux/model:window-panes win2)))
             (ctx2  (cl-tmux/format:format-context-from-session sess win2 pane2)))
        (expect (= (cl-tmux/model:window-id win2) (getf ctx2 :window-index))))))

  ;; ── #{window_raw_flags} vs #{window_flags} ──────────────────────────────────
  ;;
  ;; #{window_flags} pads to a single space when no flags apply; #{window_raw_flags}
  ;; stays empty ("") in that case.  For the active window both contain "*".

  ;; For the active window, #{window_raw_flags} and #{window_flags} both contain "*".
  (it "format-window-raw-flags-active-window-has-star"
    (with-format-context (sess win pane ctx) ()
      (let ((raw   (cl-tmux/format:expand-format "#{window_raw_flags}" ctx))
            (flags (cl-tmux/format:expand-format "#{window_flags}" ctx)))
        (expect (search "*" raw))
        (expect (search "*" flags)))))

  ;; For an inactive, never-previously-active window, #{window_raw_flags} is the
  ;; empty string while #{window_flags} is a single space (the padding fallback).
  (it "format-window-raw-flags-inactive-empty-vs-flags-space"
    (let* ((sess (make-fake-session :nwindows 2))
           (wins (cl-tmux/model:session-windows sess))
           ;; window 0 is active (make-fake-session selects the first window);
           ;; window 1 is inactive and has never been previously active.
           (inactive (second wins))
           (pane     (first (cl-tmux/model:window-panes inactive)))
           (ctx      (cl-tmux/format:format-context-from-session sess inactive pane)))
      (let ((raw   (cl-tmux/format:expand-format "#{window_raw_flags}" ctx))
            (flags (cl-tmux/format:expand-format "#{window_flags}" ctx)))
        (expect (string= "" raw))
        (expect (string= " " flags)))))

  ;; When the active window is zoomed, #{window_raw_flags} contains BOTH "*"
  ;; (active) and "Z" (zoomed).
  (it "format-window-raw-flags-zoomed-window-has-z"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (pane (first (cl-tmux/model:window-panes win))))
      (setf (cl-tmux/model:window-zoom-p win) t)
      (let* ((ctx (cl-tmux/format:format-context-from-session sess win pane))
             (raw (cl-tmux/format:expand-format "#{window_raw_flags}" ctx)))
        (expect (search "*" raw))
        (expect (search "Z" raw)))))

  ;; format-context-from-session :pane-index equals the pane's numeric id.
  (it "format-context-pane-index-matches-pane-id"
    (let* ((sess  (make-fake-session :nwindows 1 :npanes 2))
           (win   (first (cl-tmux/model:session-windows sess)))
           (panes (cl-tmux/model:window-panes win))
           (ctx1  (cl-tmux/format:format-context-from-session sess win (first panes)))
           (ctx2  (cl-tmux/format:format-context-from-session sess win (second panes))))
      (expect (= (cl-tmux/model:pane-id (first panes)) (getf ctx1 :pane-index)))
      (expect (= (cl-tmux/model:pane-id (second panes)) (getf ctx2 :pane-index)))))

  ;; format-context-from-session propagates :session-name and :window-name correctly.
  (it "format-context-names-propagated"
    (with-format-context (sess win pane ctx) ()
      (expect (string= (cl-tmux/model:session-name sess) (getf ctx :session-name)))
      (expect (string= (cl-tmux/model:window-name win) (getf ctx :window-name)))))

  ;; ── expand-format round-trips through format-context-from-session ────────────

  ;; #{window_count} expands to the window count injected via format-context-from-session.
  (it "expand-format-uses-window-count-from-context"
    (with-format-context (sess win pane ctx) (:nwindows 4)
      (expect (string= "4" (cl-tmux/format:expand-format "#{window_count}" ctx)))))

  ;; ── New context keys: time, host, host_short, window_flags, window_active ────

  ;; format-context-from-session :time is a HH:MM-format 5-char string.
  (it "format-context-time-is-hhmm"
    (with-format-context (sess win pane ctx) ()
      (let ((t-str (getf ctx :time)))
        (expect (= 5 (length t-str)))
        (expect (char= #\: (char t-str 2))))))

  ;; :host is a non-empty string; :host-short contains no dot.
  (it "format-context-host-fields"
    (with-format-context (sess win pane ctx) ()
      (expect (plusp (length (getf ctx :host))))
      (expect (null (find #\. (getf ctx :host-short))))))

  ;; :window-active is "1" for the active window and "0" for an inactive window.
  (it "format-context-window-active-table"
    (let* ((sess (make-fake-session :nwindows 2))
           (wins (cl-tmux/model:session-windows sess))
           (win1 (first wins))
           (win2 (second wins)))
      (expect (string= "1" (getf (cl-tmux/format:format-context-from-session
                                   sess win1 (first (cl-tmux/model:window-panes win1)))
                                  :window-active)))
      (expect (string= "0" (getf (cl-tmux/format:format-context-from-session
                                   sess win2 (first (cl-tmux/model:window-panes win2)))
                                  :window-active)))))

  ;; :window-flags is "*" for the active window and " " for an inactive window.
  (it "format-context-window-flags-table"
    (let* ((sess (make-fake-session :nwindows 2))
           (wins (cl-tmux/model:session-windows sess))
           (win1 (first wins))
           (win2 (second wins)))
      (expect (string= "*" (getf (cl-tmux/format:format-context-from-session
                                   sess win1 (first (cl-tmux/model:window-panes win1)))
                                  :window-flags)))
      (expect (string= " " (getf (cl-tmux/format:format-context-from-session
                                   sess win2 (first (cl-tmux/model:window-panes win2)))
                                  :window-flags)))))

  ;; #{time} expands to the :time value from context.
  (it "expand-format-time-expands"
    (with-format-context (sess win pane ctx) ()
      (let ((result (cl-tmux/format:expand-format "#{time}" ctx)))
        (expect (= 5 (length result))))))

  ;; format-context-from-window returns the same keys as format-context-from-session.
  (it "format-context-from-window-works"
    (let* ((sess (make-fake-session :nwindows 2))
           (win  (first (cl-tmux/model:session-windows sess)))
           (ctx  (cl-tmux/format:format-context-from-window sess win)))
      (expect (stringp (getf ctx :session-name)))
      (expect (stringp (getf ctx :window-name)))
      (expect (member (getf ctx :window-active) '("0" "1") :test #'string=))))

  ;; ── Internal helper unit tests ───────────────────────────────────────────────

  ;; %truthy-p matches tmux format_true: any non-empty string is truthy except the
  ;; single character "0"; "false" is truthy (only empty and "0" are false).
  (it "truthy-p-table"
    (dolist (c '(("1"     t   "\"1\" is truthy")
                 ("yes"   t   "\"yes\" is truthy")
                 ("hello" t   "arbitrary non-empty string is truthy")
                 (""      nil "empty string is not truthy")
                 ("0"     nil "\"0\" is not truthy")
                 ("00"    t   "\"00\" is truthy (only the single char \"0\" is false)")
                 ("false" t   "\"false\" is truthy in tmux (format_true)")
                 ("FALSE" t   "\"FALSE\" is truthy (the word is not special)")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (if expected
            (expect (cl-tmux/format::%truthy-p input) :to-be-truthy)
            (expect (cl-tmux/format::%truthy-p input) :to-be-falsy)))))

  ;; %variable-to-keyword converts underscored names to hyphenated keywords and plain names to keywords.
  (it "variable-to-keyword-table"
    (dolist (c '(("session_name" :session-name "session_name → :session-name")
                 ("window_index" :window-index "window_index → :window-index")
                 ("pane_index"   :pane-index   "pane_index → :pane-index")
                 ("time"         :time         "time → :time")
                 ("host"         :host         "host → :host")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (eq expected (cl-tmux/format::%variable-to-keyword input))))))

  ;; %split-conditional parses the condition/true/false triple; missing branches default to empty.
  (it "split-conditional-table"
    (dolist (c '(("1,yes,no"  "1"         "yes" "no"  "both branches")
                 ("1,yes"     "1"         "yes" ""    "missing false branch → empty")
                 ("something" "something" ""    ""    "no commas → condition only")))
      (destructuring-bind (input ec et ef desc) c
        (declare (ignore desc))
        (multiple-value-bind (cond-str true-str false-str)
            (cl-tmux/format::%split-conditional input)
          (expect (string= ec cond-str))
          (expect (string= et true-str))
          (expect (string= ef false-str))))))

  ;; %lookup returns an empty string when the key is absent from the context.
  (it "lookup-returns-empty-for-missing-key"
    (expect (string= "" (cl-tmux/format::%lookup '() :missing-key))))

  ;; %lookup returns the princ-to-string representation of the stored value.
  (it "lookup-returns-string-value"
    (let ((ctx (list :count 42 :label "hello")))
      (dolist (c '((:count "42") (:label "hello")))
        (destructuring-bind (key expected) c
          (expect (string= expected (cl-tmux/format::%lookup ctx key)))))))

  ;; %short-hostname strips the domain suffix, or passes through unchanged when there is no dot.
  (it "short-hostname-table"
    (dolist (c '(("myhost.example.com" "myhost" "FQDN → short hostname")
                 ("myhost"             "myhost" "no dot → full string unchanged")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/format::%short-hostname input)))))))
