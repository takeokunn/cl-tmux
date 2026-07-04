(in-package #:cl-tmux/test)

;;;; shorthand table, %expand-brace edge cases, %truthy-p, %variable-to-keyword, pane/client vars, #(shell) — part IV

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
