(in-package #:cl-tmux/test)

;;;; shorthand table, %expand-brace edge cases, %truthy-p, %variable-to-keyword, pane/client vars, #(shell) — part IV

(describe "format-suite"

  ;;; ── Table-driven shorthand expansion ─────────────────────────────────────────

  ;; All single-character shorthands expand to the correct context value.
  (it "expand-format-all-shorthands-table"
    (let ((cases '(("#S" :session-name "sess1" "sess1")
                   ("#I" :window-index "3"     "3")
                   ("#W" :window-name  "bash"  "bash")
                   ("#P" :pane-index   "2"     "2")
                   ("#H" :hostname     "box"   "box")
                   ("##" nil           nil     "#"))))
      (dolist (c cases)
        (destructuring-bind (tmpl key val expected) c
          (let ((ctx (if key (list key val) '())))
            (expect (string= expected (cl-tmux/format:expand-format tmpl ctx))))))))

  ;;; ── %expand-brace edge cases ──────────────────────────────────────────────────

  ;; #{, #[, and #( without a closing delimiter each emit a literal '#' and do not crash.
  (it "expand-format-unclosed-delimiter-emits-literal-hash"
    (dolist (c '(("#{no_close" "brace without closing }")
                 ("#[no_close" "bracket without closing ]")
                 ("#(no_close" "paren without closing )")))
      (destructuring-bind (input desc) c
        (declare (ignore desc))
        (let ((result (cl-tmux/format:expand-format input '())))
          (expect (char= #\# (char result 0)))))))

  ;; #[attrs] passes the full bracketed text through unchanged.
  (it "expand-format-bracket-passthrough-preserves-content"
    (expect (string= "#[fg=red,bold]" (fmt "#[fg=red,bold]"))))

  ;; #{?window_active,YES,NO} resolves window_active from the context plist.
  (it "expand-format-conditional-context-variable-resolution"
    ;; When :window-active is "1" in the context, the true branch is returned.
    (expect (string= "YES"
                 (cl-tmux/format:expand-format "#{?window_active,YES,NO}"
                                               '(:window-active "1"))))
    (expect (string= "NO"
                 (cl-tmux/format:expand-format "#{?window_active,YES,NO}"
                                               '(:window-active "0")))))

  ;; #{?1,yes,no} / #{?0,yes,no} work without any context.
  (it "expand-format-conditional-literal-true-zero"
    (expect (string= "yes" (fmt "#{?1,yes,no}")))
    (expect (string= "no"  (fmt "#{?0,yes,no}"))))

  ;; #{window_name} with no matching context key emits an empty string.
  (it "expand-format-brace-missing-key-uses-empty"
    (expect (string= "" (cl-tmux/format:expand-format "#{window_name}" '()))))

  ;; A template mixing shorthands, brace vars, and plain text expands correctly.
  (it "expand-format-mixed-template"
    (let ((ctx '(:session-name "main" :window-name "bash")))
      (expect (string= "session=main window=bash"
                   (cl-tmux/format:expand-format "session=#{session_name} window=#W" ctx)))))

  ;;; ── %expand-shorthand unknown returns nil ────────────────────────────────────

  ;; An unknown shorthand #X emits both '#' and 'X' literally.
  (it "expand-shorthand-unknown-char-emits-both-chars"
    (dolist (spec '("#Z" "#?"))
      (expect (string= spec (fmt spec)))))

  ;;; ── format-context-from-window nil window ────────────────────────────────────

  ;; format-context-from-window with NIL window returns safe defaults.
  (it "format-context-from-window-nil-window"
    (let* ((sess (make-fake-session :nwindows 1))
           (ctx  (cl-tmux/format:format-context-from-window sess nil)))
      (expect (stringp (getf ctx :session-name)))
      (expect (string= "" (getf ctx :window-name)))
      (expect (= 0 (getf ctx :window-index)))))

  ;;; ── %lookup integer value ────────────────────────────────────────────────────

  ;; %lookup converts integer values to strings via princ-to-string.
  (it "lookup-integer-value-converted-to-string"
    (expect (string= "42" (cl-tmux/format::%lookup (list :count 42) :count))))

  ;;; ── Table-driven %truthy-p boundary tests ────────────────────────────────────

  ;; %truthy-p boundary table: various inputs and expected truthiness.
  (it "truthy-p-table-driven"
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
              (expect (cl-tmux/format::%truthy-p input) :to-be-truthy)
              (expect (cl-tmux/format::%truthy-p input) :to-be-falsy))))))

  ;;; ── %variable-to-keyword table-driven ────────────────────────────────────────

  ;; %variable-to-keyword converts multiple names correctly.
  (it "variable-to-keyword-table-driven"
    (let ((cases '(("session_name"  . :session-name)
                   ("window_index"  . :window-index)
                   ("pane_index"    . :pane-index)
                   ("host_short"    . :host-short)
                   ("window_active" . :window-active)
                   ("time"          . :time))))
      (dolist (c cases)
        (expect (eq (cdr c) (cl-tmux/format::%variable-to-keyword (car c)))))))

  ;;; ── #{pane_title} expansion ───────────────────────────────────────────────────

  ;; #{pane_title} expands to :pane-title when present, or empty when absent.
  (it "expand-format-pane-title-table"
    (expect (string= "mytitle" (fmt "#{pane_title}" :pane-title "mytitle")))
    (expect (string= "" (fmt "#{pane_title}"))))

  ;; format-context-from-session :pane-title uses the pane's title slot.
  (it "format-context-pane-title-from-pane-slot"
    (let* ((sess  (make-fake-session :nwindows 1 :npanes 1))
           (win   (first (cl-tmux/model:session-windows sess)))
           (pane  (first (cl-tmux/model:window-panes win))))
      (setf (cl-tmux/model:pane-title pane) "my-pane-title")
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (expect (string= "my-pane-title" (getf ctx :pane-title))))))

  ;; format-context-from-session :pane-title is empty when pane is NIL.
  (it "format-context-pane-title-nil-pane-returns-empty"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (ctx  (cl-tmux/format:format-context-from-session sess win nil)))
      (expect (string= "" (getf ctx :pane-title)))))

  ;; #{?pane_title,has-title,no-title} branches correctly on :pane-title.
  (it "expand-format-conditional-pane-title"
    (expect (string= "has-title"
                 (cl-tmux/format:expand-format "#{?pane_title,has-title,no-title}"
                                               '(:pane-title "vim"))))
    (expect (string= "no-title"
                 (cl-tmux/format:expand-format "#{?pane_title,has-title,no-title}"
                                               '(:pane-title "")))))

  ;;; ── #{client_width}, #{client_height}, #{client_tty} ─────────────────────────

  ;; #{client_width}, #{client_height}, #{client_tty} each expand to the matching context value.
  (it "expand-format-client-fields-table"
    (dolist (c '(("#{client_width}"  :client-width  220    "220")
                 ("#{client_height}" :client-height 55     "55")
                 ("#{client_tty}"    :client-tty    "/dev/pts/0" "/dev/pts/0")))
      (destructuring-bind (spec key value expected) c
        (expect (string= expected (fmt spec key value))))))

  ;; format-context-from-session :client-width and :client-height default to 0.
  (it "format-context-client-defaults-are-zero"
    (with-format-context (sess win pane ctx) ()
      (expect (eql 0 (getf ctx :client-width)))
      (expect (eql 0 (getf ctx :client-height)))
      (expect (string= "" (getf ctx :client-tty)))))

  ;;; ── #{client_name}, #{client_session}, #{client_pid}, #{client_termname} ─────

  ;; #{client_name} defaults to the client tty path (tmux's default client name).
  (it "format-context-client-name-mirrors-tty"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session
                  sess win pane :client-tty "/dev/ttys004")))
      (expect (string= "/dev/ttys004"
                   (cl-tmux/format:expand-format "#{client_name}" ctx)))))

  ;; #{client_session} expands to the name of the session the client is viewing.
  (it "format-context-client-session-is-session-name"
    (with-format-context (sess win pane ctx) ()
      ;; make-fake-session names the session "0".
      (expect (string= (cl-tmux/model:session-name sess)
                   (cl-tmux/format:expand-format "#{client_session}" ctx)))))

  ;; #{client_pid} expands to a non-empty numeric PID string (single-process model).
  (it "format-context-client-pid-is-numeric"
    (with-format-context (sess win pane ctx) ()
      (let ((pid (cl-tmux/format:expand-format "#{client_pid}" ctx)))
        (expect (plusp (length pid)))
        (expect (every #'digit-char-p pid)))))

  ;; #{client_termname} expands to a string (the TERM env value or empty).
  (it "format-context-client-termname-is-string"
    (with-format-context (sess win pane ctx) ()
      (expect (stringp (cl-tmux/format:expand-format "#{client_termname}" ctx)))))

  ;; format-context-from-session forwards :client-width/:client-height/:client-tty.
  (it "format-context-client-keyword-args-propagated"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane
                                                             :client-width  200
                                                             :client-height 50
                                                             :client-tty    "/dev/pts/3")))
      (expect (eql 200 (getf ctx :client-width)))
      (expect (eql 50 (getf ctx :client-height)))
      (expect (string= "/dev/pts/3" (getf ctx :client-tty)))))

  ;;; ── #(shell-cmd) expansion ───────────────────────────────────────────────────

  ;; #(cmd) expands to command output; errors return empty string; embeds inline.
  (it "expand-format-shell-cmd-table"
    (dolist (c '(("#(echo hello)"           "hello"          "echo output")
                 ("#(printf '%s' foo)"      "foo"            "no trailing newline")
                 ("#(false)"               ""               "error → empty string")
                 ("status: #(echo ok) done" "status: ok done" "embedded in text")))
      (destructuring-bind (spec expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec))))))

  ;; #(cmd) captures bounded stdout before building the Lisp string.
  (it "expand-format-shell-command-capture-is-bounded"
    (let* ((payload (make-string 5000 :initial-element #\x))
           (result (fmt (format nil "#(printf '~A')" payload)))
           (limit cl-tmux/format::+format-shell-command-output-limit+))
      (expect (= limit (length result)))
      (expect (every (lambda (ch) (char= ch #\x)) result))))

  ;; #(cmd) is routed through the bounded shell capture port.
  (it "expand-format-shell-command-wrapper-documents-port"
    (let ((wrapped (cl-tmux/format::%format-shell-capture-command "printf foo"))
          (limit (write-to-string cl-tmux/format::+format-shell-command-output-limit+)))
      (expect (search "printf foo" wrapped))
      (expect (search "head -c" wrapped))
      (expect (search limit wrapped))))

  ;;; ── #[attr] style directive — no crash guarantee ─────────────────────────────

  ;; #[fg=colour231,bold] passes through without crashing.
  (it "expand-format-sgr-no-crash-complex-attr"
    (let ((result (fmt "#[fg=colour231,bold]")))
      (expect (stringp result))
      (expect (string= "#[fg=colour231,bold]" result)))))
