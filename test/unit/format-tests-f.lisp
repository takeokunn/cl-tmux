(in-package #:cl-tmux/test)

;;;; Format tests — part VI: new context keys (cursor_x/y, pane_in_mode, window_layout), modifier chaining, glob/regex match, format variables.

(in-suite format-suite)

;;; ── New context keys: cursor_x, cursor_y, pane_in_mode, window_layout ────────

(test format-context-cursor-xy-defaults
  "format-context-from-session :cursor-x and :cursor-y default to 0 when pane is NIL."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (= 0 (getf ctx :cursor-x))  ":cursor-x must default to 0")
    (is (= 0 (getf ctx :cursor-y))  ":cursor-y must default to 0")))

(test format-context-cursor-character-empty-when-pane-nil
  "#{cursor_character} is empty when there is no pane (out-of-grid safe)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "" (cl-tmux/format:expand-format "#{cursor_character}" ctx))
        "#{cursor_character} must be empty with no pane")))

(test format-context-cursor-character-reads-glyph-under-cursor
  "#{cursor_character} expands to the glyph in the cell under the cursor."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (scr  (cl-tmux/model:pane-screen pane)))
    ;; make-fake-window panes are 20x5; place 'Z' at (2,1) and move there.
    (setf (cl-tmux/terminal/types:screen-cell scr 2 1)
          (cl-tmux/terminal/types:make-cell :char #\Z))
    (setf (cl-tmux/terminal/types:screen-cursor-x scr) 2
          (cl-tmux/terminal/types:screen-cursor-y scr) 1)
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "Z" (cl-tmux/format:expand-format "#{cursor_character}" ctx))
          "#{cursor_character} must be the glyph under the cursor"))))

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

(test format-context-copy-cursor-empty-outside-copy-mode
  "#{copy_cursor_x}/#{copy_cursor_y} are empty and #{selection_present} is 0 when
   the pane is not in copy mode."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "" (cl-tmux/format:expand-format "#{copy_cursor_x}" ctx)))
    (is (string= "" (cl-tmux/format:expand-format "#{copy_cursor_y}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{selection_present}" ctx)))))

(test format-context-copy-cursor-reports-position-in-copy-mode
  "In copy mode #{copy_cursor_x}/#{copy_cursor_y} report the copy cursor (row . col)
   and #{selection_present} is 1 once a selection is being made."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (scr  (cl-tmux/model:pane-screen pane)))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) t
          (cl-tmux/terminal/types:screen-copy-cursor scr) (cons 7 3)
          (cl-tmux/terminal/types:screen-copy-selecting scr) t)
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "3" (cl-tmux/format:expand-format "#{copy_cursor_x}" ctx))
          "#{copy_cursor_x} must be the copy cursor column (cdr of (row . col))")
      (is (string= "7" (cl-tmux/format:expand-format "#{copy_cursor_y}" ctx))
          "#{copy_cursor_y} must be the copy cursor row (car of (row . col))")
      (is (string= "1" (cl-tmux/format:expand-format "#{selection_present}" ctx))
          "#{selection_present} must be 1 while selecting"))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) nil
          (cl-tmux/terminal/types:screen-copy-selecting scr) nil)))

(test format-context-window-layout-non-empty-for-window-with-panes
  "format-context-from-session :window-layout is a non-empty string for a window with a tree."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 2))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (let ((layout (getf ctx :window-layout)))
      (is (stringp layout) ":window-layout must be a string")
      (is (plusp (length layout)) ":window-layout must be non-empty for a window with panes"))))

(test format-context-window-last-flag-correct
  "#{window_last_flag} is \"1\" for the previously active window and \"0\" for all others.
   Regression: a duplicate plist key shadowed the conditional, always returning \"0\"."
  (let* ((sess (make-fake-session :nwindows 2))
         (win0 (first  (cl-tmux/model:session-windows sess)))
         (win1 (second (cl-tmux/model:session-windows sess))))
    ;; Simulate: win0 was active first (lower time), win1 is now active (higher time).
    (setf (cl-tmux/model:window-last-active-time win0) 100
          (cl-tmux/model:window-last-active-time win1) 200)
    (cl-tmux/model:session-select-window sess win1)
    (let ((ctx0 (cl-tmux/format:format-context-from-session sess win0 nil))
          (ctx1 (cl-tmux/format:format-context-from-session sess win1 nil)))
      (is (string= "1" (getf ctx0 :window-last-flag))
          "the previously active window must have window_last_flag = \"1\"")
      (is (string= "0" (getf ctx1 :window-last-flag))
          "the currently active window must have window_last_flag = \"0\""))))

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

(test format-glob-match-table
  "#{m:pattern,string} returns '1' for glob matches and '0' for misses."
  (dolist (c '(("#{m:*bash,bash}"    "1" "star-prefix match")
               ("#{m:bash*,bash-5.1}" "1" "star-suffix match")
               ("#{m:*zsh*,bash}"    "0" "no match")
               ("#{m:ba?h,bash}"     "1" "question-mark match")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec)) "~A" desc))))

(test format-glob-match-with-context-var
  "#{m:*bash,#{x}} with x='fish' → '0'."
  (is (string= "0" (fmt "#{m:*bash,#{x}}" :x "fish"))))

(test format-glob-match-in-conditional
  "#{?#{m:*bash,bash},yes,no} → 'yes'."
  (is (string= "yes" (fmt "#{?#{m:*bash,bash},yes,no}"))))

;;; ── Regex match #{m/r:pattern,string} (cl-ppcre) ─────────────────────────────

(test format-regex-match-table
  "#{m/r:pattern,string} returns '1' for matches and '0' for misses."
  (dolist (c '(("#{m/r:^h.*o$,hello}" "1" "anchored regex match")
               ("#{m/r:ell,hello}"     "1" "substring match")
               ("#{m/r:^x,hello}"      "0" "anchored no-match")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec)) "~A" desc))))

(test format-regex-match-case-insensitive
  "#{m/ri:HELLO,hello} → '1' (the i flag makes the regex case-insensitive)."
  (is (string= "1" (fmt "#{m/ri:HELLO,hello}")))
  (is (string= "0" (fmt "#{m/r:HELLO,hello}"))
      "without i, the regex is case-sensitive → no match"))

(test format-regex-match-malformed-pattern-is-zero
  "A malformed regex yields '0' (no match), never an error."
  (is (string= "0" (fmt "#{m/r:[,hello}"))))

(test format-regex-match-character-class
  "#{m/r:[0-9]+,abc123} → '1' (a regex feature glob cannot express)."
  (is (string= "1" (fmt "#{m/r:[0-9]+,abc123}")))
  (is (string= "0" (fmt "#{m/r:[0-9]+,abcdef}"))))

(test format-regex-match-with-context-vars
  "#{m/r:#{pat},#{val}} expands both operands before matching."
  (is (string= "1" (fmt "#{m/r:#{pat},#{val}}" :pat "^a.c$" :val "abc"))))

(test format-glob-still-works-after-regex-addition
  "Plain #{m:...} glob is unaffected by the m/r regex branch."
  (is (string= "1" (fmt "#{m:*bash,bash}")))
  (is (string= "0" (fmt "#{m:*zsh*,bash}"))))

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
