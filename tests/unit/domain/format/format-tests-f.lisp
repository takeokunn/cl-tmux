(in-package #:cl-tmux/test)

;;;; Format tests — part VI: new context keys (cursor_x/y, pane_in_mode, window_layout), modifier chaining, glob/regex match, format variables.

(describe "format-suite"

  ;;; ── New context keys: cursor_x, cursor_y, pane_in_mode, window_layout ────────

  ;; format-context-from-session :cursor-x and :cursor-y default to 0 when pane is NIL.
  (it "format-context-cursor-xy-defaults"
    (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
      (expect (= 0 (getf ctx :cursor-x)))
      (expect (= 0 (getf ctx :cursor-y)))))

  ;; #{cursor_character} is empty when there is no pane (out-of-grid safe).
  (it "format-context-cursor-character-empty-when-pane-nil"
    (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
      (expect (string= "" (cl-tmux/format:expand-format "#{cursor_character}" ctx)))))

  ;; #{cursor_character} expands to the glyph in the cell under the cursor.
  (it "format-context-cursor-character-reads-glyph-under-cursor"
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
        (expect (string= "Z" (cl-tmux/format:expand-format "#{cursor_character}" ctx))))))

  ;; format-context-from-session :pane-in-mode is "0" when pane is not in copy mode.
  (it "format-context-pane-in-mode-not-in-copy-mode"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (string= "0" (getf ctx :pane-in-mode)))))

  ;; format-context-from-session :pane-in-mode is "1" when pane is in copy mode.
  (it "format-context-pane-in-mode-in-copy-mode"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (scr  (cl-tmux/model:pane-screen pane)))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) t)
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (expect (string= "1" (getf ctx :pane-in-mode))))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) nil)))

  ;; #{copy_cursor_x}/#{copy_cursor_y} are empty and #{selection_present} is 0 when
  ;; the pane is not in copy mode.
  (it "format-context-copy-cursor-empty-outside-copy-mode"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (dolist (c '(("#{copy_cursor_x}" "") ("#{copy_cursor_y}" "") ("#{selection_present}" "0")))
        (destructuring-bind (spec expected) c
          (expect (string= expected (cl-tmux/format:expand-format spec ctx)))))))

  ;; In copy mode #{copy_cursor_x}/#{copy_cursor_y} report the copy cursor (row . col)
  ;; and #{selection_present} is 1 once a selection is being made.
  (it "format-context-copy-cursor-reports-position-in-copy-mode"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (scr  (cl-tmux/model:pane-screen pane)))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) t
            (cl-tmux/terminal/types:screen-copy-cursor scr) (cons 7 3)
            (cl-tmux/terminal/types:screen-copy-selecting scr) t)
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (dolist (c '(("#{copy_cursor_x}"    "3" "copy cursor column (cdr of row.col)")
                     ("#{copy_cursor_y}"    "7" "copy cursor row (car of row.col)")
                     ("#{selection_present}" "1" "selection present while selecting")))
          (destructuring-bind (spec expected desc) c
            (declare (ignore desc))
            (expect (string= expected (cl-tmux/format:expand-format spec ctx))))))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) nil
            (cl-tmux/terminal/types:screen-copy-selecting scr) nil)))

  ;; #{copy_position} and #{copy_position_limit} are empty outside copy mode.
  (it "format-context-copy-position-empty-outside-copy-mode"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (dolist (spec '("#{copy_position}" "#{copy_position_limit}"))
        (expect (string= "" (cl-tmux/format:expand-format spec ctx))))))

  ;; #{copy_position} reports the copy offset and #{copy_position_limit} reports
  ;; the scrollback length in copy mode.
  (it "format-context-copy-position-and-limit-in-copy-mode"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (scr  (cl-tmux/model:pane-screen pane)))
      (seed-scrollback scr 5)
      (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) t
            (cl-tmux/terminal/types:screen-copy-offset scr) 3)
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (expect (string= "3" (cl-tmux/format:expand-format "#{copy_position}" ctx)))
        (expect (string= "5" (cl-tmux/format:expand-format "#{copy_position_limit}" ctx))))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p scr) nil)))

  ;; format-context-from-session :window-layout is a non-empty string for a window with a tree.
  (it "format-context-window-layout-non-empty-for-window-with-panes"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 2))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (let ((layout (getf ctx :window-layout)))
        (expect (stringp layout))
        (expect (plusp (length layout))))))

  ;; #{window_last_flag} is "1" for the previously active window and "0" for all others.
  ;; Regression: a duplicate plist key shadowed the conditional, always returning "0".
  (it "format-context-window-last-flag-correct"
    (let* ((sess (make-fake-session :nwindows 2))
           (win0 (first  (cl-tmux/model:session-windows sess)))
           (win1 (second (cl-tmux/model:session-windows sess))))
      ;; Simulate: win0 was active first (lower time), win1 is now active (higher time).
      (setf (cl-tmux/model:window-last-active-time win0) 100
            (cl-tmux/model:window-last-active-time win1) 200)
      (cl-tmux/model:session-select-window sess win1)
      (let ((ctx0 (cl-tmux/format:format-context-from-session sess win0 nil))
            (ctx1 (cl-tmux/format:format-context-from-session sess win1 nil)))
        (expect (string= "1" (getf ctx0 :window-last-flag)))
        (expect (string= "0" (getf ctx1 :window-last-flag))))))

  ;;; ── Modifier chaining ────────────────────────────────────────────────────────

  ;; Format modifier chains apply right-to-left: b:d = dirname then basename; U:b = basename then uppercase.
  (it "format-modifier-chain-table"
    (dolist (row '(("#{b:d:x}"   "/a/b/c"        "b"   "b(d(x)): dirname then basename → b")
                   ("#{U:b:x}"   "/some/path/foo" "FOO" "U(b(x)): basename then uppercase → FOO")
                   ("#{U:b:d:x}" "/a/b/c"         "B"   "U(b(d(x))): dirname+basename+uppercase → B")))
      (destructuring-bind (fmt-str input expected desc) row
        (declare (ignore desc))
        (expect (string= expected (fmt fmt-str :x input))))))

  ;;; ── Glob match #{m:pattern,string} ──────────────────────────────────────────

  ;; #{m:pattern,string} returns '1' for glob matches and '0' for misses.
  (it "format-glob-match-table"
    (dolist (c '(("#{m:*bash,bash}"    "1" "star-prefix match")
                 ("#{m:bash*,bash-5.1}" "1" "star-suffix match")
                 ("#{m:*zsh*,bash}"    "0" "no match")
                 ("#{m:ba?h,bash}"     "1" "question-mark match")))
      (destructuring-bind (spec expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec))))))

  ;; #{m:*bash,#{x}} with x='fish' → '0'.
  (it "format-glob-match-with-context-var"
    (expect (string= "0" (fmt "#{m:*bash,#{x}}" :x "fish"))))

  ;; #{?#{m:*bash,bash},yes,no} → 'yes'.
  (it "format-glob-match-in-conditional"
    (expect (string= "yes" (fmt "#{?#{m:*bash,bash},yes,no}"))))

  ;;; ── Regex match #{m/r:pattern,string} (cl-ppcre) ─────────────────────────────

  ;; #{m/r:pattern,string} returns '1' for matches and '0' for misses.
  (it "format-regex-match-table"
    (dolist (c '(("#{m/r:^h.*o$,hello}" "1" "anchored regex match")
                 ("#{m/r:ell,hello}"     "1" "substring match")
                 ("#{m/r:^x,hello}"      "0" "anchored no-match")))
      (destructuring-bind (spec expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec))))))

  ;; #{m/ri:HELLO,hello} → '1' (the i flag makes the regex case-insensitive).
  (it "format-regex-match-case-insensitive"
    (expect (string= "1" (fmt "#{m/ri:HELLO,hello}")))
    (expect (string= "0" (fmt "#{m/r:HELLO,hello}"))))

  ;; A malformed regex yields '0' (no match), never an error.
  (it "format-regex-match-malformed-pattern-is-zero"
    (expect (string= "0" (fmt "#{m/r:[,hello}"))))

  ;; #{m/r:[0-9]+,abc123} → '1' (a regex feature glob cannot express).
  (it "format-regex-match-character-class"
    (expect (string= "1" (fmt "#{m/r:[0-9]+,abc123}")))
    (expect (string= "0" (fmt "#{m/r:[0-9]+,abcdef}"))))

  ;; #{m/r:#{pat},#{val}} expands both operands before matching.
  (it "format-regex-match-with-context-vars"
    (expect (string= "1" (fmt "#{m/r:#{pat},#{val}}" :pat "^a.c$" :val "abc"))))

  ;; Plain #{m:...} glob is unaffected by the m/r regex branch.
  (it "format-glob-still-works-after-regex-addition"
    (expect (string= "1" (fmt "#{m:*bash,bash}")))
    (expect (string= "0" (fmt "#{m:*zsh*,bash}"))))

  ;;; ── New format context variables ─────────────────────────────────────────────

  ;; #{session_id} is available and is an integer (via pane context).
  (it "format-context-session-id"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (integerp (getf ctx :session-id)))))

  ;; #{window_id} is available and is an integer.
  (it "format-context-window-id"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (integerp (getf ctx :window-id)))))

  ;; #{pane_current_command} is available as a non-empty string.
  (it "format-context-pane-current-command-is-string"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (stringp (getf ctx :pane-current-command)))
      (expect (plusp (length (getf ctx :pane-current-command))))))

  ;; #{session_id} and #{window_id} expand to numeric strings.
  (it "format-expand-session-id-and-window-id"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (plusp (length (cl-tmux/format:expand-format "#{session_id}" ctx))))
      (expect (plusp (length (cl-tmux/format:expand-format "#{window_id}" ctx)))))))
