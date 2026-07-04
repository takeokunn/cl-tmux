;;;; structural pane/session/window/terminal format variables

(in-package #:cl-tmux/test)
(in-suite format-suite)

;;; These are pure functions of the session/window/pane structs, wired into
;;; format-context-from-session and exercised end-to-end through expand-format.

(test format-context-exposes-structural-pane-variables
  "format-context-from-session populates pane geometry/id/pid variables that
   expand-format resolves (#{pane_width} #{pane_height} #{pane_id} #{pane_left}
   #{pane_top} #{pane_pid}).
   make-fake-window panes are 20x5 at (0,0), id 1, pid -1.
   Inclusive far-edge: right = 0+20-1 = 19, bottom = 0+5-1 = 4."
  (with-format-context (sess win pane ctx) ()
    (dolist (c '(("#{pane_width}"  "20") ("#{pane_height}" "5")
                 ("#{pane_id}"     "1")  ("#{pane_left}"   "0")
                 ("#{pane_top}"    "0")  ("#{pane_right}"  "19")
                 ("#{pane_bottom}" "4")  ("#{pane_pid}"    "-1")))
      (destructuring-bind (spec expected) c
        (is (string= expected (cl-tmux/format:expand-format spec ctx))
            "~S must expand to ~S" spec expected)))))

(test format-context-pane-variables-default-when-pane-nil
  "With a NIL pane, structural pane variables default to 0 (empty-safe)."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (dolist (spec '("#{pane_width}" "#{pane_id}" "#{pane_right}"
                    "#{pane_bottom}" "#{pane_active}"))
      (is (string= "0" (cl-tmux/format:expand-format spec ctx))
          "~S must default to 0 with nil pane" spec))))

(test window-bell-flag-respects-monitor-bell
  "#{window_bell_flag} shows ! only when monitor-bell is on (default); monitor-bell
   off suppresses the bell alert even with the sticky window bell flag set."
  (with-fresh-options
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win))))
      (setf (cl-tmux/model:window-bell-flag win) t)
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
      (dolist (c `((,ctx-a "#{pane_active}"            "1"    "active pane → #{pane_active} 1")
                   (,ctx-i "#{pane_active}"            "0"    "inactive pane → #{pane_active} 0")
                   (,ctx-a "#{?pane_active,HERE,away}" "HERE" "conditional picks the true branch")
                   (,ctx-i "#{?pane_active,HERE,away}" "away" "conditional picks the false branch")))
        (destructuring-bind (ctx spec expected desc) c
          (is (string= expected (cl-tmux/format:expand-format spec ctx)) "~A" desc))))))

(test format-context-window-panes-and-session-windows-counts
  "#{window_panes} is the pane count; #{session_windows} is the window count."
  (with-format-context (sess win pane ctx) (:nwindows 3 :npanes 2)
    (is (string= "2" (cl-tmux/format:expand-format "#{window_panes}" ctx))
        "window has 2 panes")
    (is (string= "3" (cl-tmux/format:expand-format "#{session_windows}" ctx))
        "session has 3 windows")))

(test format-context-session-count-is-numeric
  "#{session_count} expands to a non-empty numeric string (server session total,
   minimum 1 in the single-process model)."
  (with-format-context (sess win pane ctx) ()
    (let ((count (cl-tmux/format:expand-format "#{session_count}" ctx)))
      (is (plusp (length count)) "#{session_count} must be non-empty")
      (is (every #'digit-char-p count) "#{session_count} must be all digits, got ~S" count)
      (is (>= (parse-integer count) 1) "#{session_count} must be at least 1, got ~S" count))))

(test pane-dead-status-format-vars-table
  "#{pane_dead_status}/#{pane_dead_signal}/#{pane_dead_time} expand from the
   pane's death record and are empty for a live pane (tmux empty defaults)."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win))))
    (flet ((expand (spec)
             (cl-tmux/format:expand-format
              spec (cl-tmux/format:format-context-from-session sess win pane))))
      (dolist (spec '("#{pane_dead_status}" "#{pane_dead_signal}" "#{pane_dead_time}"))
        (is (string= "" (expand spec))
            "~A must be empty while the pane is alive" spec))
      (setf (cl-tmux/model:pane-dead-status pane) 1
            (cl-tmux/model:pane-dead-time pane) 3927584461)
      (is (string= "1" (expand "#{pane_dead_status}"))
          "pane_dead_status must expand to the recorded exit code")
      (is (string= "3927584461" (expand "#{pane_dead_time}"))
          "pane_dead_time must expand to the recorded universal-time")
      (is (string= "" (expand "#{pane_dead_signal}"))
          "pane_dead_signal must stay empty for a normally-exited pane"))))

(test format-terminal-state-vars-table
  "The terminal-state / selection / key-table format variables added from the
   tmux inventory diff expand from live screen state.
   Each row: (spec expected description) against a fresh fake session."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (h    (cl-tmux/terminal/types:screen-height
                (cl-tmux/model:pane-screen pane))))
    (flet ((expand (spec)
             (cl-tmux/format:expand-format
              spec (cl-tmux/format:format-context-from-session sess win pane))))
      (dolist (row `(("#{cursor_flag}"         "1"  "cursor visible by default")
                     ("#{insert_flag}"         "0"  "IRM off by default")
                     ("#{wrap_flag}"           "1"  "autowrap on by default")
                     ("#{origin_flag}"         "0"  "origin mode off")
                     ("#{alternate_on}"        "0"  "primary screen")
                     ("#{scroll_region_upper}" "0"  "scroll region top")
                     ("#{scroll_region_lower}" ,(format nil "~D" (1- h))
                                                    "scroll region bottom")
                     ("#{mouse_any_flag}"      "0"  "mouse reporting off")
                     ("#{rectangle_toggle}"    "0"  "rect select off")
                     ("#{client_key_table}"    "root" "root key table at rest")
                     ("#{window_marked_flag}"  "0"  "no marked pane")
                     ("#{pane_last}"           "0"  "no last pane yet")))
        (destructuring-bind (spec expected desc) row
          (is (string= expected (expand spec)) "~A: ~A" spec desc)))
      (is (plusp (parse-integer (expand "#{session_created}")))
          "session_created must be a construction timestamp")
      (is (string/= "" (expand "#{session_activity}"))
          "session_activity must expand")
      ;; A marked pane and copy-mode key table flip their flags.
      (setf (cl-tmux/model:pane-marked pane) t)
      (is (string= "1" (expand "#{window_marked_flag}"))
          "marking a pane must set window_marked_flag")
      (cl-tmux/commands:copy-mode-enter (cl-tmux/model:pane-screen pane))
      (is (string= "copy-mode" (expand "#{client_key_table}"))
          "copy mode must report the copy-mode key table"))))
