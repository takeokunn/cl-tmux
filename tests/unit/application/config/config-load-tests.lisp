(in-package #:cl-tmux/test)

;;;; config directive tests — load strings, streams, files, and config paths

(in-suite config-directives-suite)

;;; Test isolation helpers

(defun config-path (override xdg home)
  "Namestring of the resolved config path for the given env values + HOME
   (HOME a directory pathname)."
  (namestring (cl-tmux/config::%config-path-from override xdg home)))

;;; load-config-from-string

(test load-from-string-counts-and-applies
  "load-config-from-string ignores comments/blanks and applies real directives."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# a comment~%~%bind z new-window~%set-status-height 5~%"))))
      (is (= 2 applied)
          "exactly 2 directives should be applied, got ~A" applied)
      (is (eq :new-window (lookup-key-binding #\z))
          "#\\z should be bound to :new-window")
      (is (= 5 *status-height*)
          "*status-height* should be 5, got ~A" *status-height*))))

(test load-realistic-config-applies-all-directives
  "A realistic multi-option .tmux.conf loads end-to-end: the wired options, the
   clustered -ga/-gw flags, bind -n, and a prefix change all take effect.  Guards
   the whole `.tmux.conf completely` path against regressions."
  (with-isolated-config
    (let ((applied
            (load-config-from-string
             (format nil "~
# realistic config~%~
set-option -g status-position top~%~
set-option -g status-left-style fg=red~%~
set-option -gw monitor-bell off~%~
set-option -g alternate-screen off~%~
set-option -ga @x a~%~
set-option -ga @x b~%~
bind -n F1 next-window~%~
set-option -g prefix C-a~%"))))
      (is (= 8 applied) "all 8 directives applied (comment/blank skipped), got ~A" applied)
      (is (string= "top" (cl-tmux/options:get-option "status-position"))
          "status-position took effect")
      (is (string= "fg=red" (cl-tmux/options:get-option "status-left-style"))
          "status-left-style took effect")
      (is (null (cl-tmux/options:get-option "monitor-bell"))
          "clustered -gw set monitor-bell off")
      (is (null (cl-tmux/options:get-option "alternate-screen"))
          "alternate-screen set off")
      (is (string= "ab" (cl-tmux/options:get-option "@x"))
          "clustered -ga appended a then b -> \"ab\"")
      (is (eq :next-window
              (cl-tmux/config:key-table-command
               (cl-tmux/config:key-table-lookup "root" "F1")))
          "bind -n F1 bound F1 in the root table")
      (is (= 1 cl-tmux/config:*prefix-key-code*)
          "set-option -g prefix C-a changed the prefix to C-a (byte 1)"))))

(test load-from-string-multichar-and-quote-key
  "A single-char quote key parses as the character."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind \" split-horizontal~%bind n split-vertical~%"))))
      (is (= 2 applied)
          "both bind directives should be applied, got ~A" applied)
      (is (eq :split-horizontal (lookup-key-binding #\"))
          "the single-char token should bind the double-quote character")
      (is (eq :split-vertical (lookup-key-binding #\n))
          "#\\n should be re-bound to :split-vertical"))))

;;; config-file-path precedence (pure: %config-path-from)

(test config-path-table
  "%config-path-from: override wins; XDG used when set; ~/.config fallback; empty = unset."
  (dolist (c '(("/custom/my.conf" "/x/cfg"  #p"/home/u/" "/custom/my.conf"                  "explicit override wins")
               (nil               "/x/cfg"  #p"/home/u/" "/x/cfg/cl-tmux/cl-tmux.conf"      "XDG set")
               (nil               "/x/cfg/" #p"/home/u/" "/x/cfg/cl-tmux/cl-tmux.conf"      "XDG trailing slash")
               (nil               nil       #p"/home/u/" "/home/u/.config/cl-tmux/cl-tmux.conf" "no XDG fallback")
               (""                ""        #p"/home/u/" "/home/u/.config/cl-tmux/cl-tmux.conf" "empty env = unset")))
    (destructuring-bind (override xdg home expected desc) c
      (is (string= expected (config-path override xdg home)) "~A" desc))))

;;; load-config-file

(test load-config-file-missing-returns-nil
  "load-config-file on a non-existent path returns NIL."
  (with-isolated-config
    (is (null (load-config-file #p"/nonexistent/cl-tmux-xyz.conf"))
        "loading a non-existent config file should return NIL")))

;;; load-config-from-stream

(test load-config-from-stream-applies
  "load-config-from-stream ignores comments and applies the real directives."
  (with-isolated-config
    (let ((applied (with-input-from-string
                       (s (format nil "# leading comment~%bind z next-window~%set-status-height 4~%"))
                     (load-config-from-stream s))))
      (is (= 2 applied)
          "exactly 2 directives should be applied, got ~A" applied)
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z should be bound to :next-window after the stream directives")
      (is (= 4 *status-height*)
          "*status-height* should be 4, got ~A" *status-height*))))
