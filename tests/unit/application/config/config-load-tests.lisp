(in-package #:cl-tmux/test)

;;;; config directive tests — load strings, streams, files, and config paths

;;; Test isolation helpers

(defun config-path (override xdg home)
  "Namestring of the resolved config path for the given env values + HOME
   (HOME a directory pathname)."
  (namestring (cl-tmux/config::%config-path-from override xdg home)))

(describe "config-directives-suite"

  ;;; load-config-from-string

  ;; load-config-from-string ignores comments/blanks and applies real directives.
  (it "load-from-string-counts-and-applies"
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "# a comment~%~%bind z new-window~%set-status-height 5~%"))))
        (expect (= 2 applied))
        (expect (eq :new-window (lookup-key-binding #\z)))
        (expect (= 5 *status-height*)))))

  ;; A realistic multi-option .tmux.conf loads end-to-end: the wired options, the
  ;; clustered -ga/-gw flags, bind -n, and a prefix change all take effect.  Guards
  ;; the whole `.tmux.conf completely` path against regressions.
  (it "load-realistic-config-applies-all-directives"
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
        (expect (= 8 applied))
        (expect (string= "top" (cl-tmux/options:get-option "status-position")))
        (expect (string= "fg=red" (cl-tmux/options:get-option "status-left-style")))
        (expect (null (cl-tmux/options:get-option "monitor-bell")))
        (expect (null (cl-tmux/options:get-option "alternate-screen")))
        (expect (string= "ab" (cl-tmux/options:get-option "@x")))
        (expect (eq :next-window
                    (cl-tmux/config:key-table-command
                     (cl-tmux/config:key-table-lookup "root" "F1"))))
        (expect (= 1 cl-tmux/config:*prefix-key-code*)))))

  ;; A single-char quote key parses as the character.
  (it "load-from-string-multichar-and-quote-key"
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "bind \" split-horizontal~%bind n split-vertical~%"))))
        (expect (= 2 applied))
        (expect (eq :split-horizontal (lookup-key-binding #\")))
        (expect (eq :split-vertical (lookup-key-binding #\n))))))

  ;;; config-file-path precedence (pure: %config-path-from)

  ;; %config-path-from: override wins; XDG used when set; ~/.config fallback; empty = unset.
  (it "config-path-table"
    (dolist (c '(("/custom/my.conf" "/x/cfg"  #p"/home/u/" "/custom/my.conf"                  "explicit override wins")
                 (nil               "/x/cfg"  #p"/home/u/" "/x/cfg/cl-tmux/cl-tmux.conf"      "XDG set")
                 (nil               "/x/cfg/" #p"/home/u/" "/x/cfg/cl-tmux/cl-tmux.conf"      "XDG trailing slash")
                 (nil               nil       #p"/home/u/" "/home/u/.config/cl-tmux/cl-tmux.conf" "no XDG fallback")
                 (""                ""        #p"/home/u/" "/home/u/.config/cl-tmux/cl-tmux.conf" "empty env = unset")))
      (destructuring-bind (override xdg home expected desc) c
        (declare (ignore desc))
        (expect (string= expected (config-path override xdg home))))))

  ;;; load-config-file

  ;; load-config-file on a non-existent path returns NIL.
  (it "load-config-file-missing-returns-nil"
    (with-isolated-config
      (expect (null (load-config-file #p"/nonexistent/cl-tmux-xyz.conf")))))

  ;;; load-config-from-stream

  ;; load-config-from-stream ignores comments and applies the real directives.
  (it "load-config-from-stream-applies"
    (with-isolated-config
      (let ((applied (with-input-from-string
                         (s (format nil "# leading comment~%bind z next-window~%set-status-height 4~%"))
                       (load-config-from-stream s))))
        (expect (= 2 applied))
        (expect (eq :next-window (lookup-key-binding #\z)))
        (expect (= 4 *status-height*))))))
