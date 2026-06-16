(in-package #:cl-tmux/test)

(in-suite compat-suite)

(defmacro %define-no-server-cli-differential-tests (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 (destructuring-bind (name matrix-name args status-message) case
                   `(test ,name
                      (%assert-no-server-cli-differential
                       ,matrix-name
                       ',args
                    ,status-message))))
               cases)))

(defmacro %with-cli-compatibility-context ((matrix version binary entry
                                            matrix-kind matrix-name
                                            entry-message skip-message)
                                           &body body)
  `(let* ((,matrix (%read-compat-matrix))
          (,entry (%compat-entry ,matrix ,matrix-kind ,matrix-name))
          (,binary (%cl-tmux-binary)))
     (is-true ,entry ,entry-message)
     (%with-matching-tmux-matrix (,matrix ,version)
       (if (not ,binary)
           (skip ,skip-message)
           ,@body))))

(defun %assert-no-server-cli-differential (matrix-name args status-message)
  (let* ((matrix (%read-compat-matrix))
         (entry (%compat-entry matrix :cli matrix-name))
         (binary (%cl-tmux-binary)))
    (is-true entry
             (format nil "~A CLI compatibility caveat must be recorded"
                     matrix-name))
    (%with-matching-tmux-matrix (matrix version)
      (if (not binary)
          (skip "cl-tmux binary unavailable; set CL_TMUX_COMPAT_BINARY or build result/bin/cl-tmux")
          (let* ((socket (%tmux-clean-socket-name))
                 (tmux (%run-program-result (%tmux-clean-args socket args)))
                 (cl-tmux (%run-program-result (append (list binary) args)))
                 (matches (%no-server-result-matches-p tmux cl-tmux)))
            (is-true matches
                     (format nil
                             "cl-tmux ~A no-server stdout, stderr class, and exit code must match tmux"
                             (first args)))
            (is (string= "" (compat-run-result-stdout cl-tmux)))
            (is (= 1 (compat-run-result-exit-code cl-tmux)))
            (is (eq :partial (getf entry :status))
                status-message))))))

(test compat-list-commands-name-differential
  (%with-cli-compatibility-context (matrix version binary entry
                                    :command "list-commands"
                                    "cl-tmux list-commands compatibility caveat must be recorded"
                                    "cl-tmux binary unavailable; set CL_TMUX_COMPAT_BINARY or build result/bin/cl-tmux")
    (let* ((tmux (%run-program-result
                  (list "tmux" "list-commands" "-F" "#{command_list_name}")))
           (cl-tmux (%run-program-result
                     (list binary "list-commands" "-F" "#{command_list_name}")))
           (matches (%compat-result-matches-p tmux cl-tmux)))
      (is-true matches
               "cl-tmux list-commands command_list_name output must match tmux")
      (is (eq :partial (getf entry :status))
          "matrix stays partial until aliases, usages, flags, and behavior are proven"))))

(%define-no-server-cli-differential-tests
  (compat-list-sessions-no-server-differential
   "list-sessions without a terminal"
   ("list-sessions")
   "matrix stays partial until server-present list-sessions output and flags are proven")
  (compat-has-session-no-server-differential
   "has-session without a server"
   ("has-session" "-t" "no-such-session-xyz")
   "matrix stays partial until live-server has-session behavior and target semantics are proven")
  (compat-kill-server-no-server-differential
   "kill-server without a server"
   ("kill-server")
   "matrix stays partial until live-server kill-server behavior, flags, and socket selection are proven"))

(test compat-new-session-detached-existing-server-differential
  (%with-cli-compatibility-context (matrix version binary entry
                                    :cli "new-session -d against an existing server"
                                    "detached new-session live-server compatibility caveat must be recorded"
                                    "cl-tmux binary unavailable; set CL_TMUX_COMPAT_BINARY or build result/bin/cl-tmux")
    (let ((socket (%tmux-clean-socket-name)))
      (unwind-protect
           (progn
             (%run-program-result
              (%tmux-clean-args socket '("new-session" "-d" "-s" "0" "-n" "sh")))
             (let* ((args '("new-session" "-d" "-s" "beta" "-n" "two"))
                    (list-windows-args
                      '("list-windows" "-a" "-F" "#{session_name}:#{window_name}"))
                    (tmux-new (%run-program-result
                               (%tmux-clean-args socket args)))
                    (tmux-list (%run-program-result
                                (%tmux-clean-args
                                 socket
                                 '("list-sessions" "-F" "#{session_name}"))))
                    (tmux-windows (%run-program-result
                                   (%tmux-clean-args socket list-windows-args))))
               (%with-clean-cl-tmux-server (binary tmpdir process "alpha")
                 (let* ((cl-new (%run-program-result
                                 (%cl-tmux-env-args binary tmpdir args)))
                        (cl-list (%run-program-result
                                  (%cl-tmux-env-args
                                   binary tmpdir
                                   '("list-sessions" "-F" "#{session_name}"))))
                        (cl-windows (%run-program-result
                                     (%cl-tmux-env-args
                                      binary tmpdir list-windows-args)))
                        (tmux-beta-windows
                          (remove-if-not
                           (lambda (line) (%string-prefix-p "beta:" line))
                           (%sorted-non-empty-lines
                            (compat-run-result-stdout tmux-windows))))
                        (cl-beta-windows
                          (remove-if-not
                           (lambda (line) (%string-prefix-p "beta:" line))
                           (%sorted-non-empty-lines
                            (compat-run-result-stdout cl-windows)))))
                   (is (= 0 (compat-run-result-exit-code tmux-new)))
                   (is (= 0 (compat-run-result-exit-code cl-new)))
                   (is (equal (%sorted-non-empty-lines
                               (compat-run-result-stdout tmux-list))
                              (%sorted-non-empty-lines
                               (compat-run-result-stdout cl-list)))
                       "detached new-session must create a session in the existing live server")
                   (is (equal '("beta:two") tmux-beta-windows)
                       "real tmux must expose the beta window name used by this differential")
                   (is (equal tmux-beta-windows cl-beta-windows)
                       "detached new-session must expose the new session/window name through list-windows")
                   (is (eq :partial (getf entry :status))
                       "matrix stays partial until attached new-session, grouping, duplicate names, and target flags are proven")))))
        (%run-program-result (%tmux-clean-args socket '("kill-server")))))))

(%define-no-server-cli-differential-tests
  (compat-list-windows-no-server-differential
   "list-windows without a server"
   ("list-windows")
   "matrix stays partial until live-server list-windows output, flags, filters, and target semantics are proven")
  (compat-show-options-global-no-server-differential
   "show-options -g without a server"
   ("show-options" "-g")
   "matrix stays partial until live-server show-options output, scopes, flags, and option formatting are proven")
  (compat-show-window-options-global-no-server-differential
   "show-window-options -g without a server"
   ("show-window-options" "-g")
   "matrix stays partial until live-server show-window-options output, scopes, flags, and option formatting are proven"))
