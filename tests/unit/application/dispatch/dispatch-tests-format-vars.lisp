(in-package #:cl-tmux/test)

;;;; Dispatch coverage for format variables.

(in-suite dispatch-suite)

(test session-group-format-vars
  "#{session_grouped}/#{session_group_size}/#{session_group_list} expand from
   the group registry; ungrouped sessions report 0/empty."
  (let ((cl-tmux::*session-groups* nil))
    (with-fake-session (s1)
      (with-fake-session (s2)
        (setf (cl-tmux/model:session-name s1) "ga"
              (cl-tmux/model:session-name s2) "gb")
        (flet ((expand (spec sess)
                 (cl-tmux/format:expand-format
                  spec (cl-tmux/format:format-context-from-session
                        sess (cl-tmux/model:session-active-window sess) nil))))
          (is (string= "0" (expand "#{session_grouped}" s1))
              "ungrouped session must report 0")
          (cl-tmux::server-new-session-in-group s2 s1)
          (is (string= "1" (expand "#{session_grouped}" s1))
              "grouped session must report 1")
          (is (string= "2" (expand "#{session_group_size}" s1))
              "the group must have two members")
          (let ((names (expand "#{session_group_list}" s1)))
            (is (and (search "ga" names) (search "gb" names))
                "the group list must name both sessions (got ~S)" names)))))))

(test pane-start-and-socket-format-vars
  "#{pane_start_command}/#{pane_start_path} expand from the pane spawn record;
   #{socket_path} is empty in standalone mode and reflects the bound socket."
  (with-fake-session (s)
    (let* ((win  (cl-tmux/model:session-active-window s))
           (pane (cl-tmux/model:window-active-pane win)))
      (setf (cl-tmux/model:pane-start-command pane) "htop"
            (cl-tmux/model:pane-start-path pane) "/tmp/start-here")
      (flet ((expand (spec)
               (cl-tmux/format:expand-format
                spec (cl-tmux/format:format-context-from-session s win pane))))
        (is (string= "htop" (expand "#{pane_start_command}"))
            "pane_start_command must expand from the spawn record")
        (is (string= "/tmp/start-here" (expand "#{pane_start_path}"))
            "pane_start_path must expand from the spawn record")
        (let ((cl-tmux::*bound-socket-path* nil))
          (is (string= "" (expand "#{socket_path}"))
              "socket_path must be empty without a bound socket"))
        (let ((cl-tmux::*bound-socket-path* "/tmp/cl-tmux-1/x.sock"))
          (is (string= "/tmp/cl-tmux-1/x.sock" (expand "#{socket_path}"))
              "socket_path must reflect the bound socket"))))))

(test window-stack-index-and-client-flags-vars
  "#{window_stack_index} reflects the session MRU stack; refresh-client -f
   sets #{client_flags} ('!' removes)."
  (with-fake-session (s :nwindows 2)
    (let* ((wins (cl-tmux/model:session-windows s))
           (w0 (first wins)) (w1 (second wins)))
      (cl-tmux/model:session-select-window s w0)
      (cl-tmux/model:session-select-window s w1)
      (flet ((expand (spec win)
               (cl-tmux/format:expand-format
                spec (cl-tmux/format:format-context-from-session s win nil))))
        (is (string= "0" (expand "#{window_stack_index}" w1))
            "the current window must be stack index 0")
        (is (string= "1" (expand "#{window_stack_index}" w0))
            "the previously-current window must be stack index 1")
        (let ((cl-tmux::*client-flags* nil))
          (cl-tmux::%cmd-refresh-client-arg s '("-f" "no-output,read-only"))
          (is (string= "no-output,read-only"
                       (expand "#{client_flags}" w1))
              "refresh-client -f must set client_flags (sorted)")
          (cl-tmux::%cmd-refresh-client-arg s '("-f" "!read-only"))
          (is (string= "no-output" (expand "#{client_flags}" w1))
              "'!flag' must remove a flag"))))))
