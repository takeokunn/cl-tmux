(in-package #:cl-tmux/test)

;;;; Dispatch coverage for format variables.

(describe "dispatch-suite"

  ;; #{session_grouped}/#{session_group_size}/#{session_group_list} expand from
  ;; the group registry; ungrouped sessions report 0/empty.
  (it "session-group-format-vars"
    (let ((cl-tmux::*session-groups* nil))
      (with-fake-session (s1)
        (with-fake-session (s2)
          (setf (cl-tmux/model:session-name s1) "ga"
                (cl-tmux/model:session-name s2) "gb")
          (flet ((expand (spec sess)
                   (cl-tmux/format:expand-format
                    spec (cl-tmux/format:format-context-from-session
                          sess (cl-tmux/model:session-active-window sess) nil))))
            (expect (string= "0" (expand "#{session_grouped}" s1)))
            (cl-tmux::server-new-session-in-group s2 s1)
            (expect (string= "1" (expand "#{session_grouped}" s1)))
            (expect (string= "2" (expand "#{session_group_size}" s1)))
            (let ((names (expand "#{session_group_list}" s1)))
              (expect (and (search "ga" names) (search "gb" names)))))))))

  ;; #{pane_start_command}/#{pane_start_path} expand from the pane spawn record;
  ;; #{socket_path} is empty in standalone mode and reflects the bound socket.
  (it "pane-start-and-socket-format-vars"
    (with-fake-session (s)
      (let* ((win  (cl-tmux/model:session-active-window s))
             (pane (cl-tmux/model:window-active-pane win)))
        (setf (cl-tmux/model:pane-start-command pane) "htop"
              (cl-tmux/model:pane-start-path pane) "/tmp/start-here")
        (flet ((expand (spec)
                 (cl-tmux/format:expand-format
                  spec (cl-tmux/format:format-context-from-session s win pane))))
          (expect (string= "htop" (expand "#{pane_start_command}")))
          (expect (string= "/tmp/start-here" (expand "#{pane_start_path}")))
          (let ((cl-tmux::*bound-socket-path* nil))
            (expect (string= "" (expand "#{socket_path}"))))
          (let ((cl-tmux::*bound-socket-path* "/tmp/cl-tmux-1/x.sock"))
            (expect (string= "/tmp/cl-tmux-1/x.sock" (expand "#{socket_path}"))))))))

  ;; #{window_stack_index} reflects the session MRU stack; refresh-client -f
  ;; sets #{client_flags} ('!' removes).
  (it "window-stack-index-and-client-flags-vars"
    (with-fake-session (s :nwindows 2)
      (let* ((wins (cl-tmux/model:session-windows s))
             (w0 (first wins)) (w1 (second wins)))
        (cl-tmux/model:session-select-window s w0)
        (cl-tmux/model:session-select-window s w1)
        (flet ((expand (spec win)
                 (cl-tmux/format:expand-format
                  spec (cl-tmux/format:format-context-from-session s win nil))))
          (expect (string= "0" (expand "#{window_stack_index}" w1)))
          (expect (string= "1" (expand "#{window_stack_index}" w0)))
          (let ((cl-tmux::*client-flags* nil))
            (cl-tmux::%cmd-refresh-client-arg s '("-f" "no-output,read-only"))
            (expect (string= "no-output,read-only"
                             (expand "#{client_flags}" w1)))
            (cl-tmux::%cmd-refresh-client-arg s '("-f" "!read-only"))
            (expect (string= "no-output" (expand "#{client_flags}" w1)))))))))
