(in-package #:cl-tmux/test)

;;;; list-sessions, rename-session, switch-client, last-session, display-message,
;;;; and source-file edge cases

(describe "server-suite"

  ;;; -- list-sessions format ---------------------------------------------------

  ;; The :list-sessions command overlay contains the session name and window count.
  (it "list-sessions-format"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "mysession" :windows (list 'w1 'w2))))
        (setf cl-tmux::*server-sessions* (list (cons "mysession" s1)))
        (let ((*overlay* nil)
              (cl-tmux::*dirty* nil)
              (cl-tmux::*running* t))
          (cl-tmux::dispatch-command s1 :list-sessions nil)
          (assert-overlay-active
           ":list-sessions must produce an overlay")
          (assert-overlay-contains "mysession" *overlay*
                                   "overlay should contain the session name")
          (assert-overlay-contains "2 windows" *overlay*
                                   "overlay should contain the window count")))))

  ;;; -- rename-session updates registry key ------------------------------------

  ;; Renaming a session via :rename-session also updates the server registry key.
  (it "rename-session-updates-registry"
    (with-empty-registry
      (with-fake-session (s :nwindows 1)
        (cl-tmux::server-add-session s)
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :rename-session nil)
          (funcall (prompt-on-submit *prompt*) "renamed-sess"))
        (expect (eq s (cl-tmux::server-find-session "renamed-sess")))
        (expect (null (cl-tmux::server-find-session "0"))))))

  ;;; -- switch-client-next / switch-client-prev --------------------------------

  ;; :switch-client-next touches the next session in the registry list and marks *dirty* so the server re-renders.
  (it "switch-client-next-touches-next-session"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 1000))
            (s2 (make-session :id 2 :name "s2" :windows nil :last-active 500)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (with-loop-state
          (cl-tmux::dispatch-command s1 :switch-client-next nil)
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;; :switch-client-prev touches the previous session in the registry list.
  (it "switch-client-prev-touches-prev-session"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 100))
            (s2 (make-session :id 2 :name "s2" :windows nil :last-active 200)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (with-loop-state
          (cl-tmux::dispatch-command s2 :switch-client-prev nil)
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;;; -- last-session cycles by recency -----------------------------------------

  ;; :last-session touches the second-most-recently-active session.
  (it "last-session-cycles-by-recency"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "oldest" :windows nil :last-active 100))
            (s2 (make-session :id 2 :name "newest" :windows nil :last-active 999))
            (s3 (make-session :id 3 :name "second" :windows nil :last-active 500)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (cl-tmux::server-add-session s3)
        (let ((old-active-s3 (cl-tmux/model:session-last-active s3))
              (cl-tmux::*dirty* nil)
              (cl-tmux::*running* t))
          (cl-tmux::dispatch-command s2 :last-session nil)
          (expect (> (cl-tmux/model:session-last-active s3) old-active-s3))
          (expect cl-tmux::*dirty* :to-be-truthy)))))

  ;;; -- display-message sets overlay -------------------------------------------

  ;; :display-message prompts for a message and sets the overlay when submitted.
  (it "display-message-sets-overlay"
    (with-fake-session (s :nwindows 1)
      (let ((*overlay* nil) (*prompt* nil)
            (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
        (cl-tmux::dispatch-command s :display-message nil)
        (expect (prompt-active-p))
        (funcall (prompt-on-submit *prompt*) "hello world")
        (assert-overlay-active ":display-message on-submit must show overlay")
        (expect (search "hello world" *overlay*) :to-be-truthy))))

  ;;; -- source-file loads config ------------------------------------------------

  ;; :source-file opens a prompt for a file path.
  (it "source-file-prompts-for-path"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :source-file nil)
        (expect (prompt-active-p))
        (expect (string= "source-file" (prompt-label *prompt*)))))))
