(in-package #:cl-tmux/test)

;;;; list-sessions, rename-session, switch-client, last-session, display-message,
;;;; and source-file edge cases

(in-suite server-suite)

;;; -- list-sessions format ---------------------------------------------------

(test list-sessions-format
  :description "The :list-sessions command overlay contains the session name and window count."
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

(test rename-session-updates-registry
  :description "Renaming a session via :rename-session also updates the server registry key."
  (with-empty-registry
    (with-fake-session (s :nwindows 1)
      (cl-tmux::server-add-session s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :rename-session nil)
        (funcall (prompt-on-submit *prompt*) "renamed-sess"))
      (is (eq s (cl-tmux::server-find-session "renamed-sess"))
          "registry must index session under new name")
      (is (null (cl-tmux::server-find-session "0"))
          "old name must be removed from registry"))))

;;; -- switch-client-next / switch-client-prev --------------------------------

(test switch-client-next-touches-next-session
  :description ":switch-client-next touches the next session in the registry list and marks *dirty* so the server re-renders."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 1000))
          (s2 (make-session :id 2 :name "s2" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (with-loop-state
        (cl-tmux::dispatch-command s1 :switch-client-next nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-next must mark *dirty*")))))

(test switch-client-prev-touches-prev-session
  :description ":switch-client-prev touches the previous session in the registry list."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "s2" :windows nil :last-active 200)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (with-loop-state
        (cl-tmux::dispatch-command s2 :switch-client-prev nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-prev must mark *dirty*")))))

;;; -- last-session cycles by recency -----------------------------------------

(test last-session-cycles-by-recency
  :description ":last-session touches the second-most-recently-active session."
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
        (is (> (cl-tmux/model:session-last-active s3) old-active-s3)
            ":last-session must update last-active of the second-most-recent session")
        (is-true cl-tmux::*dirty*
                 ":last-session must mark *dirty*")))))

;;; -- display-message sets overlay -------------------------------------------

(test display-message-sets-overlay
  :description ":display-message prompts for a message and sets the overlay when submitted."
  (with-fake-session (s :nwindows 1)
    (let ((*overlay* nil) (*prompt* nil)
          (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) ":display-message must open a prompt")
      (funcall (prompt-on-submit *prompt*) "hello world")
      (assert-overlay-active ":display-message on-submit must show overlay")
      (is-true (search "hello world" *overlay*)
               "overlay text must contain the submitted message"))))

;;; -- source-file loads config ------------------------------------------------

(test source-file-prompts-for-path
  :description ":source-file opens a prompt for a file path."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :source-file nil)
      (is (prompt-active-p) ":source-file must open a prompt")
      (is (string= "source-file" (prompt-label *prompt*))
          "prompt label must be 'source-file'"))))
