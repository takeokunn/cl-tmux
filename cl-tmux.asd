(defsystem "cl-tmux"
  :description "A tmux-compatible terminal multiplexer in Common Lisp"
  :version "0.1.0"
  :author "motoki317 <motoki317@gmail.com>"
  :license "MIT"
  :depends-on (:cffi           ; C foreign-function interface
               :bordeaux-threads ; portable threads + locks
               :babel)           ; string↔octet encoding
  :components
  ((:module "src"
    :serial nil
    :components
    ((:file "package")
     (:file "config"   :depends-on ("package"))
     (:file "pty"      :depends-on ("package" "config"))
     (:file "terminal" :depends-on ("package"))
     (:file "model"    :depends-on ("package" "config" "terminal" "pty"))
     (:file "renderer" :depends-on ("package" "model" "terminal"))
     (:file "input"    :depends-on ("package" "config" "pty"))
     (:file "main"     :depends-on ("package" "config" "pty" "terminal"
                                    "model" "renderer" "input")))))
  ;; Build a standalone binary: (asdf:make :cl-tmux)
  :build-operation "program-op"
  :build-pathname "cl-tmux"
  :entry-point "cl-tmux:main"
  :in-order-to ((test-op (test-op "cl-tmux/test"))))

(defsystem "cl-tmux/test"
  :description "Test suite for cl-tmux"
  :depends-on (:cl-tmux :fiveam)
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "terminal-tests")
     (:file "layout-tests")
     (:file "pty-tests")
     (:file "suite"))))
  ;; Run with: (asdf:test-system :cl-tmux)
  :perform (test-op (op c)
             (symbol-call :cl-tmux/test :run-tests)))
