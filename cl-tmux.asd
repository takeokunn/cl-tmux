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
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "pty")
     (:file "protocol")
     (:file "transport")
     (:file "net")
     (:module "terminal"
      :serial t
      :components
      ((:file "types")
       (:file "actions")
       (:file "sgr")
       (:file "csi")
       (:file "parser")
       (:file "emulator")))
     (:file "model")
     (:file "prompt")
     (:file "commands")
     (:file "renderer")
     (:file "input")
     (:file "runtime")
     (:file "events")
     (:file "server")
     (:file "client")
     (:file "main"))))
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
     (:file "helpers")
     (:file "terminal-tests")
     (:file "layout-tests")
     (:file "model-tests")
     (:file "config-tests")
     (:file "renderer-tests")
     (:file "events-tests")
     (:file "commands-tests")    ; after events-tests (shares its no-PTY fixtures idiom)
     (:file "prompt-tests")
     (:file "protocol-tests")
     (:file "transport-tests")
     (:file "net-tests")
     (:file "server-tests")
     (:file "pty-tests")
     (:file "input-tests")
     (:file "main-tests")
     (:file "suite"))))
  ;; Run with: (asdf:test-system :cl-tmux)
  :perform (test-op (op c)
             (symbol-call :cl-tmux/test :run-tests)))
