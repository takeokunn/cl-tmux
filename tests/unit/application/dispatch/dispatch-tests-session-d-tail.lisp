(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part D tail: named paste-buffer commands,
;;;; join-pane marked-pane.

(in-suite dispatch-suite)

(defmacro with-command-rejection-cases
    ((sess case cases command-form overlay-message description
      &key before-command empty-buffers session-options bindings)
     &body body)
  (let* ((session-form
           `(with-fake-session (,sess ,@session-options)
              ,@(when before-command
                  (list before-command))
              (with-command-rejection-state (,sess
                                             ,command-form
                                             ,overlay-message
                                             ,description)
                ,@body)))
         (bound-session-form
           (if bindings
               `(let ,bindings
                  ,session-form)
               session-form)))
    `(dolist (,case ,cases)
       ,(if empty-buffers
            `(with-empty-buffers
               ,bound-session-form)
            bound-session-form))))

(defmacro with-temporary-text-file ((path prefix contents) &body body)
  `(let* ((label (format nil "~A-~D-~D.txt"
                         ,prefix
                         (get-universal-time)
                         (random 1000000)))
          (,path (namestring (merge-pathnames label (uiop:temporary-directory)))))
     (unwind-protect
          (progn
            (with-open-file (out ,path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
              (write-string ,contents out))
            ,@body)
       (ignore-errors (delete-file ,path)))))

;;; ── Named paste-buffer commands (-b name) ────────────────────────────────────

(test cmd-set-buffer-b-stores-named
  "set-buffer -b name data stores a named buffer retrievable by name."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "mybuf" "hello" "world"))
      (is (string= "hello world" (cl-tmux/buffer:get-named-buffer "mybuf"))
          "set-buffer -b stores the joined data under the name"))))

(test cmd-set-buffer-n-renames-latest-buffer
  "set-buffer -n new-name renames the most recent buffer and keeps its text."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:add-paste-buffer "hello")
      (cl-tmux::%cmd-set-buffer-arg s '("-n" "renamed" "ignored"))
      (is (equal '("renamed") (cl-tmux/buffer:buffer-names))
          "the buffer name changes to the requested new name")
      (is (string= "hello" (cl-tmux/buffer:get-named-buffer "renamed"))
          "rename preserves the source text")
      (is (null (cl-tmux/buffer:get-named-buffer "buffer0"))
          "the old automatic name is gone"))))

(test cmd-set-buffer-b-n-renames-named-buffer
  "set-buffer -b name -n new-name renames a selected buffer and ignores data."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "old" "named text")
      (cl-tmux/buffer:add-paste-buffer "keep")
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "old" "-n" "new" "ignored"))
      (is (equal '("new" "buffer0") (cl-tmux/buffer:buffer-names))
          "the selected named buffer is renamed in place")
      (is (string= "named text" (cl-tmux/buffer:get-named-buffer "new"))
          "rename keeps the original text")
      (is (null (cl-tmux/buffer:get-named-buffer "old"))
          "the old name is removed"))))

(test cmd-set-buffer-rejects-target-client-flag
  "set-buffer rejects unsupported target-client input before mutating buffers."
  (with-empty-buffers
    (with-fake-session (s)
      (with-command-rejection-state (s
                                     (cl-tmux::%cmd-set-buffer-arg
                                      s '("-t" "client" "hello"))
                                     "set-buffer: unsupported argument"
                                     "set-buffer rejects -t")
        (is (null (cl-tmux/buffer:get-paste-buffer 0))
            "set-buffer -t must not store a paste buffer after rejection")))))

(test cmd-set-buffer-n-reports-no-buffer-when-empty
  "set-buffer -n new-name on an empty ring reports that no source buffer exists."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (is (null (cl-tmux::%cmd-set-buffer-arg s '("-n" "renamed" "ignored")))
            "renaming fails cleanly without a source buffer")
        (assert-overlay-contains "no buffer" *overlay*
                                 "set-buffer -n reports the missing source buffer")
        (is (null (cl-tmux/buffer:list-paste-buffers-with-names))
            "no buffer should be created on failure")))))

(test cmd-set-buffer-a-appends-named
  "set-buffer -a -b name appends to the existing named buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%cmd-set-buffer-arg s '("-b" "b" "foo"))
      (cl-tmux::%cmd-set-buffer-arg s '("-a" "-b" "b" "bar"))
      (is (string= "foobar" (cl-tmux/buffer:get-named-buffer "b"))
          "-a appends to the named buffer"))))

(test run-command-line-set-buffer-b-stores-named
  "The tokenized command-line path dispatches set-buffer -b to the arg handler."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux::%run-command-line s "set-buffer -b mybuf hello world")
      (is (string= "hello world" (cl-tmux/buffer:get-named-buffer "mybuf"))
          "set-buffer -b must work through %run-command-line"))))

(test cmd-set-buffer-w-stores-and-accepts
  "set-buffer -w (OSC 52 clipboard flag) is accepted and still stores the buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%cmd-set-buffer-arg s '("-w" "hello"))
        (is (null *overlay*)
            "set-buffer -w must not raise an unsupported-argument overlay")
        (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
            "set-buffer -w stores the data as well as sending to the clipboard")))))

(test cmd-set-buffer-rejects-unsupported-arguments
  "set-buffer rejects flags outside the canonical local args set (ab:n:w)."
  (with-command-rejection-cases
      (s args '(("-Z" "hello"))
       (cl-tmux::%cmd-set-buffer-arg s args)
       "set-buffer: unsupported argument"
       (format nil "set-buffer rejects ~S" args)
       :empty-buffers t)
    (is (null (cl-tmux/buffer:get-paste-buffer 0))
        "~S must not store a paste buffer after rejection" args)
    (is (null (cl-tmux/buffer:get-named-buffer "ignored"))
        "~S must not store a named buffer after rejection" args)))

(test cmd-show-buffer-b-shows-named
  "show-buffer -b name shows that buffer's content in an overlay."
  (with-empty-buffers
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/buffer:set-named-buffer "b" "shown-content")
        (cl-tmux::%cmd-show-buffer-arg s '("-b" "b"))
        (assert-overlay-active "show-buffer -b opens an overlay")
        (assert-overlay-contains "shown-content" *overlay*
                                 "show-buffer -b")))))

(test cmd-delete-buffer-b-deletes-named
  "delete-buffer -b name removes that named buffer."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "b" "x")
      (cl-tmux::%cmd-delete-buffer-arg s '("-b" "b"))
      (is (null (cl-tmux/buffer:get-named-buffer "b"))
          "the named buffer is gone after delete-buffer -b"))))

(test cmd-save-buffer-b-writes-named-buffer
  "save-buffer -b name path writes the named buffer to a file."
  (with-empty-buffers
    (with-fake-session (s)
      (with-temporary-text-file (path "cl-tmux-save-buffer" "")
        (cl-tmux/buffer:set-named-buffer "saved" "named text")
        (cl-tmux::%run-command-tokens s (list "save-buffer" "-b" "saved" path))
        (is (string= "named text" (uiop:read-file-string path))
            "wrote selected named buffer")))))

(test cmd-save-buffer-a-appends
  "save-buffer -a appends the selected buffer to an existing file."
  (with-empty-buffers
    (with-fake-session (s)
      (with-temporary-text-file (path "cl-tmux-save-buffer-append" "pre:")
        (cl-tmux/buffer:add-paste-buffer "post")
        (cl-tmux::%run-command-tokens s (list "save-buffer" "-a" path))
        (is (string= "pre:post" (uiop:read-file-string path))
            "appended most recent buffer")))))

(test cmd-save-buffer-rejects-unsupported-arguments
  "save-buffer rejects unknown flags and stray positionals before writing."
  (dolist (case '(:extra-arg :unknown-flag))
    (with-empty-buffers
      (with-fake-session (s)
        (with-temporary-text-file (path "cl-tmux-save-buffer-reject" "pre:")
          (let ((args (ecase case
                        (:extra-arg (list "-b" "saved" path "extra"))
                        (:unknown-flag (list "-Z" path)))))
            (cl-tmux/buffer:set-named-buffer "saved" "named text")
            (with-command-rejection-state
                (s
                 (cl-tmux::%run-command-tokens s (cons "save-buffer" args))
                 "save-buffer: unsupported argument"
                 (format nil "save-buffer rejects ~S" args))
              (is (string= "pre:" (uiop:read-file-string path))
                  "~S must not overwrite the file after rejection" args))))))))

(test cmd-load-buffer-b-loads-named-buffer
  "load-buffer -b name path loads file contents."
  (with-empty-buffers
    (with-fake-session (s)
      (with-temporary-text-file (path "cl-tmux-load-buffer" "from file")
        (cl-tmux::%run-command-tokens s
                                      (list "load-buffer"
                                            "-b" "loaded"
                                            path))
        (is (string= "from file" (cl-tmux/buffer:get-named-buffer "loaded"))
            "loaded file into named buffer")))))

(test cmd-load-buffer-rejects-unsupported-arguments
  "load-buffer rejects unknown flags and extra positionals before loading."
  (dolist (case '(:extra-arg :unknown-flag :former-window-flag :former-target-flag))
    (with-empty-buffers
      (with-fake-session (s)
        (with-temporary-text-file (path "cl-tmux-load-buffer-reject" "from file")
          (let ((args (ecase case
                        (:extra-arg (list path "extra"))
                        (:unknown-flag (list "-Z" path))
                        (:former-window-flag (list "-w" path))
                        (:former-target-flag (list "-t" "client" path)))))
            (cl-tmux/buffer:add-paste-buffer "existing")
            (with-command-rejection-state
                (s
                 (cl-tmux::%run-command-tokens s (cons "load-buffer" args))
                 "load-buffer: unsupported argument"
                 (format nil "load-buffer rejects ~S" args))
              (is (string= "existing" (cl-tmux/buffer:get-paste-buffer 0))
                  "~S must not mutate existing buffers after rejection" args))))))))

(test cmd-paste-buffer-d-deletes-named-after-paste
  "paste-buffer -d -b name deletes the named buffer after pasting it."
  (with-empty-buffers
    (with-fake-session (s)
      (cl-tmux/buffer:set-named-buffer "b" "data")
      (cl-tmux::%cmd-paste-buffer-arg s '("-d" "-b" "b"))
      (is (null (cl-tmux/buffer:get-named-buffer "b"))
          "-d removes the named buffer after pasting"))))

(test cmd-paste-buffer-rejects-unsupported-arguments
  "paste-buffer rejects unknown flags and positional arguments before side effects."
  (with-command-rejection-cases
      (s args '(("-d" "-b" "b" "extra")
                ("-Z" "-d" "-b" "b"))
       (cl-tmux::%cmd-paste-buffer-arg s args)
       "paste-buffer: unsupported argument"
       (format nil "paste-buffer rejects ~S" args)
       :before-command (cl-tmux/buffer:set-named-buffer "b" "data")
       :empty-buffers t)
    (is (string= "data" (cl-tmux/buffer:get-named-buffer "b"))
        "~S must not delete the source buffer after rejection" args)))

(test cmd-delete-and-show-buffer-reject-unsupported-arguments
  "delete-buffer and show-buffer reject unsupported arguments before mutation/output."
  (with-command-rejection-cases
      (s case '((cl-tmux::%cmd-delete-buffer-arg
                 ("-b" "b" "extra")
                 "delete-buffer: unsupported argument")
                (cl-tmux::%cmd-delete-buffer-arg
                 ("-Z" "-b" "b")
                 "delete-buffer: unsupported argument")
                (cl-tmux::%cmd-show-buffer-arg
                 ("-b" "b" "extra")
                 "show-buffer: unsupported argument")
                (cl-tmux::%cmd-show-buffer-arg
                 ("-Z" "-b" "b")
                 "show-buffer: unsupported argument"))
       (funcall (first case) s (second case))
       (third case)
       (format nil "~S rejects ~S" (first case) (second case))
       :before-command (cl-tmux/buffer:set-named-buffer "b" "shown-content")
       :empty-buffers t)
    (is (string= "shown-content" (cl-tmux/buffer:get-named-buffer "b"))
        "~S must not mutate buffers after rejection" (first case))))

(test cmd-copy-mode-rejects-unsupported-arguments
  "copy-mode rejects unknown flags and positionals before entering copy-mode."
  (with-command-rejection-cases
      (s args '(("extra") ("-Z") ("-d") ("-S"))
       (cl-tmux::%cmd-copy-mode-arg s args)
       "copy-mode: unsupported argument"
       (format nil "copy-mode rejects ~S" args)
       :bindings ((cl-tmux::*dirty* nil)))
    (is-false (screen-copy-mode-p (active-screen s))
              "~S must not enter copy-mode after rejection" args)
    (is-false cl-tmux::*dirty*
              "~S must not mark the UI dirty after rejection" args)))

(test cmd-capture-pane-b-stores-named
  "capture-pane -b name stores the captured content under that name."
  (with-empty-buffers
    (with-fake-session (s)
      (feed (active-screen s) "captured text")
      (cl-tmux::%cmd-capture-pane-arg s '("-b" "cap"))
      (is (search "captured text" (or (cl-tmux/buffer:get-named-buffer "cap") ""))
          "capture-pane -b stores the capture under the given name"))))

(test config-bind-accepts-paste-buffer-b-flag
  "`bind X paste-buffer -b foo` is accepted by the config parser."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string "bind X paste-buffer -b foo"))
        "paste-buffer -b parses as one applied directive")))

;;; ── Coverage gap #19: join/move-pane default source = marked pane ───────────

(test run-command-line-join-and-move-pane-reject-percent-shorthand
  "join-pane and move-pane reject the removed -p percentage shorthand before moving panes."
  (dolist (command '("join-pane" "move-pane"))
    (with-fake-session (s :nwindows 2 :npanes 1)
      (let* ((wins (cl-tmux/model:session-windows s))
             (dst-win (first wins))
             (src-win (second wins))
             (dst-panes (copy-list (cl-tmux/model:window-panes dst-win)))
             (src-panes (copy-list (cl-tmux/model:window-panes src-win)))
             (command-line (format nil "~A -s @~D -p 30"
                                   command
                                   (cl-tmux/model:window-id src-win))))
        (cl-tmux/model:session-select-window s dst-win)
        (with-command-rejection-state (s
                                       (cl-tmux::%run-command-line s command-line)
                                       "unsupported argument"
                                       command-line)
          (is (equal dst-panes (cl-tmux/model:window-panes dst-win))
              "~A -p 30 must not add or reorder destination panes after rejection"
              command)
          (is (equal src-panes (cl-tmux/model:window-panes src-win))
              "~A -p 30 must not remove panes from the source window after rejection"
              command))))))

(test cmd-join-pane-uses-marked-pane-as-default-source
  "join-pane without -s uses *server-marked-pane* as source when it is set."
  (with-fake-session (s :nwindows 2)
    (let* ((wins   (cl-tmux/model:session-windows s))
           (win0   (first wins))
           (win1   (second wins))
           (pane0  (cl-tmux/model:window-active-pane win0))
           (pane1  (cl-tmux/model:window-active-pane win1))
           ;; Mark a pane in win1 — join-pane without -s should use it as source.
           (cl-tmux::*server-marked-pane* pane1))
      (declare (ignore pane0))
      ;; Point session at win0 (the destination window).
      (cl-tmux/model:session-select-window s win0)
      ;; join-pane with no flags: source = marked pane (pane1 from win1).
      (cl-tmux::%cmd-join-pane-arg s '())
      (is (member pane1 (cl-tmux/model:window-panes win0))
          "join-pane without -s must move the marked pane into the active window"))))

(test cmd-join-pane-ignores-marked-pane-when-s-given
  "join-pane with explicit -s ignores *server-marked-pane* and uses the given source."
  (with-fake-session (s :nwindows 2)
    (let* ((wins   (cl-tmux/model:session-windows s))
           (win0   (first wins))
           (win1   (second wins))
           (pane0  (cl-tmux/model:window-active-pane win0))
           (pane1  (cl-tmux/model:window-active-pane win1))
           ;; Mark pane0 — join-pane -s win1 should still use pane1.
           (cl-tmux::*server-marked-pane* pane0))
      (is (eq pane0 cl-tmux::*server-marked-pane*)
          "precondition: pane0 must be the marked pane")
      ;; Point session at win0.
      (cl-tmux/model:session-select-window s win0)
      ;; Explicit -s @N (win1 window-id sigil) targets pane1, not the marked pane.
      (cl-tmux::%cmd-join-pane-arg s (list "-s" (format nil "@~D" (cl-tmux/model:window-id win1))))
      (is (member pane1 (cl-tmux/model:window-panes win0))
          "join-pane -s must use the explicit source, not the marked pane"))))
