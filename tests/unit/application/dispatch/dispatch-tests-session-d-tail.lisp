(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part D tail: named paste-buffer commands,
;;;; join-pane marked-pane.

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

(describe "dispatch-suite"

  ;;; ── Named paste-buffer commands (-b name) ────────────────────────────────────

  ;; set-buffer -b name data stores a named buffer retrievable by name.
  (it "cmd-set-buffer-b-stores-named"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux::%cmd-set-buffer-arg s '("-b" "mybuf" "hello" "world"))
        (expect (string= "hello world" (cl-tmux/buffer:get-named-buffer "mybuf"))))))

  ;; set-buffer -n new-name renames the most recent buffer and keeps its text.
  (it "cmd-set-buffer-n-renames-latest-buffer"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux/buffer:add-paste-buffer "hello")
        (cl-tmux::%cmd-set-buffer-arg s '("-n" "renamed" "ignored"))
        (expect (equal '("renamed") (cl-tmux/buffer:buffer-names)))
        (expect (string= "hello" (cl-tmux/buffer:get-named-buffer "renamed")))
        (expect (null (cl-tmux/buffer:get-named-buffer "buffer0"))))))

  ;; set-buffer -b name -n new-name renames a selected buffer and ignores data.
  (it "cmd-set-buffer-b-n-renames-named-buffer"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux/buffer:set-named-buffer "old" "named text")
        (cl-tmux/buffer:add-paste-buffer "keep")
        (cl-tmux::%cmd-set-buffer-arg s '("-b" "old" "-n" "new" "ignored"))
        (expect (equal '("new" "buffer0") (cl-tmux/buffer:buffer-names)))
        (expect (string= "named text" (cl-tmux/buffer:get-named-buffer "new")))
        (expect (null (cl-tmux/buffer:get-named-buffer "old"))))))

  ;; set-buffer rejects unsupported target-client input before mutating buffers.
  (it "cmd-set-buffer-rejects-target-client-flag"
    (with-empty-buffers
      (with-fake-session (s)
        (with-command-rejection-state (s
                                       (cl-tmux::%cmd-set-buffer-arg
                                        s '("-t" "client" "hello"))
                                       "set-buffer: unsupported argument"
                                       "set-buffer rejects -t")
          (expect (null (cl-tmux/buffer:get-paste-buffer 0)))))))

  ;; set-buffer -n new-name on an empty ring reports that no source buffer exists.
  (it "cmd-set-buffer-n-reports-no-buffer-when-empty"
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (expect (null (cl-tmux::%cmd-set-buffer-arg s '("-n" "renamed" "ignored"))))
          (assert-overlay-contains "no buffer" *overlay*
                                   "set-buffer -n reports the missing source buffer")
          (expect (null (cl-tmux/buffer:list-paste-buffers-with-names)))))))

  ;; set-buffer -a -b name appends to the existing named buffer.
  (it "cmd-set-buffer-a-appends-named"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux::%cmd-set-buffer-arg s '("-b" "b" "foo"))
        (cl-tmux::%cmd-set-buffer-arg s '("-a" "-b" "b" "bar"))
        (expect (string= "foobar" (cl-tmux/buffer:get-named-buffer "b"))))))

  ;; The tokenized command-line path dispatches set-buffer -b to the arg handler.
  (it "run-command-line-set-buffer-b-stores-named"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux::%run-command-line s "set-buffer -b mybuf hello world")
        (expect (string= "hello world" (cl-tmux/buffer:get-named-buffer "mybuf"))))))

  ;; set-buffer -w (OSC 52 clipboard flag) is accepted and still stores the buffer.
  (it "cmd-set-buffer-w-stores-and-accepts"
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux::%cmd-set-buffer-arg s '("-w" "hello"))
          (expect (null *overlay*))
          (expect (string= "hello" (cl-tmux/buffer:get-paste-buffer 0)))))))

  ;; set-buffer rejects flags outside the canonical local args set (ab:n:w).
  (it "cmd-set-buffer-rejects-unsupported-arguments"
    (with-command-rejection-cases
        (s args '(("-Z" "hello"))
         (cl-tmux::%cmd-set-buffer-arg s args)
         "set-buffer: unsupported argument"
         (format nil "set-buffer rejects ~S" args)
         :empty-buffers t)
      (expect (null (cl-tmux/buffer:get-paste-buffer 0)))
      (expect (null (cl-tmux/buffer:get-named-buffer "ignored")))))

  ;; show-buffer -b name shows that buffer's content in an overlay.
  (it "cmd-show-buffer-b-shows-named"
    (with-empty-buffers
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux/buffer:set-named-buffer "b" "shown-content")
          (cl-tmux::%cmd-show-buffer-arg s '("-b" "b"))
          (assert-overlay-active "show-buffer -b opens an overlay")
          (assert-overlay-contains "shown-content" *overlay*
                                   "show-buffer -b")))))

  ;; delete-buffer -b name removes that named buffer.
  (it "cmd-delete-buffer-b-deletes-named"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux/buffer:set-named-buffer "b" "x")
        (cl-tmux::%cmd-delete-buffer-arg s '("-b" "b"))
        (expect (null (cl-tmux/buffer:get-named-buffer "b"))))))

  ;; save-buffer -b name path writes the named buffer to a file.
  (it "cmd-save-buffer-b-writes-named-buffer"
    (with-empty-buffers
      (with-fake-session (s)
        (with-temporary-text-file (path "cl-tmux-save-buffer" "")
          (cl-tmux/buffer:set-named-buffer "saved" "named text")
          (cl-tmux::%run-command-tokens s (list "save-buffer" "-b" "saved" path))
          (expect (string= "named text" (uiop:read-file-string path)))))))

  ;; save-buffer -a appends the selected buffer to an existing file.
  (it "cmd-save-buffer-a-appends"
    (with-empty-buffers
      (with-fake-session (s)
        (with-temporary-text-file (path "cl-tmux-save-buffer-append" "pre:")
          (cl-tmux/buffer:add-paste-buffer "post")
          (cl-tmux::%run-command-tokens s (list "save-buffer" "-a" path))
          (expect (string= "pre:post" (uiop:read-file-string path)))))))

  ;; save-buffer rejects unknown flags and stray positionals before writing.
  (it "cmd-save-buffer-rejects-unsupported-arguments"
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
                (expect (string= "pre:" (uiop:read-file-string path))))))))))

  ;; load-buffer -b name path loads file contents.
  (it "cmd-load-buffer-b-loads-named-buffer"
    (with-empty-buffers
      (with-fake-session (s)
        (with-temporary-text-file (path "cl-tmux-load-buffer" "from file")
          (cl-tmux::%run-command-tokens s
                                        (list "load-buffer"
                                              "-b" "loaded"
                                              path))
          (expect (string= "from file" (cl-tmux/buffer:get-named-buffer "loaded")))))))

  ;; load-buffer rejects unknown flags and extra positionals before loading.
  (it "cmd-load-buffer-rejects-unsupported-arguments"
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
                (expect (string= "existing" (cl-tmux/buffer:get-paste-buffer 0))))))))))

  ;; paste-buffer -d -b name deletes the named buffer after pasting it.
  (it "cmd-paste-buffer-d-deletes-named-after-paste"
    (with-empty-buffers
      (with-fake-session (s)
        (cl-tmux/buffer:set-named-buffer "b" "data")
        (cl-tmux::%cmd-paste-buffer-arg s '("-d" "-b" "b"))
        (expect (null (cl-tmux/buffer:get-named-buffer "b"))))))

  ;; paste-buffer rejects unknown flags and positional arguments before side effects.
  (it "cmd-paste-buffer-rejects-unsupported-arguments"
    (with-command-rejection-cases
        (s args '(("-d" "-b" "b" "extra")
                  ("-Z" "-d" "-b" "b"))
         (cl-tmux::%cmd-paste-buffer-arg s args)
         "paste-buffer: unsupported argument"
         (format nil "paste-buffer rejects ~S" args)
         :before-command (cl-tmux/buffer:set-named-buffer "b" "data")
         :empty-buffers t)
      (expect (string= "data" (cl-tmux/buffer:get-named-buffer "b")))))

  ;; delete-buffer and show-buffer reject unsupported arguments before mutation/output.
  (it "cmd-delete-and-show-buffer-reject-unsupported-arguments"
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
      (expect (string= "shown-content" (cl-tmux/buffer:get-named-buffer "b")))))

  ;; copy-mode rejects unknown flags and positionals before entering copy-mode.
  (it "cmd-copy-mode-rejects-unsupported-arguments"
    (with-command-rejection-cases
        (s args '(("extra") ("-Z") ("-d") ("-S"))
         (cl-tmux::%cmd-copy-mode-arg s args)
         "copy-mode: unsupported argument"
         (format nil "copy-mode rejects ~S" args)
         :bindings ((cl-tmux::*dirty* nil)))
      (expect (screen-copy-mode-p (active-screen s)) :to-be-falsy)
      (expect cl-tmux::*dirty* :to-be-falsy)))

  ;; capture-pane -b name stores the captured content under that name.
  (it "cmd-capture-pane-b-stores-named"
    (with-empty-buffers
      (with-fake-session (s)
        (feed (active-screen s) "captured text")
        (cl-tmux::%cmd-capture-pane-arg s '("-b" "cap"))
        (expect (search "captured text" (or (cl-tmux/buffer:get-named-buffer "cap") ""))))))

  ;; `bind X paste-buffer -b foo` is accepted by the config parser.
  (it "config-bind-accepts-paste-buffer-b-flag"
    (with-isolated-config
      (expect (= 1 (cl-tmux/config:load-config-from-string "bind X paste-buffer -b foo")))))

  ;;; ── Coverage gap #19: join/move-pane default source = marked pane ───────────

  ;; join-pane and move-pane reject the removed -p percentage shorthand before moving panes.
  (it "run-command-line-join-and-move-pane-reject-percent-shorthand"
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
            (expect (equal dst-panes (cl-tmux/model:window-panes dst-win)))
            (expect (equal src-panes (cl-tmux/model:window-panes src-win))))))))

  ;; join-pane without -s uses *server-marked-pane* as source when it is set.
  (it "cmd-join-pane-uses-marked-pane-as-default-source"
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
        (expect (member pane1 (cl-tmux/model:window-panes win0))))))

  ;; join-pane with explicit -s ignores *server-marked-pane* and uses the given source.
  (it "cmd-join-pane-ignores-marked-pane-when-s-given"
    (with-fake-session (s :nwindows 2)
      (let* ((wins   (cl-tmux/model:session-windows s))
             (win0   (first wins))
             (win1   (second wins))
             (pane0  (cl-tmux/model:window-active-pane win0))
             (pane1  (cl-tmux/model:window-active-pane win1))
             ;; Mark pane0 — join-pane -s win1 should still use pane1.
             (cl-tmux::*server-marked-pane* pane0))
        (expect (eq pane0 cl-tmux::*server-marked-pane*))
        ;; Point session at win0.
        (cl-tmux/model:session-select-window s win0)
        ;; Explicit -s @N (win1 window-id sigil) targets pane1, not the marked pane.
        (cl-tmux::%cmd-join-pane-arg s (list "-s" (format nil "@~D" (cl-tmux/model:window-id win1))))
        (expect (member pane1 (cl-tmux/model:window-panes win0)))))))
