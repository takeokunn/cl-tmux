(in-package #:cl-tmux)

;;;; Buffer and local chooser command handler helpers.
;;;;
;;;; These functions implement the :list-buffers, :show-buffer, :choose-buffer,
;;;; :delete-buffer, :save-buffer, :load-buffer, and local chooser scriptable
;;;; command handlers.
;;;;
;;;; Keeping them in a dedicated file reduces the size of dispatch-handlers.lisp
;;;; while keeping the handler table in a single define-command-handlers call.

;;; -- Local chooser command input ---------------------------------------------

(defun %reject-local-chooser-args-p (command-name args)
  (when args
    (%overlayf "~A: unsupported argument" command-name)
    t))

;;; -- %cmd-list-buffers -------------------------------------------------------

(defun %cmd-list-buffers (&optional session args)
  "list-buffers: show an overlay listing all paste
   buffers with name, length, and preview.  Lines read `name: NN bytes: preview`,
   matching cl-tmux's named-buffer listing.  SESSION/ARGS are optional so both
   the bare named form and the arg-table form work."
  (declare (ignore session))
  (unless (%reject-local-chooser-args-p "list-buffers" args)
    (show-overlay
     (%named-paste-buffer-listing-string
      (cl-tmux/buffer:list-paste-buffers-with-names)
      :preview-length +buffer-preview-length+))))

;;; -- %cmd-show-buffer --------------------------------------------------------

(defun %cmd-show-buffer ()
  "Show the full content of paste buffer 0 in an overlay."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (show-overlay (or buffer "(no paste buffers)"))))

;;; -- %cmd-choose-buffer ------------------------------------------------------

(defun %cmd-choose-buffer (session &optional args)
  "choose-buffer: show a numbered list of paste buffers, prompt for an index,
   and paste it.  ARGS is optional so both the bare named form and the arg-table
   form work."
  (unless (%reject-local-chooser-args-p "choose-buffer" args)
    (let ((buffers (cl-tmux/buffer:list-paste-buffers)))
      (if buffers
          (let ((listing (%paste-buffer-listing-string buffers
                                                       :preview-length +buffer-preview-length+)))
            (show-overlay listing)
            (prompt-integer "choose buffer (index)"
                            (lambda (idx)
                              (let* ((text (cl-tmux/buffer:get-paste-buffer idx))
                                     (win  (session-active-window session))
                                     (ap   (and win (window-active-pane win))))
                                (%paste-to-pane ap text)))))
          (show-overlay "(no paste buffers)")))))

;;; -- %cmd-choose-client / %cmd-choose-tree / %cmd-choose-window --------------
;;;
;;; Scriptable forms delegate to the interactive local chooser bindings and keep
;;; a strict cl-tmux command surface: chooser-format/filter/sort/template
;;; arguments are rejected.

(defmacro define-choose-commands (&rest specs)
  "Define a scriptable-args-rejecting choose-* command function for each SPEC,
   a (function-name command-name dispatch-keyword docstring) quad."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name command-name keyword docstring) spec
                   `(defun ,name (session &optional args)
                      ,docstring
                      (unless (%reject-local-chooser-args-p ,command-name args)
                        (dispatch-command session ,keyword nil)))))
               specs)))

(define-choose-commands
  (%cmd-choose-client "choose-client" :choose-client
   "choose-client: show local client information.")
  (%cmd-choose-tree "choose-tree" :choose-tree
   "choose-tree: open the session/window tree chooser overlay.")
  (%cmd-choose-window "choose-window" :choose-window
   "choose-window: open the window chooser overlay."))

;;; -- %cmd-delete-buffer ------------------------------------------------------

(defun %cmd-delete-buffer ()
  "Delete paste buffer 0 and show a confirmation overlay."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (if buffer
        (progn
          (cl-tmux/buffer:delete-paste-buffer 0)
          (show-overlay "buffer 0 deleted"))
        (show-overlay "(no paste buffers to delete)"))))

;;; -- %cmd-save-buffer --------------------------------------------------------

(defun %cmd-save-buffer ()
  "Prompt for a file path and write paste buffer 0 to it."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (if buffer
        (prompt-nonempty "save-buffer to file"
                         (lambda (path)
                           (with-overlay-on-error ("save-buffer")
                             (with-open-file (f path
                                                :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
                               (write-string buffer f))
                             (%overlayf "saved to ~A" path))))
        (show-overlay "(no paste buffers to save)"))))

;;; -- %cmd-load-buffer --------------------------------------------------------

(defun %cmd-load-buffer ()
  "Prompt for a file path and load its content into the paste buffer ring."
  (prompt-nonempty "load-buffer from file"
                   (lambda (path)
                     (with-overlay-on-error ("load-buffer")
                       (let ((content
                               (with-open-file (f path
                                                  :direction :input
                                                  :if-does-not-exist :error)
                                 (let ((s (make-string (file-length f))))
                                   (read-sequence s f)
                                   s))))
                         (cl-tmux/buffer:add-paste-buffer content)
                         (%overlayf "loaded ~D bytes from ~A"
                                    (length content) path))))))
