(in-package #:cl-tmux)

;;;; Paste-buffer command handler helpers.
;;;;
;;;; These functions implement the :list-buffers, :show-buffer, :choose-buffer,
;;;; :delete-buffer, :save-buffer, and :load-buffer command handlers that are
;;;; registered in dispatch-handlers.lisp via define-command-handlers.
;;;;
;;;; Keeping them in a dedicated file reduces the size of dispatch-handlers.lisp
;;;; while keeping the handler table in a single define-command-handlers call.

;;; -- %cmd-list-buffers -------------------------------------------------------

(defun %cmd-list-buffers (&optional session args)
  "list-buffers [-F format] [-f filter]: show an overlay listing all paste
   buffers with name, length, and preview.  Lines read `name: NN bytes: preview`,
   matching tmux's named-buffer listing.  -F/-f (format/filter) are accepted and
   their arguments consumed.  SESSION/ARGS are optional so both the bare named
   form and the arg-table form (which always passes session+args) work."
  (declare (ignore session args))
  (show-overlay
   (%named-paste-buffer-listing-string
    (cl-tmux/buffer:list-paste-buffers-with-names)
    :preview-length +buffer-preview-length+)))

;;; -- %cmd-show-buffer --------------------------------------------------------

(defun %cmd-show-buffer ()
  "Show the full content of paste buffer 0 in an overlay."
  (let ((buffer (cl-tmux/buffer:get-paste-buffer 0)))
    (show-overlay (or buffer "(no paste buffers)"))))

;;; -- %cmd-choose-buffer ------------------------------------------------------

(defun %cmd-choose-buffer (session &optional args)
  "choose-buffer [-F format] [-f filter] [-O order] [-r] [-N] [-Z] [template]:
   show a numbered list of paste buffers; prompt for an index and paste it.
   The chooser-customisation flags are accepted and their arguments consumed
   (cl-tmux renders a simple numbered listing).  ARGS is optional so both the
   bare named form and the arg-table form work."
  (declare (ignore args))
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
        (show-overlay "(no paste buffers)"))))

;;; -- %cmd-choose-tree / %cmd-choose-window -----------------------------------
;;;
;;; Scriptable forms.  These were referenced in the dispatch spec table but never
;;; defined, so `choose-tree`/`choose-window` (which route through the arg-table,
;;; whose dispatch always funcalls the handler with (session args)) errored with
;;; an undefined function.  They accept the chooser-customisation flags and
;;; delegate to the interactive :choose-tree / :choose-window bindings.

(defun %cmd-choose-tree (session &optional args)
  "choose-tree [-GNrswZ] [-F format] [-f filter] [-O order] [-t target] [template]:
   open the session/window tree chooser overlay.  Flags are accepted; cl-tmux
   renders the interactive overlay.  Scriptable form of the :choose-tree binding."
  (declare (ignore args))
  (dispatch-command session :choose-tree nil))

(defun %cmd-choose-window (session &optional args)
  "choose-window [-GNrwZ] [-F format] [-f filter] [-O order] [-t target] [template]:
   open the window chooser overlay.  Flags are accepted; cl-tmux renders the
   interactive overlay.  Scriptable form of the :choose-window binding."
  (declare (ignore args))
  (dispatch-command session :choose-window nil))

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
