;;;; Renderer output helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

(defun render-pane-output (session pane)
  "Render PANE to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-pane s session pane)))

(defmacro with-copy-mode-render-fixture ((session-var pane-var screen-var w h
                                          &key (content "")
                                               (position-format "")
                                               (options '()))
                                         &body body)
  "Bind a renderer session, its pane, and screen under isolated copy-mode defaults."
  (let ((option-pairs (if (and (consp options) (eq (car options) 'quote))
                          (second options)
                          options)))
    `(with-isolated-options ("copy-mode-position-style" "default"
                             "copy-mode-position-format" ,position-format
                             ,@option-pairs)
       (let* ((,session-var (make-renderer-test-session ,w ,h :content ,content))
              (,pane-var (first (window-panes (session-active-window ,session-var))))
              (,screen-var (pane-screen ,pane-var)))
         ,@body))))

(defmacro with-copy-mode-selection-fixture ((session-var pane-var screen-var w h
                                             &key (content "")
                                                  (mark-row nil)
                                                  (mark-col nil)
                                                  (cursor-row nil)
                                                  (cursor-col nil)
                                                  (selecting-p t)
                                                  (copy-mode-p t)
                                                  (position-format "")
                                                  (options '()))
                                            &body body)
  "Bind a copy-mode renderer fixture with selection state preconfigured."
  `(with-copy-mode-render-fixture (,session-var ,pane-var ,screen-var ,w ,h
                                   :content ,content
                                   :position-format ,position-format
                                   :options ,options)
     (setf (screen-copy-mode-p ,screen-var) ,copy-mode-p
           (screen-copy-selecting ,screen-var) ,selecting-p
           (screen-copy-offset ,screen-var) 0
           (screen-copy-mark ,screen-var)
           (and ,mark-row ,mark-col (cons ,mark-row ,mark-col))
           (screen-copy-cursor ,screen-var)
           (and ,cursor-row ,cursor-col (cons ,cursor-row ,cursor-col)))
     ,@body))

(defun render-status-bar-output (sess rows cols &key ((:status-row status-row)
                                                     nil
                                                     status-row-supplied-p))
  "Render the status bar for SESS to a string using the production renderer."
  (with-output-to-string (s)
    (if status-row-supplied-p
        (cl-tmux/renderer::render-status-bar s sess rows cols :status-row status-row)
        (cl-tmux/renderer::render-status-bar s sess rows cols))))

(defun render-overlay-output (width)
  "Render the current overlay to a string using the production renderer."
  (with-output-to-string (buf)
    (cl-tmux/renderer::render-overlay buf width)))

(defun render-popup-output (popup rows cols)
  "Render POPUP to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-popup s popup rows cols)))

(defun render-menu-output (menu rows cols)
  "Render MENU to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-menu s menu rows cols)))

(defun render-tree-borders-output (tree active-pane width)
  "Render TREE borders for ACTIVE-PANE to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-tree-borders s tree active-pane width)))
