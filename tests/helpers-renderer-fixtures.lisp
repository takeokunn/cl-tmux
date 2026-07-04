(in-package #:cl-tmux/test)

;;;; Renderer pane and session fixtures.

(defun make-test-pane (w h &key (id 1) (content "") (x 0) (y 0))
  "Build a no-PTY pane of W x H at (X, Y) with ID.
   CONTENT is fed into the pane's screen if non-empty.
   Returns the pane; the screen is accessible via (pane-screen pane)."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id id :x x :y y :width w :height h
                            :fd -1 :screen screen)))
    (unless (string= content "")
      (feed screen content))
    pane))

(defun make-selecting-screen (w h mark-row mark-col cursor-row cursor-col
                              &key (offset 0) rect)
  "Build a screen of W x H in copy-mode with an active selection.
   MARK-ROW/COL and CURSOR-ROW/COL define the selection anchor and cursor.
   OFFSET (default 0) sets the copy-mode scroll offset.
   RECT non-nil sets rectangle-select mode."
  (let ((screen (make-screen w h)))
    (setf (cl-tmux/terminal/types:screen-copy-mode-p        screen) t
          (cl-tmux/terminal/types:screen-copy-selecting     screen) t
          (cl-tmux/terminal/types:screen-copy-offset        screen) offset
          (cl-tmux/terminal/types:screen-copy-mark          screen) (cons mark-row   mark-col)
          (cl-tmux/terminal/types:screen-copy-cursor        screen) (cons cursor-row cursor-col)
          (cl-tmux/terminal/types:screen-copy-rect-select-p screen) (and rect t))
    screen))

(defun make-renderer-test-session (w h &key (content ""))
  "A 1-window, 1-pane session whose pane screen has CONTENT fed into it.
   No PTY is allocated (fd -1), so this is safe in any environment.
   Shared by renderer-tests.lisp, renderer-pane-tests.lisp, and prompt-tests.lisp."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen screen))
         (win    (make-window :id 1 :name "1" :width w :height h :panes (list pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (unless (string= content "") (feed screen content))
    sess))

(defun make-two-window-session (w h &key (w0-content "") (w1-content ""))
  "Build a 2-window session.  Each window has one pane of W x H with no PTY.
   W0-CONTENT / W1-CONTENT are fed into the respective pane screens.
   The first window is selected on return.
   Returns (values session window0 pane0 window1 pane1)."
  (let* ((screen0 (make-screen w h))
         (pane0   (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen screen0))
         (win0    (make-window :id 1 :name "alpha" :width w :height h :panes (list pane0)))
         (screen1 (make-screen w h))
         (pane1   (make-pane :id 2 :x 0 :y 0 :width w :height h :fd -1 :screen screen1))
         (win1    (make-window :id 2 :name "beta"  :width w :height h :panes (list pane1)))
         (sess    (make-session :id 1 :name "0" :windows (list win0 win1))))
    (window-select-pane win0 pane0)
    (window-select-pane win1 pane1)
    (session-select-window sess win0)
    (unless (string= w0-content "") (feed screen0 w0-content))
    (unless (string= w1-content "") (feed screen1 w1-content))
    (values sess win0 pane0 win1 pane1)))
