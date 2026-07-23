(in-package #:cl-tmux/test)

;;;; Direct unit tests for renderer-compose-overlay.lisp's %render-overlay-layer,
;;;; which previously had no dedicated coverage: render-popup/render-menu/
;;;; render-overlay are each tested directly via their own render-*-output
;;;; helpers, and the plain-cursor branch is exercised transitively through
;;;; render-session-to-string, but nothing asserted on %render-overlay-layer's
;;;; own priority dispatch (popup > menu > overlay > cursor) among *active-popup*/
;;;; *active-menu*/overlay-active-p.

(describe "renderer-suite/overlay-layer"

  ;; With no popup, menu, or overlay active and no active pane, nothing is drawn.
  (it "overlay-layer-no-state-and-no-pane-emits-nothing"
    (let ((*active-popup* nil)
          (*active-menu* nil)
          (*overlay* nil))
      (let ((out (with-output-to-string (buf)
                   (cl-tmux/renderer::%render-overlay-layer buf nil 10 20))))
        (expect (string= "" out)))))

  ;; With no popup, menu, or overlay active, an active pane gets its cursor
  ;; repositioned to the pane's screen-relative cursor location.
  (it "overlay-layer-falls-through-to-cursor-positioning"
    (let ((*active-popup* nil)
          (*active-menu* nil)
          (*overlay* nil)
          (p (make-no-pty-pane 1 3 2 20 5)))
      (let ((out (with-output-to-string (buf)
                   (cl-tmux/renderer::%render-overlay-layer buf p 10 20))))
        ;; pane-x=3, pane-y=2, default screen cursor (0,0) -> absolute row 2, col 3.
        (expect (string= (format nil "~C[3;4H" #\Escape) out)))))

  ;; With only the overlay active, %render-overlay-layer draws the overlay text.
  (it "overlay-layer-renders-overlay-when-only-overlay-active"
    (let ((*active-popup* nil)
          (*active-menu* nil)
          (*overlay* nil))
      (show-overlay "OVERLAY-SENTINEL")
      (unwind-protect
           (let ((out (with-output-to-string (buf)
                        (cl-tmux/renderer::%render-overlay-layer buf nil 10 20))))
             (expect (search "OVERLAY-SENTINEL" out)))
        (clear-overlay))))

  ;; The active menu takes priority over a merely-active overlay.
  (it "overlay-layer-menu-takes-priority-over-overlay"
    (let ((*active-popup* nil)
          (*active-menu* (make-menu :title "MENU-SENTINEL"
                                    :items '(("one" . :one))))
          (*overlay* nil))
      (show-overlay "OVERLAY-SENTINEL")
      (unwind-protect
           (let ((out (with-output-to-string (buf)
                        (cl-tmux/renderer::%render-overlay-layer buf nil 10 20))))
             (expect (search "MENU-SENTINEL" out))
             (expect (null (search "OVERLAY-SENTINEL" out))))
        (clear-overlay))))

  ;; The active popup takes priority over both an active menu and an active overlay.
  (it "overlay-layer-popup-takes-priority-over-menu-and-overlay"
    (let ((*active-popup* (make-popup :title "POPUP-SENTINEL"))
          (*active-menu* (make-menu :title "MENU-SENTINEL"
                                    :items '(("one" . :one))))
          (*overlay* nil))
      (show-overlay "OVERLAY-SENTINEL")
      (unwind-protect
           (let ((out (with-output-to-string (buf)
                        (cl-tmux/renderer::%render-overlay-layer buf nil 10 20))))
             (expect (search "POPUP-SENTINEL" out))
             (expect (null (search "MENU-SENTINEL" out)))
             (expect (null (search "OVERLAY-SENTINEL" out))))
        (clear-overlay)))))
