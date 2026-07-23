(in-package #:cl-tmux/test)

;;;; Popup and menu state tests (src/overlay.lisp).

(describe "overlay-suite"

  ;;; -- Popup struct constructors and accessors ---------------------------------

  ;; make-popup fills all slots to their documented defaults.
  (it "make-popup-defaults"
    (let ((p (make-popup)))
      (check-table (list (list (popup-width p)  40 "default width is 40")
                         (list (popup-height p) 10 "default height is 10")
                         (list (popup-screen p) nil "default screen is nil (text-only)")
                         (list (popup-pane p)   nil "default pane is nil (text-only)"))
                   :test #'equal)
      (expect (string= "" (popup-title p)))
      (expect (eq t (popup-close-on-exit p)))))

  ;; make-popup accepts keyword arguments that override all defaults.
  (it "make-popup-keyword-args"
    (let ((p (make-popup :width 80 :height 24
                         :title "Test Popup" :close-on-exit nil)))
      (check-table (list (list (popup-width p)  80 "width is 80")
                         (list (popup-height p) 24 "height is 24"))
                   :test #'equal)
      (expect (string= "Test Popup" (popup-title p)))
      (expect (null (popup-close-on-exit p)))))

  ;; make-popup :pane stores the pane reference; popup-pane returns it.
  (it "make-popup-with-non-nil-pane"
    (let* ((fake-pane (make-pane :id 42 :fd -1 :screen (make-screen 10 5)))
           (p (make-popup :pane fake-pane :title "live-popup")))
      (expect (eq fake-pane (popup-pane p)))
      (expect (= 42 (pane-id (popup-pane p))))))

  ;; Setting *active-popup* persists across accesses (session-level state).
  (it "active-popup-global-state"
    (let ((*active-popup* nil))
      (expect (null *active-popup*))
      (let ((p (make-popup :title "session popup")))
        (setf *active-popup* p)
        (expect (eq p *active-popup*))
        (expect (string= "session popup" (popup-title *active-popup*)))
        (setf *active-popup* nil)
        (expect (null *active-popup*)))))

  ;;; -- Menu struct constructors and accessors ----------------------------------

  ;; make-menu fills all slots to their documented defaults.
  (it "make-menu-defaults"
    (let ((m (make-menu)))
      (expect (string= "" (menu-title m)))
      (expect (null (menu-items m)))
      (expect (= 0  (menu-selected-index m)))))

  ;; make-menu accepts keyword arguments that override all defaults.
  (it "make-menu-keyword-args"
    (let ((items '(("Option A" . :a) ("Option B" . :b) ("Option C" . :c))))
      (let ((m (make-menu :title "Choose" :items items :selected-index 2)))
        (expect (string= "Choose" (menu-title m)))
        (expect (equal items (menu-items m)))
        (expect (= 2 (menu-selected-index m))))))

  ;; menu-x and menu-y (display-menu -x/-y position) default to NIL, meaning
  ;; 'centre on screen'.
  (it "menu-x-and-y-default-to-nil"
    (let ((m (make-menu)))
      (expect (null (menu-x m)))
      (expect (null (menu-y m)))))

  ;; make-menu :x and :y set a fixed display-menu position, overriding centring.
  (it "menu-x-and-y-keyword-args"
    (let ((m (make-menu :x 5 :y 10)))
      (expect (= 5  (menu-x m)))
      (expect (= 10 (menu-y m)))))

  ;; setf on menu-selected-index updates the struct slot.
  (it "menu-selected-index-mutable"
    (let ((m (make-menu :items '((:a) (:b) (:c)) :selected-index 0)))
      (expect (= 0 (menu-selected-index m)))
      (setf (menu-selected-index m) 2)
      (expect (= 2 (menu-selected-index m)))
      (setf (menu-selected-index m) 0)
      (expect (= 0 (menu-selected-index m)))))

  ;; Setting *active-menu* persists across accesses (session-level state).
  (it "active-menu-global-state"
    (let ((*active-menu* nil))
      (expect (null *active-menu*))
      (let ((m (make-menu :title "session menu" :items '(("X" . :x)))))
        (setf *active-menu* m)
        (expect (eq m *active-menu*))
        (expect (string= "session menu" (menu-title *active-menu*)))
        (setf *active-menu* nil)
        (expect (null *active-menu*)))))

  ;;; -- predicates and constants ------------------------------------------------

  ;; popup-p returns T for a POPUP struct and NIL for any other value.
  (it "popup-p-recognises-popup-struct"
    (let ((p (make-popup)))
      (expect (popup-p p))
      (dolist (val (list nil 42 "" (make-menu)))
        (expect (not (popup-p val))))))

  ;; menu-p returns T for a MENU struct and NIL for any other value.
  (it "menu-p-recognises-menu-struct"
    (let ((m (make-menu)))
      (expect (menu-p m))
      (dolist (val (list nil 42 "" (make-popup)))
        (expect (not (menu-p val))))))

  ;; The named defconstants match the documented default slot values.
  (it "default-popup-dimension-constants"
    (expect (= +default-popup-width+  (popup-width  (make-popup))))
    (expect (= +default-popup-height+ (popup-height (make-popup))))
    (expect (= 40 +default-popup-width+))
    (expect (= 10 +default-popup-height+))))
