(in-package #:cl-tmux/test)

;;;; Popup and menu state tests (src/overlay.lisp).

(in-suite overlay-suite)

;;; -- Popup struct constructors and accessors ---------------------------------

(test make-popup-defaults
  "make-popup fills all slots to their documented defaults."
  (let ((p (make-popup)))
    (check-table (list (list (popup-width p)  40 "default width is 40")
                       (list (popup-height p) 10 "default height is 10")
                       (list (popup-screen p) nil "default screen is nil (text-only)")
                       (list (popup-pane p)   nil "default pane is nil (text-only)"))
                 :test #'equal)
    (is (string= "" (popup-title p))   "default title is empty string")
    (is (eq t (popup-close-on-exit p)) "default close-on-exit is T")))

(test make-popup-keyword-args
  "make-popup accepts keyword arguments that override all defaults."
  (let ((p (make-popup :width 80 :height 24
                       :title "Test Popup" :close-on-exit nil)))
    (check-table (list (list (popup-width p)  80 "width is 80")
                       (list (popup-height p) 24 "height is 24"))
                 :test #'equal)
    (is (string= "Test Popup" (popup-title p)))
    (is (null (popup-close-on-exit p)) "close-on-exit can be set to NIL")))

(test make-popup-with-non-nil-pane
  "make-popup :pane stores the pane reference; popup-pane returns it."
  (let* ((fake-pane (make-pane :id 42 :fd -1 :screen (make-screen 10 5)))
         (p (make-popup :pane fake-pane :title "live-popup")))
    (is (eq fake-pane (popup-pane p))
        "popup-pane must return the supplied pane, not NIL")
    (is (= 42 (pane-id (popup-pane p)))
        "the pane stored in the popup is the exact object passed in")))

(test active-popup-global-state
  "Setting *active-popup* persists across accesses (session-level state)."
  (let ((*active-popup* nil))
    (is (null *active-popup*) "starts nil")
    (let ((p (make-popup :title "session popup")))
      (setf *active-popup* p)
      (is (eq p *active-popup*)
          "*active-popup* holds the set popup")
      (is (string= "session popup" (popup-title *active-popup*))
          "accessor works via the global")
      (setf *active-popup* nil)
      (is (null *active-popup*) "reset to nil clears it"))))

;;; -- Menu struct constructors and accessors ----------------------------------

(test make-menu-defaults
  "make-menu fills all slots to their documented defaults."
  (let ((m (make-menu)))
    (is (string= "" (menu-title m))      "default title is empty string")
    (is (null (menu-items m))            "default items list is NIL")
    (is (= 0  (menu-selected-index m))   "default selected-index is 0")))

(test make-menu-keyword-args
  "make-menu accepts keyword arguments that override all defaults."
  (let ((items '(("Option A" . :a) ("Option B" . :b) ("Option C" . :c))))
    (let ((m (make-menu :title "Choose" :items items :selected-index 2)))
      (is (string= "Choose" (menu-title m)))
      (is (equal items (menu-items m))
          "menu-items returns the full items list")
      (is (= 2 (menu-selected-index m))
          "selected-index is set to 2"))))

(test menu-x-and-y-default-to-nil
  "menu-x and menu-y (display-menu -x/-y position) default to NIL, meaning
   'centre on screen'."
  (let ((m (make-menu)))
    (is (null (menu-x m)) "default menu-x is NIL (centre)")
    (is (null (menu-y m)) "default menu-y is NIL (centre)")))

(test menu-x-and-y-keyword-args
  "make-menu :x and :y set a fixed display-menu position, overriding centring."
  (let ((m (make-menu :x 5 :y 10)))
    (is (= 5  (menu-x m)) "menu-x must be set to the supplied value")
    (is (= 10 (menu-y m)) "menu-y must be set to the supplied value")))

(test menu-selected-index-mutable
  "setf on menu-selected-index updates the struct slot."
  (let ((m (make-menu :items '((:a) (:b) (:c)) :selected-index 0)))
    (is (= 0 (menu-selected-index m)))
    (setf (menu-selected-index m) 2)
    (is (= 2 (menu-selected-index m)) "index updated to 2")
    (setf (menu-selected-index m) 0)
    (is (= 0 (menu-selected-index m)) "index wrapped back to 0")))

(test active-menu-global-state
  "Setting *active-menu* persists across accesses (session-level state)."
  (let ((*active-menu* nil))
    (is (null *active-menu*) "starts nil")
    (let ((m (make-menu :title "session menu" :items '(("X" . :x)))))
      (setf *active-menu* m)
      (is (eq m *active-menu*)
          "*active-menu* holds the set menu")
      (is (string= "session menu" (menu-title *active-menu*))
          "accessor works via the global")
      (setf *active-menu* nil)
      (is (null *active-menu*) "reset to nil clears it"))))

;;; -- predicates and constants ------------------------------------------------

(test popup-p-recognises-popup-struct
  "popup-p returns T for a POPUP struct and NIL for any other value."
  (let ((p (make-popup)))
    (is (popup-p p) "popup-p must return T for a make-popup result")
    (dolist (val (list nil 42 "" (make-menu)))
      (is (not (popup-p val)) "popup-p must return NIL for ~S" val))))

(test menu-p-recognises-menu-struct
  "menu-p returns T for a MENU struct and NIL for any other value."
  (let ((m (make-menu)))
    (is (menu-p m) "menu-p must return T for a make-menu result")
    (dolist (val (list nil 42 "" (make-popup)))
      (is (not (menu-p val)) "menu-p must return NIL for ~S" val))))

(test default-popup-dimension-constants
  "The named defconstants match the documented default slot values."
  (is (= +default-popup-width+  (popup-width  (make-popup)))
      "+default-popup-width+ must equal the default width slot")
  (is (= +default-popup-height+ (popup-height (make-popup)))
      "+default-popup-height+ must equal the default height slot")
  (is (= 40 +default-popup-width+)  "canonical popup width is 40 columns")
  (is (= 10 +default-popup-height+) "canonical popup height is 10 rows"))
