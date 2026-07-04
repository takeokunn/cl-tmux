(in-package #:cl-tmux)

;;;; Prefix and root-table repeat CPS states.

(define-cps-state %after-prefix-input-state (session byte)
  ((= byte +byte-esc+)
   (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buffer)
     (values nil (%make-prefix-csi-k session buffer))))
  (t
   (let ((result (dispatch-prefix-command session byte)))
     (if (eq result :repeatable)
         (values nil #'%after-prefix-input-state)
         (values result #'%ground-input-state)))))

(defun %run-root-table-binding (session byte)
  "Run the root-table binding matching BYTE and return its CPS result."
  (let ((entry (%key-table-entry-by-candidates
                +table-root+ (%single-byte-key-candidates byte))))
    (%run-key-table-binding session entry byte)
    (setf *dirty* t)
    (if (key-table-repeatable-p entry)
        (values :repeatable #'%after-root-repeat-input-state)
        (values nil #'%ground-input-state))))

(define-cps-state %after-root-repeat-input-state (session byte)
  ((%key-table-entry-by-candidates +table-root+ (%single-byte-key-candidates byte))
   (%run-root-table-binding session byte))
  (t
   (%ground-input-state session byte)))
