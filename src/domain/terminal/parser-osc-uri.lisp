(in-package #:cl-tmux/terminal/parser)

;;;; OSC 7 and OSC 8 helpers.

(defun %hex-digit-16 (char)
  "Return the numeric value of a hexadecimal digit CHAR, or NIL if invalid."
  (digit-char-p char 16))

(defun %flush-utf8-octets (octets out)
  "Write accumulated UTF-8 OCTETS to the string stream OUT and reset OCTETS."
  (when (> (length octets) 0)
    (write-string
     (or (handler-case
             (babel:octets-to-string octets :encoding :utf-8)
           (error () nil))
         (coerce (loop for i below (length octets)
                       collect (code-char (aref octets i)))
                 'string))
     out)
    (setf (fill-pointer octets) 0)))

(defun %percent-decode (encoded-string)
  "Decode %XX percent-escapes in ENCODED-STRING, UTF-8 aware."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t))
        (len (length encoded-string)))
    (with-output-to-string (out)
      (loop with i = 0
            while (< i len)
            for ch = (char encoded-string i)
            do (cond
                 ((and (char= ch #\%)
                       (<= (+ i 2) (1- len)))
                  (let ((hi (%hex-digit-16 (char encoded-string (1+ i))))
                        (lo (%hex-digit-16 (char encoded-string (+ i 2)))))
                    (if (and hi lo)
                        (progn
                          (vector-push-extend (+ (* hi 16) lo) octets)
                          (incf i 3))
                        (progn
                          (%flush-utf8-octets octets out)
                          (write-char ch out)
                          (incf i)))))
                 (t
                  (%flush-utf8-octets octets out)
                  (write-char ch out)
                  (incf i))))
      (%flush-utf8-octets octets out))))

(defun %handle-osc-8 (screen body)
  "Handle OSC 8 hyperlink state."
  (let ((uri-start (position #\; body)))
    (when uri-start
      (let ((uri (subseq body (1+ uri-start))))
        (setf (screen-current-hyperlink screen)
              (and (> (length uri) 0) uri))))))

(defun %osc7-path (body)
  "Extract the filesystem path from an OSC 7 file:// URL and percent-decode it."
  (let ((prefix "file://"))
    (if (and (>= (length body) (length prefix))
             (string= body prefix :end1 (length prefix) :end2 (length prefix)))
        (let* ((after-scheme (subseq body (length prefix)))
               (slash        (position #\/ after-scheme)))
          (if slash (%percent-decode (subseq after-scheme slash)) "/"))
        body)))
