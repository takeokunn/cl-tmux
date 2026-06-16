(in-package #:cl-tmux)

(defun %prompt-csi-tilde-action (buffer length)
  "Return the prompt editing action for ESC [ <param> ~, or NIL."
  (multiple-value-bind (param mod) (%csi-tilde-parse buffer length)
    (when (= (or mod 1) 1)
      (case param
        ((1 7) :bol)
        (3 :delete)
        ((4 8) :eol)
        (t nil)))))

(defun %handle-prompt-escape-sequence (buffer length)
  "Apply a complete prompt ESC sequence. Returns true when consumed."
  (let ((action
          (cond
            ((and (= length 3)
                  (= (aref buffer 1) +byte-csi-bracket+))
             (case (aref buffer 2)
               (65 :history-prev)
               (66 :history-next)
               (68 :left)
               (67 :right)
               (72 :bol)
               (70 :eol)
               (t nil)))
            ((and (= length 3)
                  (= (aref buffer 1) +byte-ss3-o+))
             (case (aref buffer 2)
               (72 :bol)
               (70 :eol)
               (t nil)))
            ((and (>= length 4)
                  (= (aref buffer 1) +byte-csi-bracket+)
                  (= (aref buffer (1- length)) +byte-tilde+))
             (%prompt-csi-tilde-action buffer length)))))
    (case action
      (:history-prev (prompt-history-prev) t)
      (:history-next (prompt-history-next) t)
      (:left   (prompt-cursor-back) t)
      (:right  (prompt-cursor-forward) t)
      (:bol    (prompt-cursor-bol) t)
      (:eol    (prompt-cursor-eol) t)
      (:delete (prompt-delete-char) t)
      (t nil))))

(defun %prompt-escape-cancel ()
  "Cancel the prompt-local escape sequence and ground the prompt state."
  (handle-prompt-key +byte-esc+)
  (%ground-values))

(defun make-prompt-escape-input-k (buffer)
  "CPS continuation for prompt-local ESC sequences.

   CSI/SS3 navigation sequences edit the prompt buffer; unknown sequences cancel
   the prompt like a bare Escape. The buffer is exposed through
   *ESC-ACCUM-BUFFER* so escape-time can also turn a lone Escape into cancel."
  (lambda (_session byte)
    (declare (ignore _session))
    (vector-push-extend byte buffer)
    (setf *esc-accum-buffer* buffer)
    (let ((length (fill-pointer buffer)))
      (cond
        ((and (= length 2)
              (or (= (aref buffer 1) +byte-csi-bracket+)
                  (= (aref buffer 1) +byte-ss3-o+)))
         (%prompt-escape-input-continue buffer))
        ((and (= length 3)
              (= (aref buffer 1) +byte-csi-bracket+)
              (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+))
         (%prompt-escape-input-continue buffer))
        ((and (>= length 4)
              (= (aref buffer 1) +byte-csi-bracket+)
              (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
              (/= (aref buffer (1- length)) +byte-tilde+)
              (< length 8))
         (%prompt-escape-input-continue buffer))
        ((%handle-prompt-escape-sequence buffer length)
         (%ground-values))
        (t
         (%prompt-escape-cancel))))))
