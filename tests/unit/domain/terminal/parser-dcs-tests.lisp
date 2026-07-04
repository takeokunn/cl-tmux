(in-package #:cl-tmux/test)

;;;; parser tests - DCS direct bridge and passthrough.

(def-suite direct-dcs-st-suite
  :description "Direct calls to make-dcs-st-k bridge continuation"
  :in terminal-suite)
(in-suite direct-dcs-st-suite)

(defun %fresh-dcs-buffer ()
  "A fresh empty adjustable octet buffer for make-dcs-st-k / make-dcs-k tests."
  (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(test make-dcs-st-k-backslash-returns-ground
  "make-dcs-st-k on backslash (#x5C) returns ground-state (ST confirmed)."
  (let* ((s   (make-screen 10 5))
         (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
         (result (funcall k s #x5C)))
    (is (eq #'cl-tmux/terminal/parser:ground-state result)
        "make-dcs-st-k on backslash must return ground-state")))

(test make-dcs-st-k-non-backslash-resumes-consuming
  "make-dcs-st-k on a non-backslash byte resumes DCS consumption (returns a continuation)."
  (let* ((s   (make-screen 10 5))
         (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
         (result (funcall k s (char-code #\A))))
    (is (functionp result)
        "make-dcs-st-k on non-backslash must return a continuation (keeps consuming DCS)")))

;;; ── tmux DCS passthrough (allow-passthrough) ─────────────────────────────────

(test dcs-passthrough-tmux-prefix-queues-inner-sequence
  "A \\ePtmux;<payload>\\e\\\\ DCS with doubled ESCs queues the un-doubled inner
   sequence on the screen's passthrough-queue."
  (let ((s (make-screen 10 5)))
    ;; Feed: ESC P t m u x ;  ESC ESC ] 1 3 3 7  ESC \\   (doubled inner ESC)
    ;; Inner un-doubled should be: ESC ] 1 3 3 7
    (cl-tmux/terminal/emulator:screen-process-bytes
     s (coerce (list #x1B #x50               ; ESC P (DCS)
                     116 109 117 120 59      ; tmux;
                     #x1B #x1B 93 49 51 51 55 ; \e\e ] 1 3 3 7  (doubled ESC)
                     #x1B #x5C)              ; ESC \\  (ST)
               '(vector (unsigned-byte 8))))
    (let ((queue (cl-tmux/terminal/types:screen-passthrough-queue s)))
      (is (= 1 (length queue)) "one passthrough sequence queued")
      (let ((seq (first queue)))
        (is (char= #\Escape (char seq 0)) "inner sequence starts with un-doubled ESC")
        (is (string= "]1337" (subseq seq 1)) "inner payload after the single ESC")))))

(test dcs-non-tmux-prefix-is-discarded
  "A non-tmux DCS (e.g. Sixel) is consumed and NOT queued for passthrough."
  (let ((s (make-screen 10 5)))
    ;; ESC P q <sixel-ish bytes> ESC \\  - prefix is 'q', not 'tmux;'
    (cl-tmux/terminal/emulator:screen-process-bytes
     s (coerce (list #x1B #x50 113 35 48 #x1B #x5C) '(vector (unsigned-byte 8))))
    (is (null (cl-tmux/terminal/types:screen-passthrough-queue s))
        "non-tmux DCS must not populate the passthrough-queue")))
