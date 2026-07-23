(in-package #:cl-tmux/test)

;;;; parser tests - DCS direct bridge and passthrough.

(defun %fresh-dcs-buffer ()
  "A fresh empty adjustable octet buffer for make-dcs-st-k / make-dcs-k tests."
  (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(describe "terminal-suite/direct-dcs-st-suite"

  ;; make-dcs-st-k on backslash (#x5C) returns ground-state (ST confirmed).
  (it "make-dcs-st-k-backslash-returns-ground"
    (let* ((s   (make-screen 10 5))
           (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
           (result (funcall k s #x5C)))
      (expect (eq #'cl-tmux/terminal/parser:ground-state result))))

  ;; make-dcs-st-k on a non-backslash byte resumes DCS consumption (returns a continuation).
  (it "make-dcs-st-k-non-backslash-resumes-consuming"
    (let* ((s   (make-screen 10 5))
           (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
           (result (funcall k s (char-code #\A))))
      (expect (functionp result))))

  ;; ── tmux DCS passthrough (allow-passthrough) ─────────────────────────────────

  ;; A \ePtmux;<payload>\e\\ DCS with doubled ESCs queues the un-doubled inner
  ;; sequence on the screen's passthrough-queue.
  (it "dcs-passthrough-tmux-prefix-queues-inner-sequence"
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
        (expect (= 1 (length queue)))
        (let ((seq (first queue)))
          (expect (char= #\Escape (char seq 0)))
          (expect (string= "]1337" (subseq seq 1)))))))

  ;; A non-tmux DCS (e.g. Sixel) is consumed and NOT queued for passthrough.
  (it "dcs-non-tmux-prefix-is-discarded"
    (let ((s (make-screen 10 5)))
      ;; ESC P q <sixel-ish bytes> ESC \\  - prefix is 'q', not 'tmux;'
      (cl-tmux/terminal/emulator:screen-process-bytes
       s (coerce (list #x1B #x50 113 35 48 #x1B #x5C) '(vector (unsigned-byte 8))))
      (expect (null (cl-tmux/terminal/types:screen-passthrough-queue s))))))
