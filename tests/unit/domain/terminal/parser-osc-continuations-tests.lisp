(in-package #:cl-tmux/test)

;;;; parser tests - OSC bridge continuations.

;;; Helper: build an adjustable byte vector pre-filled with STRING.
;;; Eliminates the repeated 3-line buffer-construction pattern.
(defun make-osc-payload-buf (string)
  "Return a fresh adjustable (unsigned-byte 8) buffer pre-filled with the
   bytes of STRING (one byte per character, Latin-1 encoded)."
  (let ((buf (make-array (length string)
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0
                         :adjustable   t)))
    (loop for ch across string
          do (vector-push-extend (char-code ch) buf))
    buf))

(describe "terminal-suite/direct-osc-continuations"

  ;; make-osc-k accumulates payload bytes and dispatches to %dispatch-osc on BEL.
  (it "make-osc-k-accumulates-and-dispatches-on-bel"
    (with-screen (s 20 5)
      ;; Simulate: OSC 0 ; title (bytes for "0;hello")
      (let ((buf (make-osc-payload-buf "0;hello"))
            (k   nil))
        (setf k (cl-tmux/terminal/parser::make-osc-k buf))
        ;; Feed BEL to terminate
        (let ((result (funcall k s #x07)))
          (expect (eq #'cl-tmux/terminal/parser:ground-state result))
          (expect (string= "hello" (cl-tmux/terminal/types:screen-title s)))))))

  ;; make-osc-k on ESC (#x1B) returns a continuation waiting for backslash.
  (it "make-osc-k-esc-transitions-to-st-state"
    (with-screen (s 10 5)
      (let* ((buf (make-osc-payload-buf ""))
             (k   (cl-tmux/terminal/parser::make-osc-k buf))
             (k2  (funcall k s #x1B)))
        (expect (functionp k2)))))

  ;; make-osc-st-k on backslash dispatches and returns ground-state.
  (it "make-osc-st-k-backslash-dispatches-and-grounds"
    (with-screen (s 20 5)
      ;; Payload: "2;xterm-st-title"
      (let* ((buf    (make-osc-payload-buf "2;xterm-st-title"))
             (k      (cl-tmux/terminal/parser::make-osc-st-k buf))
             (result (funcall k s #x5C)))      ; backslash = ST confirmed
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (string= "xterm-st-title" (cl-tmux/terminal/types:screen-title s))))))

  ;; make-osc-st-k on a non-backslash byte returns ground-state without dispatching.
  (it "make-osc-st-k-non-backslash-returns-ground"
    (with-screen (s 20 5)
      (let* ((buf    (make-osc-payload-buf "0;title"))
             (k      (cl-tmux/terminal/parser::make-osc-st-k buf))
             (result (funcall k s (char-code #\X)))) ; not a backslash
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        ;; Title must NOT have been set (malformed ST discarded)
        (expect (not (string= "title" (cl-tmux/terminal/types:screen-title s))))))))
