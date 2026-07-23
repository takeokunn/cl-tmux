(in-package #:cl-tmux/test)

;;;; Capture-pane and terminal rendering dispatch cases.

(describe "dispatch-suite"

  ;; capture-pane -a errors with tmux's 'no alternate screen' unless the pane's
  ;; alternate screen is in use; while active, -a captures the (live) alternate.
  (it "capture-pane-a-requires-alternate-screen"
    (with-fake-session (s)
      (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
        (let ((*overlay* nil))
          (cl-tmux::%cmd-capture-pane-arg s '("-a" "-p"))
          (assert-overlay-contains "no alternate screen" (overlay-lines)
                                   "capture-pane -a without an alt screen"))
        ;; Enter the alternate screen: -a now captures.
        (cl-tmux/terminal/actions:enter-alt-screen screen)
        (let ((*overlay* nil))
          (cl-tmux::%cmd-capture-pane-arg s '("-a" "-p"))
          (expect (null (and *overlay* (search "no alternate screen" *overlay*))))))))

  ;; capture-pane -J joins a scrollback row that wrapped into the next row -
  ;; the wrap flag travels with the row when it scrolls into history.
  (it "capture-pane-J-joins-wrapped-scrollback-rows"
    (with-fake-session (s)
      (let* ((pane   (cl-tmux/model:session-active-pane s))
             (screen (cl-tmux/model:pane-screen pane))
             (w      (cl-tmux/terminal/types:screen-width screen)))
        ;; Row 0 content 'AB', marked wrapped; scroll it into history.
        (setf (cl-tmux/terminal/types:cell-char
               (cl-tmux/terminal/types:screen-cell screen 0 0)) #\A
              (cl-tmux/terminal/types:cell-char
               (cl-tmux/terminal/types:screen-cell screen 1 0)) #\B)
        (cl-tmux/terminal/types:%mark-line-wrapped screen 0)
        (cl-tmux/terminal/actions:scroll-up-one screen)
        ;; The visible top row now continues the wrapped line: 'CD'.
        (setf (cl-tmux/terminal/types:cell-char
               (cl-tmux/terminal/types:screen-cell screen 0 0)) #\C
              (cl-tmux/terminal/types:cell-char
               (cl-tmux/terminal/types:screen-cell screen 1 0)) #\D)
        (expect (eq t (first (cl-tmux/terminal/types:screen-scrollback-wrapped screen))))
        (let ((joined (cl-tmux/commands:capture-pane pane
                                                     :include-scrollback t
                                                     :join t))
              (plain  (cl-tmux/commands:capture-pane pane
                                                     :include-scrollback t)))
          (declare (ignorable w))
          ;; -J preserves trailing spaces, so assert AB and CD land on the SAME
          ;; line rather than being adjacent characters.
          (flet ((line-with-ab (text)
                   (find-if (lambda (l) (search "AB" l))
                            (uiop:split-string text :separator '(#\Newline)))))
            (expect (search "CD" (or (line-with-ab joined) "")))
            (expect (null (search "CD" (or (line-with-ab plain) "")))))))))

  ;; ESC # 6 (DECDWL) records the row's line size; the renderer re-emits
  ;; ESC # 6 for that row (and ESC # 5 for unflagged rows) so the outer terminal
  ;; draws double-width lines; ESC # 5 clears; RIS resets.
  (it "decdwl-line-size-recorded-and-rendered"
    (with-fake-session (s)
      (let* ((pane   (cl-tmux/model:session-active-pane s))
             (screen (cl-tmux/model:pane-screen pane)))
        (cl-tmux/terminal/emulator:screen-process-bytes
         screen (babel:string-to-octets (format nil "~C#6WIDE" #\Escape)
                                        :encoding :utf-8))
        (expect (eql #\6 (gethash 0 (cl-tmux/terminal/types:screen-line-sizes screen))))
        (let ((out (cl-tmux/renderer::render-session-to-string s 5 20)))
          (expect (search (format nil "~C#6" #\Escape) out))
          (expect (search (format nil "~C#5" #\Escape) out)))
        (cl-tmux/terminal/emulator:screen-process-bytes
         screen (babel:string-to-octets (format nil "~C#5" #\Escape)
                                        :encoding :utf-8))
        (expect (null (gethash 0 (cl-tmux/terminal/types:screen-line-sizes screen))))
        (cl-tmux/terminal/emulator:screen-process-bytes
         screen (babel:string-to-octets (format nil "~C#6~Cc" #\Escape #\Escape)
                                        :encoding :utf-8))
        (expect (zerop (hash-table-count
                        (cl-tmux/terminal/types:screen-line-sizes screen)))))))

  ;;; -- capture-pane -S/-E line-range slicing ------------------------------------
  ;;;
  ;;; %capture-pane-parse-range-value and %capture-pane-slice-range had no test
  ;;; passing -S/-E to capture-pane at all before this — the whole line-range
  ;;; feature was untested.

  ;; %capture-pane-parse-range-value: NIL when absent, :edge for "-", an integer
  ;; (negative reaches into scrollback) otherwise, NIL for unparseable junk.
  (it "capture-pane-parse-range-value-table"
    (dolist (row '((nil    nil    "absent -S/-E value")
                   ("-"    :edge  "dash means edge of history/visible")
                   ("5"    5      "positive line number")
                   ("-3"   -3     "negative line number reaches into scrollback")
                   ("abc"  nil    "unparseable junk")))
      (destructuring-bind (raw expected desc) row
        (declare (ignore desc))
        (expect (equal expected
                       (cl-tmux::%capture-pane-parse-range-value raw))))))

  ;; %capture-pane-slice-range: line 0 is the first VISIBLE row; HEIGHT=3 over a
  ;; 5-line capture means lines 0-1 are scrollback (vis0 = 5-3 = 2) and lines
  ;; 2-4 are the visible rows 0-2.
  (it "capture-pane-slice-range-table"
    (let ((content (format nil "L0~%L1~%L2~%L3~%L4~%")))
      (dolist (row '((nil     nil     "L0~%L1~%L2~%L3~%L4~%" "no range -> unchanged")
                     (:edge   nil     "L0~%L1~%L2~%L3~%L4~%" "edge start -> whole history")
                     (0       0       "L2~%"                 "single visible row 0")
                     (-1      nil     "L1~%L2~%L3~%L4~%"     "negative start reaches into scrollback")
                     (2       0       ""                     "from >= to -> empty string")))
        (destructuring-bind (start end expected desc) row
          (declare (ignore desc))
          (expect (string= (format nil expected)
                           (cl-tmux::%capture-pane-slice-range content 3 start end)))))))

  ;; %cmd-capture-pane-arg wires -S/-E through to a real dispatch: -S 0 -E 0
  ;; (a single visible row) saves strictly less content to the paste buffer
  ;; than a plain capture-pane with no range restriction.
  (it "capture-pane-dispatch-honours-s-e-range"
    (with-fake-session (s)
      (let* ((pane (cl-tmux/model:session-active-pane s)))
        (feed (cl-tmux/model:pane-screen pane) "row0")
        (cl-tmux::%cmd-capture-pane-arg s nil)
        (let ((full (cl-tmux/buffer:get-paste-buffer 0)))
          (cl-tmux::%cmd-capture-pane-arg s '("-S" "0" "-E" "0"))
          (let ((sliced (cl-tmux/buffer:get-paste-buffer 0)))
            (expect full :to-be-truthy)
            (expect sliced :to-be-truthy)
            (expect (< (length sliced) (length full)))))))))
