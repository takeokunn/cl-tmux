(in-package #:cl-tmux/test)

;;;; Events tests — part IX: copy-mode v-select, middle-cursor-jump, mouse X10, CSI-tilde outside mode, CSI-3byte.

(in-suite events-suite)

;;; ── copy-mode v alternative for begin-selection ─────────────────────────────

(test copy-mode-v-begins-selection
  "Plain 'v' (byte 118) also begins selection in copy mode."
  (with-copy-mode-vi-state (s screen state)
    (finishes (cl-tmux::process-byte s 118 state))
    (is (screen-copy-selecting screen)
        "v must activate copy selection")))

;;; ── Middle-screen cursor jump M ──────────────────────────────────────────────

(test copy-mode-M-moves-cursor-to-middle
  "Plain 'M' (byte 77) moves the copy-mode cursor to the middle row of the screen."
  (with-copy-mode-vi-state (s screen state)
    ;; Place cursor at row 0.
    (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
    (cl-tmux::process-byte s (char-code #\M) state)
    (let* ((row    (car (screen-copy-cursor screen)))
           (height (screen-height screen))
           (mid    (floor height 2)))
      (is (= mid row) "M must place cursor at the middle row"))))

;;; ── %handle-escape-x10-mouse direct invocation ───────────────────────────────

(test handle-escape-x10-mouse-dispatches-event
  "%handle-escape-x10-mouse decodes X10 encoding and dispatches the event.
   We verify it returns (values nil ground-state) without signaling."
  (with-two-pane-mouse-session (sess win p0 p1)
    (let ((buf (make-array 6 :element-type '(unsigned-byte 8)
                             :initial-contents (list 27 91 77
                                                     (+ 0 32)   ; btn 0 = left
                                                     (+ 50 33)  ; col 50 → 0-based 49
                                                     (+ 5 33)   ; row 5  → 0-based 4
                                                     ))))
      (multiple-value-bind (outcome next)
          (cl-tmux::%handle-escape-x10-mouse sess buf)
        (is (null outcome)
            "%handle-escape-x10-mouse must return NIL outcome")
        (is (eq #'cl-tmux::%ground-input-state next)
            "%handle-escape-x10-mouse must return ground-state as next state")))))

;;; ── %handle-escape-csi-tilde outside copy mode ──────────────────────────────

(test handle-escape-function-key-forwards-outside-copy-mode
  "%handle-escape-csi-tilde forwards the sequence when not in copy mode and unbound."
  (with-fake-session (s)
    ;; Build an ESC [ 5 ~ (PageUp) buffer — not in copy mode, no binding.
    (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents (list 27 91 53 126))))
      (multiple-value-bind (outcome next)
          (cl-tmux::%handle-escape-csi-tilde s buf 4)
        (is (null outcome)
            "%handle-escape-csi-tilde outside copy-mode must return NIL outcome")
        (is (eq #'cl-tmux::%ground-input-state next)
            "%handle-escape-csi-tilde must return ground-state")))))

;;; ── %handle-escape-csi-3byte: keep-accumulating for digit ───────────────────

(test handle-escape-csi-3byte-returns-keep-accumulating-for-digit
  "%handle-escape-csi-3byte returns (values T NIL) when the third byte is a digit,
   indicating we need to keep accumulating (for ESC [ N ~ function-key sequences)."
  (with-fake-session (s)
    ;; Build ESC [ 5  — third byte is '5' (53), a digit.
    (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                             :fill-pointer 3 :adjustable t
                             :initial-contents (list 27 91 53))))
      (multiple-value-bind (keep-accumulating next-state)
          (cl-tmux::%handle-escape-csi-3byte s buf)
        (is (eq t keep-accumulating)
            "%handle-escape-csi-3byte with digit third-byte must return T (keep accumulating)")
        (is (null next-state)
            "next-state must be NIL when keep-accumulating is T")))))

(test handle-escape-csi-3byte-returns-ground-state-for-non-digit
  "%handle-escape-csi-3byte returns (values NIL ground-state) for a non-digit final byte."
  (with-fake-session (s)
    ;; Build ESC [ A  — third byte is 'A' (65), not a digit.
    (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                             :fill-pointer 3 :adjustable t
                             :initial-contents (list 27 91 65))))
      (multiple-value-bind (keep-accumulating next-state)
          (cl-tmux::%handle-escape-csi-3byte s buf)
        (is (null keep-accumulating)
            "%handle-escape-csi-3byte with non-digit must return NIL (do not keep accumulating)")
        (is (eq #'cl-tmux::%ground-input-state next-state)
            "next-state must be %ground-input-state")))))

;;; ── SGR mouse: parse with scroll-wheel button encoding ───────────────────────

(test parse-sgr-mouse-scroll-up-button
  "%parse-sgr-mouse parses SGR scroll-up (btn=64) correctly."
  (let* ((s   (format nil "~C[<64;5;3M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 64 btn)   "scroll-up btn must be 64")
      (is (= 4  col)   "col must be 0-based (5-1=4)")
      (is (= 2  row)   "row must be 0-based (3-1=2)")
      (is-false release-p "press sequence must have release-p=NIL"))))

(test parse-sgr-mouse-returns-nil-for-short-buffer
  "%parse-sgr-mouse returns (values nil nil nil nil) for a buffer shorter than 9 bytes."
  (let* ((s   (format nil "~C[<0M" #\Escape))  ; too short
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (null btn)      "short buffer must return nil btn")
      (is (null col)      "short buffer must return nil col")
      (is (null row)      "short buffer must return nil row")
      (is (null release-p) "short buffer must return nil release-p"))))

;;; ── SGR mouse dispatch via process-byte ─────────────────────────────────────

(test sgr-mouse-left-click-via-process-byte-selects-pane
  "An SGR left-click sequence fed byte-by-byte through process-byte selects the pane."
  (with-two-pane-mouse-session (sess win p0 p1)
    (setf (screen-mouse-sgr-mode (pane-screen p0)) t)
    (let ((state (cl-tmux::make-input-state))
          ;; ESC [ < 0 ; 50 ; 5 M  — btn=0, col=50, row=5 (1-based), press
          (seq   (format nil "~C[<0;50;5M" #\Escape)))
      (loop for ch across seq
            do (cl-tmux::process-byte sess (char-code ch) state))
      (is (eq p1 (window-active-pane win))
          "SGR left-click in right pane must focus p1"))))

;;; ── overlay-scroll: verify actual offset change ──────────────────────────────

(test overlay-scroll-table
  "overlay-scroll delta adjusts *overlay-scroll-offset* by delta.
   NOTE: overlay must use ~% for real newlines (CL \\n in a literal is not newline)."
  (dolist (row '((3 -1 2 "up: offset 3, delta -1 → 2")
                 (0  1 1 "down: offset 0, delta +1 → 1")))
    (destructuring-bind (initial delta expected desc) row
      (let ((*overlay* (format nil "line1~%line2~%line3~%line4~%line5~%"))
            (*overlay-scroll-offset* initial))
        (overlay-scroll delta)
        (is (= expected *overlay-scroll-offset*) "~A" desc)))))

;;; ── %border-check-node direct tests ─────────────────────────────────────────
;;;
;;; %border-check-node is the recursive tree walker inside %border-at-position.
;;; The :v split path and multi-level recursion deserve direct coverage.

(test border-check-node-leaf-returns-nil
  "%border-check-node on a layout-leaf always returns (values NIL NIL)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (leaf (make-layout-leaf p0)))
    (multiple-value-bind (split orientation)
        (cl-tmux::%border-check-node 20 10 leaf)
      (is (null split)       "layout-leaf must return NIL split")
      (is (null orientation) "layout-leaf must return NIL orientation"))))

(test border-check-node-h-split-detects-separator
  "%border-check-node returns (split :h) when col lands exactly on the horizontal separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2)))
    ;; Separator column for p0 (x=0 w=40) is at col 40.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 40 5 split)
      (is (eq split found-split)
          "%border-check-node :h split must return the split node at separator col")
      (is (eq :h orientation)
          "%border-check-node :h split must report :h orientation"))))

(test border-check-node-v-split-detects-separator
  "%border-check-node returns (split :v) when row lands exactly on the vertical separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 80 :height 10
                            :screen (make-screen 80 10)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 11 :width 80 :height 10
                            :screen (make-screen 80 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :v leaf0 leaf1 1/2)))
    ;; Separator row for p0 (y=0 h=10) is at row 10.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 5 10 split)
      (is (eq split found-split)
          "%border-check-node :v split must return the split node at separator row")
      (is (eq :v orientation)
          "%border-check-node :v split must report :v orientation"))))

(test border-check-node-h-split-inside-pane-returns-nil
  "%border-check-node returns (values NIL NIL) when col is inside a pane (not on border)."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2)))
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 20 5 split)
      (is (null found-split)   "col inside pane must return NIL split")
      (is (null orientation)   "col inside pane must return NIL orientation"))))

(test border-check-node-nested-split-finds-inner-border
  "%border-check-node recurses into child splits and finds inner borders."
  ;; Build a 3-pane layout: [p0 | [p1 above p2]]
  ;; Outer: :h split at col 40; inner: :v split at row 10.
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0  :width 40 :height 10
                            :screen (make-screen 40 10)))
         (p2    (make-pane :id 3 :fd -1 :pid -1 :x 41 :y 11 :width 40 :height 10
                            :screen (make-screen 40 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (leaf2 (make-layout-leaf p2))
         (inner-split (make-layout-split :v leaf1 leaf2 1/2))
         (outer-split (make-layout-split :h leaf0 inner-split 1/2)))
    ;; Hit the inner :v border at (col=50, row=10)
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 50 10 outer-split)
      (is (eq inner-split found-split)
          "%border-check-node must find the inner :v split node")
      (is (eq :v orientation)
          "%border-check-node must report :v for the inner split"))))

;;; ── %make-escape-buffer helper ───────────────────────────────────────────────

(test make-escape-buffer-contains-seed-byte
  "%make-escape-buffer returns an adjustable byte vector seeded with the given byte."
  (let ((buf (cl-tmux::%make-escape-buffer 27)))
    (is (= 1 (fill-pointer buf))
        "%make-escape-buffer must have fill-pointer 1 after seeding")
    (is (= 27 (aref buf 0))
        "%make-escape-buffer must store the seed byte at index 0")))

(test make-escape-buffer-is-adjustable
  "%make-escape-buffer returns an adjustable vector so subsequent bytes can be pushed."
  (let ((buf (cl-tmux::%make-escape-buffer 27)))
    (vector-push-extend 91 buf)
    (is (= 2 (fill-pointer buf))
        "vector-push-extend after %make-escape-buffer must succeed — vector must be adjustable")
    (is (= 91 (aref buf 1))
        "pushed byte must appear at index 1")))

;;; ── %status-col-to-window: multi-window traversal coverage ──────────────────
