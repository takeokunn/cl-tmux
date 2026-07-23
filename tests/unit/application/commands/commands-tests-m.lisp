(in-package #:cl-tmux/test)

;;;; commands tests — part M: swap-pane, capture-pane, shift-line-wrapped,
;;;; copy-mode-scroll-up/down, resize-pane, split-window.

(defun %make-pane-with-content (content &key (w 20) (h 5))
  "Build a no-PTY pane whose screen has been fed CONTENT."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h
                            :fd -1 :pid -1 :screen screen)))
    (unless (string= content "")
      (feed screen content))
    pane))

(describe "commands-suite"

  ;;; ── swap-pane ────────────────────────────────────────────────────────────────

  ;; swap-pane :right on a two-pane window moves the active pane to index 1 and
  ;; swaps the positions (pane-x) of the two panes.
  (it "swap-pane-right-cycles-panes-forward"
    (let* ((win (%vsplit-window 20))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win)))
           (x0-before (pane-x p0))
           (x1-before (pane-x p1)))
      ;; p0 is active (index 0); swap :right -> p0 moves to index 1
      (let ((result (swap-pane win :right)))
        (expect (eq p0 result))
        (expect (eq p1 (first  (window-panes win))))
        (expect (eq p0 (second (window-panes win))))
        ;; Geometry must be exchanged
        (expect (= x1-before (pane-x p0)))
        (expect (= x0-before (pane-x p1))))))

  ;; swap-pane :left wraps the active pane modularly backward in the panes list.
  (it "swap-pane-left-cycles-panes-backward"
    (let* ((win (%vsplit-window 20))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      ;; p0 is active at index 0; :left -> mod(-1, 2) = 1 -> swaps with p1
      (swap-pane win :left)
      (expect (eq p1 (first  (window-panes win))))
      (expect (eq p0 (second (window-panes win))))))

  ;; swap-pane :right from the last-index pane wraps modularly to index 0.
  (it "swap-pane-right-from-last-wraps-to-first"
    (let* ((win (%vsplit-window 20))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      ;; Make p1 (index 1, last) the active pane, then swap :right
      (window-select-pane win p1)
      (swap-pane win :right)
      ;; mod(1+1, 2) = 0 => p1 and p0 swap
      (expect (eq p0 (second (window-panes win))))
      (expect (eq p1 (first  (window-panes win))))))

  ;; swap-pane on a window with exactly one pane returns NIL (no neighbour to swap).
  (it "swap-pane-single-pane-returns-nil"
    (let* ((p0  (%make-test-pane))
           (win (make-window :id 1 :name "w" :width 20 :height 5
                             :tree (make-layout-leaf p0)
                             :panes (list p0))))
      (window-select-pane win p0)
      (expect (null (swap-pane win :right)))))

  ;; The x/y/width/height of both panes are exchanged by swap-pane.
  (it "swap-pane-geometry-exchanged"
    (let* ((win (%vsplit-window 20))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win)))
           (p0-x (pane-x p0)) (p0-y (pane-y p0))
           (p0-w (pane-width p0)) (p0-h (pane-height p0))
           (p1-x (pane-x p1)) (p1-y (pane-y p1))
           (p1-w (pane-width p1)) (p1-h (pane-height p1)))
      (swap-pane win :right)
      (expect (= p1-x (pane-x p0)))
      (expect (= p1-y (pane-y p0)))
      (expect (= p1-w (pane-width  p0)))
      (expect (= p1-h (pane-height p0)))
      (expect (= p0-x (pane-x p1)))
      (expect (= p0-y (pane-y p1)))
      (expect (= p0-w (pane-width  p1)))
      (expect (= p0-h (pane-height p1)))))

  ;; swap-pane :down on a horizontal (top/bottom) split swaps the active (top)
  ;; pane with its spatial neighbour below, exchanging geometry and list order.
  (it "swap-pane-down-swaps-spatially-adjacent-pane-below"
    (let* ((win (%hsplit-window 10))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win)))
           (y0-before (pane-y p0))
           (y1-before (pane-y p1)))
      (window-select-pane win p0)
      (expect (eq p0 (swap-pane win :down)))
      (expect (= y1-before (pane-y p0)))
      (expect (= y0-before (pane-y p1)))
      (expect (eq p1 (first  (window-panes win))))
      (expect (eq p0 (second (window-panes win))))))

  ;; swap-pane :up on a horizontal (top/bottom) split, with the bottom pane
  ;; active, swaps it with its spatial neighbour above.
  (it "swap-pane-up-swaps-spatially-adjacent-pane-above"
    (let* ((win (%hsplit-window 10))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win)))
           (y0-before (pane-y p0))
           (y1-before (pane-y p1)))
      (window-select-pane win p1)            ; bottom pane active
      (expect (eq p1 (swap-pane win :up)))
      (expect (= y0-before (pane-y p1)))
      (expect (= y1-before (pane-y p0)))))

  ;; swap-pane :up/:down on a vertical (left/right) split has no spatially
  ;; adjacent pane in that axis, so pane-neighbor returns NIL and swap-pane
  ;; is a no-op (returns NIL, panes list unchanged).
  (it "swap-pane-up-down-no-spatial-neighbour-is-noop"
    (dolist (dir '(:up :down))
      (let* ((win (%vsplit-window 20))
             (p0  (first (window-panes win))))
        (expect (null (swap-pane win dir)))
        (expect (eq p0 (first (window-panes win)))))))

  ;;; ── capture-pane ─────────────────────────────────────────────────────────────

  ;; capture-pane always returns a string (even on an empty pane).
  (it "capture-pane-returns-string"
    (let* ((pane   (%make-test-pane))
           (result (capture-pane pane)))
      (expect (stringp result))))

  ;; capture-pane returns the visible screen content including text fed to the pane.
  (it "capture-pane-visible-content-contains-fed-text"
    (let* ((pane   (%make-pane-with-content "ABC"))
           (result (capture-pane pane)))
      (expect (stringp result))
      (expect (search "ABC" result) :to-be-truthy)))

  ;; %capture-color-sgr maps a cell colour value to its SGR fragment:
  ;; standard (1-7), bright (8-15), 256 (16-255), and 24-bit true-colour (#x1rrggbb).
  (it "capture-color-sgr-encodes-cell-colours"
    (dolist (c '((1         nil "31"           "fg standard")
                 (1         t   "41"           "bg standard")
                 (12        nil "94"           "fg bright")
                 (12        t   "104"          "bg bright")
                 (200       nil "38;5;200"     "fg 256")
                 (200       t   "48;5;200"     "bg 256")
                 (#x1ff8000 nil "38;2;255;128;0" "fg true-colour")))
      (destructuring-bind (color bg-p expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/commands::%capture-color-sgr color bg-p))))))

  ;; %capture-cell-sgr emits reset + attrs + fg + bg.
  (it "capture-cell-sgr-includes-attrs-and-colours"
    (expect (string= (format nil "~C[0;31;40m" #\Escape)
                      (cl-tmux/commands::%capture-cell-sgr 1 0 0)))
    (expect (string= (format nil "~C[0;1;31;40m" #\Escape)
                      (cl-tmux/commands::%capture-cell-sgr 1 0 1))))

  ;; capture-pane :escapes t keeps SGR colour sequences; plain capture does not.
  (it "capture-pane-escapes-preserves-colour"
    (let* ((screen (make-screen 10 2))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 2
                              :fd -1 :pid -1 :screen screen)))
      (feed screen (esc "[31m"))     ; foreground red
      (feed screen "hi")
      (let ((plain   (capture-pane pane))
            (colored (capture-pane pane :escapes t)))
        (expect (search "hi" plain))
        (expect (not (find (code-char 27) plain)))
        (expect (search "hi" colored))
        (expect (search "31" colored))
        (expect (search (format nil "~C[0m" #\Escape) colored)))))

  ;; capture-pane without :include-scrollback only dumps visible rows, not scrollback.
  (it "capture-pane-visible-only-excludes-scrollback"
    (let* ((screen (make-screen 20 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                              :fd -1 :pid -1 :screen screen))
           (sb-row (make-array 20 :initial-element
                               (cl-tmux/terminal/types:make-cell
                                :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
      (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
      (feed screen "visible")
      (let ((result (capture-pane pane)))
        (expect (search "visible" result) :to-be-truthy)
        (expect (null (search "XXXXXXXXXXXXXXXXX" result))))))

  ;; capture-pane with :include-scrollback T prepends scrollback rows before visible rows.
  (it "capture-pane-with-scrollback-prepends-history"
    (let* ((screen (make-screen 20 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                              :fd -1 :pid -1 :screen screen))
           (sb-row (make-array 20 :initial-element
                               (cl-tmux/terminal/types:make-cell
                                :char #\Q :fg 7 :bg 0 :attrs 0 :width 1))))
      (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
      (feed screen "visible")
      (let ((result (capture-pane pane :include-scrollback t)))
        (expect (search "QQ" result) :to-be-truthy)
        (expect (search "visible" result) :to-be-truthy)
        ;; Scrollback should come before visible content in the output
        (let ((q-pos       (search "QQ"      result))
              (visible-pos (search "visible" result)))
          (expect (< q-pos visible-pos))))))

  ;; capture-pane :include-scrollback t :escapes t combines both flags: scrollback
  ;; rows get SGR-attributed text too, not just plain characters (previously only
  ;; tested separately — :escapes t for visible rows, :include-scrollback t plain).
  (it "capture-pane-scrollback-with-escapes-preserves-colour"
    (let* ((screen (make-screen 20 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                              :fd -1 :pid -1 :screen screen))
           (sb-row (make-array 20 :initial-element
                               (cl-tmux/terminal/types:make-cell
                                :char #\Q :fg 1 :bg 0 :attrs 0 :width 1))))
      (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
      (feed screen "visible")
      (let ((result (capture-pane pane :include-scrollback t :escapes t)))
        (expect (search "QQ" result) :to-be-truthy)
        (expect result :to-contain-sgr "0;31;40"))))

  ;; capture-pane emits exactly (screen-height) newline-terminated rows.
  (it "capture-pane-height-rows-newlines"
    (let* ((pane   (%make-pane-with-content "" :w 10 :h 3))
           (result (capture-pane pane))
           (lines  (count #\Newline result)))
      (expect (= 3 lines))))

  ;; capture-pane's default (no -J) strips trailing whitespace from each line —
  ;; tmux's default behaviour.  A 'hi' on a 10-wide row captures as just "hi".
  (it "capture-pane-default-trims-trailing-spaces"
    (let* ((screen (make-screen 10 1))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "hi")
      (expect (string= (format nil "hi~%") (capture-pane pane)))))

  ;; capture-pane -J (:join t) PRESERVES trailing spaces — the row keeps full width.
  (it "capture-pane-J-preserves-trailing-spaces"
    (let* ((screen (make-screen 10 1))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "hi")
      (expect (string= (format nil "hi        ~%") (capture-pane pane :join t)))))

  ;; capture-pane -N (:preserve-trailing t) keeps trailing spaces like -J — the row
  ;; stays padded to its full width.
  (it "capture-pane-N-preserves-trailing-spaces"
    (let* ((screen (make-screen 10 1))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "hi")
      (expect (string= (format nil "hi        ~%") (capture-pane pane :preserve-trailing t)))))

  ;; capture-pane -N preserves trailing spaces but, unlike -J, does NOT rejoin a
  ;; wrapped line — the distinguishing behaviour between -N and -J.
  (it "capture-pane-N-preserves-trailing-but-does-not-join"
    (let* ((screen (make-screen 5 3))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "ABCDEFGH")            ; wraps: row0 "ABCDE" → row1 "FGH"
      (let ((preserved (capture-pane pane :preserve-trailing t)))
        (expect (search "FGH  " preserved) :to-be-truthy)
        (expect (null (search "ABCDEFGH" preserved))))))

  ;; capture-pane -J rejoins a line that wrapped at the right margin into one
  ;; logical line (no newline at the wrap boundary); default capture keeps them
  ;; on separate lines.
  (it "capture-pane-J-joins-wrapped-lines"
    (let* ((screen (make-screen 5 3))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "ABCDEFGH")          ; wraps: row0 "ABCDE" → row1 "FGH"
      (let ((joined  (capture-pane pane :join t))
            (default (capture-pane pane)))
        (expect (search "ABCDEFGH" joined) :to-be-truthy)
        (expect (null (search "ABCDEFGH" default))))))

  ;; capture-pane -J does NOT join lines that did not wrap (a hard CR+LF break).
  (it "capture-pane-J-keeps-unwrapped-lines-separate"
    (let* ((screen (make-screen 10 3))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3
                              :fd -1 :pid -1 :screen screen)))
      (feed screen (format nil "foo~C~Cbar" #\Return #\Linefeed))
      (let ((joined (capture-pane pane :join t)))
        (expect (search "foo" joined) :to-be-truthy)
        (expect (search "bar" joined) :to-be-truthy)
        (expect (null (search "foobar" joined))))))

  ;; capture-pane -J joins a wrapped row across the scrollback/visible boundary:
  ;; the wrap flag travels with the row when it scrolls into history (tmux).
  (it "capture-pane-J-joins-across-scrollback-boundary"
    (let* ((screen (make-screen 5 1))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 1
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "ABCDEFGH") ; scrollback row "ABCDE" (wrapped), visible "FGH"
      (let ((joined (capture-pane pane :include-scrollback t :join t)))
        (expect (search "ABCDEFGH" joined) :to-be-truthy))))

  ;; %shift-line-wrapped-up (scroll-up of the wrap flags): a flag at row Y in the
  ;; region moves to Y-1, mirroring the content shift.
  (it "shift-line-wrapped-up-moves-flags"
    (let ((s (make-screen 5 4)))
      (cl-tmux/terminal/types:%mark-line-wrapped s 2)        ; row 2 wraps
      (cl-tmux/terminal/types:%shift-line-wrapped-up s 0 3)  ; scroll region rows 0..3
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 1) :to-be-truthy)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 2) :to-be-falsy)))

  ;; Erasing a row clears its wrap flag (erase-region), so a rewritten short line
  ;; does not over-join under -J.
  (it "line-wrapped-flag-cleared-on-erase"
    (let ((s (make-screen 5 3)))
      (cl-tmux/terminal/types:%mark-line-wrapped s 0)
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-truthy)
      (cl-tmux/terminal/actions:erase-region s 0 0 4 0)      ; erase row 0
      (expect (cl-tmux/terminal/types:%line-wrapped-p s 0) :to-be-falsy)))

  ;; A fully blank row trims to an empty captured line (just the newline) by default,
  ;; but stays full-width under -J.
  (it "capture-pane-blank-row-trims-to-empty-line"
    (let* ((screen (make-screen 5 1))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 1
                              :fd -1 :pid -1 :screen screen)))
      (expect (string= (format nil "~%")      (capture-pane pane)))
      (expect (string= (format nil "     ~%") (capture-pane pane :join t)))))

  ;; capture-pane -e also drops trailing blank cells by default — no trailing-space
  ;; run survives, and no stray reset is emitted for the trimmed region.
  (it "capture-pane-escapes-trims-trailing-by-default"
    (let* ((screen (make-screen 10 1))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                              :fd -1 :pid -1 :screen screen)))
      (feed screen "hi")
      (let ((result (capture-pane pane :escapes t)))
        (expect (null (find #\Space result)))
        (expect (search "hi" result) :to-be-truthy)
        (expect (search (format nil "~C[0m" #\Escape) result) :to-be-truthy)))))
