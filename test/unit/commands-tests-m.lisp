(in-package #:cl-tmux/test)

;;;; commands tests — part M: swap-pane, capture-pane, shift-line-wrapped,
;;;; copy-mode-scroll-up/down, resize-pane, split-window.

(in-suite commands-suite)

;;; ── swap-pane ────────────────────────────────────────────────────────────────

(test swap-pane-right-cycles-panes-forward
  "swap-pane :right on a two-pane window moves the active pane to index 1 and
   swaps the positions (pane-x) of the two panes."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (x0-before (pane-x p0))
         (x1-before (pane-x p1)))
    ;; p0 is active (index 0); swap :right -> p0 moves to index 1
    (let ((result (swap-pane win :right)))
      (is (eq p0 result)
          "swap-pane must return the active pane")
      (is (eq p1 (first  (window-panes win)))
          "after :right swap, the former neighbour occupies index 0")
      (is (eq p0 (second (window-panes win)))
          "after :right swap, the active pane occupies index 1")
      ;; Geometry must be exchanged
      (is (= x1-before (pane-x p0))
          "active pane x must equal former neighbour's x after swap")
      (is (= x0-before (pane-x p1))
          "former neighbour x must equal active pane's former x after swap"))))

(test swap-pane-left-cycles-panes-backward
  "swap-pane :left wraps the active pane modularly backward in the panes list."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; p0 is active at index 0; :left -> mod(-1, 2) = 1 -> swaps with p1
    (swap-pane win :left)
    (is (eq p1 (first  (window-panes win)))
        "after :left wrap from index 0, neighbour at (mod -1 n) occupies index 0")
    (is (eq p0 (second (window-panes win)))
        "active pane wraps to index 1 on :left from index 0")))

(test swap-pane-right-from-last-wraps-to-first
  "swap-pane :right from the last-index pane wraps modularly to index 0."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; Make p1 (index 1, last) the active pane, then swap :right
    (window-select-pane win p1)
    (swap-pane win :right)
    ;; mod(1+1, 2) = 0 => p1 and p0 swap
    (is (eq p0 (second (window-panes win)))
        "p0 moves to index 1 after :right wrap from index 1")
    (is (eq p1 (first  (window-panes win)))
        "p1 wraps to index 0 after :right from the last slot")))

(test swap-pane-single-pane-returns-nil
  "swap-pane on a window with exactly one pane returns NIL (no neighbour to swap)."
  (let* ((p0  (%make-test-pane))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0)
                           :panes (list p0))))
    (window-select-pane win p0)
    (is (null (swap-pane win :right))
        "swap-pane on a single-pane window must return NIL")))

(test swap-pane-geometry-exchanged
  "The x/y/width/height of both panes are exchanged by swap-pane."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (p0-x (pane-x p0)) (p0-y (pane-y p0))
         (p0-w (pane-width p0)) (p0-h (pane-height p0))
         (p1-x (pane-x p1)) (p1-y (pane-y p1))
         (p1-w (pane-width p1)) (p1-h (pane-height p1)))
    (swap-pane win :right)
    (is (= p1-x (pane-x p0)) "active pane x must be former neighbour x")
    (is (= p1-y (pane-y p0)) "active pane y must be former neighbour y")
    (is (= p1-w (pane-width  p0)) "active pane width must be former neighbour width")
    (is (= p1-h (pane-height p0)) "active pane height must be former neighbour height")
    (is (= p0-x (pane-x p1)) "former neighbour x must be original active pane x")
    (is (= p0-y (pane-y p1)) "former neighbour y must be original active pane y")
    (is (= p0-w (pane-width  p1)) "former neighbour width must be original active width")
    (is (= p0-h (pane-height p1)) "former neighbour height must be original active height")))

;;; ── capture-pane ─────────────────────────────────────────────────────────────

(defun %make-pane-with-content (content &key (w 20) (h 5))
  "Build a no-PTY pane whose screen has been fed CONTENT."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h
                            :fd -1 :pid -1 :screen screen)))
    (unless (string= content "")
      (feed screen content))
    pane))

(test capture-pane-returns-string
  "capture-pane always returns a string (even on an empty pane)."
  (let* ((pane   (%make-test-pane))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane must return a string")))

(test capture-pane-visible-content-contains-fed-text
  "capture-pane returns the visible screen content including text fed to the pane."
  (let* ((pane   (%make-pane-with-content "ABC"))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane result must be a string")
    (is-true (search "ABC" result)
        "capture-pane output must contain the fed text \"ABC\" (got ~S)" result)))

(test capture-color-sgr-encodes-cell-colours
  "%capture-color-sgr maps a cell colour value to its SGR fragment:
   standard (1-7), bright (8-15), 256 (16-255), and 24-bit true-colour (#x1rrggbb)."
  (dolist (c '((1         nil "31"           "fg standard")
               (1         t   "41"           "bg standard")
               (12        nil "94"           "fg bright")
               (12        t   "104"          "bg bright")
               (200       nil "38;5;200"     "fg 256")
               (200       t   "48;5;200"     "bg 256")
               (#x1ff8000 nil "38;2;255;128;0" "fg true-colour")))
    (destructuring-bind (color bg-p expected desc) c
      (is (string= expected (cl-tmux/commands::%capture-color-sgr color bg-p)) "~A" desc))))

(test capture-cell-sgr-includes-attrs-and-colours
  "%capture-cell-sgr emits reset + attrs + fg + bg."
  (is (string= (format nil "~C[0;31;40m" #\Escape)
               (cl-tmux/commands::%capture-cell-sgr 1 0 0))
      "fg red, bg black, no attrs")
  (is (string= (format nil "~C[0;1;31;40m" #\Escape)
               (cl-tmux/commands::%capture-cell-sgr 1 0 1))
      "bold (attr bit 0) adds SGR 1"))

(test capture-pane-escapes-preserves-colour
  "capture-pane :escapes t keeps SGR colour sequences; plain capture does not."
  (let* ((screen (make-screen 10 2))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 2
                            :fd -1 :pid -1 :screen screen)))
    (feed screen (esc "[31m"))     ; foreground red
    (feed screen "hi")
    (let ((plain   (capture-pane pane))
          (colored (capture-pane pane :escapes t)))
      (is (search "hi" plain) "plain capture contains the text")
      (is (not (find (code-char 27) plain)) "plain capture has no escape bytes")
      (is (search "hi" colored) "colour capture contains the text")
      (is (search "31" colored) "colour capture includes the fg=red SGR (31)")
      (is (search (format nil "~C[0m" #\Escape) colored)
          "colour capture ends each row with a reset"))))

(test capture-pane-visible-only-excludes-scrollback
  "capture-pane without :include-scrollback only dumps visible rows, not scrollback."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen))
         (sb-row (make-array 20 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
    (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
    (feed screen "visible")
    (let ((result (capture-pane pane)))
      (is-true (search "visible" result)
          "visible content must appear in capture-pane output")
      (is (null (search "XXXXXXXXXXXXXXXXX" result))
          "scrollback content must NOT appear when include-scrollback is nil"))))

(test capture-pane-with-scrollback-prepends-history
  "capture-pane with :include-scrollback T prepends scrollback rows before visible rows."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen))
         (sb-row (make-array 20 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\Q :fg 7 :bg 0 :attrs 0 :width 1))))
    (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
    (feed screen "visible")
    (let ((result (capture-pane pane :include-scrollback t)))
      (is-true (search "QQ" result)
          "scrollback content must appear when include-scrollback is T")
      (is-true (search "visible" result)
          "visible content must also appear when include-scrollback is T")
      ;; Scrollback should come before visible content in the output
      (let ((q-pos       (search "QQ"      result))
            (visible-pos (search "visible" result)))
        (is (< q-pos visible-pos)
            "scrollback rows must precede visible rows in the output")))))

(test capture-pane-height-rows-newlines
  "capture-pane emits exactly (screen-height) newline-terminated rows."
  (let* ((pane   (%make-pane-with-content "" :w 10 :h 3))
         (result (capture-pane pane))
         (lines  (count #\Newline result)))
    (is (= 3 lines)
        "capture-pane must emit exactly height (~D) newline characters (got ~D)"
        3 lines)))

(test capture-pane-default-trims-trailing-spaces
  "capture-pane's default (no -J) strips trailing whitespace from each line —
   tmux's default behaviour.  A 'hi' on a 10-wide row captures as just \"hi\"."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi~%") (capture-pane pane))
        "default capture trims the 8 trailing spaces")))

(test capture-pane-J-preserves-trailing-spaces
  "capture-pane -J (:join t) PRESERVES trailing spaces — the row keeps full width."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi        ~%") (capture-pane pane :join t))
        "join capture keeps the row padded to its full width of 10")))

(test capture-pane-N-preserves-trailing-spaces
  "capture-pane -N (:preserve-trailing t) keeps trailing spaces like -J — the row
   stays padded to its full width."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi        ~%") (capture-pane pane :preserve-trailing t))
        "-N keeps the row padded to its full width of 10")))

(test capture-pane-N-preserves-trailing-but-does-not-join
  "capture-pane -N preserves trailing spaces but, unlike -J, does NOT rejoin a
   wrapped line — the distinguishing behaviour between -N and -J."
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "ABCDEFGH")            ; wraps: row0 "ABCDE" → row1 "FGH"
    (let ((preserved (capture-pane pane :preserve-trailing t)))
      (is-true (search "FGH  " preserved)
               "-N keeps the FGH continuation row padded to full width (got ~S)"
               preserved)
      (is (null (search "ABCDEFGH" preserved))
          "-N must NOT join the wrapped line into one logical line (got ~S)"
          preserved))))

(test capture-pane-J-joins-wrapped-lines
  "capture-pane -J rejoins a line that wrapped at the right margin into one
   logical line (no newline at the wrap boundary); default capture keeps them
   on separate lines."
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "ABCDEFGH")          ; wraps: row0 "ABCDE" → row1 "FGH"
    (let ((joined  (capture-pane pane :join t))
          (default (capture-pane pane)))
      (is-true (search "ABCDEFGH" joined)
          "with -J the wrapped line is one logical line ABCDEFGH (got ~S)" joined)
      (is (null (search "ABCDEFGH" default))
          "without -J the wrapped halves stay on separate lines (got ~S)" default))))

(test capture-pane-J-keeps-unwrapped-lines-separate
  "capture-pane -J does NOT join lines that did not wrap (a hard CR+LF break)."
  (let* ((screen (make-screen 10 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen (format nil "foo~C~Cbar" #\Return #\Linefeed))
    (let ((joined (capture-pane pane :join t)))
      (is-true (search "foo" joined) "foo present")
      (is-true (search "bar" joined) "bar present")
      (is (null (search "foobar" joined))
          "foo and bar did not wrap — they stay separate, not joined (got ~S)" joined))))

(test shift-line-wrapped-up-moves-flags
  "%shift-line-wrapped-up (scroll-up of the wrap flags): a flag at row Y in the
   region moves to Y-1, mirroring the content shift."
  (let ((s (make-screen 5 4)))
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)        ; row 2 wraps
    (cl-tmux/terminal/types:%shift-line-wrapped-up s 0 3)  ; scroll region rows 0..3
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 1)
              "the row-2 wrap flag moved up to row 1")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 2)
              "row 2 no longer carries the flag")))

(test line-wrapped-flag-cleared-on-erase
  "Erasing a row clears its wrap flag (erase-region), so a rewritten short line
   does not over-join under -J."
  (let ((s (make-screen 5 3)))
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 marked wrapped")
    (cl-tmux/terminal/actions:erase-region s 0 0 4 0)      ; erase row 0
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0)
              "erasing row 0 clears its wrap flag")))

(test capture-pane-blank-row-trims-to-empty-line
  "A fully blank row trims to an empty captured line (just the newline) by default,
   but stays full-width under -J."
  (let* ((screen (make-screen 5 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (is (string= (format nil "~%")      (capture-pane pane))
        "blank row trims to an empty line")
    (is (string= (format nil "     ~%") (capture-pane pane :join t))
        "blank row stays 5 spaces wide under -J")))

(test capture-pane-escapes-trims-trailing-by-default
  "capture-pane -e also drops trailing blank cells by default — no trailing-space
   run survives, and no stray reset is emitted for the trimmed region."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (let ((result (capture-pane pane :escapes t)))
      (is (null (find #\Space result))
          "escaped default capture has no trailing spaces (got ~S)" result)
      (is-true (search "hi" result) "still contains the fed text")
      (is-true (search (format nil "~C[0m" #\Escape) result)
          "still ends the row with an SGR reset"))))
