(in-package #:cl-tmux/test)

;;;; renderer tests — part F: parse-style-string, style-to-sgr,
;;;; status-left/right length enforcement, window-status-format,
;;;; window-status-separator, render-popup, render-menu.

(in-suite renderer-suite)

;;; ── parse-style-string ───────────────────────────────────────────────────────

(test parse-style-string-nil-inputs-table
  "parse-style-string returns NIL for NIL and empty-string inputs."
  (dolist (row '((nil "NIL input → nil")
                 (""  "empty-string input → nil")))
    (destructuring-bind (input desc) row
      (is (null (cl-tmux/renderer:parse-style-string input)) "~A" desc))))

(test parse-style-string-color-key-table
  "parse-style-string sets :fg or :bg to the parsed colour name."
  (dolist (row '(("fg=red"     :fg "red"     "fg=red → :fg \"red\"")
                 ("bg=blue"    :bg "blue"     "bg=blue → :bg \"blue\"")
                 ("fg=colour4" :fg "colour4"  "fg=colour4 → :fg \"colour4\"")))
    (destructuring-bind (input key expected desc) row
      (let ((p (cl-tmux/renderer:parse-style-string input)))
        (is (string= expected (getf p key)) "~A: got ~S" desc (getf p key))))))

(test parse-style-string-bool-attr-table
  "parse-style-string sets boolean attribute keys to T."
  (dolist (row '(("bold"    :bold    "bold → :bold T")
                 ("reverse" :reverse "reverse → :reverse T")))
    (destructuring-bind (input key desc) row
      (let ((p (cl-tmux/renderer:parse-style-string input)))
        (is (getf p key) "~A: got ~S" desc (getf p key))))))

(test parse-style-string-multiple-attrs
  "parse-style-string parses fg=green,bold,underline into a combined plist."
  (let ((p (cl-tmux/renderer:parse-style-string "fg=green,bold,underline")))
    (is (string= "green" (getf p :fg))
        ":fg must be \"green\", got ~S" (getf p :fg))
    (is (getf p :bold)
        ":bold must be T, got ~S" (getf p :bold))
    (is (getf p :underline)
        ":underline must be T, got ~S" (getf p :underline))))

;;; ── style-to-sgr ────────────────────────────────────────────────────────────

(test style-to-sgr-nil-returns-default
  "style-to-sgr with NIL returns default blue-on-white SGR \"44;97\"."
  (is (string= "44;97" (cl-tmux/renderer:style-to-sgr nil))
      "style-to-sgr nil must return \"44;97\", got ~S"
      (cl-tmux/renderer:style-to-sgr nil)))

(test style-to-sgr-attrs-table
  "style-to-sgr includes the correct SGR code substring for each attribute."
  (dolist (row '(((:bold t)       "1"      ":bold T → SGR 1")
                 ((:reverse t)    "7"      ":reverse T → SGR 7")
                 ((:fg "red")     "31"     ":fg red → SGR 31")
                 ((:bg "blue")    "44"     ":bg blue → SGR 44")
                 ((:bg "colour4") "48;5;4" ":bg colour4 → SGR 48;5;4")))
    (destructuring-bind (style expected desc) row
      (let ((sgr (cl-tmux/renderer:style-to-sgr style)))
        (is (search expected sgr) "~A: got ~S" desc sgr)))))

;;; ── status-left-length / status-right-length enforcement ────────────────────

(test status-left-length-truncates-long-left
  "status-left-length truncates the expanded left string to the configured max."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-left" "abcdefghij")
    (cl-tmux/options:set-option "status-left-length" 5)
    (let* ((sess (make-test-session 80 10))
           (out  (render-status-bar-output sess 11 80)))
      (is (search "abcde" out)
          "truncated left must start with first 5 chars (got ~S)" out)
      (is (null (search "abcdefghij" out))
          "full 10-char left must NOT appear when length limit is 5 (got ~S)" out))))

(test status-right-length-truncates-long-right
  "status-right-length truncates the expanded right string to the configured max."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-right" "1234567890")
    (cl-tmux/options:set-option "status-right-length" 4)
    (let* ((sess (make-test-session 80 10))
           (out  (render-status-bar-output sess 11 80)))
      (is (search "1234" out)
          "truncated right must start with first 4 chars (got ~S)" out)
      (is (null (search "1234567890" out))
          "full 10-char right must NOT appear when length limit is 4 (got ~S)" out))))

;;; ── window-status-format and window-status-current-format ───────────────────

(test window-status-format-custom
  "window-status-format option is used when rendering inactive windows."
  (with-isolated-options ("status-left" nil "status-right" nil
                          "window-status-format" "WIN:#{window_name}"
                          "window-status-current-format" "[#{window_name}]")
    ;; make-two-window-session creates windows named "alpha" (active) and "beta".
    (multiple-value-bind (sess win0 _p0 _w1 _p1)
        (make-two-window-session 80 5)
      (declare (ignore _p0 _w1 _p1))
      (session-select-window sess win0)  ; alpha is active
      (let ((out (render-status-bar-output sess 11 80)))
        (is (search "[alpha]" out)
            "active window must use window-status-current-format [alpha] (got ~S)" out)
        (is (search "WIN:beta" out)
            "inactive window must use window-status-format WIN:beta (got ~S)" out)))))

;;; ── window-status-separator ──────────────────────────────────────────────────

(test window-status-separator-used-between-windows
  "window-status-separator is placed between window entries."
  (with-isolated-options ("status-left" nil "status-right" nil
                          "window-status-separator" "|SEP|")
    (multiple-value-bind (sess win0 _p0 _w1 _p1)
        (make-two-window-session 80 5)
      (declare (ignore _p0 _w1 _p1))
      (session-select-window sess win0)
      (let ((out (render-status-bar-output sess 11 80)))
        (is (search "|SEP|" out)
            "window-status-separator |SEP| must appear between windows (got ~S)" out)))))

;;; ── render-popup ─────────────────────────────────────────────────────────────

(test render-popup-empty-draws-borders
  "render-popup with no live pane draws top border with corners and title, plus bottom border."
  (let* ((popup (make-popup :title "Test" :x 0 :y 0 :width 20 :height 6
                            :pane nil :screen nil :close-on-exit nil))
         (out   (render-popup-output popup 24 80)))
    (is (find (code-char #x250C) out)
        "render-popup must draw top-left corner ┌ (got ~S)" out)
    (is (find (code-char #x2510) out)
        "render-popup must draw top-right corner ┐ (got ~S)" out)
    (is (find (code-char #x2514) out)
        "render-popup must draw bottom-left corner └ (got ~S)" out)
    (is (find (code-char #x2518) out)
        "render-popup must draw bottom-right corner ┘ (got ~S)" out)
    (is (search "Test" out)
        "render-popup must include the popup title (got ~S)" out)))

(test render-popup-style-colours-empty-body
  "popup-style colours the empty popup interior; with it unset the body has no SGR."
  (let ((popup (make-popup :title "T" :x 0 :y 0 :width 20 :height 6
                           :pane nil :screen nil :close-on-exit nil)))
    (with-isolated-options ("popup-style" "bg=blue")
      (is (search (format nil "~C[44m" #\Escape) (render-popup-output popup 24 80))
          "popup-style bg=blue must colour the body (SGR 44)"))
    (with-isolated-options ("popup-style" "")
      (is (null (search (format nil "~C[44m" #\Escape) (render-popup-output popup 24 80)))
          "no popup-style means no body bg SGR"))))

(test render-popup-honours-border-lines-option
  "render-popup draws the box with the popup-border-lines characters (the whole
   box: corners and vertical sides), and not the single-line glyphs."
  (with-isolated-options ("popup-border-lines" "double")
    (let* ((popup (make-popup :title "T" :x 0 :y 0 :width 20 :height 6
                              :pane nil :screen nil :close-on-exit nil))
           (out   (render-popup-output popup 24 80)))
      (is (find #\╔ out) "double border draws ╔ top-left")
      (is (find #\╗ out) "double border draws ╗ top-right")
      (is (find #\╚ out) "double border draws ╚ bottom-left")
      (is (find #\╝ out) "double border draws ╝ bottom-right")
      (is (find #\║ out) "double border draws ║ vertical sides")
      (is (null (find #\┌ out)) "no single-line ┌ corner when double is set"))))

(test render-popup-honours-border-style-colour
  "render-popup wraps the popup border in the popup-border-style SGR."
  (with-isolated-options ("popup-border-style" "fg=red")
    (let* ((expected (cl-tmux/renderer:style-to-sgr
                      (cl-tmux/renderer:parse-style-string "fg=red")))
           (popup (make-popup :title "T" :x 0 :y 0 :width 20 :height 6
                              :pane nil :screen nil :close-on-exit nil))
           (out   (render-popup-output popup 24 80)))
      (is (search (format nil "~C[~Am" #\Escape expected) out)
          "the popup-border-style SGR (~S) must appear in the rendered border"
          expected))))

(test render-popup-empty-draws-side-bars
  "render-popup with no live pane fills interior rows with │ side bars."
  (let* ((popup (make-popup :title "T" :x 0 :y 0 :width 10 :height 4
                            :pane nil :screen nil :close-on-exit nil))
         (out   (render-popup-output popup 24 80)))
    (is (find (code-char #x2502) out)
        "render-popup with empty interior must draw │ side bars (got ~S)" out)))

(test render-popup-with-pane-renders-content
  "render-popup with a live pane renders the screen cells inside the box."
  (let* ((sc    (make-screen 8 2))
         (pane  (make-pane :id 1 :x 0 :y 0 :width 8 :height 2 :fd -1 :screen sc))
         (popup (make-popup :title "P" :x 0 :y 0 :width 10 :height 4
                            :pane pane :screen sc :close-on-exit nil)))
    (feed sc "hi")
    (let ((out (render-popup-output popup 24 80)))
      (is (find #\h out)
          "render-popup with live pane must render pane content h (got ~S)" out)
      (is (find #\i out)
          "render-popup with live pane must render pane content i (got ~S)" out))))

;;; ── render-menu ──────────────────────────────────────────────────────────────

(test render-menu-draws-borders-and-items
  "render-menu draws borders, the title, and each menu item label."
  (let* ((items '(("Option A" . nil) ("Option B" . nil) ("Option C" . nil)))
         (menu  (make-menu :title "Choose" :items items :selected-index 0))
         (out   (render-menu-output menu 24 80)))
    (is (find (code-char #x250C) out)  "render-menu must draw top-left ┌ (got ~S)" out)
    (is (find (code-char #x2514) out)  "render-menu must draw bottom-left └ (got ~S)" out)
    (is (search "Choose" out)  "render-menu must include the title (got ~S)" out)
    (is (search "Option A" out) "render-menu must include item 'Option A' (got ~S)" out)
    (is (search "Option B" out) "render-menu must include item 'Option B' (got ~S)" out)
    (is (search "Option C" out) "render-menu must include item 'Option C' (got ~S)" out)))

(test render-menu-selection-indicator
  "render-menu emits ▶ for the selected item and space for others."
  (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
         (menu  (make-menu :title "M" :items items :selected-index 1))
         (out   (render-menu-output menu 24 80)))
    ;; Selected item is index 1 (Beta).
    (is (find (code-char #x25B6) out)
        "render-menu must emit ▶ for the selected item (got ~S)" out)))

(test render-menu-applies-selected-and-item-styles
  "render-menu colours the selected item with menu-selected-style and the others
   with menu-style (when set)."
  (with-isolated-options ("menu-style" "fg=blue" "menu-selected-style" "bg=red")
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 1))
           (out   (render-menu-output menu 24 80)))
      (is (search (format nil "~C[41m" #\Escape) out)
          "selected item must use menu-selected-style bg=red (SGR 41, got ~S)" out)
      (is (search (format nil "~C[34m" #\Escape) out)
          "non-selected items must use menu-style fg=blue (SGR 34, got ~S)" out))))

(test render-menu-no-style-emits-no-item-sgr
  "With menu-style/menu-selected-style empty (default), render-menu emits no item
   colour SGR — only the labels and box, preserving the plain appearance."
  (with-isolated-options ("menu-style" "" "menu-selected-style" "")
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 1))
           (out   (render-menu-output menu 24 80)))
      (is (null (search (format nil "~C[41m" #\Escape) out))
          "no menu-selected-style means no bg SGR (got ~S)" out)
      (is (search "Alpha" out) "labels are still drawn (got ~S)" out))))

(test render-menu-border-lines-selects-glyphs
  "menu-border-lines \"double\" draws the menu box with double-line glyphs (the
   sides too); the default \"single\" uses ┌│└."
  (with-isolated-options ("menu-border-lines" "double")
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 0))
           (out   (render-menu-output menu 24 80)))
      (is (find (code-char #x2554) out) "double → top-left ╔ (got ~S)" out)
      (is (find (code-char #x2551) out) "double → vertical side ║ (got ~S)" out)
      (is (null (find (code-char #x250C) out)) "no single ┌ when double (got ~S)" out))))

(test render-menu-border-style-colours-border
  "menu-border-style colours the menu box border SGR."
  (with-isolated-options ("menu-border-style" "fg=red")
    (let* ((items '(("Alpha" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 0))
           (out   (render-menu-output menu 24 80)))
      (is (search (format nil "~C[31m" #\Escape) out)
          "menu-border-style fg=red must emit SGR 31 (got ~S)" out))))
