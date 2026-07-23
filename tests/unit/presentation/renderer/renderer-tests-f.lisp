(in-package #:cl-tmux/test)

;;;; renderer tests — part F: parse-style-string, style-to-sgr,
;;;; status-left/right length enforcement, window-status-format,
;;;; window-status-separator, render-popup, render-menu.

(describe "renderer-suite"

  ;;; ── parse-style-string ───────────────────────────────────────────────────────

  ;; parse-style-string returns NIL for NIL and empty-string inputs.
  (it "parse-style-string-nil-inputs-table"
    (dolist (row '((nil "NIL input → nil")
                   (""  "empty-string input → nil")))
      (destructuring-bind (input desc) row
        (declare (ignore desc))
        (expect (null (cl-tmux/renderer:parse-style-string input))))))

  ;; parse-style-string sets :fg or :bg to the parsed colour name.
  (it "parse-style-string-color-key-table"
    (dolist (row '(("fg=red"     :fg "red"     "fg=red → :fg \"red\"")
                   ("bg=blue"    :bg "blue"     "bg=blue → :bg \"blue\"")
                   ("fg=colour4" :fg "colour4"  "fg=colour4 → :fg \"colour4\"")))
      (destructuring-bind (input key expected desc) row
        (declare (ignore desc))
        (let ((p (cl-tmux/renderer:parse-style-string input)))
          (expect (string= expected (getf p key)))))))

  ;; parse-style-string sets boolean attribute keys to T.
  (it "parse-style-string-bool-attr-table"
    (dolist (row '(("bold"    :bold    "bold → :bold T")
                   ("reverse" :reverse "reverse → :reverse T")))
      (destructuring-bind (input key desc) row
        (declare (ignore desc))
        (let ((p (cl-tmux/renderer:parse-style-string input)))
          (expect (getf p key))))))

  ;; parse-style-string parses fg=green,bold,underline into a combined plist.
  (it "parse-style-string-multiple-attrs"
    (let ((p (cl-tmux/renderer:parse-style-string "fg=green,bold,underline")))
      (expect (string= "green" (getf p :fg)))
      (expect (getf p :bold))
      (expect (getf p :underline))))

  ;;; ── style-to-sgr ────────────────────────────────────────────────────────────

  ;; style-to-sgr with NIL returns default blue-on-white SGR "44;97".
  (it "style-to-sgr-nil-returns-default"
    (expect (string= "44;97" (cl-tmux/renderer:style-to-sgr nil))))

  ;; style-to-sgr includes the correct SGR code substring for each attribute.
  (it "style-to-sgr-attrs-table"
    (dolist (row '(((:bold t)       "1"      ":bold T → SGR 1")
                   ((:reverse t)    "7"      ":reverse T → SGR 7")
                   ((:fg "red")     "31"     ":fg red → SGR 31")
                   ((:bg "blue")    "44"     ":bg blue → SGR 44")
                   ((:bg "colour4") "48;5;4" ":bg colour4 → SGR 48;5;4")))
      (destructuring-bind (style expected desc) row
        (declare (ignore desc))
        (let ((sgr (cl-tmux/renderer:style-to-sgr style)))
          (expect (search expected sgr))))))

  ;;; ── status-left-length / status-right-length enforcement ────────────────────

  ;; status-left-length truncates the expanded left string to the configured max.
  (it "status-left-length-truncates-long-left"
    (with-isolated-options ()
      (cl-tmux/options:set-option "status-left" "abcdefghij")
      (cl-tmux/options:set-option "status-left-length" 5)
      (let* ((sess (make-renderer-test-session 80 10))
             (out  (render-status-bar-output sess 11 80)))
        (expect (search "abcde" out))
        (expect (null (search "abcdefghij" out))))))

  ;; status-right-length truncates the expanded right string to the configured max.
  (it "status-right-length-truncates-long-right"
    (with-isolated-options ()
      (cl-tmux/options:set-option "status-right" "1234567890")
      (cl-tmux/options:set-option "status-right-length" 4)
      (let* ((sess (make-renderer-test-session 80 10))
             (out  (render-status-bar-output sess 11 80)))
        (expect (search "1234" out))
        (expect (null (search "1234567890" out))))))

  ;;; ── window-status-format and window-status-current-format ───────────────────

  ;; window-status-format option is used when rendering inactive windows.
  (it "window-status-format-custom"
    (with-empty-status-bar-options ("window-status-format" "WIN:#{window_name}"
                                    "window-status-current-format" "[#{window_name}]")
      ;; make-two-window-session creates windows named "alpha" (active) and "beta".
      (multiple-value-bind (sess win0 _p0 _w1 _p1)
          (make-two-window-session 80 5)
        (declare (ignore _p0 _w1 _p1))
        (session-select-window sess win0)  ; alpha is active
        (let ((out (render-status-bar-output sess 11 80)))
          (expect (search "[alpha]" out))
          (expect (search "WIN:beta" out))))))

  ;;; ── window-status-separator ──────────────────────────────────────────────────

  ;; window-status-separator is placed between window entries.
  (it "window-status-separator-used-between-windows"
    (with-empty-status-bar-options ("window-status-separator" "|SEP|")
      (multiple-value-bind (sess win0 _p0 _w1 _p1)
          (make-two-window-session 80 5)
        (declare (ignore _p0 _w1 _p1))
        (session-select-window sess win0)
        (let ((out (render-status-bar-output sess 11 80)))
          (expect (search "|SEP|" out))))))

  ;;; ── render-popup ─────────────────────────────────────────────────────────────

  ;; render-popup with no live pane draws top border with corners and title, plus bottom border.
  (it "render-popup-empty-draws-borders"
    (let* ((popup (make-popup :title "Test" :width 20 :height 6
                              :pane nil :screen nil :close-on-exit nil))
           (out   (render-popup-output popup 24 80)))
      (expect (find (code-char #x250C) out))
      (expect (find (code-char #x2510) out))
      (expect (find (code-char #x2514) out))
      (expect (find (code-char #x2518) out))
      (expect (search "Test" out))))

  ;; popup-style colours the empty popup interior; with it unset the body has no SGR.
  (it "render-popup-style-colours-empty-body"
    (let ((popup (make-popup :title "T" :width 20 :height 6
                             :pane nil :screen nil :close-on-exit nil)))
      (with-isolated-options ("popup-style" "bg=blue")
        (expect (search (format nil "~C[44m" #\Escape) (render-popup-output popup 24 80))))
      (with-isolated-options ("popup-style" "")
        (expect (null (search (format nil "~C[44m" #\Escape) (render-popup-output popup 24 80)))))))

  ;; render-popup draws the box with the popup-border-lines characters (the whole
  ;; box: corners and vertical sides), and not the single-line glyphs.
  (it "render-popup-honours-border-lines-option"
    (with-isolated-options ("popup-border-lines" "double")
      (let* ((popup (make-popup :title "T" :width 20 :height 6
                                :pane nil :screen nil :close-on-exit nil))
             (out   (render-popup-output popup 24 80)))
        (expect (find #\╔ out))
        (expect (find #\╗ out))
        (expect (find #\╚ out))
        (expect (find #\╝ out))
        (expect (find #\║ out))
        (expect (null (find #\┌ out))))))

  ;; render-popup wraps the popup border in the popup-border-style SGR.
  (it "render-popup-honours-border-style-colour"
    (with-isolated-options ("popup-border-style" "fg=red")
      (let* ((expected (cl-tmux/renderer:style-to-sgr
                        (cl-tmux/renderer:parse-style-string "fg=red")))
             (popup (make-popup :title "T" :width 20 :height 6
                                :pane nil :screen nil :close-on-exit nil))
             (out   (render-popup-output popup 24 80)))
        (expect out :to-contain-sgr expected))))

  ;; render-popup with no live pane fills interior rows with │ side bars.
  (it "render-popup-empty-draws-side-bars"
    (let* ((popup (make-popup :title "T" :width 10 :height 4
                              :pane nil :screen nil :close-on-exit nil))
           (out   (render-popup-output popup 24 80)))
      (expect (find (code-char #x2502) out))))

  ;; render-popup with a live pane renders the screen cells inside the box.
  (it "render-popup-with-pane-renders-content"
    (let* ((sc    (make-screen 8 2))
           (pane  (make-pane :id 1 :x 0 :y 0 :width 8 :height 2 :fd -1 :screen sc))
           (popup (make-popup :title "P" :width 10 :height 4
                              :pane pane :screen sc :close-on-exit nil)))
      (feed sc "hi")
      (let ((out (render-popup-output popup 24 80)))
        (expect (find #\h out))
        (expect (find #\i out)))))

  ;;; ── render-menu ──────────────────────────────────────────────────────────────

  ;; render-menu draws borders, the title, and each menu item label.
  (it "render-menu-draws-borders-and-items"
    (let* ((items '(("Option A" . nil) ("Option B" . nil) ("Option C" . nil)))
           (menu  (make-menu :title "Choose" :items items :selected-index 0))
           (out   (render-menu-output menu 24 80)))
      (expect (find (code-char #x250C) out))
      (expect (find (code-char #x2514) out))
      (expect (search "Choose" out))
      (expect (search "Option A" out))
      (expect (search "Option B" out))
      (expect (search "Option C" out))))

  ;; render-menu emits ▶ for the selected item and space for others.
  (it "render-menu-selection-indicator"
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 1))
           (out   (render-menu-output menu 24 80)))
      ;; Selected item is index 1 (Beta).
      (expect (find (code-char #x25B6) out))))

  ;; render-menu colours the selected item with menu-selected-style and the others
  ;; with menu-style (when set).
  (it "render-menu-applies-selected-and-item-styles"
    (with-isolated-options ("menu-style" "fg=blue" "menu-selected-style" "bg=red")
      (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
             (menu  (make-menu :title "M" :items items :selected-index 1))
             (out   (render-menu-output menu 24 80)))
        (expect (search (format nil "~C[41m" #\Escape) out))
        (expect (search (format nil "~C[34m" #\Escape) out)))))

  ;; With menu-style/menu-selected-style empty (default), render-menu emits no item
  ;; colour SGR — only the labels and box, preserving the plain appearance.
  (it "render-menu-no-style-emits-no-item-sgr"
    (with-isolated-options ("menu-style" "" "menu-selected-style" "")
      (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
             (menu  (make-menu :title "M" :items items :selected-index 1))
             (out   (render-menu-output menu 24 80)))
        (expect (null (search (format nil "~C[41m" #\Escape) out)))
        (expect (search "Alpha" out)))))

  ;; menu-border-lines "double" draws the menu box with double-line glyphs (the
  ;; sides too); the default "single" uses ┌│└.
  (it "render-menu-border-lines-selects-glyphs"
    (with-isolated-options ("menu-border-lines" "double")
      (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
             (menu  (make-menu :title "M" :items items :selected-index 0))
             (out   (render-menu-output menu 24 80)))
        (expect (find (code-char #x2554) out))
        (expect (find (code-char #x2551) out))
        (expect (null (find (code-char #x250C) out))))))

  ;; menu-border-style colours the menu box border SGR.
  (it "render-menu-border-style-colours-border"
    (with-isolated-options ("menu-border-style" "fg=red")
      (let* ((items '(("Alpha" . nil)))
             (menu  (make-menu :title "M" :items items :selected-index 0))
             (out   (render-menu-output menu 24 80)))
        (expect (search (format nil "~C[31m" #\Escape) out))))))
