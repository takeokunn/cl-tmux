(in-package #:cl-tmux/test)

;;;; Escape-code format primitive tests.
;;;;
;;;; Covers: ANSI escape-code helpers in src/renderer-format.lisp:
;;;;   +esc+, move-to, define-cell-attr-renderer, cursor-invisible/visible, reset-attrs

(describe "renderer-suite"

  ;; ── Helper ──────────────────────────────────────────────────────────────────

  (defun cell-attrs-string (fg bg attrs &optional (attrs2 0) (ul-color 0))
    (with-output-to-string (s)
      (cl-tmux/renderer::render-cell-attrs s fg bg attrs attrs2 ul-color)))

  ;; ── move-to (1-based conversion) ────────────────────────────────────────────

  ;; move-to converts 0-based row/col arguments to 1-based ESC[row;colH sequences.
  (it "move-to-is-one-based"
    (expect (string= (format nil "~C[1;1H" #\Escape)
                     (with-output-to-string (s)
                       (cl-tmux/renderer::move-to s 0 0))))
    (expect (string= (format nil "~C[3;5H" #\Escape)
                     (with-output-to-string (s)
                       (cl-tmux/renderer::move-to s 2 4)))))

  ;; ── render-cell-attrs (SGR codes) ───────────────────────────────────────────

  ;; render-cell-attrs emits the correct SGR code for standard fg, bg, and bright fg.
  (it "render-cell-attrs-basic-color-table"
    (dolist (c '((1 0 ";31" "fg 1 → ;31 (red)")
                 (0 2 ";42" "bg 2 → ;42 (green)")
                 (9 0 ";91" "bright fg 9 → ;91")))
      (destructuring-bind (fg bg expected desc) c
        (declare (ignore desc))
        (let ((out (cell-attrs-string fg bg 0)))
          (expect (search expected out))))))

  ;; render-cell-attrs always wraps its SGR codes in a leading ESC[0 reset and a trailing m.
  (it "render-cell-attrs-frame"
    (let ((out (cell-attrs-string 1 2 1)))
      (expect (eql 0 (search (format nil "~C[0" #\Escape) out)))
      (expect (char= #\m (char out (1- (length out)))))))

  ;; fg/bg outside 0..15 emit no colour code — only the leading reset remains.
  (it "render-cell-attrs-default-color-omitted"
    (let ((out (cell-attrs-string -1 -1 0)))
      (expect (string= (format nil "~C[0m" #\Escape) out))))

  ;; ── cursor-invisible / cursor-visible ───────────────────────────────────────

  ;; cursor-invisible emits ESC[?25l; cursor-visible emits ESC[?25h.
  (it "cursor-visibility-sequences-table"
    (dolist (c '((cl-tmux/renderer::cursor-invisible "?25l" "cursor-invisible → ESC[?25l")
                 (cl-tmux/renderer::cursor-visible   "?25h" "cursor-visible → ESC[?25h")))
      (destructuring-bind (fn suffix desc) c
        (declare (ignore desc))
        (let ((out (with-output-to-string (s) (funcall fn s))))
          (expect (string= (format nil "~C[~A" #\Escape suffix) out))))))

  ;; ── reset-attrs ─────────────────────────────────────────────────────────────

  ;; reset-attrs writes ESC[0m to the stream.
  (it "reset-attrs-emits-sgr-zero-m"
    (let ((s (make-string-output-stream)))
      (cl-tmux/renderer::reset-attrs s)
      (expect (string= (format nil "~C[0m" #\Escape)
                       (get-output-stream-string s)))))

  ;; define-cell-attr-renderer is a defined macro.
  (it "define-cell-attr-renderer-macro-is-defined"
    (expect (macro-function 'cl-tmux/renderer::define-cell-attr-renderer)))

  ;; ── render-cell-attrs attribute-bit table ───────────────────────────────────
  ;;
  ;; All single-bit attribute flags: each row is (bit-value SGR-search-string label).

  ;; Each attrs bit-flag causes render-cell-attrs to include the correct SGR code.
  ;; Rows: (attrs-value expected-sgr-substring label).
  (it "render-cell-attrs-attribute-bits"
    (dolist (c '((1   ";1"  "bold (bit0)")
                 (2   ";2"  "dim (bit1)")
                 (4   ";7"  "reverse (bit2)")
                 (8   ";4"  "underline (bit3)")
                 (16  ";5"  "blink (bit4)")
                 (32  ";3"  "italic (bit5)")
                 (64  ";8"  "conceal (bit6)")
                 (128 ";9"  "strikethrough (bit7)")))
      (destructuring-bind (attrs sgr label) c
        (declare (ignore label))
        (let ((out (cell-attrs-string 0 0 attrs)))
          (expect (search sgr out))))))

  ;; ── Extended colour emit paths ───────────────────────────────────────────────

  ;; 256-color palette indices emit the correct extended SGR sequences.
  (it "render-cell-attrs-256color-table"
    (dolist (c '((200 0   ";38;5;200" "fg 200 → ;38;5;200")
                 (0   42  ";48;5;42"  "bg 42 → ;48;5;42")))
      (destructuring-bind (fg bg expected desc) c
        (declare (ignore desc))
        (let ((out (cell-attrs-string fg bg 0)))
          (expect (search expected out))))))

  ;; True-color values emit the correct 38;2;R;G;B / 48;2;R;G;B SGR sequences.
  (it "render-cell-attrs-truecolor-table"
    (dolist (c (list (list (logior #x1000000 (ash 255 16) (ash 128 8)   0) 0 ";38;2;255;128;0"   "truecolor fg → ;38;2;255;128;0")
                     (list 0 (logior #x1000000 (ash 0   16) (ash 128 8) 255) ";48;2;0;128;255"   "truecolor bg → ;48;2;0;128;255")))
      (destructuring-bind (fg bg expected desc) c
        (declare (ignore desc))
        (let ((out (cell-attrs-string fg bg 0)))
          (expect (search expected out))))))

  ;; ── %split-style-tokens ─────────────────────────────────────────────────────

  ;; %split-style-tokens splits on commas; single-token and empty-string edge cases.
  (it "split-style-tokens-table"
    (dolist (c '(("bold"                 ("bold")                       "single token")
                 ("fg=red,bold,underline" ("fg=red" "bold" "underline") "comma-split")
                 (""                     ("")                           "empty → one empty string")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/renderer::%split-style-tokens input))))))

  ;; ── %dispatch-style-token ───────────────────────────────────────────────────

  ;; %dispatch-style-token sets the correct plist key for bold, reverse, and underline.
  (it "dispatch-style-token-sets-attr-table"
    (dolist (c '(("bold"      :bold      "bold sets :bold T")
                 ("reverse"   :reverse   "reverse sets :reverse T")
                 ("underline" :underline "underline sets :underline T")))
      (destructuring-bind (token key desc) c
        (declare (ignore desc))
        (let ((cell (list nil)))
          (expect (cl-tmux/renderer::%dispatch-style-token token cell) :to-be-truthy)
          (expect (getf (car cell) key) :to-be-truthy)))))

  ;; %dispatch-style-token 'nobold' sets :bold NIL in result-cell.
  (it "dispatch-style-token-nobold"
    (let ((cell (list (list :bold t))))
      (cl-tmux/renderer::%dispatch-style-token "nobold" cell)
      (expect (null (getf (car cell) :bold)))))

  ;; %dispatch-style-token returns NIL for an unknown token.
  (it "dispatch-style-token-unknown-returns-nil"
    (let ((cell (list nil)))
      (expect (null (cl-tmux/renderer::%dispatch-style-token "completely-unknown" cell)))))

  ;; ── %emit-style-attrs ───────────────────────────────────────────────────────

  ;; %emit-style-attrs with :bold T pushes "1" onto parts.
  (it "emit-style-attrs-bold"
    (let ((parts (cl-tmux/renderer::%emit-style-attrs '(:bold t) nil)))
      (expect (member "1" parts :test #'string=))))

  ;; %emit-style-attrs pushes codes for all set attributes.
  (it "emit-style-attrs-reverse-and-underline"
    (let ((parts (cl-tmux/renderer::%emit-style-attrs '(:reverse t :underline t) nil)))
      (expect (member "7" parts :test #'string=))
      (expect (member "4" parts :test #'string=))))

  ;; %emit-style-attrs with an empty style plist returns the unchanged parts.
  (it "emit-style-attrs-empty-style-returns-nil-parts"
    (let ((parts (cl-tmux/renderer::%emit-style-attrs nil nil)))
      (expect (null parts))))

  ;; ── %border-color-sgr ───────────────────────────────────────────────────────

  ;; %border-color-sgr maps known colour names to SGR codes; nil for unknown; case-insensitive.
  (it "border-color-sgr-table"
    (dolist (c '(("green"    32  "green → 32")
                 ("red"      31  "red → 31")
                 ("Blue"     34  "mixed-case Blue → 34")
                 ("notacolor" nil "unknown → nil")))
      (destructuring-bind (color expected desc) c
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/renderer::%border-color-sgr color))))))

  ;; ── %color-name-to-sgr-number ───────────────────────────────────────────────

  ;; %color-name-to-sgr-number maps named colours, colour-N, default, and unknown correctly
  ;; for both fg (is-bg NIL) and bg (is-bg T).
  (it "color-name-to-sgr-number-table"
    (dolist (c '(("red"      nil "31")
                 ("red"      t   "41")
                 ("colour4"  nil "38;5;4")
                 ("colour4"  t   "48;5;4")
                 ("default"  nil "39")
                 ("default"  t   "49")
                 ("notacolor" nil "39")
                 ("notacolor" t   "49")))
      (destructuring-bind (color is-bg expected) c
        (expect (string= expected
                         (cl-tmux/renderer::%color-name-to-sgr-number color is-bg))))))

  ;; ── %status-sgr-from-style ───────────────────────────────────────────────────

  ;; %status-sgr-from-style returns the default blue-on-white SGR for nil and empty string.
  (it "status-sgr-from-style-default-table"
    (dolist (c '((nil "nil arg → default SGR")
                 (""  "empty string → default SGR")))
      (destructuring-bind (arg desc) c
        (declare (ignore desc))
        (expect (string= "44;97" (cl-tmux/renderer::%status-sgr-from-style arg))))))

  ;; %status-sgr-from-style with 'bold' includes SGR code 1.
  (it "status-sgr-from-style-bold"
    (let ((sgr (cl-tmux/renderer::%status-sgr-from-style "bold")))
      (expect (search "1" sgr))))

  ;; ── %effective-status-style ─────────────────────────────────────────────────

  ;; %effective-status-style is empty when status-style is not set.
  (it "effective-status-style-empty-when-nothing-set"
    (with-isolated-config
      (expect (string= "" (cl-tmux/renderer::%effective-status-style)))))

  ;; %effective-status-style returns the status-style option value directly.
  (it "effective-status-style-returns-status-style"
    (with-isolated-config
      (cl-tmux/options:set-option "status-style" "fg=white,bg=blue,bold")
      (let ((eff (cl-tmux/renderer::%effective-status-style)))
        (expect (search "bold" eff))
        (expect (search "fg=white" eff))
        (expect (search "bg=blue" eff)))))

  ;; ── set-cursor-shape ─────────────────────────────────────────────────────────

  ;; set-cursor-shape emits the DECSCUSR sequence ESC[N q for each shape number.
  (it "set-cursor-shape-table"
    (dolist (c '((2 "2 q" "shape 2 (steady block)")
                 (1 "1 q" "shape 1 (blinking block)")))
      (destructuring-bind (shape suffix desc) c
        (declare (ignore desc))
        (let ((out (with-output-to-string (s) (cl-tmux/renderer::set-cursor-shape s shape))))
          (expect (search (format nil "~C[~A" #\Escape suffix) out))))))

  ;; ── %emit-fg / %emit-bg palette boundaries ────────────────────────────────────

  ;; Verifies fg/bg SGR emission at standard-colour, bright, and 256-colour boundaries.
  ;; Each row is (fg bg expected-sgr-substring description).
  (it "emit-fg-bg-palette-boundary-table"
    (dolist (c '((7   0   ";37"      "fg 7 (white)")
                 (0   0   ";40"      "bg 0 (black); fg=0 also emits ;30 but ;40 is present")
                 (8   0   ";90"      "fg 8 (first bright)")
                 (15  0   ";97"      "fg 15 (last bright)")
                 (16  0   ";38;5;16" "fg 16 (first 256-colour)")
                 (255 0   ";38;5;255" "fg 255 (last 256-colour)")
                 (0   16  ";48;5;16" "bg 16 (first 256-colour)")
                 (0   255 ";48;5;255" "bg 255 (last 256-colour)")))
      (destructuring-bind (fg bg expected desc) c
        (declare (ignore desc))
        (let ((out (cell-attrs-string fg bg 0)))
          (expect (search expected out))))))

  ;; ── render-cell-attrs all attributes table ───────────────────────────────────
  )
