(in-package #:cl-tmux/test)

;;;; Escape-code format primitive tests.
;;;;
;;;; Covers: ANSI escape-code helpers in src/renderer-format.lisp:
;;;;   +esc+, move-to, define-cell-attr-renderer, cursor-invisible/visible, reset-attrs

(def-suite renderer-suite :description "Escape-code renderer")
(in-suite renderer-suite)

;;; ── Helper ──────────────────────────────────────────────────────────────────

(defun cell-attrs-string (fg bg attrs)
  (with-output-to-string (s)
    (cl-tmux/renderer::render-cell-attrs s fg bg attrs)))

;;; ── move-to (1-based conversion) ────────────────────────────────────────────

(test move-to-is-one-based
  (is (string= (format nil "~C[1;1H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 0 0)))
      "move-to 0,0 should emit ESC[1;1H")
  (is (string= (format nil "~C[3;5H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 2 4)))
      "move-to 2,4 should emit ESC[3;5H"))

;;; ── render-cell-attrs (SGR codes) ───────────────────────────────────────────

(test render-cell-attrs-foreground
  (let ((out (cell-attrs-string 1 0 0)))
    (is (search ";31" out) "fg 1 should emit ;31 (got ~S)" out)))

(test render-cell-attrs-background
  (let ((out (cell-attrs-string 0 2 0)))
    (is (search ";42" out) "bg 2 should emit ;42 (got ~S)" out)))

(test render-cell-attrs-bold
  (let ((out (cell-attrs-string 0 0 1)))      ; bit0 = bold
    (is (search ";1" out) "bold (attrs bit0) should emit ;1 (got ~S)" out)))

(test render-cell-attrs-reverse
  (let ((out (cell-attrs-string 0 0 4)))      ; bit2 = reverse video
    (is (search ";7" out) "reverse (attrs bit2) should emit ;7 (got ~S)" out)))

(test render-cell-attrs-bright-foreground
  (let ((out (cell-attrs-string 9 0 0)))      ; bright fg uses 82+fg => 91
    (is (search ";91" out) "bright fg 9 should emit ;91 (got ~S)" out)))

(test render-cell-attrs-frame
  (let ((out (cell-attrs-string 1 2 1)))
    (is (eql 0 (search (format nil "~C[0" #\Escape) out))
        "render-cell-attrs should start with ESC[0 (got ~S)" out)
    (is (char= #\m (char out (1- (length out))))
        "render-cell-attrs should end with m (got ~S)" out)))

(test render-cell-attrs-dim
  (let ((out (cell-attrs-string 7 0 2)))      ; bit1 = dim
    (is (search ";2" out) "dim (attrs bit1) should emit ;2 (got ~S)" out)))

(test render-cell-attrs-default-color-omitted
  ;; fg/bg outside 0..15 emit no colour code — only the leading reset remains.
  (let ((out (cell-attrs-string -1 -1 0)))
    (is (string= (format nil "~C[0m" #\Escape) out)
        "out-of-range fg/bg should omit colour codes (got ~S)" out)))

;;; ── cursor-invisible / cursor-visible ───────────────────────────────────────

(test cursor-invisible-emits-hide-sequence
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::cursor-invisible s))))
    (is (string= (format nil "~C[?25l" #\Escape) out)
        "cursor-invisible should emit ESC[?25l (got ~S)" out)))

(test cursor-visible-emits-show-sequence
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::cursor-visible s))))
    (is (string= (format nil "~C[?25h" #\Escape) out)
        "cursor-visible should emit ESC[?25h (got ~S)" out)))

;;; ── reset-attrs ─────────────────────────────────────────────────────────────

(test reset-attrs-emits-sgr-zero-m
  "reset-attrs writes ESC[0m to the stream."
  (let ((s (make-string-output-stream)))
    (cl-tmux/renderer::reset-attrs s)
    (is (string= (format nil "~C[0m" #\Escape)
                 (get-output-stream-string s)))))

(test render-cell-attrs-underline
  "Underline attribute (bit 3 = #x08 = +attr-underline+) emits ;4."
  (let ((out (cell-attrs-string 0 0 8)))     ; bit3 = underline
    (is (search ";4" out) "underline (attrs bit3) should emit ;4 (got ~S)" out)))

(test render-cell-attrs-blink
  "Blink attribute (bit 4 = #x10 = +attr-blink+) emits ;5."
  (let ((out (cell-attrs-string 0 0 16)))    ; bit4 = blink
    (is (search ";5" out) "blink (attrs bit4) should emit ;5 (got ~S)" out)))

(test define-cell-attr-renderer-macro-is-defined
  "define-cell-attr-renderer is a defined macro."
  (is (macro-function 'cl-tmux/renderer::define-cell-attr-renderer)))

;;; ── New attribute bits: emit path ───────────────────────────────────────────

(test render-cell-attrs-italic
  "Italic attribute (bit 5 = #x20 = +attr-italic+) emits ;3 in the SGR string."
  (let ((out (cell-attrs-string 0 0 32)))    ; bit5 = #b00100000 = 32
    (is (search ";3" out) "italic (attrs bit5) must emit ;3 (got ~S)" out)))

(test render-cell-attrs-conceal
  "Conceal attribute (bit 6 = #x40 = +attr-conceal+) emits ;8."
  (let ((out (cell-attrs-string 0 0 64)))    ; bit6 = #b01000000 = 64
    (is (search ";8" out) "conceal (attrs bit6) must emit ;8 (got ~S)" out)))

(test render-cell-attrs-strikethrough
  "Strikethrough attribute (bit 7 = #x80 = +attr-strikethrough+) emits ;9."
  (let ((out (cell-attrs-string 0 0 128)))   ; bit7 = #b10000000 = 128
    (is (search ";9" out) "strikethrough (attrs bit7) must emit ;9 (got ~S)" out)))

;;; ── Extended colour emit paths ───────────────────────────────────────────────

(test render-cell-attrs-256color-fg
  "fg index 200 (256-color palette) emits the sequence ;38;5;200 in the SGR string."
  (let ((out (cell-attrs-string 200 0 0)))
    (is (search ";38;5;200" out)
        "256-color fg 200 must emit ;38;5;200 (got ~S)" out)))

(test render-cell-attrs-256color-bg
  "bg index 42 (256-color palette) emits the sequence ;48;5;42 in the SGR string."
  (let ((out (cell-attrs-string 0 42 0)))
    (is (search ";48;5;42" out)
        "256-color bg 42 must emit ;48;5;42 (got ~S)" out)))

(test render-cell-attrs-truecolor-fg
  "True-color fg #x1FF8000 (R=255, G=128, B=0) emits ;38;2;255;128;0."
  (let* ((fg  (logior #x1000000 (ash 255 16) (ash 128 8) 0))
         (out (cell-attrs-string fg 0 0)))
    (is (search ";38;2;255;128;0" out)
        "truecolor fg must emit ;38;2;255;128;0 (got ~S)" out)))

(test render-cell-attrs-truecolor-bg
  "True-color bg #x10080FF (R=0, G=128, B=255) emits ;48;2;0;128;255."
  (let* ((bg  (logior #x1000000 (ash 0 16) (ash 128 8) 255))
         (out (cell-attrs-string 0 bg 0)))
    (is (search ";48;2;0;128;255" out)
        "truecolor bg must emit ;48;2;0;128;255 (got ~S)" out)))
