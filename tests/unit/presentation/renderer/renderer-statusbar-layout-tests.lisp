(in-package #:cl-tmux/test)

;;;; renderer tests — direct unit tests for the renderer-statusbar-layout.lisp
;;;; helpers that had no dedicated coverage: %sgr-sequence-end, %split-comma-attrs,
;;;; %status-segment-limit, %status-align-block-step, %status-align-step,
;;;; %status-emit-segment, %status-pad-to, and %expand-segment-or-empty.
;;;; These were previously exercised only transitively (if at all) through
;;;; %compose-aligned-line and #{...} integration tests.

(describe "renderer-suite/statusbar-layout"

  ;;; ── %sgr-sequence-end ────────────────────────────────────────────────────────

  ;; %sgr-sequence-end returns the index just past a CSI sequence's final byte.
  (it "sgr-sequence-end-finds-final-byte"
    (let ((s (format nil "~C[1;32mtext" #\Escape)))
      (expect (= 7 (cl-tmux/renderer::%sgr-sequence-end s 0)))))

  ;; %sgr-sequence-end returns NIL when START is not the start of a CSI sequence.
  (it "sgr-sequence-end-returns-nil-for-plain-text"
    (expect (null (cl-tmux/renderer::%sgr-sequence-end "plain text" 0))))

  ;; %sgr-sequence-end returns NIL when ESC is the last character (no room for '[').
  (it "sgr-sequence-end-returns-nil-for-trailing-esc"
    (expect (null (cl-tmux/renderer::%sgr-sequence-end (string #\Escape) 0))))

  ;; %sgr-sequence-end returns the string length for an unterminated CSI sequence.
  (it "sgr-sequence-end-unterminated-consumes-rest"
    (let ((s (format nil "~C[1;32" #\Escape)))
      (expect (= (length s) (cl-tmux/renderer::%sgr-sequence-end s 0)))))

  ;;; ── %split-comma-attrs ───────────────────────────────────────────────────────

  ;; %split-comma-attrs splits on commas and preserves empty fields.
  (it "split-comma-attrs-preserves-empty-fields"
    (expect (equal '("a" "" "b") (cl-tmux/renderer::%split-comma-attrs "a,,b"))))

  ;; %split-comma-attrs with no comma returns a single-element list.
  (it "split-comma-attrs-single-element"
    (expect (equal '("fg=red") (cl-tmux/renderer::%split-comma-attrs "fg=red"))))

  ;; %split-comma-attrs on an empty string returns a single empty-string element.
  (it "split-comma-attrs-empty-string"
    (expect (equal '("") (cl-tmux/renderer::%split-comma-attrs ""))))

  ;; %split-comma-attrs with a trailing comma includes a trailing empty field.
  (it "split-comma-attrs-trailing-comma"
    (expect (equal '("a" "b" "") (cl-tmux/renderer::%split-comma-attrs "a,b,"))))

  ;;; ── %status-segment-limit ────────────────────────────────────────────────────

  ;; %status-segment-limit returns the truncated numeric value when given a number.
  (it "status-segment-limit-numeric-value"
    (expect (= 20 (cl-tmux/renderer::%status-segment-limit 20)))
    (expect (= 20 (cl-tmux/renderer::%status-segment-limit 20.7))))

  ;; %status-segment-limit falls back to the tmux default of 40 for non-numbers.
  (it "status-segment-limit-defaults-for-non-numeric"
    (expect (= 40 (cl-tmux/renderer::%status-segment-limit nil)))
    (expect (= 40 (cl-tmux/renderer::%status-segment-limit "40"))))

  ;; %status-segment-limit clamps negative values to 0.
  (it "status-segment-limit-clamps-negative"
    (expect (= 0 (cl-tmux/renderer::%status-segment-limit -5))))

  ;;; ── %status-align-block-step / %status-align-step ───────────────────────────

  ;; %status-align-step copies an ordinary character into the CURRENT bucket and
  ;; advances by one, when the character is not the start of a #[…] block.
  (it "status-align-step-copies-plain-char"
    (let ((buckets (list :left (make-string-output-stream)
                          :centre (make-string-output-stream)
                          :right (make-string-output-stream))))
      (multiple-value-bind (next-i next-current)
          (cl-tmux/renderer::%status-align-step "abc" 0 buckets :left)
        (expect (= 1 next-i))
        (expect (eq :left next-current))
        (expect (string= "a" (get-output-stream-string (getf buckets :left)))))))

  ;; %status-align-step dispatches to %status-align-block-step on a #[…] marker.
  (it "status-align-step-dispatches-on-align-marker"
    (let ((buckets (list :left (make-string-output-stream)
                          :centre (make-string-output-stream)
                          :right (make-string-output-stream))))
      (multiple-value-bind (next-i next-current)
          (cl-tmux/renderer::%status-align-step "#[align=right]" 0 buckets :left)
        (expect (= 14 next-i))
        (expect (eq :right next-current)))))

  ;; %status-align-block-step on an align=… block switches CURRENT and swallows
  ;; the marker, writing nothing when there are no other attrs.
  (it "status-align-block-step-switches-bucket-on-align-only"
    (let ((buckets (list :left (make-string-output-stream)
                          :centre (make-string-output-stream)
                          :right (make-string-output-stream))))
      (multiple-value-bind (next-i next-current)
          (cl-tmux/renderer::%status-align-block-step "#[align=centre]" 0 buckets :left)
        (expect (= 15 next-i))
        (expect (eq :centre next-current))
        (expect (string= "" (get-output-stream-string (getf buckets :centre)))))))

  ;; %status-align-block-step on a combined align+attr block re-emits the
  ;; non-align attrs into the NEW current bucket as a #[…] prefix.
  (it "status-align-block-step-preserves-combined-attrs"
    (let ((buckets (list :left (make-string-output-stream)
                          :centre (make-string-output-stream)
                          :right (make-string-output-stream))))
      (cl-tmux/renderer::%status-align-block-step "#[align=right,fg=red]" 0 buckets :left)
      (expect (string= "#[fg=red]" (get-output-stream-string (getf buckets :right))))))

  ;; %status-align-block-step on a non-align block copies it verbatim into CURRENT.
  (it "status-align-block-step-copies-non-align-block-verbatim"
    (let ((buckets (list :left (make-string-output-stream)
                          :centre (make-string-output-stream)
                          :right (make-string-output-stream))))
      (multiple-value-bind (next-i next-current)
          (cl-tmux/renderer::%status-align-block-step "#[fg=green]" 0 buckets :left)
        (expect (= 11 next-i))
        (expect (eq :left next-current))
        (expect (string= "#[fg=green]" (get-output-stream-string (getf buckets :left)))))))

  ;; %status-align-block-step on an unterminated '#[' copies just the '#' and
  ;; advances by one, leaving the rest for the next step to process.
  (it "status-align-block-step-unterminated-marker"
    (let ((buckets (list :left (make-string-output-stream)
                          :centre (make-string-output-stream)
                          :right (make-string-output-stream))))
      (multiple-value-bind (next-i next-current)
          (cl-tmux/renderer::%status-align-block-step "#[unterminated" 0 buckets :left)
        (expect (= 1 next-i))
        (expect (eq :left next-current))
        (expect (string= "#" (get-output-stream-string (getf buckets :left)))))))

  ;;; ── %status-pad-to ───────────────────────────────────────────────────────────

  ;; %status-pad-to pads OUT with spaces until CURRENT reaches TARGET.
  (it "status-pad-to-pads-and-returns-new-column"
    (let ((out (make-string-output-stream)))
      (expect (= 5 (cl-tmux/renderer::%status-pad-to out 2 5)))
      (expect (string= "   " (get-output-stream-string out)))))

  ;; %status-pad-to is a no-op (returns CURRENT unchanged) when already at/past TARGET.
  (it "status-pad-to-noop-when-already-at-target"
    (let ((out (make-string-output-stream)))
      (expect (= 5 (cl-tmux/renderer::%status-pad-to out 5 5)))
      (expect (= 6 (cl-tmux/renderer::%status-pad-to out 6 5)))
      (expect (string= "" (get-output-stream-string out)))))

  ;;; ── %status-emit-segment ─────────────────────────────────────────────────────

  ;; %status-emit-segment pads up to POS, writes SEG, and returns the new column.
  (it "status-emit-segment-pads-then-writes"
    (let ((out (make-string-output-stream)))
      (expect (= 8 (cl-tmux/renderer::%status-emit-segment out 0 80 "abc" 3 5)))
      (expect (string= "     abc" (get-output-stream-string out)))))

  ;; %status-emit-segment is a no-op returning CURRENT unchanged when WIDTH is zero.
  (it "status-emit-segment-noop-for-zero-width"
    (let ((out (make-string-output-stream)))
      (expect (= 3 (cl-tmux/renderer::%status-emit-segment out 3 80 "" 0 5)))
      (expect (string= "" (get-output-stream-string out)))))

  ;; %status-emit-segment does not write SEG when CURRENT is already past COLS.
  (it "status-emit-segment-skips-when-past-cols"
    (let ((out (make-string-output-stream)))
      (cl-tmux/renderer::%status-emit-segment out 80 80 "xyz" 3 80)
      (expect (string= "" (get-output-stream-string out)))))

  ;;; ── %expand-segment-or-empty ─────────────────────────────────────────────────

  ;; %expand-segment-or-empty returns "" for an empty RAW string.
  (it "expand-segment-or-empty-returns-empty-for-empty-raw"
    (expect (string= "" (cl-tmux/renderer::%expand-segment-or-empty "" "44;97" "reset"))))

  ;; %expand-segment-or-empty expands #[…] blocks in RAW and appends RESET.
  (it "expand-segment-or-empty-expands-and-appends-reset"
    (let ((result (cl-tmux/renderer::%expand-segment-or-empty "plain" "44;97" "RESET-MARKER")))
      (expect (string= "plainRESET-MARKER" result))))

  ;; %expand-segment-or-empty with a style block still appends RESET at the end.
  (it "expand-segment-or-empty-with-style-block-appends-reset"
    (let ((result (cl-tmux/renderer::%expand-segment-or-empty "#[fg=red]x" "44;97" "RESET-MARKER")))
      (expect (search "RESET-MARKER" result))
      (expect (char= #\x (char result (1- (- (length result) (length "RESET-MARKER"))))))))
  )
