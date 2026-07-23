(in-package #:cl-tmux/test)

;;;; events tests: copy-mode numeric-prefix repeat counts.

(describe "events-suite"

  ;;; ── Copy-mode numeric-prefix repeat counts ───────────────────────────────────
  ;;;
  ;;; %copy-mode-accumulate-digit folds digit bytes 1-9 (and 0 once a non-zero
  ;;; prefix has started) into *copy-mode-prefix*; the next non-digit byte
  ;;; applies the accumulated count (clamped to a minimum of 1) and resets the
  ;;; prefix to 0.  These end-to-end tests drive the accumulator entirely
  ;;; through process-byte, one byte at a time, matching how real keystrokes
  ;;; arrive.
  ;;;
  ;;; Every test here pins mode-keys to "vi" via WITH-ISOLATED-CONFIG because
  ;;; numeric prefixes are applied to repeatable entries in the active copy-mode
  ;;; key table.  The emacs table keeps emacs meanings for the same bytes instead
  ;;; of falling through to vi behavior.

  ;; A numeric prefix (e.g. "3j") repeats the following navigation command that
  ;; many times; digits 1-9 always start/continue accumulation.
  (it "copy-mode-numeric-prefix-repeats-scroll-table"
    (with-isolated-config
      (cl-tmux/options:set-option "mode-keys" "vi")
      (dolist (c (list (list "3j" 0 3 "3j must move the cursor down 3 rows")
                        (list "9j" 0 4 "9j must move the cursor to the viewport bottom")))
        (destructuring-bind (keys start-row expected-row desc) c
          (declare (ignore desc))
          (with-copy-mode-state (s screen input-state)
            (seed-scrollback screen 10)
            (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
                  (cons start-row 0))
            (loop for ch across keys
                  do (cl-tmux::process-byte s (char-code ch) input-state))
            (expect (= expected-row
                   (car (cl-tmux/terminal/types:screen-copy-cursor screen))))
            (expect (zerop cl-tmux::*copy-mode-prefix*)))))))

  ;; "12j" accumulates a two-digit prefix (1 then 2 -> 12) before dispatching.
  (it "copy-mode-numeric-prefix-multi-digit-accumulates"
    (with-isolated-config
      (cl-tmux/options:set-option "mode-keys" "vi")
      (with-copy-mode-state (s screen input-state)
        (seed-scrollback screen 20)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s (char-code #\1) input-state)
        (expect (= 1 cl-tmux::*copy-mode-prefix*))
        (cl-tmux::process-byte s (char-code #\2) input-state)
        (expect (= 12 cl-tmux::*copy-mode-prefix*))
        (cl-tmux::process-byte s (char-code #\j) input-state)
        (expect (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor screen))))
        (expect (zerop cl-tmux::*copy-mode-prefix*)))))

  ;; A bare '0' (no prior non-zero prefix digit) is the vi 'beginning of line'
  ;; command, not the start of a numeric prefix — matching tmux's vi convention.
  (it "copy-mode-bare-zero-goes-to-line-start-not-accumulated"
    (with-isolated-config
      (cl-tmux/options:set-option "mode-keys" "vi")
      (with-copy-mode-state (s screen input-state)
        (seed-scrollback screen 10)
        (expect (zerop cl-tmux::*copy-mode-prefix*))
        (cl-tmux::process-byte s (char-code #\0) input-state)
        (expect (zerop cl-tmux::*copy-mode-prefix*)))))

  ;; Once a non-zero digit has started a prefix, a following '0' DOES continue
  ;; the accumulation (vi convention: "10j" means repeat count 10).
  (it "copy-mode-zero-after-nonzero-prefix-is-accumulated"
    (with-isolated-config
      (cl-tmux/options:set-option "mode-keys" "vi")
      (with-copy-mode-state (s screen input-state)
        (seed-scrollback screen 20)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s (char-code #\1) input-state)
        (expect (= 1 cl-tmux::*copy-mode-prefix*))
        (cl-tmux::process-byte s (char-code #\0) input-state)
        (expect (= 10 cl-tmux::*copy-mode-prefix*))
        (cl-tmux::process-byte s (char-code #\j) input-state)
        (expect (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor screen))))))))
