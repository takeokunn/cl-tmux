(in-package #:cl-tmux/test)

;;;; events tests: copy-mode numeric-prefix repeat counts.

(in-suite events-suite)

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

(test copy-mode-numeric-prefix-repeats-scroll-table
  "A numeric prefix (e.g. \"3j\") repeats the following navigation command that
   many times; digits 1-9 always start/continue accumulation."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (dolist (c (list (list "3j" 0 3 "3j must move the cursor down 3 rows")
                      (list "9j" 0 4 "9j must move the cursor to the viewport bottom")))
      (destructuring-bind (keys start-row expected-row desc) c
        (with-copy-mode-state (s screen input-state)
          (seed-scrollback screen 10)
          (setf (cl-tmux/terminal/types:screen-copy-cursor screen)
                (cons start-row 0))
          (loop for ch across keys
                do (cl-tmux::process-byte s (char-code ch) input-state))
          (is (= expected-row
                 (car (cl-tmux/terminal/types:screen-copy-cursor screen)))
              "~A" desc)
          (is (zerop cl-tmux::*copy-mode-prefix*)
              "prefix must reset to 0 after the command applies"))))))

(test copy-mode-numeric-prefix-multi-digit-accumulates
  "\"12j\" accumulates a two-digit prefix (1 then 2 -> 12) before dispatching."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 20)
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
      (cl-tmux::process-byte s (char-code #\1) input-state)
      (is (= 1 cl-tmux::*copy-mode-prefix*)
          "first digit '1' must be accumulated, not yet dispatched")
      (cl-tmux::process-byte s (char-code #\2) input-state)
      (is (= 12 cl-tmux::*copy-mode-prefix*)
          "second digit '2' folds in: 1*10+2 = 12")
      (cl-tmux::process-byte s (char-code #\j) input-state)
      (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor screen)))
          "12j must move the cursor to the viewport bottom")
      (is (zerop cl-tmux::*copy-mode-prefix*) "prefix resets after dispatch"))))

(test copy-mode-bare-zero-goes-to-line-start-not-accumulated
  "A bare '0' (no prior non-zero prefix digit) is the vi 'beginning of line'
   command, not the start of a numeric prefix — matching tmux's vi convention."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 10)
      (is (zerop cl-tmux::*copy-mode-prefix*))
      (cl-tmux::process-byte s (char-code #\0) input-state)
      (is (zerop cl-tmux::*copy-mode-prefix*)
          "bare 0 must not be accumulated as a prefix digit"))))

(test copy-mode-zero-after-nonzero-prefix-is-accumulated
  "Once a non-zero digit has started a prefix, a following '0' DOES continue
   the accumulation (vi convention: \"10j\" means repeat count 10)."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "vi")
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 20)
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
      (cl-tmux::process-byte s (char-code #\1) input-state)
      (is (= 1 cl-tmux::*copy-mode-prefix*))
      (cl-tmux::process-byte s (char-code #\0) input-state)
      (is (= 10 cl-tmux::*copy-mode-prefix*)
          "'0' after a non-zero prefix digit continues accumulation: 1*10+0 = 10")
      (cl-tmux::process-byte s (char-code #\j) input-state)
      (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor screen)))
          "10j must move the cursor to the viewport bottom"))))
