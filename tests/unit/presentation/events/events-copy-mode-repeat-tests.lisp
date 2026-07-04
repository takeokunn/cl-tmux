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
;;; Every test here pins mode-keys to "emacs" via WITH-ISOLATED-CONFIG: the
;;; emacs copy-mode key table has no entry for 'j'/'k', so those bytes fall
;;; through to the hardcoded %dispatch-copy-mode-byte :repeat path that
;;; actually honours COUNT.  (The copy-mode-vi table binds j/k directly to
;;; :copy-mode-cursor-down/-up, which %run-key-table-binding dispatches
;;; exactly once regardless of any numeric prefix — pinning the mode keeps
;;; this test deterministic regardless of what mode-keys another suite left
;;; installed.)

(test copy-mode-numeric-prefix-repeats-scroll-table
  "A numeric prefix (e.g. \"3j\") repeats the following navigation command that
   many times; digits 1-9 always start/continue accumulation."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "emacs")
    (dolist (c (list (list "3j" 5 2 "3j must scroll down 3 lines (5 -> 2)")
                      (list "9j" 9 0 "9j must clamp at the scrollback bound (9 -> 0)")))
      (destructuring-bind (keys start-offset expected-offset desc) c
        (with-copy-mode-state (s screen input-state)
          (seed-scrollback screen 10)
          (cl-tmux/commands::copy-mode-scroll screen start-offset)
          (is (= start-offset (screen-copy-offset screen)))
          (loop for ch across keys
                do (cl-tmux::process-byte s (char-code ch) input-state))
          (is (= expected-offset (screen-copy-offset screen)) "~A" desc)
          (is (zerop cl-tmux::*copy-mode-prefix*)
              "prefix must reset to 0 after the command applies"))))))

(test copy-mode-numeric-prefix-multi-digit-accumulates
  "\"12j\" accumulates a two-digit prefix (1 then 2 -> 12) before dispatching."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "emacs")
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 20)
      (cl-tmux/commands::copy-mode-scroll screen 15)
      (is (= 15 (screen-copy-offset screen)))
      (cl-tmux::process-byte s (char-code #\1) input-state)
      (is (= 1 cl-tmux::*copy-mode-prefix*)
          "first digit '1' must be accumulated, not yet dispatched")
      (cl-tmux::process-byte s (char-code #\2) input-state)
      (is (= 12 cl-tmux::*copy-mode-prefix*)
          "second digit '2' folds in: 1*10+2 = 12")
      (cl-tmux::process-byte s (char-code #\j) input-state)
      (is (= 3 (screen-copy-offset screen))
          "12j must scroll down 12 lines (15 -> 3)")
      (is (zerop cl-tmux::*copy-mode-prefix*) "prefix resets after dispatch"))))

(test copy-mode-bare-zero-goes-to-line-start-not-accumulated
  "A bare '0' (no prior non-zero prefix digit) is the vi 'beginning of line'
   command, not the start of a numeric prefix — matching tmux's vi convention."
  (with-isolated-config
    (cl-tmux/options:set-option "mode-keys" "emacs")
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
    (cl-tmux/options:set-option "mode-keys" "emacs")
    (with-copy-mode-state (s screen input-state)
      (seed-scrollback screen 20)
      (cl-tmux/commands::copy-mode-scroll screen 15)
      (cl-tmux::process-byte s (char-code #\1) input-state)
      (is (= 1 cl-tmux::*copy-mode-prefix*))
      (cl-tmux::process-byte s (char-code #\0) input-state)
      (is (= 10 cl-tmux::*copy-mode-prefix*)
          "'0' after a non-zero prefix digit continues accumulation: 1*10+0 = 10")
      (cl-tmux::process-byte s (char-code #\j) input-state)
      (is (= 5 (screen-copy-offset screen))
          "10j must scroll down 10 lines (15 -> 5)"))))
