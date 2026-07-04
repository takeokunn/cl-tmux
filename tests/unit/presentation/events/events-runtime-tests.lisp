(in-package #:cl-tmux/test)

;;;; events tests: prompt UTF-8 decoding, event-loop cycle, automatic rename.

(in-suite events-suite)

;;; ── handle-prompt-key UTF-8 3-byte / 4-byte sequences ────────────────────────
;;;
;;; events-tests-c.lisp only exercises the 2-byte lead-byte branch (#xC3 #xA9).
;;; These tests cover the 3-byte and 4-byte branches of the same decode table,
;;; plus the invalid-codepoint fallback that silently drops an undecodable
;;; sequence instead of signalling.

(test handle-prompt-key-utf8-three-byte-sequence-inserts-char
  "A 3-byte UTF-8 sequence (U+20AC, the euro sign) fed byte-by-byte into
   handle-prompt-key decodes and inserts the correct character."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; U+20AC in UTF-8: 0xE2 0x82 0xAC
    (cl-tmux::handle-prompt-key #xE2)
    (cl-tmux::handle-prompt-key #x82)
    (cl-tmux::handle-prompt-key #xAC)
    (is (string= "€" (prompt-buffer *prompt*))
        "3-byte UTF-8 sequence must decode and insert € into prompt")
    (is (null cl-tmux::*prompt-utf8-continuation*)
        "UTF-8 decode continuation must return to ground state (NIL) once the sequence completes")))

(test handle-prompt-key-utf8-four-byte-sequence-inserts-char
  "A 4-byte UTF-8 sequence (U+1F600, grinning face) fed byte-by-byte into
   handle-prompt-key decodes and inserts the correct character."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; U+1F600 in UTF-8: 0xF0 0x9F 0x98 0x80
    (cl-tmux::handle-prompt-key #xF0)
    (cl-tmux::handle-prompt-key #x9F)
    (cl-tmux::handle-prompt-key #x98)
    (cl-tmux::handle-prompt-key #x80)
    (is (string= (string (code-char #x1F600)) (prompt-buffer *prompt*))
        "4-byte UTF-8 sequence must decode and insert U+1F600 into prompt")))

(test handle-prompt-key-utf8-invalid-codepoint-is-dropped-not-signalled
  "A structurally-valid but out-of-range 4-byte lead byte (0xF7) combined with
   all-1s continuation bytes decodes to code point #x1FFFFF, which exceeds
   CHAR-CODE-LIMIT; handle-prompt-key must swallow the error (via
   IGNORE-ERRORS) and insert nothing rather than signal."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; 0xF7 is a 4-byte lead byte (mask #x07, all payload bits set) and the
    ;; three continuation bytes 0xBF also set all 6 payload bits, so the
    ;; accumulated code point is #x1FFFFF (2 097 151) — well beyond both the
    ;; Unicode maximum (#x10FFFF) and SBCL's CHAR-CODE-LIMIT, so CODE-CHAR
    ;; signals and IGNORE-ERRORS returns NIL.
    (finishes
      (progn
        (cl-tmux::handle-prompt-key #xF7)
        (cl-tmux::handle-prompt-key #xBF)
        (cl-tmux::handle-prompt-key #xBF)
        (cl-tmux::handle-prompt-key #xBF)))
    (is (string= "" (prompt-buffer *prompt*))
        "an undecodable code point must not be inserted into the prompt buffer")
    (is (null cl-tmux::*prompt-utf8-continuation*)
        "UTF-8 decode continuation must still return to ground state (NIL) after an invalid codepoint")))

;;; ── Event-loop per-iteration cycle ────────────────────────────────────────────
;;;
;;; event-loop itself blocks on *running*, so it cannot be unit-tested directly;
;;; %process-one-event-cycle and %read-and-dispatch-one-byte are the extracted,
;;; directly-callable building blocks that make one iteration's read/dispatch/
;;; idle-yield logic independently testable.  In the test sandbox stdin never
;;; has data ready, so read-byte-nonblock reliably returns NIL and the idle
;;; branch is exercised deterministically.

(test read-and-dispatch-one-byte-idle-increments-counter
  "With no byte available, %read-and-dispatch-one-byte increments and returns
   the idle counter unchanged in kind (still below the yield threshold)."
  (with-fake-session (s)
    (with-input-state (input-state)
      (let ((next (cl-tmux::%read-and-dispatch-one-byte s input-state 0)))
        (is (= 1 next)
            "idle counter must increment by 1 when no byte is available")))))

(test read-and-dispatch-one-byte-idle-yields-and-resets-at-threshold
  "Once the idle counter reaches +event-loop-max-idle-iterations+, the next
   idle read yields (sleeps briefly) and resets the counter to 0."
  (with-fake-session (s)
    (with-input-state (input-state)
      (let ((next (cl-tmux::%read-and-dispatch-one-byte
                   s input-state
                   (1- cl-tmux::+event-loop-max-idle-iterations+))))
        (is (zerop next)
            "idle counter must reset to 0 once the max-idle-iterations bound is hit")))))

(test process-one-event-cycle-runs-without-error-and-returns-idle-counter
  "%process-one-event-cycle resolves the current session, drains repeat/escape
   timers, reads (no byte available in the test sandbox), and returns the next
   idle-counter value without touching *running*."
  (with-fake-session (s)
    (let ((state (cl-tmux::make-input-state))
          (cl-tmux::*running* t))
      (let ((next (cl-tmux::%process-one-event-cycle s state 0)))
        (is (= 1 next)
            "one idle cycle with no input must advance the idle counter by 1")
        (is-true cl-tmux::*running*
            "%process-one-event-cycle must not itself stop the loop on idle input")))))

(test event-loop-returns-immediately-when-not-running
  "EVENT-LOOP's LOOP WHILE *running* body never executes when *running* is
   already NIL on entry, so the call returns at once instead of blocking —
   the one deterministic way to exercise the top-level entry point directly
   without a live byte stream."
  (with-fake-session (s)
    (let ((cl-tmux::*running* nil))
      (finishes (cl-tmux::event-loop s)))))

;;; ── Automatic-rename helper decomposition ────────────────────────────────────

(test automatic-rename-enabled-p-true-table
  "%automatic-rename-enabled-p is T only when both the window flag and the
   per-window automatic-rename option are enabled."
  (with-fake-session (s :nwindows 1)
    (let ((window (session-active-window s)))
      (setf (window-automatic-rename-p window) t)
      (cl-tmux/options:set-option-for-window "automatic-rename" t window)
      (is-true (cl-tmux::%automatic-rename-enabled-p window)
          "both flag and option enabled must report T"))))

(test automatic-rename-enabled-p-false-when-struct-flag-off
  "%automatic-rename-enabled-p is NIL when the window's automatic-rename-p
   struct flag is off, regardless of the option value."
  (with-fake-session (s :nwindows 1)
    (let ((window (session-active-window s)))
      (setf (window-automatic-rename-p window) nil)
      (is-false (cl-tmux::%automatic-rename-enabled-p window)
          "struct flag off must short-circuit to NIL"))))
