(in-package #:cl-tmux/test)

;;;; events tests: prompt UTF-8 decoding, event-loop cycle, automatic rename.

(describe "events-suite"

  ;;; ── handle-prompt-key UTF-8 3-byte / 4-byte sequences ────────────────────────
  ;;;
  ;;; events-tests-c.lisp only exercises the 2-byte lead-byte branch (#xC3 #xA9).
  ;;; These tests cover the 3-byte and 4-byte branches of the same decode table,
  ;;; plus the invalid-codepoint fallback that silently drops an undecodable
  ;;; sequence instead of signalling.

  ;; A 3-byte UTF-8 sequence (U+20AC, the euro sign) fed byte-by-byte into
  ;; handle-prompt-key decodes and inserts the correct character.
  (it "handle-prompt-key-utf8-three-byte-sequence-inserts-char"
    (with-clean-prompt
      (prompt-start "test" ""
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; U+20AC in UTF-8: 0xE2 0x82 0xAC
      (cl-tmux::handle-prompt-key #xE2)
      (cl-tmux::handle-prompt-key #x82)
      (cl-tmux::handle-prompt-key #xAC)
      (expect (string= "€" (prompt-buffer *prompt*)))
      (expect (null cl-tmux::*prompt-utf8-continuation*))))

  ;; A 4-byte UTF-8 sequence (U+1F600, grinning face) fed byte-by-byte into
  ;; handle-prompt-key decodes and inserts the correct character.
  (it "handle-prompt-key-utf8-four-byte-sequence-inserts-char"
    (with-clean-prompt
      (prompt-start "test" ""
                    (lambda (buf) (declare (ignore buf)) nil))
      ;; U+1F600 in UTF-8: 0xF0 0x9F 0x98 0x80
      (cl-tmux::handle-prompt-key #xF0)
      (cl-tmux::handle-prompt-key #x9F)
      (cl-tmux::handle-prompt-key #x98)
      (cl-tmux::handle-prompt-key #x80)
      (expect (string= (string (code-char #x1F600)) (prompt-buffer *prompt*)))))

  ;; A structurally-valid but out-of-range 4-byte lead byte (0xF7) combined with
  ;; all-1s continuation bytes decodes to code point #x1FFFFF, which exceeds
  ;; CHAR-CODE-LIMIT; handle-prompt-key must swallow the error (via
  ;; IGNORE-ERRORS) and insert nothing rather than signal.
  (it "handle-prompt-key-utf8-invalid-codepoint-is-dropped-not-signalled"
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
      (expect (string= "" (prompt-buffer *prompt*)))
      (expect (null cl-tmux::*prompt-utf8-continuation*))))

  ;;; ── Event-loop per-iteration cycle ────────────────────────────────────────────
  ;;;
  ;;; event-loop itself blocks on *running*, so it cannot be unit-tested directly;
  ;;; %process-one-event-cycle and %read-and-dispatch-one-byte are the extracted,
  ;;; directly-callable building blocks that make one iteration's read/dispatch/
  ;;; idle-yield logic independently testable.  In the test sandbox stdin never
  ;;; has data ready, so read-byte-nonblock reliably returns NIL and the idle
  ;;; branch is exercised deterministically.

  ;; With no byte available, %read-and-dispatch-one-byte increments and returns
  ;; the idle counter unchanged in kind (still below the yield threshold).
  (it "read-and-dispatch-one-byte-idle-increments-counter"
    (with-fake-session (s)
      (with-input-state (input-state)
        (let ((next (cl-tmux::%read-and-dispatch-one-byte s input-state 0)))
          (expect (= 1 next))))))

  ;; Once the idle counter reaches +event-loop-max-idle-iterations+, the next
  ;; idle read yields (sleeps briefly) and resets the counter to 0.
  (it "read-and-dispatch-one-byte-idle-yields-and-resets-at-threshold"
    (with-fake-session (s)
      (with-input-state (input-state)
        (let ((next (cl-tmux::%read-and-dispatch-one-byte
                     s input-state
                     (1- cl-tmux::+event-loop-max-idle-iterations+))))
          (expect (zerop next))))))

  ;; %process-one-event-cycle resolves the current session, drains repeat/escape
  ;; timers, reads (no byte available in the test sandbox), and returns the next
  ;; idle-counter value without touching *running*.
  (it "process-one-event-cycle-runs-without-error-and-returns-idle-counter"
    (with-fake-session (s)
      (let ((state (cl-tmux::make-input-state))
            (cl-tmux::*running* t))
        (let ((next (cl-tmux::%process-one-event-cycle s state 0)))
          (expect (= 1 next))
          (expect cl-tmux::*running* :to-be-truthy)))))

  ;; EVENT-LOOP's LOOP WHILE *running* body never executes when *running* is
  ;; already NIL on entry, so the call returns at once instead of blocking —
  ;; the one deterministic way to exercise the top-level entry point directly
  ;; without a live byte stream.
  (it "event-loop-returns-immediately-when-not-running"
    (with-fake-session (s)
      (let ((cl-tmux::*running* nil))
        (finishes (cl-tmux::event-loop s)))))

  ;;; ── Automatic-rename helper decomposition ────────────────────────────────────

  ;; %automatic-rename-enabled-p is T only when both the window flag and the
  ;; per-window automatic-rename option are enabled.
  (it "automatic-rename-enabled-p-true-table"
    (with-fake-session (s :nwindows 1)
      (let ((window (session-active-window s)))
        (setf (window-automatic-rename-p window) t)
        (cl-tmux/options:set-option-for-window "automatic-rename" t window)
        (expect (cl-tmux::%automatic-rename-enabled-p window) :to-be-truthy))))

  ;; %automatic-rename-enabled-p is NIL when the window's automatic-rename-p
  ;; struct flag is off, regardless of the option value.
  (it "automatic-rename-enabled-p-false-when-struct-flag-off"
    (with-fake-session (s :nwindows 1)
      (let ((window (session-active-window s)))
        (setf (window-automatic-rename-p window) nil)
        (expect (cl-tmux::%automatic-rename-enabled-p window) :to-be-falsy)))))
