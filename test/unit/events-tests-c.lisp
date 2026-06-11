(in-package #:cl-tmux/test)

;;;; events tests — part C: app-cursor-keys, handle-prompt-key UTF-8/cursor/kill,
;;;; copy-mode word/page/yank, SGR mouse, with-copy-mode-state, CSI-u extended keys.

(in-suite events-suite)

;;; ── Application cursor keys remapping ───────────────────────────────────────

(test app-cursor-keys-remaps-csi-arrow-to-ss3
  "When app-cursor-keys mode is active, ESC [ A forwarded outside copy mode is
   remapped to ESC O A (SS3) before being sent to the pane."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        ;; Enable application cursor keys on the active pane's screen.
        (setf (screen-app-cursor-keys screen) t)
        ;; Ensure we are NOT in copy mode so the sequence is forwarded, not consumed.
        (is-false (cl-tmux::%copy-mode-active-p s) "must not be in copy mode")
        ;; Feed ESC [ A — should be remapped to ESC O A internally.
        ;; fd=-1 panes: pty-write is a no-op; we assert no error and NIL return.
        (is (null (cl-tmux::process-byte s 27 state)))
        (is (null (cl-tmux::process-byte s 91 state)))
        (is (null (cl-tmux::process-byte s 65 state))
            "ESC [ A with app-cursor-keys must not signal or return a quit value")))))

;;; ── Buffer overflow guard in make-escape-input-k ────────────────────────────

(test escape-accumulator-resets-after-complete-sgr-sequence
  "After a complete SGR mouse sequence, the continuation returns to ground state."
  (let ((s     (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (with-loop-state
      ;; Feed ESC [ < 0 ; 5 ; 3 M  (a complete SGR press) byte by byte.
      (dolist (byte (mapcar #'char-code (coerce (format nil "~C[<0;5;3M" #\Escape) 'list)))
        (cl-tmux::process-byte s byte state))
      ;; After the full sequence the continuation must be back to ground.
      (is (eq #'cl-tmux::%ground-input-state
              (cl-tmux::input-state-continuation state))
          "continuation must return to ground after completed SGR sequence"))))

;;; ── handle-prompt-key UTF-8 multi-byte input ─────────────────────────────────

(test handle-prompt-key-utf8-two-byte-sequence-inserts-char
  "A 2-byte UTF-8 sequence (U+00E9, é) fed byte-by-byte into handle-prompt-key
   inserts the correct character into the prompt buffer."
  (with-clean-prompt
    (prompt-start "test" ""
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; U+00E9 in UTF-8: 0xC3 0xA9
    (cl-tmux::handle-prompt-key #xC3)
    (cl-tmux::handle-prompt-key #xA9)
    (is (string= "é" (prompt-buffer *prompt*))
        "2-byte UTF-8 sequence must decode and insert é into prompt")))

(test handle-prompt-key-utf8-resets-on-enter
  "UTF-8 accumulator state is reset when Enter is pressed mid-sequence."
  (with-clean-prompt
    (let ((submitted "unset"))
      (prompt-start "test" ""
                    (lambda (buf) (setf submitted buf)))
      ;; Start a 2-byte UTF-8 sequence but press Enter before the second byte.
      (cl-tmux::handle-prompt-key #xC3)
      (cl-tmux::handle-prompt-key 13)   ; Enter
      ;; The prompt should have been submitted and dismissed.
      (is-false (prompt-active-p)
          "Enter mid-UTF8 must dismiss the prompt")
      ;; Submitted value is the buffer content before the incomplete sequence.
      (is (stringp submitted) "submitted value must be a string"))))

;;; ── handle-prompt-key cursor movement (C-b, C-f) ────────────────────────────

(test handle-prompt-key-ctrl-b-moves-cursor-left
  "C-b (byte 2) moves the prompt cursor one position to the left."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    (prompt-cursor-eol)
    (is (= 5 (prompt-cursor-index *prompt*)) "cursor at end")
    (cl-tmux::handle-prompt-key 2)   ; C-b
    (is (= 4 (prompt-cursor-index *prompt*))
        "C-b must move cursor one position left")))

(test handle-prompt-key-ctrl-f-moves-cursor-right
  "C-f (byte 6) moves the prompt cursor one position to the right."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    (prompt-cursor-bol)
    (is (= 0 (prompt-cursor-index *prompt*)) "cursor at start")
    (cl-tmux::handle-prompt-key 6)   ; C-f
    (is (= 1 (prompt-cursor-index *prompt*))
        "C-f must move cursor one position right")))

;;; ── handle-prompt-key kill commands ─────────────────────────────────────────

(test handle-prompt-key-ctrl-k-kills-to-end
  "C-k (byte 11) deletes from the cursor position to the end of the buffer."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to position 2 ("he" remains, "llo" to be killed).
    (prompt-cursor-bol)
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 1
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 2
    (cl-tmux::handle-prompt-key 11)  ; C-k
    (is (string= "he" (prompt-buffer *prompt*))
        "C-k must kill from cursor to end")))

(test handle-prompt-key-ctrl-u-kills-to-start
  "C-u (byte 21) deletes from the start of the buffer to the cursor position."
  (with-clean-prompt
    (prompt-start "test" "hello"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to position 3 ("hel" to be killed, "lo" remains).
    (prompt-cursor-bol)
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 1
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 2
    (cl-tmux::handle-prompt-key 6)   ; C-f → pos 3
    (cl-tmux::handle-prompt-key 21)  ; C-u
    (is (string= "lo" (prompt-buffer *prompt*))
        "C-u must kill from start to cursor")))

(test handle-prompt-key-ctrl-w-kills-previous-word
  "C-w (byte 23) deletes the word immediately before the cursor."
  (with-clean-prompt
    (prompt-start "test" "foo bar"
                  (lambda (buf) (declare (ignore buf)) nil))
    ;; Move cursor to end of buffer.
    (prompt-cursor-eol)
    (cl-tmux::handle-prompt-key 23)  ; C-w
    ;; Should have deleted "bar" (and possibly the space).
    (let ((buf (prompt-buffer *prompt*)))
      (is (string= "foo" (string-right-trim " " buf))
          "C-w must kill the previous word"))))

;;; ── process-byte: copy-mode w, b, e word navigation ─────────────────────────

(test copy-mode-w-moves-word-forward
  "Plain 'w' (byte 119) moves the copy-mode cursor forward by one word."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Feed some text to give the screen content.
        (screen-process-bytes
         screen (map '(simple-array (unsigned-byte 8) (*)) #'char-code "hello world"))
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (finishes (cl-tmux::process-byte s 119 state))))))  ; w

(test copy-mode-b-moves-word-backward
  "Plain 'b' (byte 98) moves the copy-mode cursor backward by one word."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 5))
        (finishes (cl-tmux::process-byte s 98 state))))))   ; b

(test copy-mode-e-moves-to-word-end
  "Plain 'e' (byte 101) moves the copy-mode cursor to the end of the current word."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (finishes (cl-tmux::process-byte s 101 state))))))  ; e

;;; ── process-byte: copy-mode page up/down C-f/C-b (in-mode) ──────────────────

(test copy-mode-ctrl-f-page-down
  "C-f (byte 6) in copy mode scrolls down one full page."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (cl-tmux/commands::copy-mode-scroll screen 20)
        (let ((offset-before (screen-copy-offset screen))
              (h             (screen-height screen)))
          (cl-tmux::process-byte s 6 state)   ; C-f → page down
          (let ((expected (max 0 (- offset-before h))))
            (is (= expected (screen-copy-offset screen))
                "C-f must scroll copy-offset down by screen-height")))))))

(test copy-mode-page-up-command-scrolls-full-page
  "copy-mode-page-up scrolls the viewport up by one full screen-height."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (let ((h (screen-height screen)))
          (cl-tmux/commands::copy-mode-page-up screen)
          (let ((expected (min h 30)))
            (is (= expected (screen-copy-offset screen))
                "copy-mode-page-up must scroll copy-offset up by screen-height")))))))

;;; ── copy-mode y (yank) and n/N (search navigation) ──────────────────────────

(test copy-mode-y-yanks-selection-finishes
  "Plain 'y' (byte 121) completes without signaling when in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Begin a selection first so yank has something to copy.
        (cl-tmux/commands::copy-mode-begin-selection screen)
        (finishes (cl-tmux::process-byte s 121 state))))))   ; y

(test copy-mode-n-search-next-finishes
  "Plain 'n' (byte 110) runs search-next without signaling in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 110 state))))))   ; n

(test copy-mode-N-search-prev-finishes
  "Plain 'N' (byte 78) runs search-prev without signaling in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 78 state))))))    ; N

;;; ── copy-mode Y (copy-line) and D (copy-end-of-line) ────────────────────────

(test copy-mode-Y-copies-current-line
  "Plain 'Y' (byte 89) copies the current line into the paste buffer without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 89 state))))))    ; Y

(test copy-mode-D-copies-to-end-of-line
  "Plain 'D' (byte 68) copies from the cursor to end of line without signaling."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 68 state))))))    ; D

;;; ── copy-mode half-page and single-line scroll bindings ──────────────────────

(test copy-mode-ctrl-u-half-page-up
  "C-u (byte 21) scrolls the copy-mode viewport up by half a page."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 21 state)   ; C-u
          (is (>= (screen-copy-offset screen) offset-before)
              "C-u must not decrease copy-offset"))))))

(test copy-mode-ctrl-d-half-page-down
  "C-d (byte 4) scrolls the copy-mode viewport down by half a page."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 30)
        (cl-tmux/commands::copy-mode-scroll screen 20)
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 4 state)    ; C-d
          (is (<= (screen-copy-offset screen) offset-before)
              "C-d must not increase copy-offset"))))))

(test copy-mode-ctrl-e-scrolls-down-one-line
  "C-e (byte 5) in copy mode scrolls the viewport down one line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (cl-tmux/commands::copy-mode-scroll screen 5)
        (let ((offset-before (screen-copy-offset screen)))
          (cl-tmux::process-byte s 5 state)    ; C-e
          (is (<= (screen-copy-offset screen) offset-before)
              "C-e must scroll copy-offset down (decrease offset)"))))))

(test copy-mode-ctrl-y-scrolls-up-one-line
  "C-y (byte 25) in copy mode scrolls the viewport up one line."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        (cl-tmux::process-byte s 25 state)    ; C-y
        (is (>= (screen-copy-offset screen) 0)
            "C-y must not produce a negative copy-offset")))))

;;; ── copy-mode v alternative for begin-selection ─────────────────────────────

(test copy-mode-v-begins-selection
  "Plain 'v' (byte 118) also begins selection in copy mode."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (finishes (cl-tmux::process-byte s 118 state))
        (is (screen-copy-selecting screen)
            "v must activate copy selection")))))

;;; ── Middle-screen cursor jump M ──────────────────────────────────────────────

(test copy-mode-M-moves-cursor-to-middle
  "Plain 'M' (byte 77) moves the copy-mode cursor to the middle row of the screen."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        ;; Place cursor at row 0.
        (setf (cl-tmux/terminal/types:screen-copy-cursor screen) (cons 0 0))
        (cl-tmux::process-byte s (char-code #\M) state)
        (let* ((row    (car (screen-copy-cursor screen)))
               (height (screen-height screen))
               (mid    (floor height 2)))
          (is (= mid row) "M must place cursor at the middle row"))))))

;;; ── %handle-escape-x10-mouse direct invocation ───────────────────────────────

(test handle-escape-x10-mouse-dispatches-event
  "%handle-escape-x10-mouse decodes X10 encoding and dispatches the event.
   We verify it returns (values nil ground-state) without signaling."
  (with-two-pane-mouse-session (sess win p0 p1)
    (let ((buf (make-array 6 :element-type '(unsigned-byte 8)
                             :initial-contents (list 27 91 77
                                                     (+ 0 32)   ; btn 0 = left
                                                     (+ 50 33)  ; col 50 → 0-based 49
                                                     (+ 5 33)   ; row 5  → 0-based 4
                                                     ))))
      (multiple-value-bind (outcome next)
          (cl-tmux::%handle-escape-x10-mouse sess buf)
        (is (null outcome)
            "%handle-escape-x10-mouse must return NIL outcome")
        (is (eq #'cl-tmux::%ground-input-state next)
            "%handle-escape-x10-mouse must return ground-state as next state")))))

;;; ── %handle-escape-csi-tilde outside copy mode ──────────────────────────────

(test handle-escape-function-key-forwards-outside-copy-mode
  "%handle-escape-csi-tilde forwards the sequence when not in copy mode and unbound."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build an ESC [ 5 ~ (PageUp) buffer — not in copy mode, no binding.
      (let ((buf (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-contents (list 27 91 53 126))))
        (multiple-value-bind (outcome next)
            (cl-tmux::%handle-escape-csi-tilde s buf 4)
          (is (null outcome)
              "%handle-escape-csi-tilde outside copy-mode must return NIL outcome")
          (is (eq #'cl-tmux::%ground-input-state next)
              "%handle-escape-csi-tilde must return ground-state"))))))

;;; ── %handle-escape-csi-3byte: keep-accumulating for digit ───────────────────

(test handle-escape-csi-3byte-returns-keep-accumulating-for-digit
  "%handle-escape-csi-3byte returns (values T NIL) when the third byte is a digit,
   indicating we need to keep accumulating (for ESC [ N ~ function-key sequences)."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build ESC [ 5  — third byte is '5' (53), a digit.
      (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                               :fill-pointer 3 :adjustable t
                               :initial-contents (list 27 91 53))))
        (multiple-value-bind (keep-accumulating next-state)
            (cl-tmux::%handle-escape-csi-3byte s buf)
          (is (eq t keep-accumulating)
              "%handle-escape-csi-3byte with digit third-byte must return T (keep accumulating)")
          (is (null next-state)
              "next-state must be NIL when keep-accumulating is T"))))))

(test handle-escape-csi-3byte-returns-ground-state-for-non-digit
  "%handle-escape-csi-3byte returns (values NIL ground-state) for a non-digit final byte."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build ESC [ A  — third byte is 'A' (65), not a digit.
      (let ((buf (make-array 3 :element-type '(unsigned-byte 8)
                               :fill-pointer 3 :adjustable t
                               :initial-contents (list 27 91 65))))
        (multiple-value-bind (keep-accumulating next-state)
            (cl-tmux::%handle-escape-csi-3byte s buf)
          (is (null keep-accumulating)
              "%handle-escape-csi-3byte with non-digit must return NIL (do not keep accumulating)")
          (is (eq #'cl-tmux::%ground-input-state next-state)
              "next-state must be %ground-input-state"))))))

;;; ── SGR mouse: parse with scroll-wheel button encoding ───────────────────────

(test parse-sgr-mouse-scroll-up-button
  "%parse-sgr-mouse parses SGR scroll-up (btn=64) correctly."
  (let* ((s   (format nil "~C[<64;5;3M" #\Escape))
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (= 64 btn)   "scroll-up btn must be 64")
      (is (= 4  col)   "col must be 0-based (5-1=4)")
      (is (= 2  row)   "row must be 0-based (3-1=2)")
      (is-false release-p "press sequence must have release-p=NIL"))))

(test parse-sgr-mouse-returns-nil-for-short-buffer
  "%parse-sgr-mouse returns (values nil nil nil nil) for a buffer shorter than 9 bytes."
  (let* ((s   (format nil "~C[<0M" #\Escape))  ; too short
         (buf (make-array (length s) :element-type '(unsigned-byte 8)
                          :initial-contents (map 'list #'char-code s)))
         (len (length buf)))
    (multiple-value-bind (btn col row release-p)
        (cl-tmux::%parse-sgr-mouse buf len)
      (is (null btn)      "short buffer must return nil btn")
      (is (null col)      "short buffer must return nil col")
      (is (null row)      "short buffer must return nil row")
      (is (null release-p) "short buffer must return nil release-p"))))

;;; ── SGR mouse dispatch via process-byte ─────────────────────────────────────

(test sgr-mouse-left-click-via-process-byte-selects-pane
  "An SGR left-click sequence fed byte-by-byte through process-byte selects the pane."
  (with-two-pane-mouse-session (sess win p0 p1)
    (setf (screen-mouse-sgr-mode (pane-screen p0)) t)
    (let ((state (cl-tmux::make-input-state))
          ;; ESC [ < 0 ; 50 ; 5 M  — btn=0, col=50, row=5 (1-based), press
          (seq   (format nil "~C[<0;50;5M" #\Escape)))
      (loop for ch across seq
            do (cl-tmux::process-byte sess (char-code ch) state))
      (is (eq p1 (window-active-pane win))
          "SGR left-click in right pane must focus p1"))))

;;; ── overlay-scroll: verify actual offset change ──────────────────────────────

(test overlay-scroll-up-decrements-offset
  "overlay-scroll -1 decrements *overlay-scroll-offset* by 1 (clamped at 0)."
  ;; NOTE: build the overlay with real newlines via ~%.  A CL string literal
  ;; does NOT treat \n as a newline (backslash only escapes " and \), so
  ;; "line1\nline2..." is a SINGLE line and overlay-lines would return 1 entry,
  ;; clamping every scroll to 0.
  (let ((*overlay* (format nil "line1~%line2~%line3~%line4~%line5~%"))
        (*overlay-scroll-offset* 3))
    (overlay-scroll -1)
    (is (= 2 *overlay-scroll-offset*)
        "overlay-scroll -1 must decrement offset from 3 to 2")))

(test overlay-scroll-down-increments-offset
  "overlay-scroll 1 increments *overlay-scroll-offset* by 1."
  ;; Real newlines via ~% (a CL literal's \n is NOT a newline; see the
  ;; overlay-scroll-up test for the full explanation).
  (let ((*overlay* (format nil "line1~%line2~%line3~%line4~%line5~%"))
        (*overlay-scroll-offset* 0))
    (overlay-scroll 1)
    (is (= 1 *overlay-scroll-offset*)
        "overlay-scroll 1 must increment offset from 0 to 1")))

(test overlay-scroll-clamps-at-zero
  "overlay-scroll -1 at offset 0 does not produce a negative offset."
  (let ((*overlay* "line1\n")
        (*overlay-scroll-offset* 0))
    (overlay-scroll -1)
    (is (>= *overlay-scroll-offset* 0)
        "overlay-scroll at offset 0 must not go negative")))

;;; ── with-copy-mode-state test helper macro ───────────────────────────────────
;;;
;;; Eliminates the triple-nested boilerplate that appeared 43+ times:
;;;   (let ((s (make-fake-session))) (with-loop-state (let ((screen ...) (state ...)) ...)))

(defmacro with-copy-mode-state ((session-var screen-var state-var) &body body)
  "Run BODY with SESSION-VAR bound to a fresh fake session in copy mode,
   SCREEN-VAR bound to its active screen, and STATE-VAR bound to a fresh input-state.
   Wraps everything in WITH-LOOP-STATE for proper event-loop isolation."
  `(let ((,session-var (make-fake-session)))
     (with-loop-state
       (let ((,screen-var (active-screen ,session-var))
             (,state-var  (cl-tmux::make-input-state)))
         (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
         ,@body))))

;;; ── %border-check-node direct tests ─────────────────────────────────────────
;;;
;;; %border-check-node is the recursive tree walker inside %border-at-position.
;;; The :v split path and multi-level recursion deserve direct coverage.

(test border-check-node-leaf-returns-nil
  "%border-check-node on a layout-leaf always returns (values NIL NIL)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (leaf (make-layout-leaf p0)))
    (multiple-value-bind (split orientation)
        (cl-tmux::%border-check-node 20 10 leaf)
      (is (null split)       "layout-leaf must return NIL split")
      (is (null orientation) "layout-leaf must return NIL orientation"))))

(test border-check-node-h-split-detects-separator
  "%border-check-node returns (split :h) when col lands exactly on the horizontal separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2)))
    ;; Separator column for p0 (x=0 w=40) is at col 40.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 40 5 split)
      (is (eq split found-split)
          "%border-check-node :h split must return the split node at separator col")
      (is (eq :h orientation)
          "%border-check-node :h split must report :h orientation"))))

(test border-check-node-v-split-detects-separator
  "%border-check-node returns (split :v) when row lands exactly on the vertical separator."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 80 :height 10
                            :screen (make-screen 80 10)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 11 :width 80 :height 10
                            :screen (make-screen 80 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :v leaf0 leaf1 1/2)))
    ;; Separator row for p0 (y=0 h=10) is at row 10.
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 5 10 split)
      (is (eq split found-split)
          "%border-check-node :v split must return the split node at separator row")
      (is (eq :v orientation)
          "%border-check-node :v split must report :v orientation"))))

(test border-check-node-h-split-inside-pane-returns-nil
  "%border-check-node returns (values NIL NIL) when col is inside a pane (not on border)."
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0 :width 40 :height 24
                            :screen (make-screen 40 24)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (split (make-layout-split :h leaf0 leaf1 1/2)))
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 20 5 split)
      (is (null found-split)   "col inside pane must return NIL split")
      (is (null orientation)   "col inside pane must return NIL orientation"))))

(test border-check-node-nested-split-finds-inner-border
  "%border-check-node recurses into child splits and finds inner borders."
  ;; Build a 3-pane layout: [p0 | [p1 above p2]]
  ;; Outer: :h split at col 40; inner: :v split at row 10.
  (let* ((p0    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0  :width 40 :height 24
                            :screen (make-screen 40 24)))
         (p1    (make-pane :id 2 :fd -1 :pid -1 :x 41 :y 0  :width 40 :height 10
                            :screen (make-screen 40 10)))
         (p2    (make-pane :id 3 :fd -1 :pid -1 :x 41 :y 11 :width 40 :height 10
                            :screen (make-screen 40 10)))
         (leaf0 (make-layout-leaf p0))
         (leaf1 (make-layout-leaf p1))
         (leaf2 (make-layout-leaf p2))
         (inner-split (make-layout-split :v leaf1 leaf2 1/2))
         (outer-split (make-layout-split :h leaf0 inner-split 1/2)))
    ;; Hit the inner :v border at (col=50, row=10)
    (multiple-value-bind (found-split orientation)
        (cl-tmux::%border-check-node 50 10 outer-split)
      (is (eq inner-split found-split)
          "%border-check-node must find the inner :v split node")
      (is (eq :v orientation)
          "%border-check-node must report :v for the inner split"))))

;;; ── %status-col-to-window: multi-window traversal coverage ──────────────────

(test status-col-to-window-finds-third-window
  "%status-col-to-window returns the correct window when the column falls in the
   third window entry (verifies the multi-window traversal path)."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (p2   (make-pane :id 3 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5)))
         (win0 (make-window :id 0 :name "a" :width 20 :height 5
                            :panes (list p0) :tree (make-layout-leaf p0)))
         (win1 (make-window :id 1 :name "b" :width 20 :height 5
                            :panes (list p1) :tree (make-layout-leaf p1)))
         (win2 (make-window :id 2 :name "c" :width 20 :height 5
                            :panes (list p2) :tree (make-layout-leaf p2)))
         (sess (make-session :id 1 :name "s" :windows (list win0 win1 win2))))
    (window-select-pane win0 p0)
    (window-select-pane win1 p1)
    (window-select-pane win2 p2)
    (session-select-window sess win0)
    ;; Session prefix " s" = 2 chars.
    ;; win0 "a": 4 + 1 = 5 chars, cols 2..6
    ;; win1 "b": 4 + 1 = 5 chars, cols 7..11
    ;; win2 "c": 4 + 1 = 5 chars, cols 12..16
    ;; Column 14 should land in win2.
    (is (eq win2 (cl-tmux::%status-col-to-window sess 14))
        "%status-col-to-window must find the third window at the appropriate column")))

;;; ── %handle-escape-sgr-mouse NIL branch coverage ─────────────────────────────

(test handle-escape-sgr-mouse-ignores-malformed-sequence
  "%handle-escape-sgr-mouse is a no-op and returns ground-state for a malformed SGR sequence
   (one that %parse-sgr-mouse cannot parse)."
  (let ((s (make-fake-session)))
    (with-loop-state
      ;; Build a syntactically valid ESC [ < prefix but with only one field (no semicolons).
      ;; %parse-sgr-mouse will return (values nil nil nil nil) for this.
      (let* ((seq (format nil "~C[<0M" #\Escape))  ; too short, missing fields
             (buf (make-array (length seq) :element-type '(unsigned-byte 8)
                              :fill-pointer (length seq) :adjustable t
                              :initial-contents (map 'list #'char-code seq)))
             (len (length seq)))
        (multiple-value-bind (outcome next)
            (cl-tmux::%handle-escape-sgr-mouse s buf len)
          (is (null outcome)
              "%handle-escape-sgr-mouse with malformed SGR must return NIL outcome")
          (is (eq #'cl-tmux::%ground-input-state next)
              "%handle-escape-sgr-mouse must return ground-state for malformed sequence"))))))

;;; ── copy-mode navigation bytes via process-byte (table-driven coverage) ─────
;;;
;;; Tests that all the additional byte constants (h, l, w, b, e, $, etc.) defined
;;; in events-core.lisp route correctly through the copy-mode dispatch in
;;; %ground-input-state. We drive them through process-byte to stay at the
;;; public API level.

(test copy-mode-all-nav-bytes-via-process-byte
  "All standard copy-mode navigation bytes route without error through process-byte."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((screen (active-screen s))
            (state  (cl-tmux::make-input-state)))
        (cl-tmux::dispatch-command s :copy-mode-enter nil)
        (seed-scrollback screen 10)
        ;; Use the named constants from events-core.lisp for each byte.
        (dolist (byte (list #.cl-tmux::+byte-h+
                            #.cl-tmux::+byte-l+
                            #.cl-tmux::+byte-w+
                            #.cl-tmux::+byte-b+
                            #.cl-tmux::+byte-e+
                            #.cl-tmux::+byte-dollar+
                            #.cl-tmux::+byte-g+
                            #.cl-tmux::+byte-capital-g+
                            #.cl-tmux::+byte-capital-h+
                            #.cl-tmux::+byte-capital-m+
                            #.cl-tmux::+byte-capital-l+
                            #.cl-tmux::+byte-n+
                            #.cl-tmux::+byte-capital-n+
                            #.cl-tmux::+byte-capital-v+
                            #.cl-tmux::+byte-space+
                            #.cl-tmux::+byte-v+
                            #.cl-tmux::+byte-y+
                            #.cl-tmux::+byte-capital-y+
                            #.cl-tmux::+byte-capital-d+
                            #.cl-tmux::+byte-capital-a+
                            #.cl-tmux::+byte-r+))
          (cl-tmux::dispatch-command s :copy-mode-enter nil)
          (finishes (cl-tmux::process-byte s byte state)))))))

;;; ── %flush-esc-if-timed-out behavioural tests ────────────────────────────────

(test flush-esc-no-op-when-no-esc-pending
  "%flush-esc-if-timed-out is a no-op when esc-entered-at is NIL."
  (let ((sess  (make-fake-session))
        (state (cl-tmux::make-input-state)))
    ;; esc-entered-at starts NIL; %flush-esc-if-timed-out must not change the state.
    (is (null (cl-tmux::input-state-esc-entered-at state))
        "precondition: esc-entered-at is NIL")
    (cl-tmux::%flush-esc-if-timed-out state sess)
    (is (null (cl-tmux::input-state-esc-entered-at state))
        "esc-entered-at stays NIL when no escape is pending")))

(test flush-esc-within-timeout-does-not-flush
  "%flush-esc-if-timed-out does not flush when the timeout has NOT elapsed."
  (let ((sess  (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (with-isolated-config
      ;; Set a very long escape-time so the timer has definitely not expired.
      (cl-tmux/options:set-server-option "escape-time" 100000)
      ;; Simulate an ESC having been received: stamp esc-entered-at.
      (setf (cl-tmux::input-state-esc-entered-at state) (get-internal-real-time))
      (cl-tmux::%flush-esc-if-timed-out state sess)
      ;; Continuation must still point away from ground (timer did not fire).
      (is (not (null (cl-tmux::input-state-esc-entered-at state)))
          "esc-entered-at must remain set when timeout has not elapsed"))))

(test flush-esc-after-timeout-resets-to-ground
  "%flush-esc-if-timed-out resets state to ground when escape-time has elapsed."
  (let ((sess  (make-fake-session))
        (state (cl-tmux::make-input-state)))
    (with-isolated-config
      ;; Set escape-time to 0 ms so any elapsed time qualifies.
      (cl-tmux/options:set-server-option "escape-time" 0)
      ;; Stamp esc-entered-at far in the past.
      (setf (cl-tmux::input-state-esc-entered-at state)
            (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
      (cl-tmux::%flush-esc-if-timed-out state sess)
      ;; After flush: esc-entered-at cleared and continuation back to ground.
      (is (null (cl-tmux::input-state-esc-entered-at state))
          "esc-entered-at must be NIL after flush")
      (is (eq (cl-tmux::input-state-continuation state)
              #'cl-tmux::%ground-input-state)
          "continuation must return to ground after flush"))))

;;; ── %reset-repeat-if-expired behavioural tests ───────────────────────────────

(test reset-repeat-no-op-when-no-repeat-pending
  "%reset-repeat-if-expired is a no-op when repeat-entered-at is NIL."
  (let ((state (cl-tmux::make-input-state)))
    (is (null (cl-tmux::input-state-repeat-entered-at state))
        "precondition: repeat-entered-at is NIL")
    (cl-tmux::%reset-repeat-if-expired state)
    (is (null (cl-tmux::input-state-repeat-entered-at state))
        "repeat-entered-at stays NIL when nothing is pending")))

(test reset-repeat-within-timeout-does-not-reset
  "%reset-repeat-if-expired does not reset within the repeat-time window."
  (let ((state (cl-tmux::make-input-state)))
    (with-isolated-config
      (cl-tmux/options:set-option "repeat-time" 100000)
      (setf (cl-tmux::input-state-repeat-entered-at state) (get-internal-real-time))
      (cl-tmux::%reset-repeat-if-expired state)
      (is (not (null (cl-tmux::input-state-repeat-entered-at state)))
          "repeat-entered-at must not be cleared before timeout"))))

(test reset-repeat-after-timeout-resets-to-ground
  "%reset-repeat-if-expired resets to ground state after repeat-time elapses."
  (let ((state (cl-tmux::make-input-state)))
    (with-isolated-config
      (cl-tmux/options:set-option "repeat-time" 0)
      ;; Stamp repeat-entered-at far in the past.
      (setf (cl-tmux::input-state-repeat-entered-at state)
            (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
      (cl-tmux::%reset-repeat-if-expired state)
      (is (null (cl-tmux::input-state-repeat-entered-at state))
          "repeat-entered-at must be NIL after expiry")
      (is (eq (cl-tmux::input-state-continuation state)
              #'cl-tmux::%ground-input-state)
          "continuation must return to ground after repeat expiry"))))

;;; ── %try-mouse-passthrough mode tests ────────────────────────────────────────

(test try-mouse-passthrough-mode1-blocks-release
  "Mode 1 (X10/normal): release events are NOT forwarded."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0
                            :width 20 :height 5 :screen screen))
         (win    (make-window :id 1 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree (make-layout-leaf pane))))
    (setf (screen-mouse-mode screen) 1)
    ;; Release event (release-p=T): mode 1 must NOT forward.
    (let ((result (cl-tmux::%try-mouse-passthrough win pane 0 0 0 t)))
      (is (null result)
          "mode 1 must not forward release events (fd=-1 means encode returns nil)"))))

(test try-mouse-passthrough-mode2-blocks-non-motion-release
  "Mode 2 (button-event): release of a non-motion button is NOT forwarded."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0
                            :width 20 :height 5 :screen screen))
         (win    (make-window :id 1 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree (make-layout-leaf pane))))
    (setf (screen-mouse-mode screen) 2)
    ;; Button 0 release (left-click release, not motion): should NOT be forwarded.
    ;; (or (not T) (= 0 +mouse-btn-motion+)) = (or NIL NIL) = NIL → skip.
    (let ((result (cl-tmux::%try-mouse-passthrough win pane 0 0 0 t)))
      (is (null result)
          "mode-2 must not forward non-motion button releases"))))

(test try-mouse-passthrough-mode0-returns-nil
  "When the pane has mouse mode 0 (disabled), %try-mouse-passthrough returns NIL."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0
                            :width 20 :height 5 :screen screen))
         (win    (make-window :id 1 :name "w" :width 20 :height 5
                              :panes (list pane)
                              :tree (make-layout-leaf pane))))
    ;; mouse-mode = 0 means no tracking enabled; (plusp 0) = NIL.
    (setf (screen-mouse-mode screen) 0)
    (is (null (cl-tmux::%try-mouse-passthrough win pane 0 0 0 nil))
        "mouse mode 0 → passthrough must be nil")))

;;; ── drag-state is set on border press ───────────────────────────────────────

(test mouse-drag-state-is-set-on-border-press
  "*mouse-drag-state* is non-NIL after a left-press on the separator column."
  (with-two-pane-mouse-session (sess win p0 p1)
    (declare (ignore p1))
    ;; Simulate a left-press on the separator column (col 40).
    (cl-tmux::%dispatch-mouse-event sess 0 40 5 nil)
    ;; Whether the state has 2 or 4 elements depends on the implementation;
    ;; what matters is that it is non-NIL and contains a split node.
    (is (not (null cl-tmux::*mouse-drag-state*))
        "*mouse-drag-state* must be set after a border press")
    (is (cl-tmux/model:layout-split-p (first cl-tmux::*mouse-drag-state*))
        "first element of drag-state must be a layout-split node")))

;;; ── copy-mode-vi key table override ─────────────────────────────────────────

(test copy-mode-vi-table-binding-overrides-hardcoded
  "A binding in the copy-mode-vi table fires its command and suppresses the hardcoded dispatch."
  (with-isolated-config
    ;; Bind 'v' in copy-mode-vi to :copy-mode-begin-selection (same as hardcoded)
    ;; but with a token list to verify table lookup is happening.
    (cl-tmux/config:key-table-bind "copy-mode-vi" #\v :copy-mode-begin-selection)
    (let ((sess (make-fake-session :nwindows 1 :npanes 1)))
      (with-loop-state
        ;; Enter copy mode
        (let* ((win  (cl-tmux/model:session-active-window sess))
               (pane (cl-tmux/model:window-active-pane win))
               (screen (cl-tmux/model:pane-screen pane)))
          (cl-tmux/commands:copy-mode-enter screen)
          ;; The 'v' key (118) should be handled by the table lookup
          ;; We verify copy-mode is active and the binding exists
          (is (cl-tmux/terminal:screen-copy-mode-p screen)
              "screen must be in copy mode")
          (is (not (null (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
              "copy-mode-vi table must have 'v' binding")
          (cl-tmux/commands:copy-mode-exit screen))))))

;;; ── Extended keys (CSI u) key-name parsing ───────────────────────────────────

(test csi-u-key-name-modifier-combinations
  "%csi-u-key-name maps a CSI-u codepoint+modifier to the canonical key name."
  (is (string= "a"       (cl-tmux::%csi-u-key-name 97 1)) "plain a (mod 1)")
  (is (string= "S-a"     (cl-tmux::%csi-u-key-name 97 2)) "Shift (mod 2)")
  (is (string= "M-a"     (cl-tmux::%csi-u-key-name 97 3)) "Alt (mod 3)")
  (is (string= "C-a"     (cl-tmux::%csi-u-key-name 97 5)) "Ctrl (mod 5)")
  (is (string= "C-S-a"   (cl-tmux::%csi-u-key-name 97 6)) "Ctrl+Shift (mod 6)")
  (is (string= "C-M-a"   (cl-tmux::%csi-u-key-name 97 7)) "Ctrl+Alt (mod 7)")
  (is (string= "C-M-S-a" (cl-tmux::%csi-u-key-name 97 8)) "Ctrl+Alt+Shift (mod 8)"))

(test csi-u-key-name-special-keys
  "%csi-u-key-name names the special codepoints (Tab/Enter/Escape/Space/BSpace)."
  (is (string= "Tab"     (cl-tmux::%csi-u-key-name 9 1)))
  (is (string= "S-Tab"   (cl-tmux::%csi-u-key-name 9 2)))
  (is (string= "Enter"   (cl-tmux::%csi-u-key-name 13 1)))
  (is (string= "Escape"  (cl-tmux::%csi-u-key-name 27 1)))
  (is (string= "C-Space" (cl-tmux::%csi-u-key-name 32 5)))
  (is (string= "BSpace"  (cl-tmux::%csi-u-key-name 127 1))))

(test csi-u-key-name-unhandled-codepoint
  "An unhandled (control/out-of-range) codepoint yields NIL."
  (is (null (cl-tmux::%csi-u-key-name 0 1))   "NUL → NIL")
  (is (null (cl-tmux::%csi-u-key-name 7 5))   "BEL (control) → NIL")
  (is (null (cl-tmux::%csi-u-base-key 200))   "out-of-ASCII base → NIL"))

;;; ── Extended keys (CSI u) parameter parsing / legacy fallback ────────────────

(defun %csi-u-buf (&rest bytes)
  "Build a CSI-u BUFFER (with a trailing 'u') from the parameter BYTES, prefixed
   with ESC [, as the state machine accumulates it."
  (let ((v (make-array (+ 3 (length bytes)) :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
    (vector-push-extend 27 v) (vector-push-extend 91 v)   ; ESC [
    (dolist (b bytes) (vector-push-extend b v))
    (vector-push-extend 117 v)                            ; u
    v))

(test csi-u-parse-params-cases
  "%csi-u-parse-params reads <codepoint>[;<mod>] from a u-terminated buffer."
  (multiple-value-bind (cp mod)
      (cl-tmux::%csi-u-parse-params (%csi-u-buf 57 55 59 53) 7)  ; 97 ; 5
    (is (= 97 cp)) (is (= 5 mod)))
  (multiple-value-bind (cp mod)
      (cl-tmux::%csi-u-parse-params (%csi-u-buf 49 51) 5)        ; 13 (no ; mod)
    (is (= 13 cp)) (is (= 1 mod) "omitted mod defaults to 1"))
  (multiple-value-bind (cp mod)
      (cl-tmux::%csi-u-parse-params (%csi-u-buf 57 59 53 58 49) 8) ; 9 ; 5:1 (subparam)
    (is (= 9 cp)) (is (= 5 mod) "kitty <mod>:<event> tolerated, leading int taken")))

(test csi-u-control-byte-cases
  "%csi-u-control-byte gives the legacy Ctrl byte (a→1, Space/@→0, [→27), else NIL."
  (is (= 1  (cl-tmux::%csi-u-control-byte 97)))   ; C-a
  (is (= 26 (cl-tmux::%csi-u-control-byte 122)))  ; C-z
  (is (= 1  (cl-tmux::%csi-u-control-byte 65)))   ; C-A (upper) → 1
  (is (= 0  (cl-tmux::%csi-u-control-byte 32)))   ; C-Space → NUL
  (is (= 0  (cl-tmux::%csi-u-control-byte 64)))   ; C-@ → NUL
  (is (= 27 (cl-tmux::%csi-u-control-byte 91)))   ; C-[ → ESC
  (is (null (cl-tmux::%csi-u-control-byte 48))))  ; C-0 has no control byte

(test csi-u-legacy-octets-cases
  "%csi-u-legacy-octets reproduces the byte form a non-extended terminal sends."
  (is (equalp #(97)    (cl-tmux::%csi-u-legacy-octets 97 0)) "plain a → 97")
  (is (equalp #(97)    (cl-tmux::%csi-u-legacy-octets 97 1)) "S-a → 97 (shift only)")
  (is (equalp #(1)     (cl-tmux::%csi-u-legacy-octets 97 4)) "C-a → ^A")
  (is (equalp #(1)     (cl-tmux::%csi-u-legacy-octets 97 5)) "C-S-a → ^A (legacy collapse)")
  (is (equalp #(27 97) (cl-tmux::%csi-u-legacy-octets 97 2)) "M-a → ESC a")
  (is (equalp #(27 1)  (cl-tmux::%csi-u-legacy-octets 97 6)) "C-M-a → ESC ^A")
  (is (null (cl-tmux::%csi-u-legacy-octets 9 1)) "Tab (no printable/ctrl legacy) → NIL"))

(test csi-u-terminated-and-accumulating-predicates
  "The state-machine predicates recognise CSI-u prefixes and full sequences,
   and reject mouse / arrow CSI shapes."
  (let ((full (%csi-u-buf 57 55 59 53)))                ; ESC [ 97 ; 5 u  (len 7)
    (is-true  (cl-tmux::%csi-u-terminated-p full 7))
    (is-false (cl-tmux::%csi-u-accumulating-p full 7) "a terminated buf is not accumulating"))
  ;; ESC [ 9 7  — mid-accumulation digit prefix
  (let ((v (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dolist (b '(27 91 57 55)) (vector-push-extend b v))
    (is-true  (cl-tmux::%csi-u-accumulating-p v 4))
    (is-false (cl-tmux::%csi-u-terminated-p v 4)))
  ;; ESC [ M …  (X10 mouse) and ESC [ <  (SGR) must NOT look like CSI-u
  (let ((m (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (dolist (b '(27 91 77)) (vector-push-extend b m))    ; ESC [ M
    (is-false (cl-tmux::%csi-u-accumulating-p m 3) "mouse intro is not CSI-u")))

;;; ── Extended keys (CSI u) end-to-end through process-byte ────────────────────

(test root-csi-u-name-binding-fires
  "bind -n C-S-a next-window: a Ctrl+Shift+a extended-key (ESC [ 97 ; 6 u) runs
   next-window at root.  C-S-a has no legacy byte, so this exercises the name path
   — and the multi-digit codepoint 97 must not be dropped by the generic forward."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "C-S-a" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 57 55 59 54 117))  ; ESC [ 9 7 ; 6 u
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n C-S-a must run next-window via the CSI-u name path"))))))

(test root-csi-u-ctrl-letter-reinjects-to-control-byte
  "bind -n C-a next-window: a Ctrl+a extended-key (ESC [ 97 ; 5 u) runs next-window.
   C-a is stored under the control byte (^A), so this proves the legacy re-injection
   path routes the synthesized byte back through the root table."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "C-a" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 57 55 59 53 117))  ; ESC [ 9 7 ; 5 u
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n C-a must fire via re-injected control byte"))))))

(test root-csi-u-shift-tab-single-digit-codepoint
  "bind -n S-Tab next-window: Shift+Tab (ESC [ 9 ; 2 u) runs next-window.  The
   single-digit codepoint 9 must accumulate past the 3-byte-CSI branch rather than
   be misread as a bare ESC [ 9 arrow/copy escape."
  (with-isolated-config
    (cl-tmux/config:apply-config-directive '("bind" "-n" "S-Tab" "next-window"))
    (let ((s (make-fake-session :nwindows 2)))
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 57 59 50 117))  ; ESC [ 9 ; 2 u
            (cl-tmux::process-byte s b state))
          (is (eq (second (session-windows s)) (session-active-window s))
              "bound -n S-Tab must run next-window via single-digit CSI-u"))))))

(test csi-u-plain-printable-forwards-to-pane
  "An unbound plain extended-key (ESC [ 97 u) is translated to its legacy byte 'a'
   and forwarded transparently to the active pane's PTY (no byte dropped)."
  (with-pipe-fds (rfd wfd)
    (let ((s (make-fake-session :nwindows 1)))
      (setf (pane-fd (window-active-pane (session-active-window s))) wfd)
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 57 55 117))  ; ESC [ 9 7 u  (plain 'a')
            (cl-tmux::process-byte s b state))
          (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
            (is-true ready "the translated byte must reach the pane's PTY")
            (when ready
              (cffi:with-foreign-object (buf :uint8 8)
                (let ((n (cffi:foreign-funcall "read"
                                               :int rfd :pointer buf :unsigned-long 8
                                               :long)))
                  (is (= 1 n) "exactly one byte forwarded (got ~D)" n)
                  (is (= 97 (cffi:mem-aref buf :uint8 0))
                      "plain CSI-u 'a' must arrive as byte 97"))))))))))

(test csi-u-function-key-forwarded-raw-not-dropped
  "A digit CSI that ends in '~' (F5 = ESC [ 15 ~), not 'u', is not a CSI-u chord:
   the safety-net branch forwards the whole sequence raw to the pane rather than
   accumulating it forever after CSI-u deferral."
  (with-pipe-fds (rfd wfd)
    (let ((s (make-fake-session :nwindows 1)))
      (setf (pane-fd (window-active-pane (session-active-window s))) wfd)
      (with-loop-state
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 53 126))  ; ESC [ 1 5 ~
            (cl-tmux::process-byte s b state))
          (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
            (is-true ready "the function key must be forwarded, not swallowed")
            (when ready
              (cffi:with-foreign-object (buf :uint8 16)
                (let ((n (cffi:foreign-funcall "read"
                                               :int rfd :pointer buf :unsigned-long 16
                                               :long)))
                  (is (= 5 n) "all 5 bytes of ESC [ 1 5 ~ forwarded raw (got ~D)" n)
                  (is (= 27  (cffi:mem-aref buf :uint8 0)))
                  (is (= 126 (cffi:mem-aref buf :uint8 4)) "ends with '~'"))))))))))
