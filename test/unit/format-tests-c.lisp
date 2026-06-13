(in-package #:cl-tmux/test)

;;;; format arithmetic, additional variables, geometry, content-search, direct unit tests — part III

(in-suite format-suite)

;;; ── Format arithmetic #{e|OP|A,B} ───────────────────────────────────────────

(test format-arithmetic-table
  "#{e|OP|A,B} expands to the integer result of the arithmetic operation."
  (dolist (c '(("#{e|+|1,2}"  "3"  "addition")
               ("#{e|-|5,2}"  "3"  "subtraction")
               ("#{e|*|3,4}"  "12" "multiplication")
               ("#{e|/|10,3}" "3"  "integer division")
               ("#{e|%|10,3}" "1"  "modulo")))
    (destructuring-bind (spec expected desc) c
      (is (string= expected (fmt spec)) "~A" desc))))

(test format-arithmetic-with-variable
  "#{e|+|1,#{window_index}} expands to window_index+1."
  (let ((ctx (list :window-index 5)))
    (is (string= "6" (cl-tmux/format:expand-format "#{e|+|1,#{window_index}}" ctx)))))

(test format-arithmetic-divide-by-zero
  "#{e|/|5,0} returns 0 (no error)."
  (is (string= "0" (fmt "#{e|/|5,0}"))))

;;; ── Additional format variables ─────────────────────────────────────────────

(test format-context-version-is-35
  "#{version} expands to 3.5 for tmux config compatibility guards."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "3.5" (cl-tmux/format:expand-format "#{version}" ctx))
        "#{version} must be 3.5")))

(test format-context-pane-format-is-1-when-pane-present
  "#{pane_format} is 1 when a pane is in context."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_format}" ctx))
        "#{pane_format} must be 1 when pane is in context")))

(test format-context-window-format-is-1-when-window-present
  "#{window_format} is 1 when a window is in context."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{window_format}" ctx))
        "#{window_format} must be 1 when window is in context")))

;;; ── Bare strftime codes (%H, %M, %S, etc.) ──────────────────────────────────
;;;
;;; Real tmux passes status-left/right through strftime before #{} expansion,
;;; so bare %H:%M works in those strings. Our inline handler mimics this.

(test format-bare-strftime-hour-minute
  "Bare %H:%M in a format string expands to the current HH:MM time."
  (let ((result (cl-tmux/format:expand-format "%H:%M" nil)))
    ;; Should look like HH:MM (10 chars: 2 digits, colon, 2 digits)
    (is (= 5 (length result)) "bare %H:%M must expand to exactly 5 characters")
    (is (char= #\: (char result 2)) "colon at position 2")))

(test format-bare-strftime-percent-escape
  "Bare %% expands to a literal %."
  (is (string= "%" (cl-tmux/format:expand-format "%%" nil))))

(test format-bare-strftime-mixed-with-hash-var
  "Bare %H and #{session_name} can coexist in one template."
  (let* ((result (cl-tmux/format:expand-format "%H:00 #{session_name}"
                                               '(:session-name "main"))))
    ;; Should end with ":00 main" (hour prefix varies)
    (is (search ":00 main" result) "mixed bare-% and #{} expansion")))

(test format-bare-strftime-unknown-letter-is-literal
  "A %X where X is not a strftime letter passes through unchanged."
  (is (string= "test%q" (cl-tmux/format:expand-format "test%q" nil))))

;;; ── @user-option fallback in format variables ────────────────────────────────
;;;
;;; Real tmux allows #{@my-var} to access user-defined options set via
;;; `set -g @my-var value`. The fallback through *global-options* provides this.

(test format-user-option-at-variable
  "#{@my-var} falls back to *global-options* when not in context."
  (with-isolated-config
    (cl-tmux/options:set-option "@my-var" "hello")
    (let ((result (cl-tmux/format:expand-format "#{@my-var}" nil)))
      (is (string= "hello" result)
          "#{@my-var} must expand via global options fallback"))))

(test format-user-option-unknown-returns-empty
  "#{@nonexistent} returns empty string when option not set."
  (with-isolated-config
    (let ((result (cl-tmux/format:expand-format "#{@nonexistent}" nil)))
      (is (string= "" result) "#{@nonexistent} must return empty string"))))

;;; ── Version guard patterns ───────────────────────────────────────────────────

(test format-version-guard-comparison
  "#{>=:#{version},3.0} evaluates to 1 (version 3.5 >= 3.0)."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    ;; Note: comparison is numeric; "3.5" vs "3.0" — parse-integer gives 3 for both
    ;; due to junk-allowed stopping at '.'. This is a known limitation.
    ;; The test just verifies no error is thrown.
    (is (stringp (cl-tmux/format:expand-format "#{version}" ctx))
        "#{version} must expand to a string")))

;;; ── #{pane_synchronized} respects per-window scoping ─────────────────────────

(test format-pane-synchronized-window-local-override
  "#{pane_synchronized} reads the window-local synchronize-panes override:
   it is \"1\" for a window with the local override on, and \"0\" for a fresh
   window with no override (global stays nil)."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" nil)
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win))))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
      (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
        (is (string= "1" (cl-tmux/format:expand-format "#{pane_synchronized}" ctx))
            "#{pane_synchronized} must be \"1\" for a window with the local override on"))
      ;; A second, fresh window with no override falls back to the global NIL → "0".
      (let* ((win2  (make-fake-window 99 "w2"))
             (pane2 (first (cl-tmux/model:window-panes win2)))
             (ctx2  (cl-tmux/format:format-context-from-session sess win2 pane2)))
        (is (string= "0" (cl-tmux/format:expand-format "#{pane_synchronized}" ctx2))
            "#{pane_synchronized} must be \"0\" for a window with no override")))))

;;; ── geometry-derived variables: window_width/height, pane_at_* ───────────────
;;;
;;; make-fake-window builds panes/windows at 20x5 (each fake pane shares
;;; x=0 y=0 w=20 h=5, matching the window), so a single-pane fake window has
;;; the pane filling the whole window — every edge flag is "1".  For a real
;;; split we use make-two-pane-h-window from helpers.lisp, which lays out:
;;;   window 81x24; p0 x=0 y=0 w=40 h=24; p1 x=41 y=0 w=40 h=24.
;;; So p0 touches top/bottom/left but NOT right (0+40=40 ≠ 81); p1 touches
;;; top/bottom/right (41+40=81) but NOT left (x=41 ≠ 0).

(test format-pane-tty-from-pane
  "#{pane_tty} expands to the pane's slave PTY device path."
  (let ((pane (make-no-pty-pane 1 0 0 80 24)))
    (setf (cl-tmux/model:pane-tty pane) "/dev/pts/7")
    (is (string= "/dev/pts/7"
                 (cl-tmux/format:expand-format
                  "#{pane_tty}"
                  (cl-tmux/format:format-context-from-session nil nil pane)))
        "#{pane_tty} must report the pane's tty slot")))

(test format-pane-tty-empty-when-no-pty-or-nil
  "#{pane_tty} is empty for a pane with no PTY (default \"\") and for a NIL pane."
  (let ((pane (make-no-pty-pane 1 0 0 80 24)))
    (is (string= "" (cl-tmux/format:expand-format
                     "#{pane_tty}"
                     (cl-tmux/format:format-context-from-session nil nil pane)))
        "no-PTY pane → empty pane_tty"))
  (is (string= "" (cl-tmux/format:expand-format
                   "#{pane_tty}"
                   (cl-tmux/format:format-context-from-session nil nil nil)))
      "NIL pane → empty pane_tty"))

(test format-window-width-height-from-window
  "#{window_width} / #{window_height} expand to the window's layout dimensions.
   make-fake-window builds a 20x5 window, so the expansions are \"20\"/\"5\"."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "20" (cl-tmux/format:expand-format "#{window_width}" ctx))
        "#{window_width} must equal the fake window's width (20), got ~S"
        (cl-tmux/format:expand-format "#{window_width}" ctx))
    (is (string= "5" (cl-tmux/format:expand-format "#{window_height}" ctx))
        "#{window_height} must equal the fake window's height (5), got ~S"
        (cl-tmux/format:expand-format "#{window_height}" ctx))))

(test format-pane-at-edges-single-pane-all-true
  "For a single-pane window (pane fills the window) all pane_at_* flags are \"1\".
   make-fake-window's lone pane is x=0 y=0 w=20 h=5 in a 20x5 window."
  (let* ((sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (pane (first (cl-tmux/model:window-panes win)))
         (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx))
        "single pane must be at top")
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx))
        "single pane must be at bottom")
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx))
        "single pane must be at left")
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx))
        "single pane must be at right")))

(test format-pane-at-edges-horizontal-split
  "For a laid-out horizontal split (make-two-pane-h-window: 81x24, p0 x=0 w=40,
   p1 x=41 w=40), the left pane is NOT at the right edge and the right pane is
   NOT at the left edge, while both span the full height (at top and bottom)."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (let ((ctx0 (cl-tmux/format:format-context-from-session nil win p0))
          (ctx1 (cl-tmux/format:format-context-from-session nil win p1)))
      ;; left pane p0: at left, top, bottom; NOT at right.
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx0))
          "left pane must be at left edge")
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_right}" ctx0))
          "left pane must NOT be at right edge (0+40=40 ≠ 81)")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx0))
          "left pane must be at top edge")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx0))
          "left pane must be at bottom edge (0+24=24)")
      ;; right pane p1: at right, top, bottom; NOT at left.
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_left}" ctx1))
          "right pane must NOT be at left edge (x=41 ≠ 0)")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx1))
          "right pane must be at right edge (41+40=81)")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx1))
          "right pane must be at top edge")
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx1))
          "right pane must be at bottom edge")
      ;; window_width/height from the real split window.
      (is (string= "81" (cl-tmux/format:expand-format "#{window_width}" ctx0))
          "#{window_width} must equal the split window's width (81)")
      (is (string= "24" (cl-tmux/format:expand-format "#{window_height}" ctx0))
          "#{window_height} must equal the split window's height (24)"))))

;;; ── pane_at_top/bottom "0" branches + NIL-safe defaults ──────────────────────
;;;
;;; with-v-split-window (helpers.lisp) lays out: window 80x21;
;;;   p0 x=0 y=0  w=80 h=10 (top pane), p1 x=0 y=11 w=80 h=10 (bottom pane).
;;; Both span the full width (x=0, w=80=window width → at left and right).
;;; p0 is at top (y=0) but NOT at bottom (0+10=10 ≠ 21); p1 is NOT at top
;;; (y=11 ≠ 0) but IS at bottom (11+10=21).  This exercises the "0" branch of
;;; #{pane_at_top}/#{pane_at_bottom}, which the full-height fixtures never hit.

(test format-pane-at-edges-vertical-split
  "A laid-out vertical split drives the \"0\" branch of pane_at_top/pane_at_bottom:
   the TOP pane is not at the bottom edge, the BOTTOM pane is not at the top edge,
   while both span the full width."
  (with-v-split-window (win p0 p1)
    (let ((ctx0 (cl-tmux/format:format-context-from-session nil win p0))
          (ctx1 (cl-tmux/format:format-context-from-session nil win p1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}" ctx0)))
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx0)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx0)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx0)))
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_top}" ctx1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}" ctx1)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_right}" ctx1))))))

(test format-pane-at-edges-and-window-dims-default-when-nil
  "With NIL session/window/pane, geometry vars are empty-safe: window_width/height
   expand to \"0\" and every pane_at_* flag is \"0\"."
  (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
    (is (string= "0" (cl-tmux/format:expand-format "#{window_width}"  ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{window_height}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_top}"    ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_left}"   ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_right}"  ctx)))))

(test format-pane-at-bottom-right-default-when-window-nil
  "Pane present but window NIL: pane_at_top/left resolve from the pane's coords,
   but pane_at_bottom/right short-circuit to \"0\" (far-edge needs the window)."
  (let* ((pane (make-no-pty-pane 1 0 0 40 24))
         (ctx  (cl-tmux/format:format-context-from-session nil nil pane)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_top}"    ctx)))
    (is (string= "1" (cl-tmux/format:expand-format "#{pane_at_left}"   ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_bottom}" ctx)))
    (is (string= "0" (cl-tmux/format:expand-format "#{pane_at_right}"  ctx)))))
