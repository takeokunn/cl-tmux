(in-package #:cl-tmux/test)

;;;; format arithmetic, additional variables, geometry, content-search, direct unit tests — part III

(describe "format-suite"

  ;;; ── Format arithmetic #{e|OP|A,B} ───────────────────────────────────────────

  ;; #{e|OP|A,B} expands to the integer result of the arithmetic operation.
  (it "format-arithmetic-table"
    (dolist (c '(("#{e|+|1,2}"  "3"  "addition")
                 ("#{e|-|5,2}"  "3"  "subtraction")
                 ("#{e|*|3,4}"  "12" "multiplication")
                 ("#{e|/|10,3}" "3"  "integer division")
                 ("#{e|%|10,3}" "1"  "modulo")))
      (destructuring-bind (spec expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec))))))

  ;; #{e|+|1,#{window_index}} expands to window_index+1.
  (it "format-arithmetic-with-variable"
    (let ((ctx (list :window-index 5)))
      (expect (string= "6" (cl-tmux/format:expand-format "#{e|+|1,#{window_index}}" ctx)))))

  ;; #{e|/|5,0} returns 0 (no error).
  (it "format-arithmetic-divide-by-zero"
    (expect (string= "0" (fmt "#{e|/|5,0}"))))

  ;; #{e|m|A,B} is not a cl-tmux arithmetic operator.
  (it "format-arithmetic-rejects-modulo-m-alias"
    (expect (string= "" (fmt "#{e|m|10,3}")))
    (expect (string= "" (fmt "#{e|m|9,3}")))
    (expect (string= "" (fmt "#{e|m|5,0}"))))

  ;; #{e|OP|A,B} supports comparison operators returning tmux's 1/0 strings.
  (it "format-arithmetic-comparison-operators"
    (dolist (c '(("#{e|<|1,2}"   "1" "less-than true")
                 ("#{e|<|2,1}"   "0" "less-than false")
                 ("#{e|>|3,2}"   "1" "greater-than true")
                 ("#{e|>=|2,2}"  "1" "ge equal")
                 ("#{e|<=|2,3}"  "1" "le true")
                 ("#{e|==|5,5}"  "1" "eq true")
                 ("#{e|==|5,6}"  "0" "eq false")
                 ("#{e|!=|5,6}"  "1" "ne true")
                 ("#{e|!=|5,5}"  "0" "ne false")))
      (destructuring-bind (spec expected desc) c
        (declare (ignore desc))
        (expect (string= expected (fmt spec))))))

  ;; #{e|OP|f|PREC|A,B} performs float arithmetic and formats to PREC decimals.
  (it "format-arithmetic-float-flag-and-precision"
    ;; default float precision is 2 when f is present and no precision field given
    (expect (string= "16.50" (fmt "#{e|*|f|5.5,3}")))
    ;; explicit precision field
    (expect (string= "16.5000" (fmt "#{e|*|f|4|5.5,3}")))
    ;; float division keeps the fractional part
    (expect (string= "3.33" (fmt "#{e|/|f|10,3}")))
    ;; without f, integer (truncating) division and no decimals
    (expect (string= "3" (fmt "#{e|/|10,3}"))))

  ;; Float == / != use a 1e-9 epsilon tolerance like tmux.
  (it "format-arithmetic-float-comparison-epsilon"
    (expect (string= "1" (fmt "#{e|==|f|0.1,0.1}")))
    (expect (string= "0" (fmt "#{e|!=|f|0.1,0.1}")))
    (expect (string= "1" (fmt "#{e|<|f|0.1,0.2}"))))

  ;;; ── Additional format variables ─────────────────────────────────────────────

  ;; #{version} expands to the cl-tmux runtime version.
  (it "format-context-version-is-cl-tmux-version"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (string= (cl-tmux/version:version-string)
                       (cl-tmux/format:expand-format "#{version}" ctx)))))

  ;; #{pane_format} is 1 when a pane is in context.
  (it "format-context-pane-format-is-1-when-pane-present"
    (let* ((sess (make-fake-session :nwindows 1 :npanes 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (string= "1" (cl-tmux/format:expand-format "#{pane_format}" ctx)))))

  ;; #{window_format} is 1 when a window is in context.
  (it "format-context-window-format-is-1-when-window-present"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (string= "1" (cl-tmux/format:expand-format "#{window_format}" ctx)))))

  ;;; ── Bare strftime codes (%H, %M, %S, etc.) ──────────────────────────────────
  ;;;
  ;;; Real tmux passes status-left/right through strftime before #{} expansion,
  ;;; so bare %H:%M works in those strings. Our inline handler mimics this.

  ;; Bare %H:%M in a format string expands to the current HH:MM time.
  (it "format-bare-strftime-hour-minute"
    (let ((result (cl-tmux/format:expand-format "%H:%M" nil)))
      ;; Should look like HH:MM (10 chars: 2 digits, colon, 2 digits)
      (expect (= 5 (length result)))
      (expect (char= #\: (char result 2)))))

  ;; Bare %% expands to a literal %.
  (it "format-bare-strftime-percent-escape"
    (expect (string= "%" (cl-tmux/format:expand-format "%%" nil))))

  ;; Bare %H and #{session_name} can coexist in one template.
  (it "format-bare-strftime-mixed-with-hash-var"
    (let* ((result (cl-tmux/format:expand-format "%H:00 #{session_name}"
                                                 '(:session-name "main"))))
      ;; Should end with ":00 main" (hour prefix varies)
      (expect (search ":00 main" result))))

  ;; A %X where X is not a strftime letter passes through unchanged.
  (it "format-bare-strftime-unknown-letter-is-literal"
    (expect (string= "test%q" (cl-tmux/format:expand-format "test%q" nil))))

  ;;; ── @user-option fallback in format variables ────────────────────────────────
  ;;;
  ;;; Real tmux allows #{@my-var} to access user-defined options set via
  ;;; `set -g @my-var value`. The fallback through *global-options* provides this.

  ;; #{@my-var} falls back to *global-options* when not in context.
  (it "format-user-option-at-variable"
    (with-isolated-config
      (cl-tmux/options:set-option "@my-var" "hello")
      (let ((result (cl-tmux/format:expand-format "#{@my-var}" nil)))
        (expect (string= "hello" result)))))

  ;; #{@nonexistent} returns empty string when option not set.
  (it "format-user-option-unknown-returns-empty"
    (with-isolated-config
      (let ((result (cl-tmux/format:expand-format "#{@nonexistent}" nil)))
        (expect (string= "" result)))))

  ;;; ── Version guard patterns ───────────────────────────────────────────────────

  ;; #{version} can be expanded and consumed by comparison modifiers.
  (it "format-version-guard-comparison"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      ;; Comparison parsing currently treats dotted versions as integers with
      ;; junk allowed; this test only verifies version expansion remains usable.
      (expect (stringp (cl-tmux/format:expand-format "#{version}" ctx)))))

  ;;; ── #{pane_synchronized} respects per-window scoping ─────────────────────────

  ;; #{pane_synchronized} reads the window-local synchronize-panes override:
  ;; it is "1" for a window with the local override on, and "0" for a fresh
  ;; window with no override (global stays nil).
  (it "format-pane-synchronized-window-local-override"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (let* ((sess (make-fake-session :nwindows 1))
             (win  (first (cl-tmux/model:session-windows sess)))
             (pane (first (cl-tmux/model:window-panes win))))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
        (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
          (expect (string= "1" (cl-tmux/format:expand-format "#{pane_synchronized}" ctx))))
        ;; A second, fresh window with no override falls back to the global NIL → "0".
        (let* ((win2  (make-fake-window 99 "w2"))
               (pane2 (first (cl-tmux/model:window-panes win2)))
               (ctx2  (cl-tmux/format:format-context-from-session sess win2 pane2)))
          (expect (string= "0" (cl-tmux/format:expand-format "#{pane_synchronized}" ctx2)))))))

  ;;; ── geometry-derived variables: window_width/height, pane_at_* ───────────────
  ;;;
  ;;; make-fake-window builds panes/windows at 20x5 (each fake pane shares
  ;;; x=0 y=0 w=20 h=5, matching the window), so a single-pane fake window has
  ;;; the pane filling the whole window — every edge flag is "1".  For a real
  ;;; split we use make-two-pane-h-window from helpers-layout-fixtures.lisp, which lays out:
  ;;;   window 81x24; p0 x=0 y=0 w=40 h=24; p1 x=41 y=0 w=40 h=24.
  ;;; So p0 touches top/bottom/left but NOT right (0+40=40 ≠ 81); p1 touches
  ;;; top/bottom/right (41+40=81) but NOT left (x=41 ≠ 0).

  ;; #{pane_tty} expands to the pane's slave PTY device path.
  (it "format-pane-tty-from-pane"
    (let ((pane (make-no-pty-pane 1 0 0 80 24)))
      (setf (cl-tmux/model:pane-tty pane) "/dev/pts/7")
      (expect (string= "/dev/pts/7"
                       (cl-tmux/format:expand-format
                        "#{pane_tty}"
                        (cl-tmux/format:format-context-from-session nil nil pane))))))

  ;; #{pane_tty} is empty for a pane with no PTY (default "") and for a NIL pane.
  (it "format-pane-tty-empty-when-no-pty-or-nil"
    (let ((pane (make-no-pty-pane 1 0 0 80 24)))
      (expect (string= "" (cl-tmux/format:expand-format
                           "#{pane_tty}"
                           (cl-tmux/format:format-context-from-session nil nil pane)))))
    (expect (string= "" (cl-tmux/format:expand-format
                         "#{pane_tty}"
                         (cl-tmux/format:format-context-from-session nil nil nil)))))

  ;; #{window_width} / #{window_height} expand to the window's layout dimensions.
  ;; make-fake-window builds a 20x5 window, so the expansions are "20"/"5".
  (it "format-window-width-height-from-window"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (expect (string= "20" (cl-tmux/format:expand-format "#{window_width}" ctx)))
      (expect (string= "5" (cl-tmux/format:expand-format "#{window_height}" ctx)))))

  ;; For a single-pane window (pane fills the window) all pane_at_* flags are "1".
  ;; make-fake-window's lone pane is x=0 y=0 w=20 h=5 in a 20x5 window.
  (it "format-pane-at-edges-single-pane-all-true"
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win)))
           (ctx  (cl-tmux/format:format-context-from-session sess win pane)))
      (dolist (spec '("#{pane_at_top}" "#{pane_at_bottom}"
                      "#{pane_at_left}" "#{pane_at_right}"))
        (expect (string= "1" (cl-tmux/format:expand-format spec ctx))))))

  ;; For a laid-out horizontal split (make-two-pane-h-window from helpers-layout-fixtures.lisp: 81x24, p0 x=0 w=40,
  ;; p1 x=41 w=40), the left pane is NOT at the right edge and the right pane is
  ;; NOT at the left edge, while both span the full height (at top and bottom).
  (it "format-pane-at-edges-horizontal-split"
    (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
      (let ((ctx0 (cl-tmux/format:format-context-from-session nil win p0))
            (ctx1 (cl-tmux/format:format-context-from-session nil win p1)))
        (dolist (c `((,ctx0 "#{pane_at_left}"   "1" "left pane at left edge")
                     (,ctx0 "#{pane_at_right}"  "0" "left pane NOT at right edge (0+40≠81)")
                     (,ctx0 "#{pane_at_top}"    "1" "left pane at top edge")
                     (,ctx0 "#{pane_at_bottom}" "1" "left pane at bottom edge (0+24=24)")
                     (,ctx1 "#{pane_at_left}"   "0" "right pane NOT at left edge (x=41≠0)")
                     (,ctx1 "#{pane_at_right}"  "1" "right pane at right edge (41+40=81)")
                     (,ctx1 "#{pane_at_top}"    "1" "right pane at top edge")
                     (,ctx1 "#{pane_at_bottom}" "1" "right pane at bottom edge")
                     (,ctx0 "#{window_width}"   "81" "window_width equals split window width")
                     (,ctx0 "#{window_height}"  "24" "window_height equals split window height")))
          (destructuring-bind (ctx spec expected desc) c
            (declare (ignore desc))
            (expect (string= expected (cl-tmux/format:expand-format spec ctx))))))))

  ;;; ── pane_at_top/bottom "0" branches + NIL-safe defaults ──────────────────────
  ;;;
  ;;; with-v-split-window (helpers-layout-fixtures.lisp) lays out: window 80x21;
  ;;;   p0 x=0 y=0  w=80 h=10 (top pane), p1 x=0 y=11 w=80 h=10 (bottom pane).
  ;;; Both span the full width (x=0, w=80=window width → at left and right).
  ;;; p0 is at top (y=0) but NOT at bottom (0+10=10 ≠ 21); p1 is NOT at top
  ;;; (y=11 ≠ 0) but IS at bottom (11+10=21).  This exercises the "0" branch of
  ;;; #{pane_at_top}/#{pane_at_bottom}, which the full-height fixtures never hit.

  ;; A laid-out vertical split drives the "0" branch of pane_at_top/pane_at_bottom:
  ;; the TOP pane is not at the bottom edge, the BOTTOM pane is not at the top edge,
  ;; while both span the full width.
  (it "format-pane-at-edges-vertical-split"
    (with-v-split-window (win p0 p1)
      (let ((ctx0 (cl-tmux/format:format-context-from-session nil win p0))
            (ctx1 (cl-tmux/format:format-context-from-session nil win p1)))
        (dolist (c `((,ctx0 "#{pane_at_top}"    "1") (,ctx0 "#{pane_at_bottom}" "0")
                     (,ctx0 "#{pane_at_left}"   "1") (,ctx0 "#{pane_at_right}"  "1")
                     (,ctx1 "#{pane_at_top}"    "0") (,ctx1 "#{pane_at_bottom}" "1")
                     (,ctx1 "#{pane_at_left}"   "1") (,ctx1 "#{pane_at_right}"  "1")))
          (destructuring-bind (ctx spec expected) c
            (expect (string= expected (cl-tmux/format:expand-format spec ctx))))))))

  ;; With NIL session/window/pane, geometry vars are empty-safe: window_width/height
  ;; expand to "0" and every pane_at_* flag is "0".
  (it "format-pane-at-edges-and-window-dims-default-when-nil"
    (let ((ctx (cl-tmux/format:format-context-from-session nil nil nil)))
      (dolist (spec '("#{window_width}" "#{window_height}"
                      "#{pane_at_top}" "#{pane_at_bottom}"
                      "#{pane_at_left}" "#{pane_at_right}"))
        (expect (string= "0" (cl-tmux/format:expand-format spec ctx))))))

  ;; Pane present but window NIL: pane_at_top/left resolve from the pane's coords,
  ;; but pane_at_bottom/right short-circuit to "0" (far-edge needs the window).
  (it "format-pane-at-bottom-right-default-when-window-nil"
    (let* ((pane (make-no-pty-pane 1 0 0 40 24))
           (ctx  (cl-tmux/format:format-context-from-session nil nil pane)))
      (dolist (c '(("#{pane_at_top}"    "1") ("#{pane_at_left}"   "1")
                   ("#{pane_at_bottom}" "0") ("#{pane_at_right}"  "0")))
        (destructuring-bind (spec expected) c
          (expect (string= expected (cl-tmux/format:expand-format spec ctx))))))))
