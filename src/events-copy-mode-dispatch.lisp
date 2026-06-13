(in-package #:cl-tmux)

;;;; Copy-mode single-byte dispatch and helpers.
;;;;
;;;; This file houses the declarative copy-mode vi key table, extracted from the
;;;; monolithic %ground-input-state in events-keystroke.lisp.  It mirrors the
;;;; pattern of define-csi-rules / define-command-handlers: a top-level macro
;;;; generates a named dispatch function from an ordered table of Prolog-like rules.

;;; ── Macro: define-copy-mode-vi-rules ────────────────────────────────────────
;;;
;;; Each RULE is (BYTE-OR-BYTE-LIST ACTION-FORM ...).
;;; The macro generates %dispatch-copy-mode-byte which is called with a screen, a
;;; raw byte, and the already-computed repeat count.  The repeat helper and the
;;; char-jump continuation builder are top-level functions so they are independently
;;; testable and do not pollute the ground-state closure.

(defmacro define-copy-mode-vi-rules (&rest rules)
  "Generate %DISPATCH-COPY-MODE-BYTE (SCREEN BYTE COUNT SESSION) from an ordered
   table of copy-mode vi rules.  Each RULE is (PATTERN &rest BODY) where PATTERN
   is an integer, a list of integers, or a read-time evaluated constant expression
   such as #.+byte-f+.  The first matching PATTERN wins (ordered-cut semantics).
   Returns (values HANDLED NEXT-STATE) where HANDLED is T or NIL and NEXT-STATE
   is a CPS continuation function or NIL to return to ground state."
  `(defun %dispatch-copy-mode-byte (screen byte count session)
     "Dispatch BYTE in copy-mode using the accumulated repeat COUNT.
      SESSION is passed through for continuations that need to return a new CPS state.
      Returns (values HANDLED NEXT-STATE) where HANDLED is T or NIL and NEXT-STATE
      is a CPS continuation function or NIL to return to ground state."
     (case byte
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (pattern &rest body) rule
              `(,pattern ,@body)))
          rules)
       (otherwise (values nil nil)))))

;;; ── Helpers for the dispatch table ──────────────────────────────────────────

(defun %copy-mode-repeat (function screen count)
  "Call FUNCTION on SCREEN COUNT times (count >= 1).
   Used by copy-mode vi navigation commands that accept a numeric prefix."
  (dotimes (_ count) (funcall function screen)))

(defun %copy-mode-char-jump-continuation (jump-fn screen count)
  "Return a CPS continuation that reads one byte then calls JUMP-FN on SCREEN
   for that character COUNT times, then returns to %ground-input-state.
   Used for vi f/F/t/T commands which need a second byte (the target character)."
  (lambda (_session byte2)
    (declare (ignore _session))
    (dotimes (_ count) (funcall jump-fn screen (code-char byte2)))
    (setf *dirty* t)
    (%ground-values)))

;;; ── Copy-mode vi dispatch table ──────────────────────────────────────────────
;;;
;;; Rules read as Prolog facts: each arm maps one or more byte values to an action.
;;; Raw integer literals are replaced by named constants from events-core.lisp.
;;; The #. read-time eval allows using constant symbols in case patterns.

(define-copy-mode-vi-rules
  ;; ── Exit copy mode ────────────────────────────────────────────────────────
  ;; q / Q / C-c — exit copy mode
  ((#.+byte-q+ 81 3)
   (copy-mode-exit screen)
   (values t nil))
  ;; i — exit copy mode (non-standard but kept for compat)
  (105
   (copy-mode-exit screen)
   (values t nil))
  ;; ── Yank (copy) ───────────────────────────────────────────────────────────
  ;; Enter (13) / C-j (10) — copy selection and return to ground
  ((13 10)
   (copy-mode-yank screen)
   (values t nil))
  ;; ── Cursor navigation ─────────────────────────────────────────────────────
  ;; h / C-h (8) — move cursor left
  ((#.+byte-h+ 8)
   (%copy-mode-repeat (lambda (s) (copy-mode-move-cursor s :left)) screen count)
   (values t nil))
  ;; l — move cursor right
  (#.+byte-l+
   (%copy-mode-repeat (lambda (s) (copy-mode-move-cursor s :right)) screen count)
   (values t nil))
  ;; j / C-n (14) — move cursor down (viewport follows at edge)
  ((#.+byte-j+ 14)
   (%copy-mode-repeat (lambda (s) (copy-mode-move-cursor s :down)) screen count)
   (values t nil))
  ;; k / C-p (16) — move cursor up (viewport follows at edge)
  ((#.+byte-k+ 16)
   (%copy-mode-repeat (lambda (s) (copy-mode-move-cursor s :up)) screen count)
   (values t nil))
  ;; ── Viewport scrolling ────────────────────────────────────────────────────
  ;; J — scroll-down (viewport toward newer content; vi J = C-e analogue)
  (74
   (%copy-mode-repeat #'copy-mode-scroll-down-line screen count)
   (values t nil))
  ;; K — scroll-up (viewport toward older content; vi K = C-y analogue)
  (75
   (%copy-mode-repeat #'copy-mode-scroll-up-line screen count)
   (values t nil))
  ;; C-f (6) — page down
  (6
   (%copy-mode-repeat #'copy-mode-page-down screen count)
   (values t nil))
  ;; C-b (2) — page up
  (2
   (%copy-mode-repeat #'copy-mode-page-up screen count)
   (values t nil))
  ;; C-u (21) — scroll up half page
  (21
   (%copy-mode-repeat #'copy-mode-half-page-up screen count)
   (values t nil))
  ;; C-d (4) — scroll down half page
  (4
   (%copy-mode-repeat #'copy-mode-half-page-down screen count)
   (values t nil))
  ;; C-e (5) — scroll down one line
  (5
   (%copy-mode-repeat #'copy-mode-scroll-down-line screen count)
   (values t nil))
  ;; C-y (25) — scroll up one line
  (25
   (%copy-mode-repeat #'copy-mode-scroll-up-line screen count)
   (values t nil))
  ;; ── Word motions ──────────────────────────────────────────────────────────
  ;; w — word forward
  (#.+byte-w+
   (%copy-mode-repeat #'copy-mode-word-forward screen count)
   (values t nil))
  ;; W — WORD forward (whitespace-delimited)
  (87
   (%copy-mode-repeat #'copy-mode-space-forward screen count)
   (values t nil))
  ;; b — word backward
  (#.+byte-b+
   (%copy-mode-repeat #'copy-mode-word-backward screen count)
   (values t nil))
  ;; B — WORD backward (whitespace-delimited)
  (66
   (%copy-mode-repeat #'copy-mode-space-backward screen count)
   (values t nil))
  ;; e — word end
  (#.+byte-e+
   (%copy-mode-repeat #'copy-mode-word-end screen count)
   (values t nil))
  ;; E — WORD end (whitespace-delimited)
  (69
   (%copy-mode-repeat #'copy-mode-space-end screen count)
   (values t nil))
  ;; ── Line position ─────────────────────────────────────────────────────────
  ;; 0 — line start (bare '0' with no prefix)
  (#.+byte-digit-0+
   (copy-mode-line-start screen)
   (values t nil))
  ;; ^ — back-to-indentation (first non-blank)
  (94
   (copy-mode-back-to-indentation screen)
   (values t nil))
  ;; $ — line end
  (#.+byte-dollar+
   (copy-mode-line-end screen)
   (values t nil))
  ;; ── Jump to scrollback extremes ───────────────────────────────────────────
  ;; g — jump to top (maximum scrollback)
  (#.+byte-g+
   (copy-mode-top screen)
   (values t nil))
  ;; G — jump to bottom (offset = 0, live view)
  (71
   (copy-mode-bottom screen)
   (values t nil))
  ;; H — cursor to top of screen
  (#.+byte-capital-h+
   (copy-mode-high screen)
   (values t nil))
  ;; M — cursor to middle of screen
  (#.(char-code #\M)
   (copy-mode-middle screen)
   (values t nil))
  ;; L — cursor to bottom of screen
  (#.+byte-capital-l+
   (copy-mode-low screen)
   (values t nil))
  ;; ── Scroll centering ──────────────────────────────────────────────────────
  ;; z — scroll-middle: scroll viewport so cursor row is centered
  (122
   (copy-mode-scroll-middle screen)
   (values t nil))
  ;; ── Selection ─────────────────────────────────────────────────────────────
  ;; V — begin line selection
  (#.+byte-capital-v+
   (copy-mode-begin-line-selection screen)
   (values t nil))
  ;; Space / v — begin selection
  ((#.+byte-space+ #.+byte-v+)
   (copy-mode-begin-selection screen)
   (values t nil))
  ;; o / O — swap mark and cursor ends of selection
  ((111 79)
   (copy-mode-other-end screen)
   (values t nil))
  ;; C-v (22) — toggle rectangle select
  (22
   (copy-mode-toggle-rectangle screen)
   (values t nil))
  ;; ── Copy actions ─────────────────────────────────────────────────────────
  ;; y — yank selection
  (#.+byte-y+
   (copy-mode-yank screen)
   (values t nil))
  ;; D — copy to end of line
  (#.+byte-capital-d+
   (copy-mode-copy-end-of-line screen)
   (values t nil))
  ;; Y — copy current line
  (#.+byte-capital-y+
   (copy-mode-copy-line screen)
   (values t nil))
  ;; A — append selection to paste buffer and cancel
  (#.+byte-capital-a+
   (copy-mode-append-selection screen)
   (values t nil))
  ;; ── Search ────────────────────────────────────────────────────────────────
  ;; n — search next
  (#.+byte-n+
   (copy-mode-search-next screen)
   (values t nil))
  ;; N — search prev
  (#.+byte-capital-n+
   (copy-mode-search-prev screen)
   (values t nil))
  ;; / — interactive search forward prompt
  (#.+byte-slash+
   (%copy-mode-search-prompt session "/" #'copy-mode-search-forward)
   (values t nil))
  ;; ? — interactive search backward prompt
  (#.+byte-question+
   (%copy-mode-search-prompt session "?" #'copy-mode-search-backward)
   (values t nil))
  ;; C-s (19) — incremental forward search
  (19
   (copy-mode-search-forward-incremental screen)
   (values t nil))
  ;; C-r (18) — incremental backward search
  (18
   (copy-mode-search-backward-incremental screen)
   (values t nil))
  ;; ── Char-jump motions (need a second byte) ────────────────────────────────
  ;; f — jump forward to char on line (vi f<char>): arm a one-byte continuation
  (#.+byte-f+
   (setf *dirty* t)
   (values t (%copy-mode-char-jump-continuation #'copy-mode-jump-forward screen count)))
  ;; F — jump backward to char on line (vi F<char>)
  (#.+byte-capital-f+
   (setf *dirty* t)
   (values t (%copy-mode-char-jump-continuation #'copy-mode-jump-backward screen count)))
  ;; t — jump to just before next char (vi t<char>)
  (#.+byte-t+
   (setf *dirty* t)
   (values t (%copy-mode-char-jump-continuation #'copy-mode-jump-to screen count)))
  ;; T — jump to just after previous char (vi T<char>)
  (#.+byte-capital-t+
   (setf *dirty* t)
   (values t (%copy-mode-char-jump-continuation #'copy-mode-jump-to-backward screen count)))
  ;; ── Paragraph jumps ───────────────────────────────────────────────────────
  ;; { — previous-paragraph (jump to nearest blank line above)
  (123
   (%copy-mode-repeat #'copy-mode-previous-paragraph screen count)
   (values t nil))
  ;; } — next-paragraph (jump to nearest blank line below)
  (125
   (%copy-mode-repeat #'copy-mode-next-paragraph screen count)
   (values t nil))
  ;; ── Bracket matching ─────────────────────────────────────────────────────
  ;; % — jump to matching bracket
  (37
   (copy-mode-next-matching-bracket screen)
   (values t nil))
  ;; ── Jump repeat ──────────────────────────────────────────────────────────
  ;; ; — repeat last jump
  (59
   (dotimes (_ count) (copy-mode-jump-again screen))
   (values t nil))
  ;; , — reverse last jump
  (44
   (dotimes (_ count) (copy-mode-jump-reverse screen))
   (values t nil))
  ;; ── Mark operations ──────────────────────────────────────────────────────
  ;; m — set mark at current cursor position
  (109
   (copy-mode-set-mark screen)
   (values t nil))
  ;; ' — jump to mark
  (39
   (copy-mode-jump-to-mark screen)
   (values t nil)))
