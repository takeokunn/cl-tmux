(in-package #:cl-tmux)

;;;; Copy-mode single-byte dispatch and helpers.
;;;;
;;;; This file houses the declarative copy-mode vi key table, extracted from the
;;;; monolithic %ground-input-state in events-keystroke.lisp.  It mirrors the
;;;; pattern of define-csi-rules / define-command-handlers: a top-level macro
;;;; generates a named dispatch function from an ordered table of Prolog-like rules.

;;; ── Macro: define-copy-mode-vi-rules ────────────────────────────────────────
;;;
;;; Each RULE is (BYTE-OR-BYTE-LIST ACTION...).
;;; Two shorthands reduce boilerplate in the common cases:
;;;   (PATTERN :repeat FN)  → (%copy-mode-repeat FN screen count) (values t nil)
;;;   (PATTERN :call   FN)  → (FN screen) (values t nil)
;;; For non-standard bodies (search prompts, continuations) the forms are
;;; used verbatim.  The macro generates %dispatch-copy-mode-byte which is
;;; called with a screen, a raw byte, and the already-computed repeat count.

(defmacro define-copy-mode-vi-rules (&rest rules)
  "Generate %DISPATCH-COPY-MODE-BYTE (SCREEN BYTE COUNT SESSION) from an ordered
   table of copy-mode vi rules.  Each RULE is (PATTERN BODY...) where:
     (PATTERN :repeat FN)  →  (%copy-mode-repeat FN screen count) (values t nil)
     (PATTERN :call   FN)  →  (FN screen) (values t nil)
     (PATTERN body...)     →  body... verbatim (for complex/continuation arms)
   PATTERN is an integer, a list of integers, or a read-time constant.
   The first matching PATTERN wins (ordered-cut semantics like Prolog).
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
              (let ((expanded
                     (cond
                       ;; :repeat FN — call FN N times via %copy-mode-repeat
                       ((and (= 2 (length body)) (eq (first body) :repeat))
                        `((%copy-mode-repeat ,(second body) screen count)
                          (values t nil)))
                       ;; :call #'sym — direct no-repeat call (unwrap #' for clarity)
                       ;; :call (lambda ...) — funcall
                       ((and (= 2 (length body)) (eq (first body) :call))
                        (let ((fn (second body)))
                          (if (and (consp fn) (eq (car fn) 'function))
                              `((,(cadr fn) screen) (values t nil))
                              `((funcall ,fn screen) (values t nil)))))
                       ;; Verbatim body — for search prompts, continuations, etc.
                       (t body))))
                `(,pattern ,@expanded))))
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
;;; Reads as Prolog facts: each arm maps one or more byte values to an action.
;;; :repeat FN   — move/scroll commands that honour a numeric prefix count.
;;; :call   FN   — one-shot commands (enter/exit, yank, selection toggles, marks).
;;; Verbatim bodies — search prompts (need SESSION) and char-jump continuations.

(define-copy-mode-vi-rules
  ;; ── Exit copy mode ────────────────────────────────────────────────────────
  ;; q / Q / C-c — exit copy mode
  ((#.+byte-q+ 81 3) :call #'copy-mode-exit)
  ;; i — exit copy mode (vi insert-mode key repurposed as exit)
  (105               :call #'copy-mode-exit)
  ;; ── Yank (copy) ───────────────────────────────────────────────────────────
  ;; Enter (13) / C-j (10) — copy selection and return to ground
  ((13 10) :call #'copy-mode-yank)
  ;; ── Cursor navigation ─────────────────────────────────────────────────────
  ;; h / C-h (8) — move cursor left
  ((#.+byte-h+ 8)  :repeat (lambda (s) (copy-mode-move-cursor s :left)))
  ;; l — move cursor right
  (#.+byte-l+      :repeat (lambda (s) (copy-mode-move-cursor s :right)))
  ;; j / C-n (14) — move cursor down (viewport follows at edge)
  ((#.+byte-j+ 14) :repeat (lambda (s) (copy-mode-move-cursor s :down)))
  ;; k / C-p (16) — move cursor up (viewport follows at edge)
  ((#.+byte-k+ 16) :repeat (lambda (s) (copy-mode-move-cursor s :up)))
  ;; ── Viewport scrolling ────────────────────────────────────────────────────
  ;; J — scroll-down (viewport toward newer content; vi J = C-e analogue)
  (74  :repeat #'copy-mode-scroll-down-line)
  ;; K — scroll-up (viewport toward older content; vi K = C-y analogue)
  (75  :repeat #'copy-mode-scroll-up-line)
  ;; C-f (6) — page down
  (6   :repeat #'copy-mode-page-down)
  ;; C-b (2) — page up
  (2   :repeat #'copy-mode-page-up)
  ;; C-u (21) — scroll up half page
  (21  :repeat #'copy-mode-half-page-up)
  ;; C-d (4) — scroll down half page
  (4   :repeat #'copy-mode-half-page-down)
  ;; C-e (5) — scroll down one line
  (5   :repeat #'copy-mode-scroll-down-line)
  ;; C-y (25) — scroll up one line
  (25  :repeat #'copy-mode-scroll-up-line)
  ;; ── Word motions ──────────────────────────────────────────────────────────
  ;; w — word forward
  (#.+byte-w+ :repeat #'copy-mode-word-forward)
  ;; W — WORD forward (whitespace-delimited)
  (87         :repeat #'copy-mode-space-forward)
  ;; b — word backward
  (#.+byte-b+ :repeat #'copy-mode-word-backward)
  ;; B — WORD backward (whitespace-delimited)
  (66         :repeat #'copy-mode-space-backward)
  ;; e — word end
  (#.+byte-e+ :repeat #'copy-mode-word-end)
  ;; E — WORD end (whitespace-delimited)
  (69         :repeat #'copy-mode-space-end)
  ;; ── Line position ─────────────────────────────────────────────────────────
  ;; 0 — line start (bare '0' with no prefix)
  (#.+byte-digit-0+ :call #'copy-mode-line-start)
  ;; ^ — back-to-indentation (first non-blank)
  (94               :call #'copy-mode-back-to-indentation)
  ;; $ — line end
  (#.+byte-dollar+  :call #'copy-mode-line-end)
  ;; ── Jump to scrollback extremes ───────────────────────────────────────────
  ;; g — jump to top (maximum scrollback)
  (#.+byte-g+      :call #'copy-mode-top)
  ;; G — jump to bottom (offset = 0, live view)
  (71              :call #'copy-mode-bottom)
  ;; H — cursor to top of screen
  (#.+byte-capital-h+  :call #'copy-mode-high)
  ;; M — cursor to middle of screen
  (#.(char-code #\M)   :call #'copy-mode-middle)
  ;; L — cursor to bottom of screen
  (#.+byte-capital-l+  :call #'copy-mode-low)
  ;; ── Scroll centering ─────────────────────────────────────────────────────
  ;; z — scroll-middle: scroll viewport so cursor row is centered
  (122 :call #'copy-mode-scroll-middle)
  ;; ── Selection ─────────────────────────────────────────────────────────────
  ;; V — begin line selection
  (#.+byte-capital-v+       :call #'copy-mode-begin-line-selection)
  ;; Space / v — begin selection
  ((#.+byte-space+ #.+byte-v+) :call #'copy-mode-begin-selection)
  ;; o / O — swap mark and cursor ends of selection
  ((111 79) :call #'copy-mode-other-end)
  ;; C-v (22) — toggle rectangle select
  (22       :call #'copy-mode-toggle-rectangle)
  ;; ── Copy actions ──────────────────────────────────────────────────────────
  ;; y — yank selection
  (#.+byte-y+       :call #'copy-mode-yank)
  ;; D — copy to end of line, pipe if configured, and cancel
  (#.+byte-capital-d+ (copy-mode-copy-pipe-end-of-line screen nil) (values t nil))
  ;; Y — copy current line
  (#.+byte-capital-y+ :call #'copy-mode-copy-line)
  ;; A — append selection to paste buffer and cancel
  (#.+byte-capital-a+ :call #'copy-mode-append-selection)
  ;; ── Search ────────────────────────────────────────────────────────────────
  ;; # — search backward for word under cursor
  (35 :call #'cl-tmux/commands::copy-mode-search-backward-word)
  ;; * — search forward for word under cursor
  (42 :call #'cl-tmux/commands::copy-mode-search-forward-word)
  ;; n — search next
  (#.+byte-n+         :call #'copy-mode-search-next)
  ;; N — search prev
  (#.+byte-capital-n+ :call #'copy-mode-search-prev)
  ;; / — interactive search forward prompt (needs session — verbatim body)
  (#.+byte-slash+
   (%copy-mode-search-prompt session "/" #'copy-mode-search-forward)
   (values t nil))
  ;; ? — interactive search backward prompt (needs session — verbatim body)
  (#.+byte-question+
   (%copy-mode-search-prompt session "?" #'copy-mode-search-backward)
   (values t nil))
  ;; C-s (19) — incremental forward search
  (19 :call #'copy-mode-search-forward-incremental)
  ;; C-r (18) — incremental backward search
  (18 :call #'copy-mode-search-backward-incremental)
  ;; ── Char-jump motions (need a second byte — verbatim continuation bodies) ─
  ;; f — jump forward to char on line (vi f<char>)
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
  (123 :repeat #'copy-mode-previous-paragraph)
  ;; } — next-paragraph (jump to nearest blank line below)
  (125 :repeat #'copy-mode-next-paragraph)
  ;; ── Bracket matching ──────────────────────────────────────────────────────
  ;; % — jump to matching bracket
  (37 :call #'copy-mode-next-matching-bracket)
  ;; ── Jump repeat ───────────────────────────────────────────────────────────
  ;; ; — repeat last jump
  (59 :repeat #'copy-mode-jump-again)
  ;; , — reverse last jump
  (44 :repeat #'copy-mode-jump-reverse)
  ;; ── Mark operations ───────────────────────────────────────────────────────
  ;; m — set mark at current cursor position
  (109 :call #'copy-mode-set-mark)
  ;; ' — jump to mark
  (39  :call #'copy-mode-jump-to-mark))
