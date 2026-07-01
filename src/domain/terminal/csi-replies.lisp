(in-package #:cl-tmux/terminal/csi)

;;;; CSI response-queue helper layer.
;;;;
;;;; Everything here builds and enqueues terminal reply strings (DSR, DA1/DA2/DA3,
;;;; XTVERSION, CPR, DECRQM for both DEC-private and ANSI modes, XTWINOPS size
;;;; reports) onto the screen's response queue, so the PTY loop can drain them
;;;; back to the application.  The CSI final-byte dispatch table that calls these
;;;; helpers lives in csi.lisp, which loads after this file.

;;; ── Response-queue primitive ───────────────────────────────────────────────

(declaim (inline %enqueue-reply))
(defun %enqueue-reply (screen reply)
  "Push REPLY string onto SCREEN's response queue."
  (push reply (screen-response-queue screen)))

;;; ── Fixed-string reply enqueuers ───────────────────────────────────────────
;;;
;;; define-fixed-reply-enqueuers generates the static-string variants as a
;;; Prolog-style fact table so the escape strings are interned once at load
;;; time (load-time-value).

(defmacro define-fixed-reply-enqueuers (&rest specs)
  "Generate enqueuer functions for static (load-time) reply strings.
   Each SPEC is (fn-name reply-form docstring).
   REPLY-FORM is evaluated once at load time via load-time-value."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name reply doc) spec
                   `(defun ,name (screen)
                      ,doc
                      (%enqueue-reply screen (load-time-value ,reply t)))))
               specs)))

(define-fixed-reply-enqueuers
  (enqueue-dsr-reply
   (format nil "~C[0n" #\Escape)
   "Push Device Status Report OK (ESC[0n) onto SCREEN's response queue.")
  (enqueue-da1-reply
   (format nil "~C[?1;2c" #\Escape)
   "Push Primary Device Attributes (ESC[?1;2c — VT100 with AVO) onto SCREEN's response queue.")
  (enqueue-da2-reply
   (format nil "~C[>1;10;0c" #\Escape)
   "Push Secondary Device Attributes (ESC[>1;10;0c) onto SCREEN's response queue.")
  (enqueue-da3-reply
   (format nil "~CP!|00000000~C\\" #\Escape #\Escape)
   "Push Tertiary Device Attributes (DCS P!|00000000 ST) onto SCREEN's response queue.")
  (enqueue-xtversion-reply
   (format nil "~CP>|cl-tmux ~A~C\\" #\Escape (cl-tmux/version:version-string) #\Escape)
   "Push XTVERSION reply (DCS>|cl-tmux VERSION ST) onto SCREEN's response queue."))

(defun enqueue-cpr-reply (screen)
  "Push a Cursor Position Report (ESC [ row ; col R, 1-based) onto SCREEN's
   response queue.  In DECOM origin mode (?6) the row is relative to the scroll
   region top margin (row 1 = top margin); otherwise it is absolute."
  (%enqueue-reply screen
                  (format nil "~C[~D;~DR" #\Escape
                          (if (screen-origin-mode screen)
                              (1+ (- (screen-cursor-y screen) (screen-scroll-top screen)))
                              (1+ (screen-cursor-y screen)))
                          (1+ (screen-cursor-x screen)))))

;;; ── DECRQM mode-state helpers ──────────────────────────────────────────────
;;;
;;; %decrqm-flag-code encodes a boolean flag as the DECRQM wire integer (1 =
;;; set, 2 = reset).  define-decrqm-mode-table is a Prolog-style fact table
;;; that generates %decrqm-mode-state from a declarative (mode accessor) list,
;;; with special sentinels for mouse-mode comparisons, the alt-screen predicate,
;;; and fixed values.

(defun %decrqm-flag-code (x)
  "Encode a flag for a DECRQM reply: T → 1 (set, wire code), NIL → 2 (reset).
   The wire protocol uses 1/2, not 0/1, to distinguish 'set' from 'not recognised' (0)."
  (if x 1 2))

(defmacro define-decrqm-mode-table (&rest specs)
  "Generate %DECRQM-MODE-STATE from a declarative (mode-number accessor-fn) table.
   SPECS forms:
     (mode-num accessor-fn)         — call (accessor-fn screen) and encode as flag
     (mode-num :mouse-mode N)       — flag code for (= (screen-mouse-mode screen) N)
     (mode-num :alt-screen)         — flag code for (and (screen-alt-cells screen) t)
     (mode-num :fixed code)         — always return CODE (for modes not tracked dynamically)"
  `(defun %decrqm-mode-state (screen mode)
     "DECRQM reply value for DEC private MODE: 1 = set, 2 = reset, 0 = not recognised.
      Reports from the screen's tracked mode flags so an application querying support
      gets an accurate answer; an unknown mode reports 0 (so the app falls back)."
     (case mode
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (mode-number &rest rest) spec
                     (cond
                       ;; Keyword sentinel specs must be checked BEFORE the plain-symbol
                       ;; accessor branch, because keywords satisfy (symbolp ...) too.
                       ((and (= (length rest) 2) (eq (first rest) :mouse-mode))
                        `(,mode-number (%decrqm-flag-code (= (screen-mouse-mode screen) ,(second rest)))))
                       ((and (= (length rest) 1) (eq (first rest) :alt-screen))
                        `(,mode-number (%decrqm-flag-code (and (screen-alt-cells screen) t))))
                       ((and (= (length rest) 2) (eq (first rest) :fixed))
                        `(,mode-number ,(second rest)))
                       ;; Plain symbol: an accessor function — must be a non-keyword symbol.
                       ((and (= (length rest) 1) (symbolp (first rest)) (not (keywordp (first rest))))
                        `(,mode-number (%decrqm-flag-code (,(first rest) screen))))
                       (t (error "Unrecognised define-decrqm-mode-table spec: ~S" spec)))))
                 specs)
       (t 0))))

(define-decrqm-mode-table
  (1    screen-app-cursor-keys)       ; DECCKM — application cursor keys
  (5    screen-reverse-screen)        ; DECSCNM — reverse video
  (6    screen-origin-mode)           ; DECOM — origin mode
  (7    screen-autowrap)              ; DECAWM — auto-wrap
  (25   screen-cursor-visible)        ; DECTCEM — cursor visibility
  (1000 :mouse-mode 1)                ; X10 / normal mouse
  (1002 :mouse-mode 2)                ; button-event mouse
  (1003 :mouse-mode 3)                ; any-event mouse
  (1004 screen-focus-events)          ; focus event reporting
  (1006 screen-mouse-sgr-mode)        ; SGR mouse encoding
  (47   :alt-screen)                  ; alternate screen (old form)
  (1047 :alt-screen)                  ; alternate screen (new form)
  (1049 :alt-screen)                  ; alternate screen + save cursor
  (2004 screen-bracketed-paste)       ; bracketed paste
  (2026 :fixed 2))                    ; synchronized output: not a persistent mode → always reset

(defun enqueue-decrqm-reply (screen mode)
  "Push the DECRQM report (ESC [ ? MODE ; Pm $ y) onto SCREEN's response queue,
   where Pm is %decrqm-mode-state for MODE."
  (%enqueue-reply screen
                  (format nil "~C[?~D;~D$y" #\Escape mode (%decrqm-mode-state screen mode))))

(defun %decrqm-ansi-mode-state (screen mode)
  "DECRQM reply value for an ANSI (non-private) MODE: 1 = set, 2 = reset, 0 = not
   recognised.  Covers IRM (4) and LNM (20); other ANSI modes report 0."
  (case mode
    (4  (%decrqm-flag-code (screen-insert-mode screen)))   ; IRM — insert/replace mode
    (20 (%decrqm-flag-code (screen-newline-mode screen)))  ; LNM — line feed/new line mode
    (t  0)))

(defun enqueue-decrqm-ansi-reply (screen mode)
  "Push the ANSI-mode DECRQM report (ESC [ MODE ; Pm $ y — NO ? marker) onto the
   response queue, where Pm is %decrqm-ansi-mode-state for MODE."
  (%enqueue-reply screen
                  (format nil "~C[~D;~D$y" #\Escape mode (%decrqm-ansi-mode-state screen mode))))

;;; ── XTWINOPS size-report constants ─────────────────────────────────────────
;;;
;;; XTWINOPS (CSI Ps ; … t) operations 18 and 19 query the grid size.
;;; The reply encodes the op as a different code: 18 → code 8 (text-area
;;; report), 19 → code 9 (screen report).  Named constants document the
;;; mapping so the dispatch rule and the reply encoder stay in sync.

(defconstant +xtwinops-text-area-query+ 18
  "XTWINOPS op 18: query text-area size in characters; reply uses code 8.")
(defconstant +xtwinops-screen-query+    19
  "XTWINOPS op 19: query screen size in characters; reply uses code 9.")
(defconstant +xtwinops-text-area-reply+ 8
  "XTWINOPS reply code for op 18 (text-area query): ESC [ 8 ; rows ; cols t.")
(defconstant +xtwinops-screen-reply+    9
  "XTWINOPS reply code for op 19 (screen query): ESC [ 9 ; rows ; cols t.")

(defun enqueue-xtwinops-reply (screen op)
  "Push the XTWINOPS size REPORT for operation OP onto SCREEN's response queue:
     +xtwinops-text-area-query+ (18) → ESC [ 8 ; rows ; cols t
     +xtwinops-screen-query+    (19) → ESC [ 9 ; rows ; cols t
   Only ops 18/19 (grid-size queries) produce a reply; other XTWINOPS operations
   (resize/move/iconify) are silently ignored — a multiplexer cannot resize the
   outer window and a wrong pixel size would mislead callers more than no reply."
  (let ((code (case op
                (#.+xtwinops-text-area-query+ +xtwinops-text-area-reply+)
                (#.+xtwinops-screen-query+    +xtwinops-screen-reply+))))
    (when code
      (%enqueue-reply screen (format nil "~C[~D;~D;~Dt" #\Escape
                                     code (screen-height screen) (screen-width screen))))))
