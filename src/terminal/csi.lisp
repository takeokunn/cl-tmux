(in-package #:cl-tmux/terminal/csi)

;;;; CSI (Control Sequence Introducer) macro-driven dispatch.
;;;;
;;;; define-csi-rules builds a COND-based dispatcher keyed on the final
;;;; character and optional intermediate character of the escape sequence.
;;;; execute-csi is the public entry point called by the parser.

;;; ── Macro ──────────────────────────────────────────────────────────────────

(defmacro define-csi-rules (&rest rules)
  "Each RULE is (condition-form &body forms).
   Available bindings in every rule body:
     SCREEN   – the screen struct
     FINAL    – the sequence final character (type character)
     INTERMED – intermediate character (character or nil; e.g. #\\? for DEC)
     PARAMS   – full parameter list (list of fixnum)
     P1       – first  parameter or 0
     P2       – second parameter or 0
     P1*      – (max 1 p1)
     P2*      – (max 1 p2)
   Expands into a DEFUN for EXECUTE-CSI that dispatches via COND."
  `(defun execute-csi (screen final intermed params)
     (declare (type screen screen)
              (type character final)
              (ignorable intermed))
     (let* ((p1  (or (first  params) 0))
            (p2  (or (second params) 0))
            (p1* (max 1 p1))
            (p2* (max 1 p2)))
       (declare (type fixnum p1 p2 p1* p2*) (ignorable p1 p2 p1* p2*))
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (condition &rest body) rule
                       `(,condition ,@body)))
                   rules)
         (t (values))))))

;;; All grid mutations (insert/delete chars, scroll-region margins, alternate
;;; screen) live in cl-tmux/terminal/actions; the rule table below calls them
;;; directly.  DECSTBM parameters arrive 1-based, so they are converted to the
;;; 0-based inclusive margins that ACTIONS:DECSTBM expects at the call site.

;;; ── CSI rule table ─────────────────────────────────────────────────────────

(define-csi-rules

  ;; CUU – Cursor Up
  ((and (null intermed) (char= final #\A))
   (set-cursor screen (screen-cx screen) (- (screen-cy screen) p1*)))

  ;; CUD – Cursor Down
  ((and (null intermed) (char= final #\B))
   (set-cursor screen (screen-cx screen) (+ (screen-cy screen) p1*)))

  ;; CUF – Cursor Forward (right)
  ((and (null intermed) (char= final #\C))
   (set-cursor screen (+ (screen-cx screen) p1*) (screen-cy screen)))

  ;; CUB – Cursor Back (left)
  ((and (null intermed) (char= final #\D))
   (set-cursor screen (- (screen-cx screen) p1*) (screen-cy screen)))

  ;; CNL – Cursor Next Line
  ((and (null intermed) (char= final #\E))
   (set-cursor screen 0 (+ (screen-cy screen) p1*)))

  ;; CPL – Cursor Preceding Line
  ((and (null intermed) (char= final #\F))
   (set-cursor screen 0 (- (screen-cy screen) p1*)))

  ;; CHA – Cursor Horizontal Absolute (1-based column)
  ((and (null intermed) (char= final #\G))
   (set-cursor screen (1- p1*) (screen-cy screen)))

  ;; CUP – Cursor Position (row P1, col P2, 1-based)
  ((and (null intermed) (char= final #\H))
   (set-cursor screen (1- p2*) (1- p1*)))

  ;; ICH – Insert Characters
  ((and (null intermed) (char= final #\@))
   (insert-chars screen p1*))

  ;; ED – Erase in Display
  ((and (null intermed) (char= final #\J))
   (erase-display screen p1))

  ;; EL – Erase in Line
  ((and (null intermed) (char= final #\K))
   (erase-line screen p1))

  ;; IL – Insert Lines at the cursor row
  ((and (null intermed) (char= final #\L))
   (insert-lines screen p1*))

  ;; DL – Delete Lines at the cursor row
  ((and (null intermed) (char= final #\M))
   (delete-lines screen p1*))

  ;; DCH – Delete Characters
  ((and (null intermed) (char= final #\P))
   (delete-chars screen p1*))

  ;; SU – Scroll Up
  ((and (null intermed) (char= final #\S))
   (dotimes (_ p1*) (scroll-up-one screen)))

  ;; SD – Scroll Down
  ((and (null intermed) (char= final #\T))
   (dotimes (_ p1*) (scroll-down-one screen)))

  ;; ECH – Erase Characters (fill with blanks, no shift)
  ((and (null intermed) (char= final #\X))
   (erase-region screen
                 (screen-cx screen) (screen-cy screen)
                 (min (+ (screen-cx screen) p1* -1)
                      (1- (screen-width screen)))
                 (screen-cy screen)))

  ;; REP – Repeat Preceding Character (CSI Ps b)
  ;; Repeats the last printed character P1* times.  The preceding character is
  ;; tracked via the SCREEN-LAST-CHAR slot; if nothing has been written yet
  ;; the sequence is a no-op, which matches xterm behaviour.
  ((and (null intermed) (char= final #\b))
   (let ((ch (screen-last-char screen)))
     (when ch
       (dotimes (_ p1*) (write-char-at-cursor screen ch)))))

  ;; VPA – Vertical Position Absolute (1-based row)
  ((and (null intermed) (char= final #\d))
   (set-cursor screen (screen-cx screen) (1- p1*)))

  ;; HVP – Horizontal and Vertical Position (same as CUP)
  ((and (null intermed) (char= final #\f))
   (set-cursor screen (1- p2*) (1- p1*)))

  ;; SGR – Select Graphic Rendition
  ((and (null intermed) (char= final #\m))
   (apply-sgr screen params))

  ;; DSR – Device Status Report; we do not send replies from inside the emulator
  ((and (null intermed) (char= final #\n) (= p1 5))
   (values))

  ;; DECSTBM – Set Top and Bottom Margins.  Params are 1-based; an omitted
  ;; bottom (p2 = 0) means "full screen", matching real-terminal ESC[r reset.
  ((and (null intermed) (char= final #\r))
   (decstbm screen
            (1- (max 1 p1))
            (if (zerop p2) (1- (screen-height screen)) (1- p2))))

  ;; CHT – Cursor Forward Tabulation (CSI N I)
  ((and (null intermed) (char= final #\I))
   (cursor-cht screen p1*))

  ;; CBT – Cursor Backward Tabulation (CSI N Z)
  ((and (null intermed) (char= final #\Z))
   (cursor-cbt screen p1*))

  ;; DA1 – Primary Device Attributes (CSI c or CSI 0 c)
  ;; Response: ESC [ ? 1 ; 2 c  (VT100 with AVO)
  ((and (null intermed) (char= final #\c))
   (push (format nil "~C[?1;2c" #\Escape)
         (screen-response-queue screen)))

  ;; DA2 – Secondary Device Attributes (CSI > c or CSI > 0 c)
  ;; Response: ESC [ > 1 ; 10 ; 0 c
  ((and (eql intermed #\>) (char= final #\c))
   (push (format nil "~C[>1;10;0c" #\Escape)
         (screen-response-queue screen)))

  ;; DEC Private Mode Set (?...h) — e.g. ?1049h enters the alternate screen
  ((and (eql intermed #\?) (char= final #\h))
   (dec-pm-set screen params))

  ;; DEC Private Mode Reset (?...l) — e.g. ?1049l exits the alternate screen
  ((and (eql intermed #\?) (char= final #\l))
   (dec-pm-reset screen params))

  ;; DECSCUSR — cursor shape: CSI N SP q (intermediate = space, final = q)
  ((and (eql intermed #\Space) (char= final #\q))
   (setf (screen-cursor-shape screen) (clamp p1 0 6))))
