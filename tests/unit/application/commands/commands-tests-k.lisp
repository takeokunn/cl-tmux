(in-package #:cl-tmux/test)

;;;; commands tests — part K: copy-mode-begin-line-selection, copy-end-of-line (D),
;;;; copy-line (Y), search-forward/backward, wrap-search, search-across-scrollback.

(defmacro define-copy-mode-search-cases (&body cases)
  `(progn
     ,@(loop for case in cases
             for name = (first case)
             for options = (cddr case)
             for width = (or (getf options :width) 30)
             for height = (or (getf options :height) 5)
             for fixture = (getf options :fixture)
             for cursor = (getf options :cursor)
             for action = (getf options :action)
             for expectations = (getf options :expectations)
             for wrap-search = (getf options :wrap-search)
             for body = `(let ((s (make-screen ,width ,height)))
                           ,fixture
                           (cl-tmux/commands::copy-mode-enter s)
                           ,@(when cursor
                               `((setf (cl-tmux/terminal/types:screen-copy-cursor s)
                                       ,cursor)))
                           ,action
                           (%check-copy-mode-search-expectations s ',expectations))
             collect
             `(it ,(string-downcase (symbol-name name))
                ,(if (eq wrap-search :off)
                     `(with-isolated-options ("wrap-search" nil)
                        ,body)
                     body)))))

(describe "commands-suite"

  ;; ── copy-mode-begin-line-selection ──────────────────────────────────────────

  ;; copy-mode-begin-line-selection sets line-selection-p and activates the selection.
  (it "copy-mode-begin-line-selection-sets-line-selection-p"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
      (cl-tmux/commands::copy-mode-begin-line-selection s)
      (expect (cl-tmux/terminal/types:screen-copy-line-selection-p s) :to-be-truthy)
      (expect (cl-tmux/terminal/types:screen-copy-selecting s) :to-be-truthy)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-mark s))))
      (expect (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-begin-line-selection is a no-op when not in copy mode.
  (it "copy-mode-begin-line-selection-noop-outside-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
        ;; Do NOT enter copy mode.
        (cl-tmux/commands::copy-mode-begin-line-selection s)
        (expect (cl-tmux/terminal/types:screen-copy-line-selection-p s) :to-be-falsy)
        (expect (cl-tmux/terminal/types:screen-copy-selecting s) :to-be-falsy))))

  ;; ── copy-mode-copy-end-of-line (D) ──────────────────────────────────────────

  ;; copy-mode-copy-end-of-line copies text from cursor to end of row and exits.
  (it "copy-mode-copy-end-of-line-yanks-from-cursor"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello world")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 6))
        (cl-tmux/commands::copy-mode-copy-end-of-line s)
        (expect (screen-copy-mode-p s) :to-be-truthy)
        (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
          (expect (and yanked (string= "world" yanked)))))))


  ;; ── copy-mode-copy-line (Y) ──────────────────────────────────────────────────

  ;; copy-mode-copy-line copies the full current row content and exits.
  (it "copy-mode-copy-line-yanks-full-row"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 10))
        (cl-tmux/commands::copy-mode-copy-line s)
        (expect (screen-copy-mode-p s) :to-be-truthy)
        (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
          (expect (and yanked (search "hello" yanked)))))))

  ;; copy-mode-copy-end-of-line and copy-mode-copy-line leave paste-buffers empty outside copy mode.
  (it "copy-mode-copy-yank-noop-outside-copy-mode-table"
    (dolist (c '((cl-tmux/commands::copy-mode-copy-end-of-line "copy-end-of-line")
                 (cl-tmux/commands::copy-mode-copy-line        "copy-line")))
      (destructuring-bind (fn desc) c
        (declare (ignore desc))
        (let ((cl-tmux/buffer:*paste-buffers* nil))
          (let ((s (make-screen 20 5)))
            (feed s "hello world")
            (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
            (funcall fn s)
            (expect (null cl-tmux/buffer:*paste-buffers*)))))))

  ;; ── copy-mode-search-forward / search-backward ──────────────────────────────

  (defun %check-copy-mode-search-expectations (screen expectations)
    (dolist (expectation expectations)
      (destructuring-bind (kind expected desc) expectation
        (declare (ignore desc))
        (ecase kind
          (:cursor
           (expect (equal expected (cl-tmux/terminal/types:screen-copy-cursor screen))))
          (:cursor-col
           (expect (= expected (cdr (cl-tmux/terminal/types:screen-copy-cursor screen)))))
          (:offset
           (expect (= expected (cl-tmux/terminal/types:screen-copy-offset screen))))
          (:search-term
           (expect (string= expected (cl-tmux/terminal/types:screen-copy-search-term screen))))))))

  (define-copy-mode-search-cases
    (copy-mode-search-forward-finds-term
     "copy-mode-search-forward moves cursor to the first match after current position."
     :fixture (feed s "abc def abc")
     :cursor (cons 0 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "abc")
     :expectations ((:cursor-col 8 "search-forward must find second 'abc' at col 8")))
    (copy-mode-search-forward-saves-term
     "copy-mode-search-forward saves the search term for n/N repeats."
     :fixture (feed s "foo bar foo")
     :cursor (cons 0 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "foo")
     :expectations ((:search-term "foo" "search term must be saved after search-forward")))
    (copy-mode-search-backward-finds-term
     "copy-mode-search-backward moves cursor to the nearest match before current position."
     :fixture (feed s "abc def abc")
     :cursor (cons 0 11)
     :action (cl-tmux/commands::copy-mode-search-backward s "abc")
     :expectations ((:cursor-col 8 "search-backward must find 'abc' at col 8")))
    (copy-mode-search-forward-regex-dot
     "search-forward treats the term as a regex: 'a.c' matches 'abc'."
     :fixture (feed s "xy abc z")
     :cursor (cons 0 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "a.c")
     :expectations ((:cursor-col 3 "regex a.c must match 'abc' at col 3")))
    (copy-mode-search-forward-regex-char-class
     "search-forward regex character class '[0-9]+' finds the first digit run."
     :fixture (feed s "abc 123 def")
     :cursor (cons 0 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "[0-9]+")
     :expectations ((:cursor-col 4 "regex [0-9]+ must match '123' starting at col 4")))
    (copy-mode-search-invalid-regex-falls-back-to-literal
     "An invalid regex falls back to a literal substring search."
     :fixture (feed s "a (b) c")
     :cursor (cons 0 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "(")
     :expectations ((:cursor-col 2 "literal '(' must be found at col 2")))
    (copy-mode-search-forward-word-searches-literal-word
     "copy-mode-search-forward-word searches for the literal word under the cursor."
     :fixture (feed s "xx a.b aXb a.b")
     :cursor (cons 0 3)
     :action (cl-tmux/commands::copy-mode-search-forward-word s)
     :expectations ((:cursor-col 11 "forward word search must land on the next literal match")
                    (:search-term "a\\.b" "forward word search must save the escaped literal term")))
    (copy-mode-search-backward-word-searches-literal-word
     "copy-mode-search-backward-word searches for the literal word under the cursor."
     :fixture (feed s "xx a.b aXb a.b")
     :cursor (cons 0 12)
     :action (cl-tmux/commands::copy-mode-search-backward-word s)
     :expectations ((:cursor-col 11 "backward word search must land on the nearest literal match")
                    (:search-term "a\\.b" "backward word search must save the escaped literal term"))))

  ;; ── wrap-search: search wraps around the buffer ends (default on) ────────────

  (define-copy-mode-search-cases
    (copy-mode-search-forward-wraps-to-top
     "With wrap-search on, forward search wraps to the first match in the buffer."
     :fixture (feed s "abc")
     :cursor (cons 2 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "abc")
     :expectations ((:cursor (0 . 0) "no match below -> wrap to row 0 col 0")))
    (copy-mode-search-forward-no-wrap-when-off
     "With wrap-search off, forward search with no lower match leaves the cursor."
     :wrap-search :off
     :fixture (feed s "abc")
     :cursor (cons 2 0)
     :action (cl-tmux/commands::copy-mode-search-forward s "abc")
     :expectations ((:cursor (2 . 0) "wrap-search off -> cursor stays put")))
    (copy-mode-search-backward-wraps-to-bottom
     "With wrap-search on, backward search wraps to the last match in the buffer."
     :fixture (feed-lines s "" "" "" "" "abc")
     :cursor (cons 0 0)
     :action (cl-tmux/commands::copy-mode-search-backward s "abc")
     :expectations ((:cursor (4 . 0) "no match above -> wrap to row 4 col 0")))
    (copy-mode-search-backward-regex
     "search-backward matches a regex and finds the nearest match before the cursor."
     :fixture (feed s "a1b a2b a3b")
     :cursor (cons 0 11)
     :action (cl-tmux/commands::copy-mode-search-backward s "a.b")
     :expectations ((:cursor-col 8 "regex a.b backward must find the last match before col 11")))
    (copy-mode-search-next-repeats-forward
     "copy-mode-search-next repeats forward search and wraps when needed."
     :fixture (feed s "abc def abc")
     :cursor (cons 0 0)
     :action (progn
               (cl-tmux/commands::copy-mode-search-forward s "abc")
               (expect (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
               (cl-tmux/commands::copy-mode-search-next s))
     :expectations ((:cursor-col 0 "search-next wraps to the first match")))
    (copy-mode-search-prev-noop-without-term
     "copy-mode-search-prev does nothing when no search term is saved."
     :width 20
     :cursor (cons 0 5)
     :action (progn
               (setf (cl-tmux/terminal/types:screen-copy-search-term s) nil)
               (cl-tmux/commands::copy-mode-search-prev s))
     :expectations ((:cursor-col 5 "search-prev must not move cursor when no term is saved"))))

  ;; ── copy-mode search across scrollback boundary ─────────────────────────────

  (defun %make-text-row (width text)
    "Create a scrollback row vector WIDTH wide with TEXT followed by space cells."
    (let ((row (make-array width
                           :initial-element
                           (cl-tmux/terminal/types:make-cell
                            :char #\Space :fg 7 :bg 0 :attrs 0 :width 1))))
      (loop for i from 0 below (min (length text) width)
            do (setf (aref row i)
                     (cl-tmux/terminal/types:make-cell
                      :char (char text i) :fg 7 :bg 0 :attrs 0 :width 1)))
      row))

  ;; Forward search with wrap-search wraps from the live grid into the scrollback buffer
  ;; when the term is only present in the scrollback.
  (it "copy-mode-search-forward-wraps-into-scrollback"
    ;; Screen 20x3; scrollback newest-first: sb[0]=row with term, sb[1]=blank.
    ;; Virtual rows: vrow0=sb[1](blank), vrow1=sb[0](term), vrow2-4=live(blank).
    (let* ((s    (make-screen 20 3))
           (sb0  (%make-text-row 20 "findme here"))
           (sb1  (%make-text-row 20 "")))
      (setf (cl-tmux/terminal/types:screen-scrollback s) (list sb0 sb1))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Cursor starts at bottom of live grid (row 2, col 0), offset 0.
      (cl-tmux/commands::copy-mode-search-forward s "findme")
      ;; After wrap the term is at virtual row 1 (newest scrollback); set_vrow
      ;; sets offset=1, cursor-row=0.
      (expect (= 1 (cl-tmux/terminal/types:screen-copy-offset s)))
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; Backward search from the live grid finds a term in the scrollback without wrapping.
  (it "copy-mode-search-backward-finds-term-in-scrollback"
    ;; Screen 20x3; sb[0]=newest='target row', sb[1]=oldest=blank.
    ;; Cursor at live-grid top (row 0, offset 0).
    (let* ((s    (make-screen 20 3))
           (sb0  (%make-text-row 20 "target row"))
           (sb1  (%make-text-row 20 "")))
      (setf (cl-tmux/terminal/types:screen-scrollback s) (list sb0 sb1))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-search-backward s "target")
      ;; target is in vrow 1 (newest scrollback); set_vrow → offset=1, row=0.
      (expect (= 1 (cl-tmux/terminal/types:screen-copy-offset s)))
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))))
