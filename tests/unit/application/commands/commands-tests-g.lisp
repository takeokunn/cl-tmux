(in-package #:cl-tmux/test)

;;;; command tokenizer, kill-window reselection, copy-mode find, join-pane tests

;;; ── kill-window reselection: tmux session_detach order ───────────────────────
;;; %window-after-kill matches tmux: the last-used (MRU) window first
;;; (session_last), else the previous window by index, wrapping to the highest id
;;; (session_previous).  Verified against tmux source (session.c) via deepwiki.

(defun %make-window-after-kill-window (spec)
  (destructuring-bind (id &optional (last-active-time 0)) spec
    (make-window :id id
                 :name (format nil "w~D" id)
                 :width 20
                 :height 5
                 :panes nil
                 :last-active-time last-active-time)))

(defun %window-after-kill-case-values (window-specs killed-id expected-id)
  (let* ((windows (mapcar #'%make-window-after-kill-window window-specs))
         (expected (find expected-id windows :key #'window-id)))
    (values (cl-tmux/commands::%window-after-kill windows killed-id)
            expected)))

(defmacro define-window-after-kill-cases (&body cases)
  `(progn
     ,@(loop for (name doc window-specs killed-id expected-id desc) in cases
             collect
             `(it ,(string-downcase (symbol-name name))
                (multiple-value-bind (actual expected)
                    (%window-after-kill-case-values ',window-specs
                                                    ,killed-id
                                                    ,expected-id)
                  (expect (eq expected actual)))))))

;;; ── %copy-mode-find-forward / %copy-mode-find-backward ──────────────────────

(defun %copy-mode-find-result (fn width height text term row col)
  (let ((s (make-screen width height)))
    (feed s text)
    (cl-tmux/commands::copy-mode-enter s)
    (funcall fn s term row col)))

(defun %check-copy-mode-find-case (width height text term case)
  (destructuring-bind (fn srow scol expected-row expected-col desc) case
    (multiple-value-bind (row col)
        (%copy-mode-find-result fn width height text term srow scol)
      (check-table
       (list (list row expected-row
                   (format nil "~A: row (got ~S)" desc row))
             (list col expected-col
                   (format nil "~A: col (got ~S)" desc col)))
       :test #'equal))))

(defmacro define-copy-mode-find-cases (&body cases)
  `(progn
     ,@(loop for (name doc width height text term rows) in cases
             collect
             `(it ,(string-downcase (symbol-name name))
                (dolist (case ',rows)
                  (%check-copy-mode-find-case ,width ,height ,text ,term case))))))

;;; ── join-pane / move-pane (scriptable %cmd-join-pane-arg) ────────────────────

(defun %join-arg-fixture (&key (width 18) (height 14))
  "Two single-pane windows (\"src\", \"dst\") in one session, dst current.
   Returns (values sess src-win src-pane dst-win dst-pane)."
  (let* ((src-pane (%make-test-pane :id 1))
         (dst-pane (%make-test-pane :id 2))
         (src-win  (make-window :id 1 :name "src" :width width :height height
                                :tree (make-layout-leaf src-pane) :panes (list src-pane)))
         (dst-win  (make-window :id 2 :name "dst" :width width :height height
                                :tree (make-layout-leaf dst-pane) :panes (list dst-pane)))
         (sess     (make-session :id 1 :name "0" :windows (list src-win dst-win))))
    (session-select-window sess dst-win)          ; current window = dst
    (window-select-pane src-win src-pane)
    (window-select-pane dst-win dst-pane)
    (values sess src-win src-pane dst-win dst-pane)))

(defun %join-command-args (&rest flags)
  (list* "-s" ":src" "-t" ":dst" flags))

(defun %pane-in-window-p (pane window)
  (not (null (member pane (window-panes window)))))

(defmacro with-join-command-fixture ((sess src-win src-pane dst-win dst-pane)
                                     &body body)
  `(multiple-value-bind (,sess ,src-win ,src-pane ,dst-win ,dst-pane)
       (%join-arg-fixture)
     (with-registered-sessions (("0" ,sess))
       (let ((cl-tmux::*dirty* nil))
         ,@body))))

(defparameter *cmd-join-pane-before-cases*
  '((:h "-b on a horizontal split" pane-x pane-height window-height)
    (:v "-b on a vertical split"   pane-y pane-width  window-width)))

(defparameter *cmd-join-pane-full-window-cases*
  '((:h "-f on a horizontal split" pane-height window-height)
    (:v "-f on a vertical split"   pane-width  window-width)))

(defparameter *cmd-join-pane-size-hint-cases*
  '((:h "-l 8 on a horizontal split" pane-width 9 8)
    (:v "-l 8 on a vertical split"   pane-height 5 8)))

(defun %join-command-direction-flag (direction)
  (ecase direction
    (:h "-h")
    (:v "-v")))

(defun %check-cmd-join-pane-before-case (case)
  (destructuring-bind (direction desc pos-access cross-access window-cross-access) case
    (with-join-command-fixture (sess src-win src-pane dst-win dst-pane)
      (declare (ignore src-win))
      (let ((result (cl-tmux::%cmd-join-pane-arg
                     sess
                     (%join-command-args "-b" (%join-command-direction-flag direction)))))
        (check-table
         (list (list (not (null result)) t
                     (format nil "~A must accept the -b layout flag" desc))
               (list (= 0 (funcall pos-access src-pane)) t
                     (format nil "~A must place the moved pane first" desc))
               (list (> (funcall pos-access dst-pane)
                        (funcall pos-access src-pane))
                     t
                     (format nil "~A must place the destination pane after the moved pane" desc))
               (list (= (funcall window-cross-access dst-win)
                        (funcall cross-access src-pane)
                        (funcall cross-access dst-pane))
                     t
                     (format nil "~A must keep the split full on the cross axis" desc))
               (list cl-tmux::*dirty* t
                     (format nil "~A must mark the model dirty" desc)))
         :test #'eq)))))

(defun %check-cmd-join-pane-full-window-case (case)
  (destructuring-bind (direction desc pane-access window-access) case
    (with-join-command-fixture (sess src-win src-pane dst-win dst-pane)
      (declare (ignore src-win))
      (let ((result (cl-tmux::%cmd-join-pane-arg
                     sess
                     (%join-command-args "-f" (%join-command-direction-flag direction)))))
        (check-table
         (list (list (not (null result)) t
                     (format nil "~A must accept the -f layout flag" desc))
               (list (= (funcall window-access dst-win)
                        (funcall pane-access src-pane)
                        (funcall pane-access dst-pane))
                     t
                     (format nil "~A must span the full window on the split axis" desc))
               (list cl-tmux::*dirty* t
                     (format nil "~A must mark the model dirty" desc)))
         :test #'eq)))))

(defun %check-cmd-join-pane-size-hint-case (case)
  (destructuring-bind (direction desc pane-access other-size expected-size) case
    (with-join-command-fixture (sess src-win src-pane dst-win dst-pane)
      (declare (ignore src-win))
      (let ((result (cl-tmux::%cmd-join-pane-arg
                     sess
                     (%join-command-args "-l" "8" (%join-command-direction-flag direction)))))
        (check-table
         (list (list (not (null result)) t
                     (format nil "~A must accept the -l layout flag" desc))
               (list (equal (sort (list (funcall pane-access src-pane)
                                        (funcall pane-access dst-pane))
                                  #'<)
                            (sort (list expected-size other-size) #'<))
                     t
                     (format nil "~A must honor the requested size hint" desc))
               (list cl-tmux::*dirty* t
                     (format nil "~A must mark the model dirty" desc)))
         :test #'eq)))))

(describe "commands-suite"

  ;;; ── tokenize-command-string (shell-style command lexer) ──────────────────────

  ;; tokenize-command-string splits on whitespace; handles quoted spans, escapes, and unterminated quotes.
  (it "tokenize-command-string-table"
    (dolist (c '(("a b c"          ("a" "b" "c")  "basic whitespace split")
                 ("  a   b  "      ("a" "b")       "leading/trailing collapses")
                 ("   "            ()              "all-whitespace → empty list")
                 ("'a b' c"        ("a b" "c")     "space inside single quotes is literal")
                 ("'a\\b'"         ("a\\b")        "backslash in single quotes is literal")
                 ("''"             ("")            "empty single-quoted token")
                 ("\"a b\""        ("a b")         "space inside double quotes kept")
                 ("\"a\\\"b\""     ("a\"b")        "escaped double-quote inside double quotes")
                 ("a\\ b"          ("a b")         "backslash-space joins one argument")
                 ("a\\b"           ("ab")          "bare backslash-char collapses")
                 ("foo\"bar baz\"" ("foobar baz")  "adjacent spans concatenate")
                 ("'ab'' cd'"      ("ab cd")       "adjacent single-quoted spans join")
                 ("'a b"           ("a b")         "unterminated single quote")
                 ("\"xy"           ("xy")          "unterminated double quote")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/commands:tokenize-command-string input))))))

  ;;; ── add-message-log ──────────────────────────────────────────────────────────

  ;; add-message-log prepends a (timestamp . text) cons to *message-log*.
  (it "add-message-log-prepends-entry"
    (let ((cl-tmux::*message-log* nil))
      (cl-tmux::add-message-log "first-message")
      (expect cl-tmux::*message-log* :to-be-truthy)
      (expect (string= "first-message" (cdr (first cl-tmux::*message-log*))))))

  ;; add-message-log caps *message-log* at the message-limit option, not a constant.
  (it "add-message-log-caps-honors-message-limit-option"
    (with-isolated-options ("message-limit" 3)
      (let ((cl-tmux::*message-log* nil))
        (loop repeat 10 do (cl-tmux::add-message-log "x"))
        (expect (= 3 (length cl-tmux::*message-log*))))))

  ;; add-message-log puts newest entry first.
  (it "add-message-log-ordering"
    (let ((cl-tmux::*message-log* nil))
      (cl-tmux::add-message-log "first")
      (cl-tmux::add-message-log "second")
      (expect (string= "second" (cdr (first cl-tmux::*message-log*))))))

  (define-window-after-kill-cases
    (window-after-kill-prefers-mru
     "The last-used (MRU) window -- strictly greatest positive last-active-time --
      is selected first, regardless of id distance (tmux session_last)."
     ((0 100) (3 300) (7 200))
     5
     3
     "MRU window (w3, latest last-active-time) wins over id distance")
    (window-after-kill-previous-by-index-without-mru
     "With no focus history (all timestamps 0), falls back to the previous window
      by index: the greatest id strictly less than the killed id."
     ((1) (3) (8))
     5
     3
     "picks w3 (greatest id < 5)")
    (window-after-kill-differs-from-old-nearest
     "Regression: killed-id=2, remaining {1,3}, no MRU: tmux picks the PREVIOUS
      (w1), not the old numerically-nearest higher-id tie."
     ((1) (3))
     2
     1
     "previous-by-index picks w1; old %nearest-window picked w3")
    (window-after-kill-previous-wraps-to-highest
     "When no window has a lower id than the killed one, previous-by-index wraps
      to the HIGHEST id (tmux session_previous wrap)."
     ((2) (5) (8))
     0
     8
     "wraps to the highest-id window (w8) when killed was the lowest")
    (window-after-kill-mru-tie-falls-back-to-index
     "A TIE at the greatest last-active-time is no unambiguous last-used window,
      so the selection falls back to previous-by-index."
     ((1 50) (4 50))
     3
     1
     "tie at max time -> previous-by-index picks w1 (greatest id < 3)")
    (window-after-kill-single-window
     "A single remaining window is always selected."
     ((2))
     99
     2
     "the sole remaining window is selected regardless of killed id"))

  ;;; ── %copy-mode-find-forward / %copy-mode-find-backward ──────────────────────

  (define-copy-mode-find-cases
    (copy-mode-find-locates-term
     "%copy-mode-find-forward and %copy-mode-find-backward both find TERM in 'abc def abc'."
     30
     5
     "abc def abc"
     "abc"
     ((cl-tmux/commands::%copy-mode-find-forward  0 1  0 8 "forward from col 1 finds second 'abc' at col 8")
      (cl-tmux/commands::%copy-mode-find-backward 0 11 0 8 "backward from col 11 finds 'abc' at col 8")))
    (copy-mode-find-no-match-returns-nil-nil
     "%copy-mode-find-forward and %copy-mode-find-backward both return (values nil nil) when no match exists."
     20
     5
     "hello world"
     "zzz"
     ((cl-tmux/commands::%copy-mode-find-forward  0 0 nil nil "forward: no match")
      (cl-tmux/commands::%copy-mode-find-backward 0 5 nil nil "backward: no match"))))

  ;;; ── join-pane ────────────────────────────────────────────────────────────────

  ;; join-pane removes SRC-PANE from SRC-WINDOW and inserts it into DST-WINDOW.
  (it "join-pane-moves-pane-into-destination-window"
    (let* ((src-pane (%make-test-pane :id 1))
           (dst-pane (%make-test-pane :id 2))
           (src-win  (make-window :id 1 :name "src" :width 20 :height 5
                                  :tree (make-layout-leaf src-pane)
                                  :panes (list src-pane)))
           (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                  :tree (make-layout-leaf dst-pane)
                                  :panes (list dst-pane)))
           (sess     (make-session :id 1 :name "0"
                                   :windows (list src-win dst-win))))
      (session-select-window sess src-win)
      (window-select-pane src-win src-pane)
      (window-select-pane dst-win dst-pane)
      (let ((result (cl-tmux/commands:join-pane sess src-win src-pane dst-win :h)))
        (expect (eq src-pane result))
        ;; src-window had only one pane -- it must have been killed.
        (expect (member src-win (session-windows sess)) :to-be-falsy)
        ;; dst-window must now contain both dst-pane and src-pane.
        (expect (member src-pane (window-panes dst-win))))))

  ;; join-pane returns NIL immediately when any required argument is NIL.
  (it "join-pane-returns-nil-on-nil-args"
    (expect (null (cl-tmux/commands:join-pane nil nil nil nil :h))))

  ;; join-pane -s SRC -t DST moves SRC's active pane into DST's window and, without
  ;; -d, makes the joined pane active.  The emptied source window is removed.
  (it "cmd-join-pane-moves-source-into-destination"
    (with-join-command-fixture (sess src-win src-pane dst-win dst-pane)
      (declare (ignore dst-pane))
      (cl-tmux::%cmd-join-pane-arg sess (%join-command-args "-v"))
      (check-table
       (list (list (%pane-in-window-p src-pane dst-win) t
                   "src-pane must now be in dst-window")
             (list (null (member src-win (session-windows sess))) t
                   "emptied src-window must be removed from the session")
             (list (eq src-pane (window-active-pane dst-win)) t
                   "the joined pane becomes active (no -d)"))
       :test #'eq)))

  ;; join-pane -d moves the pane but leaves the destination's original pane active.
  (it "cmd-join-pane-d-keeps-destination-active"
    (with-join-command-fixture (sess src-win src-pane dst-win dst-pane)
      (declare (ignore src-win))
      (cl-tmux::%cmd-join-pane-arg sess (%join-command-args "-d"))
      (check-table
       (list (list (%pane-in-window-p src-pane dst-win) t
                   "src-pane is still moved into dst-window with -d")
             (list (eq dst-pane (window-active-pane dst-win)) t
                   "-d keeps the destination's original pane active"))
       :test #'eq)))

  ;; join-pane -b inserts the moved pane before the destination pane.
  (it "cmd-join-pane-b-splits-before-the-active-pane"
    (dolist (case *cmd-join-pane-before-cases*)
      (%check-cmd-join-pane-before-case case)))

  ;; join-pane -f makes the split span the full window on the split axis.
  (it "cmd-join-pane-f-spans-the-full-window"
    (dolist (case *cmd-join-pane-full-window-cases*)
      (%check-cmd-join-pane-full-window-case case)))

  ;; join-pane -l applies the requested size hint on the split axis.
  (it "cmd-join-pane-l-honors-size-hint"
    (dolist (case *cmd-join-pane-size-hint-cases*)
      (%check-cmd-join-pane-size-hint-case case)))

  ;; join-pane with src and dst the same window is a no-op (guarded, no crash).
  (it "cmd-join-pane-same-window-is-noop"
    (with-join-command-fixture (sess src-win src-pane dst-win dst-pane)
      (declare (ignore src-pane dst-win dst-pane))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":src"))
      (check-table
       (list (list (= 1 (length (window-panes src-win))) t
                   "same-window join leaves the source pane in place")
             (list (not (null (member src-win (session-windows sess)))) t
                   "the source window is not removed by a same-window no-op"))
       :test #'eq))))
