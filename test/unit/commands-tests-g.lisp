(in-package #:cl-tmux/test)

;;;; send-keys, key-name translation, tokenize, kill-window reselection, join-pane — part IV

(in-suite commands-suite)

;;; ── send-keys-to-pane ────────────────────────────────────────────────────────

(test send-keys-to-pane-noop
  "send-keys-to-pane is a no-op for a NIL pane and for a pane with fd=-1."
  (finishes (cl-tmux/commands:send-keys-to-pane nil "hello")
            "nil pane must not signal")
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:send-keys-to-pane pane "hello")
              "fd=-1 pane must not signal")))

;;; ── send-keys key-name translation ───────────────────────────────────────────

(test key-name-to-bytes-table
  "%key-name-to-bytes maps named, control, meta, and CSI-modified keys to their byte sequences."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (dolist (row '(("Enter"    (13)                    "Enter → CR")
                   ("Tab"      (9)                     "Tab → HT")
                   ("Escape"   (27)                    "Escape → ESC")
                   ("Space"    (32)                    "Space → SP")
                   ("BSpace"   (127)                   "BSpace → DEL")
                   ("Up"       (27 91 65)               "Up → ESC [ A")
                   ("Down"     (27 91 66)               "Down → ESC [ B")
                   ("F1"       (27 79 80)               "F1 → ESC O P")
                   ("PageUp"   (27 91 53 126)           "PageUp → ESC [ 5 ~")
                   ("C-c"      (3)                     "C-c → 0x03")
                   ("C-a"      (1)                     "C-a → 0x01")
                   ("C-z"      (26)                    "C-z → 0x1a")
                   ("C-@"      (0)                     "C-@ → 0x00")
                   ("M-x"      (27 120)                "M-x → ESC x")
                   ("C-Up"     (27 91 49 59 53 65)     "C-Up → ESC[1;5A")
                   ("M-Left"   (27 91 49 59 51 68)     "M-Left → ESC[1;3D")
                   ("S-Down"   (27 91 49 59 50 66)     "S-Down → ESC[1;2B")
                   ("C-M-Left" (27 91 49 59 55 68)     "C-M-Left → ESC[1;7D")
                   ("S-Home"   (27 91 49 59 50 72)     "S-Home → ESC[1;2H")
                   ("C-F5"     (27 91 49 53 59 53 126) "C-F5 → ESC[15;5~")
                   ("C-PageUp" (27 91 53 59 53 126)    "C-PageUp → ESC[5;5~")
                   ("S-Delete" (27 91 51 59 50 126)    "S-Delete → ESC[3;2~")))
      (destructuring-bind (name expected desc) row
        (is (equal expected (bytes name)) "~A" desc)))))

(test split-key-modifiers-decodes-csi-modifier
  "%split-key-modifiers strips C-/M-/S- prefixes into the CSI modifier code."
  (flet ((mods (name) (multiple-value-list (cl-tmux/commands::%split-key-modifiers name))))
    (dolist (c (list (list "Up"      '(1 "Up")   "no modifier -> 1")
                     (list "C-Up"    '(5 "Up")   "Ctrl -> 5")
                     (list "M-Left"  '(3 "Left") "Alt -> 3")
                     (list "S-Down"  '(2 "Down") "Shift -> 2")
                     (list "C-M-Left" '(7 "Left") "Ctrl+Alt -> 7")
                     (list "C-S-Up"  '(6 "Up")   "Ctrl+Shift -> 6")))
      (destructuring-bind (name expected desc) c
        (is (equal expected (mods name)) "~A" desc)))))


(test key-name-to-bytes-modified-does-not-break-control-chars
  "A C-/M- prefix on a plain char still yields the control/meta byte, not a CSI
   sequence (the modified-special path only triggers for named special keys)."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(3)      (bytes "C-c")) "C-c stays the control byte")
    (is (equal '(27 120) (bytes "M-x")) "M-x stays ESC x")))

(test key-name-to-bytes-unknown-returns-nil
  "%key-name-to-bytes returns NIL for text that is not a key name."
  (is (null (cl-tmux/commands::%key-name-to-bytes "hello")))
  (is (null (cl-tmux/commands::%key-name-to-bytes "echo"))))

(test translate-send-keys-keys-vs-literal
  "%translate-send-keys parses arguments shell-style and translates each: key
   names become their byte sequences, other args are sent literally.  Spaces
   separate arguments unless quoted (tmux semantics)."
  (flet ((bytes (s) (coerce (cl-tmux/commands::%translate-send-keys s) 'list)))
    (is (equal '(13) (bytes "Enter")) "single key → its bytes")
    (is (equal '(27 91 65 27 91 65 27 91 66) (bytes "Up Up Down"))
        "all-keys → concatenated (ESC[A ESC[A ESC[B)")
    ;; tmux semantics: unquoted spaces split args, so they vanish between literals.
    (is (equal (map 'list #'char-code "echohi") (bytes "echo hi"))
        "unquoted 'echo hi' → two literal args, no space (tmux-correct)")
    ;; A literal arg before a key: text then CR.
    (is (equal (append (map 'list #'char-code "foo") '(13)) (bytes "foo Enter"))
        "literal arg followed by a key → text then the key's bytes")
    ;; Quoting preserves the embedded space.
    (is (equal (append (map 'list #'char-code "echo hi") '(13))
               (bytes "\"echo hi\" Enter"))
        "quoted arg keeps its space, then Enter → CR")))

(test send-keys-to-pane-translates-named-key-to-pty
  "send-keys-to-pane translates a named key (Enter) and writes CR to the PTY."
  (with-pipe-fds (rfd wfd)
    (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                           :screen (make-screen 20 5))))
      (cl-tmux/commands:send-keys-to-pane pane "Enter")
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
        (is-true ready "the translated key must reach the PTY")
        (when ready
          (cffi:with-foreign-object (buf :uint8 8)
            (let ((n (cffi:foreign-funcall "read"
                                           :int rfd :pointer buf :unsigned-long 4
                                           :long)))
              (is (= 1 n) "Enter is one byte (got ~D)" n)
              (is (= 13 (cffi:mem-aref buf :uint8 0)) "byte must be CR (13)"))))))))

;;; ── tokenize-command-string (shell-style command lexer) ──────────────────────

(test tokenize-command-string-table
  "tokenize-command-string splits on whitespace; handles quoted spans, escapes, and unterminated quotes."
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
      (is (equal expected (cl-tmux/commands:tokenize-command-string input))
          "~A" desc))))

;;; ── add-message-log ──────────────────────────────────────────────────────────

(test add-message-log-prepends-entry
  "add-message-log prepends a (timestamp . text) cons to *message-log*."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first-message")
    (is-true cl-tmux::*message-log*
        "*message-log* must be non-nil after add-message-log")
    (is (string= "first-message" (cdr (first cl-tmux::*message-log*)))
        "message text must be in cdr of first entry (got ~S)"
        (cdr (first cl-tmux::*message-log*)))))

(test add-message-log-caps-honors-message-limit-option
  "add-message-log caps *message-log* at the message-limit option, not a constant."
  (with-isolated-options ("message-limit" 3)
    (let ((cl-tmux::*message-log* nil))
      (loop repeat 10 do (cl-tmux::add-message-log "x"))
      (is (= 3 (length cl-tmux::*message-log*))
          "*message-log* must be capped at message-limit (3, got ~D)"
          (length cl-tmux::*message-log*)))))

(test add-message-log-ordering
  "add-message-log puts newest entry first."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first")
    (cl-tmux::add-message-log "second")
    (is (string= "second" (cdr (first cl-tmux::*message-log*)))
        "second (most recent) message must be at the head of *message-log*")))

;;; ── kill-window reselection: tmux session_detach order ───────────────────────
;;; %window-after-kill matches tmux: the last-used (MRU) window first
;;; (session_last), else the previous window by index, wrapping to the highest id
;;; (session_previous).  Verified against tmux source (session.c) via deepwiki.

(test window-after-kill-prefers-mru
  "The last-used (MRU) window — strictly greatest positive last-active-time — is
   selected first, regardless of id distance (tmux session_last)."
  (let ((w0 (make-window :id 0 :name "a" :width 20 :height 5 :panes nil
                         :last-active-time 100))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil
                         :last-active-time 300))
        (w7 (make-window :id 7 :name "c" :width 20 :height 5 :panes nil
                         :last-active-time 200)))
    (is (eq w3 (cl-tmux/commands::%window-after-kill (list w0 w3 w7) 5))
        "MRU window (w3, latest last-active-time) wins over id distance")))

(test window-after-kill-previous-by-index-without-mru
  "With no focus history (all timestamps 0), falls back to the previous window by
   index: the greatest id strictly less than the killed id (tmux session_previous)."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil))
        (w8 (make-window :id 8 :name "c" :width 20 :height 5 :panes nil)))
    (is (eq w3 (cl-tmux/commands::%window-after-kill (list w1 w3 w8) 5))
        "picks w3 (greatest id < 5)")))

(test window-after-kill-differs-from-old-nearest
  "Regression: the OLD numerically-nearest rule broke ties toward the HIGHER id.
   killed-id=2, remaining {1,3}, no MRU: tmux picks the PREVIOUS (w1); the old
   %nearest-window wrongly picked w3 (equidistant tie → higher id)."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil)))
    (is (eq w1 (cl-tmux/commands::%window-after-kill (list w1 w3) 2))
        "previous-by-index picks w1; old %nearest-window picked w3")))

(test window-after-kill-previous-wraps-to-highest
  "When no window has a lower id than the killed one, previous-by-index wraps to
   the HIGHEST id (tmux session_previous wrap)."
  (let ((w2 (make-window :id 2 :name "a" :width 20 :height 5 :panes nil))
        (w5 (make-window :id 5 :name "b" :width 20 :height 5 :panes nil))
        (w8 (make-window :id 8 :name "c" :width 20 :height 5 :panes nil)))
    (is (eq w8 (cl-tmux/commands::%window-after-kill (list w2 w5 w8) 0))
        "wraps to the highest-id window (w8) when killed was the lowest")))

(test window-after-kill-mru-tie-falls-back-to-index
  "A TIE at the greatest last-active-time is no unambiguous last-used window (like
   tmux's empty lastw) → fall back to previous-by-index."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil
                         :last-active-time 50))
        (w4 (make-window :id 4 :name "b" :width 20 :height 5 :panes nil
                         :last-active-time 50)))
    (is (eq w1 (cl-tmux/commands::%window-after-kill (list w1 w4) 3))
        "tie at max time → previous-by-index picks w1 (greatest id < 3)")))

(test window-after-kill-single-window
  "A single remaining window is always selected."
  (let ((w2 (make-window :id 2 :name "a" :width 20 :height 5 :panes nil)))
    (is (eq w2 (cl-tmux/commands::%window-after-kill (list w2) 99))
        "the sole remaining window is selected regardless of killed id")))

;;; ── %copy-mode-find-forward / %copy-mode-find-backward ──────────────────────

(test copy-mode-find-locates-term
  "%copy-mode-find-forward and %copy-mode-find-backward both find TERM in 'abc def abc'."
  (dolist (c '((cl-tmux/commands::%copy-mode-find-forward  0 1  0 8 "forward from col 1 finds second 'abc' at col 8")
               (cl-tmux/commands::%copy-mode-find-backward 0 11 0 8 "backward from col 11 finds 'abc' at col 8")))
    (destructuring-bind (fn srow scol erow ecol desc) c
      (let ((s (make-screen 30 5)))
        (feed s "abc def abc")
        (cl-tmux/commands::copy-mode-enter s)
        (multiple-value-bind (row col)
            (funcall fn s "abc" srow scol)
          (is (= erow row) "~A: row (got ~S)" desc row)
          (is (= ecol col) "~A: col (got ~S)" desc col))))))

(test copy-mode-find-no-match-returns-nil-nil
  "%copy-mode-find-forward and %copy-mode-find-backward both return (values nil nil) when no match exists."
  (dolist (c '((cl-tmux/commands::%copy-mode-find-forward  0 0 "forward: no match")
               (cl-tmux/commands::%copy-mode-find-backward 0 5 "backward: no match")))
    (destructuring-bind (fn srow scol desc) c
      (let ((s (make-screen 20 5)))
        (feed s "hello world")
        (cl-tmux/commands::copy-mode-enter s)
        (multiple-value-bind (row col)
            (funcall fn s "zzz" srow scol)
          (is (null row) "~A: row must be NIL" desc)
          (is (null col) "~A: col must be NIL" desc))))))

;;; ── join-pane ────────────────────────────────────────────────────────────────

(test join-pane-moves-pane-into-destination-window
  "join-pane removes SRC-PANE from SRC-WINDOW and inserts it into DST-WINDOW."
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
      (is (eq src-pane result) "join-pane must return src-pane on success")
      ;; src-window had only one pane -- it must have been killed.
      (is-false (member src-win (session-windows sess))
          "src-window must be removed from session when it becomes empty after join-pane")
      ;; dst-window must now contain both dst-pane and src-pane.
      (is (member src-pane (window-panes dst-win))
          "src-pane must appear in dst-window's pane list after join-pane"))))

(test join-pane-returns-nil-on-nil-args
  "join-pane returns NIL immediately when any required argument is NIL."
  (is (null (cl-tmux/commands:join-pane nil nil nil nil :h))
      "join-pane with all-nil args must return NIL without signalling"))

;;; ── join-pane / move-pane (scriptable %cmd-join-pane-arg) ────────────────────

(defun %join-arg-fixture ()
  "Two single-pane windows (\"src\", \"dst\") in one session, dst current.
   Returns (values sess src-win src-pane dst-win dst-pane)."
  (let* ((src-pane (%make-test-pane :id 1))
         (dst-pane (%make-test-pane :id 2))
         (src-win  (make-window :id 1 :name "src" :width 20 :height 6
                                :tree (make-layout-leaf src-pane) :panes (list src-pane)))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 6
                                :tree (make-layout-leaf dst-pane) :panes (list dst-pane)))
         (sess     (make-session :id 1 :name "0" :windows (list src-win dst-win))))
    (session-select-window sess dst-win)          ; current window = dst
    (window-select-pane src-win src-pane)
    (window-select-pane dst-win dst-pane)
    (values sess src-win src-pane dst-win dst-pane)))

(test cmd-join-pane-moves-source-into-destination
  "join-pane -s SRC -t DST moves SRC's active pane into DST's window and, without
   -d, makes the joined pane active.  The emptied source window is removed."
  (multiple-value-bind (sess src-win src-pane dst-win dst-pane) (%join-arg-fixture)
    (declare (ignore dst-pane))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":dst" "-v"))
      (is (member src-pane (window-panes dst-win))
          "src-pane must now be in dst-window")
      (is-false (member src-win (session-windows sess))
          "emptied src-window must be removed from the session")
      (is (eq src-pane (window-active-pane dst-win))
          "the joined pane becomes active (no -d)"))))

(test cmd-join-pane-d-keeps-destination-active
  "join-pane -d moves the pane but leaves the destination's original pane active."
  (multiple-value-bind (sess src-win src-pane dst-win dst-pane) (%join-arg-fixture)
    (declare (ignore src-win))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess)))
          (cl-tmux::*dirty* nil))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":dst" "-d"))
      (is (member src-pane (window-panes dst-win))
          "src-pane is still moved into dst-window with -d")
      (is (eq dst-pane (window-active-pane dst-win))
          "-d keeps the destination's original pane active"))))

(test cmd-join-pane-same-window-is-noop
  "join-pane with src and dst the same window is a no-op (guarded, no crash)."
  (multiple-value-bind (sess src-win src-pane dst-win dst-pane) (%join-arg-fixture)
    (declare (ignore src-pane dst-win dst-pane))
    (let ((cl-tmux::*server-sessions* (list (cons "0" sess))))
      (cl-tmux::%cmd-join-pane-arg sess '("-s" ":src" "-t" ":src"))
      (is (= 1 (length (window-panes src-win)))
          "same-window join leaves the source pane in place")
      (is (member src-win (session-windows sess))
          "the source window is not removed by a same-window no-op"))))

