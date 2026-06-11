(in-package #:cl-tmux/test)

;;;; send-keys, key-name translation, tokenize, kill-window reselection, join-pane — part IV

(in-suite commands-suite)

;;; ── send-keys-to-pane ────────────────────────────────────────────────────────

(test send-keys-to-pane-noop-with-negative-fd
  "send-keys-to-pane is a no-op (no error) when the pane has fd=-1."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:send-keys-to-pane pane "hello")
              "send-keys-to-pane with fd=-1 must not signal an error")))

(test send-keys-to-pane-noop-with-nil-pane
  "send-keys-to-pane with NIL pane does not signal an error."
  (finishes (cl-tmux/commands:send-keys-to-pane nil "hello")
            "send-keys-to-pane with nil pane must not signal an error"))

;;; ── send-keys key-name translation ───────────────────────────────────────────

(test key-name-to-bytes-named-keys
  "%key-name-to-bytes maps named keys to their byte sequences."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(13)  (bytes "Enter"))   "Enter → CR")
    (is (equal '(9)   (bytes "Tab"))     "Tab → HT")
    (is (equal '(27)  (bytes "Escape"))  "Escape → ESC")
    (is (equal '(32)  (bytes "Space"))   "Space → SP")
    (is (equal '(127) (bytes "BSpace"))  "BSpace → DEL")
    (is (equal '(27 91 65) (bytes "Up"))      "Up → ESC [ A")
    (is (equal '(27 91 66) (bytes "Down"))    "Down → ESC [ B")
    (is (equal '(27 79 80) (bytes "F1"))      "F1 → ESC O P")
    (is (equal '(27 91 53 126) (bytes "PageUp")) "PageUp → ESC [ 5 ~")))

(test key-name-to-bytes-control-keys
  "%key-name-to-bytes maps C-<char> to the corresponding control byte."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(3)  (bytes "C-c")) "C-c → 0x03")
    (is (equal '(1)  (bytes "C-a")) "C-a → 0x01")
    (is (equal '(26) (bytes "C-z")) "C-z → 0x1a")
    (is (equal '(0)  (bytes "C-@")) "C-@ → 0x00")))

(test key-name-to-bytes-meta-keys
  "%key-name-to-bytes maps M-<char> to ESC followed by the char."
  (is (equal '(27 120) (coerce (cl-tmux/commands::%key-name-to-bytes "M-x") 'list))
      "M-x → ESC x"))

(test split-key-modifiers-decodes-csi-modifier
  "%split-key-modifiers strips C-/M-/S- prefixes into the CSI modifier code."
  (flet ((mods (name) (multiple-value-list (cl-tmux/commands::%split-key-modifiers name))))
    (is (equal '(1 "Up")   (mods "Up"))    "no modifier → 1")
    (is (equal '(5 "Up")   (mods "C-Up"))  "Ctrl → 5")
    (is (equal '(3 "Left") (mods "M-Left")) "Alt → 3")
    (is (equal '(2 "Down") (mods "S-Down")) "Shift → 2")
    (is (equal '(7 "Left") (mods "C-M-Left")) "Ctrl+Alt → 7")
    (is (equal '(6 "Up")   (mods "C-S-Up")) "Ctrl+Shift → 6")))

(test key-name-to-bytes-modified-arrows-and-nav
  "%key-name-to-bytes encodes modified arrows / Home / End as ESC [ 1 ; mod final."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(27 91 49 59 53 65) (bytes "C-Up"))    "C-Up → ESC[1;5A")
    (is (equal '(27 91 49 59 51 68) (bytes "M-Left"))  "M-Left → ESC[1;3D")
    (is (equal '(27 91 49 59 50 66) (bytes "S-Down"))  "S-Down → ESC[1;2B")
    (is (equal '(27 91 49 59 55 68) (bytes "C-M-Left")) "C-M-Left → ESC[1;7D")
    (is (equal '(27 91 49 59 50 72) (bytes "S-Home"))  "S-Home → ESC[1;2H")))

(test key-name-to-bytes-modified-function-keys
  "%key-name-to-bytes encodes modified F-keys / page keys as ESC [ param ; mod ~."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(27 91 49 53 59 53 126) (bytes "C-F5"))     "C-F5 → ESC[15;5~")
    (is (equal '(27 91 53 59 53 126)    (bytes "C-PageUp")) "C-PageUp → ESC[5;5~")
    (is (equal '(27 91 51 59 50 126)    (bytes "S-Delete")) "S-Delete → ESC[3;2~")))

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

(test tokenize-command-string-basic-whitespace
  "Whitespace separates arguments; runs of spaces/tabs collapse."
  (is (equal '("a" "b" "c") (cl-tmux/commands:tokenize-command-string "a b c")))
  (is (equal '("a" "b") (cl-tmux/commands:tokenize-command-string "  a   b  ")))
  (is (equal '() (cl-tmux/commands:tokenize-command-string "   "))))

(test tokenize-command-string-single-quotes-literal
  "'...' is a literal span: spaces inside are kept and no escapes are processed."
  (is (equal '("a b" "c") (cl-tmux/commands:tokenize-command-string "'a b' c")))
  (is (equal '("a\\b") (cl-tmux/commands:tokenize-command-string "'a\\b'")))
  (is (equal '("") (cl-tmux/commands:tokenize-command-string "''"))
      "an explicit empty quoted token yields an empty-string argument"))

(test tokenize-command-string-double-quotes-with-escapes
  "\"...\" keeps spaces and processes backslash escapes."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "\"a b\"")))
  (is (equal '("a\"b") (cl-tmux/commands:tokenize-command-string "\"a\\\"b\""))
      "escaped double-quote stays inside the argument"))

(test tokenize-command-string-bare-backslash-escape
  "A bare backslash escapes the next character (e.g. a space joins one arg)."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "a\\ b")))
  (is (equal '("ab") (cl-tmux/commands:tokenize-command-string "a\\b"))))

(test tokenize-command-string-adjacent-spans-join
  "Adjacent quoted/bare spans concatenate into a single argument."
  (is (equal '("foobar baz")
             (cl-tmux/commands:tokenize-command-string "foo\"bar baz\"")))
  (is (equal '("ab cd")
             (cl-tmux/commands:tokenize-command-string "'ab'' cd'"))))

(test tokenize-command-string-unterminated-quote-tolerated
  "An unterminated quote consumes to end of string without error."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "'a b")))
  (is (equal '("xy") (cl-tmux/commands:tokenize-command-string "\"xy"))))

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

(test copy-mode-find-forward-locates-term
  "%copy-mode-find-forward finds TERM at the correct row/col from start position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-forward s "abc" 0 1)
      (is (= 0 row) "forward search must find match on row 0 (got ~S)" row)
      (is (= 8 col) "forward search from col 1 must find second 'abc' at col 8 (got ~S)" col))))

(test copy-mode-find-forward-no-match-returns-nil-nil
  "%copy-mode-find-forward returns (values nil nil) when no match exists."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-forward s "zzz" 0 0)
      (is (null row) "no match: row must be NIL")
      (is (null col) "no match: col must be NIL"))))

(test copy-mode-find-backward-locates-term
  "%copy-mode-find-backward finds the nearest match before the cursor position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Search backward from col 11 on row 0 => nearest match before col 11 is
    ;; the second "abc" at col 8.
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-backward s "abc" 0 11)
      (is (= 0 row) "backward search must find match on row 0 (got ~S)" row)
      (is (= 8 col) "backward search from col 11 must find 'abc' at col 8 (got ~S)" col))))

(test copy-mode-find-backward-no-match-returns-nil-nil
  "%copy-mode-find-backward returns (values nil nil) when no match exists."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-backward s "zzz" 0 5)
      (is (null row) "no match: row must be NIL")
      (is (null col) "no match: col must be NIL"))))

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

