(in-package #:cl-tmux/test)

;;;; parser tests — part D: direct-osc-continuations, osc-dispatch-edge-cases,
;;;; osc52-coverage, osc7/percent-decode, parser-suite, base64-decode, csi-colon-subparams.

(def-suite direct-osc-continuations
  :description "Direct calls to make-osc-k and make-osc-st-k"
  :in terminal-suite)
(in-suite direct-osc-continuations)

;;; Helper: build an adjustable byte vector pre-filled with STRING.
;;; Eliminates the repeated 3-line buffer-construction pattern.
(defun make-osc-payload-buf (string)
  "Return a fresh adjustable (unsigned-byte 8) buffer pre-filled with the
   bytes of STRING (one byte per character, Latin-1 encoded)."
  (let ((buf (make-array (length string)
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0
                         :adjustable   t)))
    (loop for ch across string
          do (vector-push-extend (char-code ch) buf))
    buf))

(test make-osc-k-accumulates-and-dispatches-on-bel
  "make-osc-k accumulates payload bytes and dispatches to %dispatch-osc on BEL."
  (with-screen (s 20 5)
    ;; Simulate: OSC 0 ; title (bytes for "0;hello")
    (let ((buf (make-osc-payload-buf "0;hello"))
          (k   nil))
      (setf k (cl-tmux/terminal/parser::make-osc-k buf))
      ;; Feed BEL to terminate
      (let ((result (funcall k s #x07)))
        (is (eq #'cl-tmux/terminal/parser:ground-state result)
            "make-osc-k must return ground-state after BEL")
        (is (string= "hello" (cl-tmux/terminal/types:screen-title s))
            "make-osc-k BEL must dispatch OSC 0 and set screen-title")))))

(test make-osc-k-esc-transitions-to-st-state
  "make-osc-k on ESC (#x1B) returns a continuation waiting for backslash."
  (with-screen (s 10 5)
    (let* ((buf (make-osc-payload-buf ""))
           (k   (cl-tmux/terminal/parser::make-osc-k buf))
           (k2  (funcall k s #x1B)))
      (is (functionp k2)
          "make-osc-k on ESC must return a function (bridge continuation)"))))

(test make-osc-st-k-backslash-dispatches-and-grounds
  "make-osc-st-k on backslash dispatches and returns ground-state."
  (with-screen (s 20 5)
    ;; Payload: "2;xterm-st-title"
    (let* ((buf    (make-osc-payload-buf "2;xterm-st-title"))
           (k      (cl-tmux/terminal/parser::make-osc-st-k buf))
           (result (funcall k s #x5C)))      ; backslash = ST confirmed
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "make-osc-st-k on backslash must return ground-state")
      (is (string= "xterm-st-title" (cl-tmux/terminal/types:screen-title s))
          "make-osc-st-k must dispatch OSC 2 and set screen-title"))))

(test make-osc-st-k-non-backslash-returns-ground
  "make-osc-st-k on a non-backslash byte returns ground-state without dispatching."
  (with-screen (s 20 5)
    (let* ((buf    (make-osc-payload-buf "0;title"))
           (k      (cl-tmux/terminal/parser::make-osc-st-k buf))
           (result (funcall k s (char-code #\X)))) ; not a backslash
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "make-osc-st-k on non-backslash must still return ground-state")
      ;; Title must NOT have been set (malformed ST discarded)
      (is (not (string= "title" (cl-tmux/terminal/types:screen-title s)))
          "make-osc-st-k non-backslash must not dispatch the OSC"))))

;;; ── SUITE: osc-dispatch-edge-cases ──────────────────────────────────────────

(def-suite osc-dispatch-edge-cases
  :description "OSC dispatch edge cases: no-semicolon payload, unknown command"
  :in terminal-suite)
(in-suite osc-dispatch-edge-cases)

(test osc-payload-no-semicolon-is-noop
  "An OSC payload with no semicolon is silently discarded (no command to dispatch)."
  (with-screen (s 20 5)
    ;; Feed OSC with no semicolon: just the command number, BEL terminated.
    ;; This should not crash and must not set screen-title.
    (finishes
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]notanumber~C" #\Escape (code-char 7))
          :encoding :utf-8)))
    ;; screen-title must remain at its default (NIL or empty string).
    (let ((title (cl-tmux/terminal/types:screen-title s)))
      (is (or (null title) (string= "" title))
          "screen-title must be unset after invalid OSC payload"))))

(test osc-unknown-command-is-silently-ignored
  "An OSC payload with a valid integer command but no matching rule is silently ignored."
  (with-screen (s 20 5)
    ;; OSC 99 is not handled — must not crash.
    (finishes
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]99;some-data~C" #\Escape (code-char 7))
          :encoding :utf-8)))
    ;; screen-title must remain unset (OSC 99 has no handler).
    (let ((title (cl-tmux/terminal/types:screen-title s)))
      (is (or (null title) (string= "" title))
          "unknown OSC command must not alter screen-title"))))

(test osc-empty-payload-bel-is-noop
  "An OSC terminated immediately by BEL (empty payload) is consumed without error."
  (with-screen (s 20 5)
    (feed s "A")
    ;; ESC ] BEL — empty payload
    (screen-process-bytes s
      (make-array 3 :element-type '(unsigned-byte 8)
                    :initial-contents (list #x1B #x5D #x07)))
    (feed s "B")
    (is (char= #\A (char-at s 0 0)) "char before empty OSC must survive")
    (is (char= #\B (char-at s 1 0)) "char after empty OSC must be written")))

;;; ── SUITE: osc52-coverage ────────────────────────────────────────────────────

(def-suite osc52-coverage
  :description "OSC 52 clipboard handler: callback path and nil handler (silently dropped)"
  :in terminal-suite)
(in-suite osc52-coverage)

(test osc52-handler-invoked-with-decoded-text
  "When *osc52-handler* is set, OSC 52 with a valid Base64 payload invokes it
   with the decoded text string."
  (with-screen (s 20 5)
    ;; Base64-encode \"hello\" → SGVsbG8=
    (let* ((received nil)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      ;; Base64 of "hello" is aGVsbG8=  (SGVsbG8= would decode to "Hello").
      ;; Feed OSC 52 ; c ; aGVsbG8= BEL  (c = clipboard target, ignored)
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]52;c;aGVsbG8=~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (is (string= "hello" received)
          "osc52-handler must be called with decoded text 'hello'"))))

(test osc52-nil-handler-silently-dropped
  "When *osc52-handler* is NIL, an OSC 52 sequence is consumed without error."
  (with-screen (s 20 5)
    (let ((cl-tmux/terminal/parser:*osc52-handler* nil))
      (finishes
        (screen-process-bytes s
          (babel:string-to-octets
            (format nil "~C]52;c;SGVsbG8=~C" #\Escape (code-char 7))
            :encoding :utf-8))))))

(test osc52-read-request-silently-ignored
  "OSC 52 with payload '?' (clipboard read request) is silently ignored."
  (with-screen (s 20 5)
    (let* ((received :not-called)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]52;c;?~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (is (eq :not-called received)
          "handler must NOT be invoked for a clipboard read request ('?')"))))

;;; ── OSC 7: current working directory (file://host/path) ──────────────────────

(test osc7-path-extraction
  "%osc7-path extracts the path from a file:// URL, with or without a host."
  (flet ((p (s) (cl-tmux/terminal/parser::%osc7-path s)))
    (dolist (c '(("file://host/home/u" "/home/u"   "with host")
                 ("file:///home/u"     "/home/u"   "empty host")
                 ("file://host"        "/"         "host but no path → /")
                 ("not-a-url"          "not-a-url" "non-file:// → unchanged")))
      (destructuring-bind (input expected desc) c
        (is (string= expected (p input)) "~A" desc)))))

(test osc7-sets-screen-cwd-end-to-end
  "Feeding ESC ] 7 ; file://host/path BEL sets screen-cwd to the path."
  (with-screen (s 20 5)
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]7;file://myhost/home/user/project~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (is (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s))
        "screen-cwd must be the OSC 7 path after the sequence (got ~S)"
        (cl-tmux/terminal/types:screen-cwd s))))

(test percent-decode-cases
  "%percent-decode handles %20 spaces, UTF-8 multibyte, no-% passthrough, and an
   incomplete trailing % (left literal)."
  (flet ((d (s) (cl-tmux/terminal/parser::%percent-decode s)))
    (dolist (c '(("a%20b"     "a b" "%20 → space")
                 ("abc"       "abc" "no % → unchanged")
                 ("%2F"       "/"   "%2F → /")
                 ("a%"        "a%"  "incomplete trailing % is literal")
                 ("a%zz"      "a%zz" "non-hex after % is literal")
                 ("%E2%9C%93" "✓"  "UTF-8 multibyte (U+2713) decodes")))
      (destructuring-bind (input expected desc) c
        (is (string= expected (d input)) "~A" desc)))))

(test osc7-path-percent-decoded
  "OSC 7 paths are percent-decoded — e.g. macOS '/Application Support'."
  (dolist (c '(("file://host/My%20Docs"              "/My Docs")
               ("file:///Library/Application%20Support" "/Library/Application Support")))
    (destructuring-bind (url expected) c
      (is (string= expected (cl-tmux/terminal/parser::%osc7-path url))
          "~S" url))))

(test screen-cwd-defaults-empty
  "screen-cwd is empty on a fresh screen (no OSC 7 reported yet)."
  (with-screen (s 20 5)
    (is (string= "" (cl-tmux/terminal/types:screen-cwd s))
        "a fresh screen has no reported cwd")))

;;; ── Coverage gap: define-osc-rules macro ─────────────────────────────────────
;;;
;;; Audit finding: define-osc-rules was not tested as a macro in isolation.
;;; Symmetry with the define-state and define-dec-graphics-table assertions.

(test define-osc-rules-macro-is-defined
  "define-osc-rules is a defined macro in the parser package."
  (is (macro-function 'cl-tmux/terminal/parser::define-osc-rules)
      "define-osc-rules must be a macro"))

;;; ── Coverage gap: make-dcs-st-k direct test ──────────────────────────────────
;;;
;;; make-dcs-st-k was extracted from the inline lambda inside make-dcs-k.
;;; Test it directly to confirm symmetry with make-osc-st-k.

(def-suite direct-dcs-st-suite
  :description "Direct calls to make-dcs-st-k bridge continuation"
  :in terminal-suite)
(in-suite direct-dcs-st-suite)

(defun %fresh-dcs-buffer ()
  "A fresh empty adjustable octet buffer for make-dcs-st-k / make-dcs-k tests."
  (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(test make-dcs-st-k-backslash-returns-ground
  "make-dcs-st-k on backslash (#x5C) returns ground-state (ST confirmed)."
  (let* ((s   (make-screen 10 5))
         (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
         (result (funcall k s #x5C)))
    (is (eq #'cl-tmux/terminal/parser:ground-state result)
        "make-dcs-st-k on backslash must return ground-state")))

(test make-dcs-st-k-non-backslash-resumes-consuming
  "make-dcs-st-k on a non-backslash byte resumes DCS consumption (returns a continuation)."
  (let* ((s   (make-screen 10 5))
         (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
         (result (funcall k s (char-code #\A))))
    (is (functionp result)
        "make-dcs-st-k on non-backslash must return a continuation (keeps consuming DCS)")))

;;; ── tmux DCS passthrough (allow-passthrough) ─────────────────────────────────

(test dcs-passthrough-tmux-prefix-queues-inner-sequence
  "A \\ePtmux;<payload>\\e\\\\ DCS with doubled ESCs queues the un-doubled inner
   sequence on the screen's passthrough-queue."
  (let ((s (make-screen 10 5)))
    ;; Feed: ESC P t m u x ;  ESC ESC ] 1 3 3 7  ESC \   (doubled inner ESC)
    ;; Inner un-doubled should be: ESC ] 1 3 3 7
    (cl-tmux/terminal/emulator:screen-process-bytes
     s (coerce (list #x1B #x50               ; ESC P (DCS)
                     116 109 117 120 59      ; tmux;
                     #x1B #x1B 93 49 51 51 55 ; \e\e ] 1 3 3 7  (doubled ESC)
                     #x1B #x5C)              ; ESC \  (ST)
               '(vector (unsigned-byte 8))))
    (let ((queue (cl-tmux/terminal/types:screen-passthrough-queue s)))
      (is (= 1 (length queue)) "one passthrough sequence queued")
      (let ((seq (first queue)))
        (is (char= #\Escape (char seq 0)) "inner sequence starts with un-doubled ESC")
        (is (string= "]1337" (subseq seq 1)) "inner payload after the single ESC")))))

(test dcs-non-tmux-prefix-is-discarded
  "A non-tmux DCS (e.g. Sixel) is consumed and NOT queued for passthrough."
  (let ((s (make-screen 10 5)))
    ;; ESC P q <sixel-ish bytes> ESC \  — prefix is 'q', not 'tmux;'
    (cl-tmux/terminal/emulator:screen-process-bytes
     s (coerce (list #x1B #x50 113 35 48 #x1B #x5C) '(vector (unsigned-byte 8))))
    (is (null (cl-tmux/terminal/types:screen-passthrough-queue s))
        "non-tmux DCS must not populate the passthrough-queue")))

;;; ── Coverage gap: make-bytes / feed-osc helpers ──────────────────────────────
;;;
;;; Audit finding: the pattern
;;;   (make-array N :element-type '(unsigned-byte 8) :initial-contents '(...))
;;; is repeated 7+ times in parser-tests.lisp.  Centralise it as make-bytes.
;;; The pattern
;;;   (screen-process-bytes s (babel:string-to-octets (format nil "~C]N;...~C" ...) :encoding :utf-8))
;;; is repeated 10+ times.  Centralise it as feed-osc.

(defun make-bytes (&rest byte-values)
  "Return a simple (unsigned-byte 8) vector containing BYTE-VALUES."
  (make-array (length byte-values)
              :element-type '(unsigned-byte 8)
              :initial-contents byte-values))

(defun feed-osc (screen command-number body-string)
  "Feed an OSC sequence with integer COMMAND-NUMBER and BODY-STRING to SCREEN,
   terminated by BEL (ASCII 7).  Uses UTF-8 encoding to match real terminal behaviour."
  (screen-process-bytes screen
    (babel:string-to-octets
      (format nil "~C]~D;~A~C" #\Escape command-number body-string (code-char 7))
      :encoding :utf-8)))

;;; Verify the helpers function correctly before relying on them in later tests.

(test make-bytes-helper
  "make-bytes returns a (unsigned-byte 8) vector with the given byte values."
  (let ((bytes (make-bytes #x1B #x5D #x07)))
    (is (= 3 (length bytes)) "length must be 3")
    (is (= #x1B (aref bytes 0)) "first byte must be ESC")
    (is (= #x5D (aref bytes 1)) "second byte must be ]")
    (is (= #x07 (aref bytes 2)) "third byte must be BEL")))

(test feed-osc-helper
  "feed-osc sends an OSC sequence that causes the expected side-effect."
  (with-screen (s 20 5)
    (feed-osc s 0 "test-title")
    (is (string= "test-title" (cl-tmux/terminal/types:screen-title s))
        "feed-osc for OSC 0 must set screen-title")))

;;; ── Coverage gap: zero-length buffer in screen-process-bytes ─────────────────
;;;
;;; Audit finding: screen-process-bytes with start=0, end=0 on a zero-length
;;; buffer was not tested.

(def-suite parser-suite
  :description "Parser and emulator coverage gap tests"
  :in terminal-suite)
(in-suite parser-suite)

(test screen-process-bytes-zero-length-buffer-is-noop
  "screen-process-bytes on a zero-length buffer (start=end=0) is a no-op."
  (with-screen (s 10 5)
    (let ((buf (make-array 0 :element-type '(unsigned-byte 8))))
      (screen-process-bytes s buf :start 0 :end 0))
    (is (char= #\Space (char-at s 0 0))
        "zero-length buffer must leave screen unchanged")))

;;; ── Coverage gap: %base64-decode edge cases ──────────────────────────────────
;;;
;;; Audit finding: Base64 padding ('='), truncated input, and invalid characters
;;; were not directly asserted.

(def-suite base64-decode-suite
  :description "Direct coverage of %base64-decode edge cases"
  :in terminal-suite)
(in-suite base64-decode-suite)

(test base64-decode-basic-string
  "%base64-decode decodes a standard Base64 string ('hello' = aGVsbG8=)."
  (let ((result (cl-tmux/terminal/parser::%base64-decode "aGVsbG8=")))
    (is (not (null result)) "must return a byte vector, not NIL")
    (is (string= "hello"
                 (babel:octets-to-string result :encoding :utf-8))
        "aGVsbG8= must decode to 'hello'")))

(test base64-decode-empty-string
  "%base64-decode on an empty string returns an empty byte vector."
  (let ((result (cl-tmux/terminal/parser::%base64-decode "")))
    (is (or (null result) (zerop (length result)))
        "empty input must produce empty output or NIL")))

(test base64-decode-truncated-group
  "%base64-decode on input shorter than 4 chars does not crash."
  (finishes (cl-tmux/terminal/parser::%base64-decode "YQ"))
  ;; 'YQ' decodes to 'a' (no padding); should succeed without error.
  (let ((result (cl-tmux/terminal/parser::%base64-decode "YQ==")))
    (is (not (null result)) "padded 2-char group must decode successfully")))

;;; ── Coverage gap: %parse-osc-command error branch ────────────────────────────
;;;
;;; Audit finding: the error-return branch (non-integer command field) was not
;;; directly asserted.

(test parse-osc-command-returns-nil-for-non-integer
  "%parse-osc-command returns NIL when the command field is not a valid integer."
  (let ((result (cl-tmux/terminal/parser::%parse-osc-command "notanumber" 10)))
    (is (null result)
        "%parse-osc-command must return NIL for a non-integer command field")))

(test parse-osc-command-returns-integer-for-valid-input
  "%parse-osc-command returns the integer for a valid command field."
  (let ((result (cl-tmux/terminal/parser::%parse-osc-command "52;data" 2)))
    (is (= 52 result)
        "%parse-osc-command must return 52 for '52' prefix")))

;;; ── Coverage gap: %handle-osc-52 no-inner-semicolon branch ──────────────────
;;;
;;; Audit finding: the branch where the OSC 52 body has no semicolon was not
;;; directly tested.

(test handle-osc-52-no-inner-semicolon-is-noop
  "%handle-osc-52 is a no-op when the body has no semicolon (malformed OSC 52)."
  (let ((received :not-called)
        (cl-tmux/terminal/parser:*osc52-handler*
          (lambda (text) (setf received text))))
    (finishes (cl-tmux/terminal/parser::%handle-osc-52 "nodatahere"))
    (is (eq :not-called received)
        "%handle-osc-52 with no semicolon must not invoke the handler")))

;;; ── CSI colon sub-parameters (ISO 8613-6) ───────────────────────────────────
;;;
;;; A colon introduces sub-parameters within one CSI parameter (SGR 4:3 undercurl,
;;; 38:2::R:G:B true-colour).  The parser keeps the leading value and skips the
;;; rest, so such a sequence neither aborts (printing stray bytes) nor mis-applies.

(def-suite csi-colon-subparams :description "CSI colon sub-parameter handling"
  :in parser-suite)
(in-suite csi-colon-subparams)

(test csi-colon-undercurl-keeps-leading-underline
  "CSI 4:3 m (undercurl) keeps the leading 4 → underline; no stray bytes print."
  (with-screen (s 8 2)
    (feed s (esc "[4:3m"))            ; undercurl via colon sub-parameter
    (feed s "X")
    (is (char= #\X (char-at s 0 0))
        "X must be the first cell — the colon sequence printed nothing")
    (is (logbitp 3 (attrs-at s 0 0))
        "the leading 4 must set the underline attribute (bit 3)")))

(test csi-colon-multi-param-mixed
  "CSI 0;4:3;1 m applies reset, underline (from 4:3), bold — colon does not
   bleed into the neighbouring parameters."
  (with-screen (s 8 2)
    (feed s (esc "[0;4:3;1m"))
    (feed s "Y")
    (is (char= #\Y (char-at s 0 0)) "Y is the first cell")
    (is (logbitp 3 (attrs-at s 0 0)) "underline set (from 4:3)")
    (is (logbitp 0 (attrs-at s 0 0)) "bold set (from the trailing ;1)")))

(test csi-colon-truecolor-form-does-not-abort
  "CSI 38:2::255:0:0 m (colon true-colour) must not abort and spew bytes; the
   following text writes cleanly at column 0."
  (with-screen (s 8 2)
    (feed s (esc "[38:2::255:0:0m"))
    (feed s "Z")
    (is (char= #\Z (char-at s 0 0))
        "Z must be the first cell — no stray sub-parameter bytes printed")))
