(in-package #:cl-tmux/test)

;;;; send-keys command tests

(defparameter *key-name-to-bytes-cases*
  '(("Enter"    (13)                    "Enter -> CR")
    ("Tab"      (9)                     "Tab -> HT")
    ("Escape"   (27)                    "Escape -> ESC")
    ("Space"    (32)                    "Space -> SP")
    ("BSpace"   (127)                   "BSpace -> DEL")
    ("Up"       (27 91 65)               "Up -> ESC [ A")
    ("Down"     (27 91 66)               "Down -> ESC [ B")
    ("F1"       (27 79 80)               "F1 -> ESC O P")
    ("PageUp"   (27 91 53 126)           "PageUp -> ESC [ 5 ~")
    ("C-c"      (3)                     "C-c -> 0x03")
    ("C-a"      (1)                     "C-a -> 0x01")
    ("C-z"      (26)                    "C-z -> 0x1a")
    ("C-@"      (0)                     "C-@ -> 0x00")
    ("M-x"      (27 120)                "M-x -> ESC x")
    ("C-Up"     (27 91 49 59 53 65)     "C-Up -> ESC[1;5A")
    ("M-Left"   (27 91 49 59 51 68)     "M-Left -> ESC[1;3D")
    ("S-Down"   (27 91 49 59 50 66)     "S-Down -> ESC[1;2B")
    ("C-M-Left" (27 91 49 59 55 68)     "C-M-Left -> ESC[1;7D")
    ("S-Home"   (27 91 49 59 50 72)     "S-Home -> ESC[1;2H")
    ("C-F5"     (27 91 49 53 59 53 126) "C-F5 -> ESC[15;5~")
    ("C-PageUp" (27 91 53 59 53 126)    "C-PageUp -> ESC[5;5~")
    ("S-Delete" (27 91 51 59 50 126)    "S-Delete -> ESC[3;2~")
    ("C-F2"     (27 91 49 59 53 81)     "C-F2 -> ESC[1;5Q")
    ("S-End"    (27 91 49 59 50 70)     "S-End -> ESC[1;2F")))

(defparameter *split-key-modifier-cases*
  '(("Up"       (1 "Up")    "no modifier -> 1")
    ("C-Up"     (5 "Up")    "Ctrl -> 5")
    ("M-Left"   (3 "Left")  "Alt -> 3")
    ("S-Down"   (2 "Down")  "Shift -> 2")
    ("C-M-Left" (7 "Left")  "Ctrl+Alt -> 7")
    ("C-S-Up"   (6 "Up")    "Ctrl+Shift -> 6")))

(defparameter *translate-send-keys-cases*
  '(("Enter"             (13)                         "single key -> its bytes")
    ("Up Up Down"        (27 91 65 27 91 65 27 91 66) "all keys -> concatenated CSI sequences")
    ("echo hi"           (101 99 104 111 104 105)     "unquoted spaces split literal args")
    ("foo Enter"         (102 111 111 13)             "literal arg followed by a key")
    ("\"echo hi\" Enter" (101 99 104 111 32 104 105 13) "quoted arg preserves its space")))

(defun %check-key-name-to-bytes-case (case)
  (destructuring-bind (name expected desc) case
    (declare (ignore desc))
    (expect (equal expected (key-name-bytes name)))))

(defun %check-split-key-modifier-case (case)
  (destructuring-bind (name expected desc) case
    (declare (ignore desc))
    (expect (equal expected (split-key-modifiers-values name)))))

(defun %check-translate-send-keys-case (case)
  (destructuring-bind (input expected desc) case
    (declare (ignore desc))
    (expect (equal expected (translate-send-keys-bytes input)))))

(defmacro define-command-case-table-test (name doc cases checker)
  (declare (ignore doc))
  `(it ,(string-downcase (symbol-name name))
     (dolist (case ,cases)
       (,checker case))))

(describe "commands-suite"

  ;;; send-keys-to-pane

  ;; send-keys-to-pane is a no-op for a NIL pane and for a pane with fd=-1.
  (it "send-keys-to-pane-noop"
    (finishes (cl-tmux/commands:send-keys-to-pane nil "hello"))
    (let ((pane (%make-test-pane)))
      (finishes (cl-tmux/commands:send-keys-to-pane pane "hello"))))

  ;;; send-keys key-name translation

  ;; %escape-sequence prepends exactly one ESC char to the concatenation of its
  ;; TAIL string arguments, regardless of how many TAIL arguments are given.
  (it "escape-sequence-prepends-single-esc"
    (expect (string= (string (code-char 27)) (cl-tmux/commands::%escape-sequence)))
    (expect (string= (concatenate 'string (string (code-char 27)) "[A")
                      (cl-tmux/commands::%escape-sequence "[A")))
    (expect (string= (concatenate 'string (string (code-char 27)) "[" "1" ";" "5" "A")
                      (cl-tmux/commands::%escape-sequence "[" "1" ";" "5" "A"))))

  (define-command-case-table-test key-name-to-bytes-table
    "%key-name-to-bytes maps named, control, meta, and CSI-modified keys to their byte sequences."
    *key-name-to-bytes-cases*
    %check-key-name-to-bytes-case)

  (define-command-case-table-test split-key-modifiers-decodes-csi-modifier
    "%split-key-modifiers strips C-/M-/S- prefixes into the CSI modifier code."
    *split-key-modifier-cases*
    %check-split-key-modifier-case)

  ;; A C-/M- prefix on a plain char still yields the control/meta byte, not a CSI
  ;; sequence (the modified-special path only triggers for named special keys).
  (it "key-name-to-bytes-modified-does-not-break-control-chars"
    (expect (equal '(3)      (key-name-bytes "C-c")))
    (expect (equal '(27 120) (key-name-bytes "M-x"))))

  ;; %key-name-to-bytes returns NIL for text that is not a key name.
  (it "key-name-to-bytes-unknown-returns-nil"
    (expect (null (cl-tmux/commands::%key-name-to-bytes "hello")))
    (expect (null (cl-tmux/commands::%key-name-to-bytes "echo"))))

  (define-command-case-table-test translate-send-keys-keys-vs-literal
    "%translate-send-keys parses arguments shell-style and translates each: key
   names become their byte sequences, other args are sent literally.  Spaces
   separate arguments unless quoted (tmux semantics)."
    *translate-send-keys-cases*
    %check-translate-send-keys-case)

  ;; send-keys-to-pane translates a named key (Enter) and writes CR to the PTY.
  (it "send-keys-to-pane-translates-named-key-to-pty"
    (with-pipe-fds (rfd wfd)
      (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                             :screen (make-screen 20 5))))
        (cl-tmux/commands:send-keys-to-pane pane "Enter")
        (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
          (expect ready :to-be-truthy)
          (when ready
            (cffi:with-foreign-object (buf :uint8 8)
              (let ((n (cffi:foreign-funcall "read"
                                             :int rfd :pointer buf :unsigned-long 4
                                             :long)))
                (expect (= 1 n))
                (expect (= 13 (cffi:mem-aref buf :uint8 0)))))))))))
