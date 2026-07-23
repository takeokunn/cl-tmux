(in-package #:cl-tmux/test)

;;;; CSI function and navigation key dispatch

(describe "events-suite"

  ;;; ── Function / navigation keys: ESC [ N ~ → key name → binding ───────────────

  ;; %csi-tilde-parse returns (values PARAM MOD); MOD defaults to 1 and a ';mod'
  ;; field carries the modifier (the modified-function-key form).
  (it "csi-tilde-parse-reads-param-and-modifier"
    ;; ESC [ 5 ~  → 5, 1  (unmodified)
    (multiple-value-bind (p m)
        (cl-tmux::%csi-tilde-parse
         (make-array 4 :element-type '(unsigned-byte 8)
                       :initial-contents '(27 91 53 126)) 4)
      (expect (= 5 p)) (expect (= 1 m)))
    ;; ESC [ 1 5 ~ → 15, 1  (F5)
    (multiple-value-bind (p m)
        (cl-tmux::%csi-tilde-parse
         (make-array 5 :element-type '(unsigned-byte 8)
                       :initial-contents '(27 91 49 53 126)) 5)
      (expect (= 15 p)) (expect (= 1 m)))
    ;; ESC [ 1 5 ; 5 ~ → 15, 5  (Ctrl+F5)
    (multiple-value-bind (p m)
        (cl-tmux::%csi-tilde-parse
         (make-array 7 :element-type '(unsigned-byte 8)
                       :initial-contents '(27 91 49 53 59 53 126)) 7)
      (expect (= 15 p)) (expect (= 5 m)))
    ;; ESC [ ~ (empty param) -> NIL -> raw forward
    (expect (null (cl-tmux::%csi-tilde-parse
                   (make-array 3 :element-type '(unsigned-byte 8)
                                 :initial-contents '(27 91 126)) 3))))

  ;; %csi-tilde-key combines base key + modifier prefix: F5, C-F5, S-Home.
  (it "csi-tilde-key-joins-base-and-modifier"
    (flet ((k (bytes) (cl-tmux::%csi-tilde-key
                       (make-array (length bytes) :element-type '(unsigned-byte 8)
                                                  :initial-contents bytes)
                       (length bytes))))
      (dolist (c '(((27 91 49 53 126)       "F5"     "ESC [ 15 ~       -> F5")
                   ((27 91 49 53 59 53 126) "C-F5"   "ESC [ 15 ; 5 ~  -> C-F5")
                   ((27 91 49 59 50 126)    "S-Home" "ESC [ 1 ; 2 ~   -> S-Home")
                   ((27 91 50 48 48 126)    nil       "ESC [ 200 ~ (paste) -> NIL")))
        (destructuring-bind (bytes expected desc) c
          (declare (ignore desc))
          (expect (equal expected (k bytes)))))))

  ;; %csi-tilde-key-name maps vt parameters to canonical tmux key names;
  ;; an unknown parameter maps to NIL (forwarded raw, not bound).
  (it "csi-tilde-key-name-maps-known-params"
    (dolist (c '((1  "Home") (3  "Delete") (5  "PageUp")
                 (6  "PageDown") (15 "F5") (24 "F12")
                 (99 nil)))
      (destructuring-bind (param expected) c
        (expect (equal expected (cl-tmux::%csi-tilde-key-name param))))))

  ;; %parse-key-token is canonical-only: PPage/NPage/IC remain literal key names,
  ;; not input-side aliases for PageUp/PageDown/Insert.
  (it "parse-key-token-keeps-navigation-spellings-literal"
    (dolist (c '(("PPage" "PPage") ("NPage" "NPage")
                 ("IC"    "IC")    ("F5"    "F5")))
      (destructuring-bind (input expected) c
        (expect (string= expected (cl-tmux/config::%parse-key-token input))))))

  ;; bind -n F5 fires when ESC [ 1 5 ~ is fed through the input state machine.
  (it "function-key-root-binding-fires-from-byte-stream"
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state)))
        (key-table-bind "root" "F5" :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 1 5 ~  byte by byte.
               (dolist (byte '(27 91 49 53 126))
                 (cl-tmux::process-byte s byte state))
               (expect (eq (second (session-windows s)) (session-active-window s)))
               (expect (eq #'cl-tmux::%ground-input-state
                           (cl-tmux::input-state-continuation state))))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash "F5" tbl)))))))

  ;; bind -n PPage stores a literal key name.  ESC [ 5 ~ resolves to PageUp, so it
  ;; must not fire a PPage binding.
  (it "page-up-literal-binding-does-not-fire-from-page-up-byte-stream"
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state))
            (key   (cl-tmux/config::%parse-key-token "PPage")))
        (key-table-bind "root" key :next-window)
        (unwind-protect
             (progn
               ;; ESC [ 5 ~  byte by byte.
               (dolist (byte '(27 91 53 126))
                 (cl-tmux::process-byte s byte state))
               (expect (eq (first (session-windows s)) (session-active-window s))))
          (let ((tbl (gethash "root" *key-tables*)))
            (when tbl (remhash key tbl)))))))

  ;; An unbound F5 (ESC [ 15 ~) leaves the state machine at ground without firing a
  ;; binding — preserving transparency so the pane application receives the key.
  (it "unbound-function-key-forwards-to-pane-not-bindings"
    (with-fake-session (s :nwindows 2)
      (let ((state (cl-tmux::make-input-state))
            (before (session-active-window s)))
        ;; No binding installed for F5: feeding ESC [ 15 ~ must not switch windows.
        (dolist (byte '(27 91 49 53 126))
          (cl-tmux::process-byte s byte state))
        (expect (eq before (session-active-window s)))
        (expect (eq #'cl-tmux::%ground-input-state
                    (cl-tmux::input-state-continuation state)))))))
