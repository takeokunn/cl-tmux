(in-package #:cl-tmux/test)

;;;; events tests: CSI-u extended key parsing and dispatch

(defun %csi-u-buf (&rest bytes)
  "Build a CSI-u BUFFER (with a trailing 'u') from the parameter BYTES, prefixed
   with ESC [, as the state machine accumulates it."
  (let ((v (make-array (+ 3 (length bytes)) :element-type '(unsigned-byte 8)
                                            :fill-pointer 0 :adjustable t)))
    (vector-push-extend 27 v) (vector-push-extend 91 v)   ; ESC [
    (dolist (b bytes) (vector-push-extend b v))
    (vector-push-extend 117 v)                            ; u
    v))

(describe "events-suite"

  ;;; ── Extended keys (CSI u) key-name parsing ───────────────────────────────────

  ;; %csi-u-key-name maps codepoint+modifier to the canonical key name, covering
  ;; modifier combinations and special codepoints (Tab/Enter/Escape/Space/BSpace).
  ;; Each row: (code mod expected description).
  (it "csi-u-key-name-table"
    (dolist (c '((97 1 "a"        "plain a (mod 1)")
                 (97 2 "S-a"      "Shift (mod 2)")
                 (97 3 "M-a"      "Alt (mod 3)")
                 (97 5 "C-a"      "Ctrl (mod 5)")
                 (97 6 "C-S-a"    "Ctrl+Shift (mod 6)")
                 (97 7 "C-M-a"    "Ctrl+Alt (mod 7)")
                 (97 8 "C-M-S-a"  "Ctrl+Alt+Shift (mod 8)")
                 (9   1 "Tab"     "Tab (code 9 mod 1)")
                 (9   2 "S-Tab"   "S-Tab (code 9 mod 2)")
                 (13  1 "Enter"   "Enter (code 13 mod 1)")
                 (27  1 "Escape"  "Escape (code 27 mod 1)")
                 (32  5 "C-Space" "C-Space (code 32 mod 5)")
                 (127 1 "BSpace"  "BSpace (code 127 mod 1)")))
      (destructuring-bind (code mod expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux::%csi-u-key-name code mod))))))

  ;; An unhandled (control/out-of-range) codepoint yields NIL.
  (it "csi-u-key-name-unhandled-codepoint"
    (expect (null (cl-tmux::%csi-u-key-name 0 1)))
    (expect (null (cl-tmux::%csi-u-key-name 7 5)))
    (expect (null (cl-tmux::%csi-u-base-key 200))))

  ;;; ── Extended keys (CSI u) parameter parsing ──────────────────────────────────

  ;; %csi-u-parse-params reads <codepoint>[;<mod>] from a u-terminated buffer.
  (it "csi-u-parse-params-cases"
    (multiple-value-bind (cp mod)
        (cl-tmux::%csi-u-parse-params (%csi-u-buf 57 55 59 53) 7)  ; 97 ; 5
      (expect (= 97 cp)) (expect (= 5 mod)))
    (multiple-value-bind (cp mod)
        (cl-tmux::%csi-u-parse-params (%csi-u-buf 49 51) 5)        ; 13 (no ; mod)
      (expect (= 13 cp)) (expect (= 1 mod)))
    (multiple-value-bind (cp mod)
        (cl-tmux::%csi-u-parse-params (%csi-u-buf 57 59 53 58 49) 8) ; 9 ; 5:1 (subparam)
      (expect (= 9 cp)) (expect (= 5 mod))))

  ;; The state-machine predicates recognise CSI-u prefixes and full sequences,
  ;; and reject mouse / arrow CSI shapes.
  (it "csi-u-terminated-and-accumulating-predicates"
    (let ((full (%csi-u-buf 57 55 59 53)))                ; ESC [ 97 ; 5 u  (len 7)
      (expect (cl-tmux::%csi-u-terminated-p full 7) :to-be-truthy)
      (expect (cl-tmux::%csi-u-accumulating-p full 7) :to-be-falsy))
    ;; ESC [ 9 7  — mid-accumulation digit prefix
    (let ((v (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
      (dolist (b '(27 91 57 55)) (vector-push-extend b v))
      (expect (cl-tmux::%csi-u-accumulating-p v 4) :to-be-truthy)
      (expect (cl-tmux::%csi-u-terminated-p v 4) :to-be-falsy))
    ;; ESC [ M …  (X10 mouse) and ESC [ <  (SGR) must NOT look like CSI-u
    (let ((m (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
      (dolist (b '(27 91 77)) (vector-push-extend b m))    ; ESC [ M
      (expect (cl-tmux::%csi-u-accumulating-p m 3) :to-be-falsy)))

  ;;; ── Extended keys (CSI u) end-to-end through process-byte ────────────────────

  ;; bind -n C-S-a next-window: a Ctrl+Shift+a extended-key (ESC [ 97 ; 6 u) runs
  ;; next-window at root.  This exercises the CSI-u name path, and the
  ;; multi-digit codepoint 97 must not be dropped by the generic forward.
  (it "root-csi-u-name-binding-fires"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "-n" "C-S-a" "next-window"))
      (with-fake-session (s :nwindows 2)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 57 55 59 54 117))  ; ESC [ 9 7 ; 6 u
            (cl-tmux::process-byte s b state))
          (expect (eq (second (session-windows s)) (session-active-window s)))))))

  ;; bind -n S-Tab next-window: Shift+Tab (ESC [ 9 ; 2 u) runs next-window.  The
  ;; single-digit codepoint 9 must accumulate past the 3-byte-CSI branch rather than
  ;; be misread as a bare ESC [ 9 arrow/copy escape.
  (it "root-csi-u-shift-tab-single-digit-codepoint"
    (with-isolated-config
      (cl-tmux/config:apply-config-directive '("bind" "-n" "S-Tab" "next-window"))
      (with-fake-session (s :nwindows 2)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 57 59 50 117))  ; ESC [ 9 ; 2 u
            (cl-tmux::process-byte s b state))
          (expect (eq (second (session-windows s)) (session-active-window s)))))))

  ;; In emacs mode, CSI-u Ctrl+Meta keys use the copy-mode table as C-M-* keys.
  (it "copy-mode-csi-u-control-meta-key-uses-copy-mode-table"
    (with-isolated-config
      (cl-tmux/options:set-option "mode-keys" "emacs")
      (cl-tmux/config:key-table-bind "copy-mode-vi" "C-M-b" :copy-mode-page-up)
      (cl-tmux/config:key-table-bind "copy-mode" "C-M-b" :copy-mode-exit)
      (with-copy-mode-state (s screen state)
          (dolist (b '(27 91 57 56 59 55 117)) ; ESC [ 98 ; 7 u
            (cl-tmux::process-byte s b state))
        (expect (cl-tmux/terminal:screen-copy-mode-p screen) :to-be-falsy))))

  ;; A digit CSI that ends in '~' (F5 = ESC [ 15 ~), not 'u', is not a CSI-u chord:
  ;; the safety-net branch forwards the whole sequence raw to the pane rather than
  ;; accumulating it forever after CSI-u deferral.
  (it "csi-u-function-key-forwarded-raw-not-dropped"
    (with-pipe-fds (rfd wfd)
      (with-fake-session (s :nwindows 1)
        (setf (pane-fd (window-active-pane (session-active-window s))) wfd)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 53 126))  ; ESC [ 1 5 ~
            (cl-tmux::process-byte s b state))
          (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
            (expect ready :to-be-truthy)
            (when ready
              (cffi:with-foreign-object (buf :uint8 16)
                (let ((n (cffi:foreign-funcall "read"
                                               :int rfd :pointer buf :unsigned-long 16
                                               :long)))
                  (expect (= 5 n))
                  (expect (= 27  (cffi:mem-aref buf :uint8 0)))
                  (expect (= 126 (cffi:mem-aref buf :uint8 4)))))))))))

  ;; Digit-leading CSI input must stay buffered until a CSI final byte arrives.
  ;; This prevents ESC [ 1 5 from being forwarded before the terminating '~'.
  (it "digit-leading-csi-is-buffered-until-final-byte"
    (with-pipe-fds (rfd wfd)
      (with-fake-session (s :nwindows 1)
        (setf (pane-fd (window-active-pane (session-active-window s))) wfd)
        (let ((state (cl-tmux::make-input-state)))
          (dolist (b '(27 91 49 53))  ; ESC [ 1 5
            (cl-tmux::process-byte s b state))
          (expect (cl-tmux/pty:select-fds (list rfd) 20000) :to-be-falsy)
          (cl-tmux::process-byte s 126 state)
          (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
            (expect ready :to-be-truthy)
            (when ready
              (cffi:with-foreign-object (buf :uint8 16)
                (let ((n (cffi:foreign-funcall "read"
                                               :int rfd :pointer buf :unsigned-long 16
                                               :long)))
                  (expect (= 5 n))
                  (expect (= 27  (cffi:mem-aref buf :uint8 0)))
                  (expect (= 126 (cffi:mem-aref buf :uint8 4))))))))))))
