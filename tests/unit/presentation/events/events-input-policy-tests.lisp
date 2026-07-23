(in-package #:cl-tmux/test)

;;;; Events tests: input policy for rename, backspace, and assume-paste.

(defmacro define-rename-from-osc-title-cases (&body cases)
  "Define %RENAME-FROM-OSC-TITLE tests from declarative rows."
  `(progn
     ,@(loop for (name doc title allow-title expected assertion-message) in cases
             collect (locally (declare (ignore doc assertion-message))
                       `(it ,(string-downcase (symbol-name name))
                          (with-screen (sc 20 5)
                            (setf (screen-title sc) ,title)
                            (expect (string= ,expected
                                             (cl-tmux::%rename-from-osc-title sc
                                                                              ,allow-title)))))))))

(defmacro define-backspace-option-byte-cases (&body cases)
  "Define %BACKSPACE-OPTION-BYTE parsing tests from declarative rows."
  `(progn
     ,@(loop for (name doc option-value expected assertion-message) in cases
             collect (locally (declare (ignore doc assertion-message))
                       `(it ,(string-downcase (symbol-name name))
                          (with-isolated-config
                            (cl-tmux/options:set-server-option "backspace"
                                                               ,option-value)
                            (expect (eql ,expected
                                        (cl-tmux::%backspace-option-byte)))))))))

(describe "events-suite"

  (define-rename-from-osc-title-cases
    (rename-from-osc-title-returns-title-when-allow-title
     "%rename-from-osc-title returns the non-empty screen title when ALLOW-TITLE is T."
     "my-title" t "my-title"
     "%rename-from-osc-title must return the screen title when allow-title is T")
    (rename-from-osc-title-returns-empty-when-not-allow-title
     "%rename-from-osc-title returns empty string when ALLOW-TITLE is NIL."
     "my-title" nil ""
     "%rename-from-osc-title must return \"\" when allow-title is NIL")
    (rename-from-osc-title-returns-empty-when-title-is-empty
     "%rename-from-osc-title returns empty string when the screen title is empty,
     even when ALLOW-TITLE is T."
     "" t ""
     "%rename-from-osc-title must return \"\" when the title is empty"))

  ;; %auto-rename-name returns the OSC title for a pane with no real process (pid <= 0).
  (it "auto-rename-name-uses-osc-title-for-process-less-pane"
    (with-auto-rename-session (screen pane win sess :win-name "old")
      ;; pane-pid is <= 0 by default in with-auto-rename-session.
      (setf (screen-title screen) "osc-title")
      (expect (string= "osc-title"
                       (cl-tmux::%auto-rename-name sess win pane screen :allow-title t)))))

  (define-backspace-option-byte-cases
    (backspace-option-byte-c-question-is-del
     "%backspace-option-byte parses C-? as DEL."
     "C-?" 127
     "C-? is DEL (the identity default)")
    (backspace-option-byte-c-h-is-bs
     "%backspace-option-byte parses C-h as BS."
     "C-h" 8
     "C-h is BS")
    (backspace-option-byte-c-uppercase-h-is-bs
     "%backspace-option-byte parses C-H as BS."
     "C-H" 8
     "C-H (uppercase) is BS")
    (backspace-option-byte-single-character-is-own-code
     "%backspace-option-byte parses a single character as its character code."
     "x" 120
     "a single character is its own code")
    (backspace-option-byte-bogus-value-is-nil
     "%backspace-option-byte returns NIL for unrecognised values."
     "bogus-value" nil
     "unrecognised values yield NIL"))

  ;; %translate-backspace-octets rewrites DEL per the backspace option and is the
  ;; identity for the default C-?.
  (it "translate-backspace-octets-rewrites-del"
    (with-isolated-config
      (let ((octets (coerce #(97 127 98) '(vector (unsigned-byte 8)))))
        (cl-tmux/options:set-server-option "backspace" "C-h")
        (expect (equalp #(97 8 98) (cl-tmux::%translate-backspace-octets octets)))
        (cl-tmux/options:set-server-option "backspace" "C-?")
        (expect (eq octets (cl-tmux::%translate-backspace-octets octets))))))

  ;; %assume-paste-byte-p: NIL with no key history; T within the window after a
  ;; forwarded key; NIL when assume-paste-time is 0.
  (it "assume-paste-byte-p-table"
    (with-isolated-config
      (let ((cl-tmux::*last-ground-key-time* nil))
        (expect (null (cl-tmux::%assume-paste-byte-p)))
        (cl-tmux/options:set-option "assume-paste-time" 1000) ; generous 1s window
        (cl-tmux::%stamp-ground-key-time)
        (expect (eq t (and (cl-tmux::%assume-paste-byte-p) t)))
        (cl-tmux/options:set-option "assume-paste-time" 0)
        (expect (null (cl-tmux::%assume-paste-byte-p))))))

  ;; A root -n bound key arriving within assume-paste-time of pane content is
  ;; forwarded to the pane instead of running the binding (tmux paste protection);
  ;; with assume-paste-time 0 the binding runs.
  (it "assume-paste-time-bypasses-root-binding-during-burst"
    (dolist (row '((1000 nil "fast key during a burst must NOT run the binding")
                   (0    t   "assume-paste-time 0 must run the binding")))
      (destructuring-bind (paste-ms expect-switch desc) row
        (declare (ignore desc))
        (with-isolated-config
          (with-fake-session (s :nwindows 2)
            (cl-tmux/options:set-option "assume-paste-time" paste-ms)
            (cl-tmux/config:key-table-bind "root" #\x :next-window)
            (let ((first-win (cl-tmux/model:session-active-window s))
                  (state (cl-tmux::make-input-state)))
              ;; Plain content byte: forwarded, stamps the burst clock.
              (cl-tmux::process-byte s (char-code #\a) state)
              ;; Bound key arrives "immediately" (microseconds later).
              (cl-tmux::process-byte s (char-code #\x) state)
              (if expect-switch
                  (expect (not (eq first-win (cl-tmux/model:session-active-window s))))
                  (expect (eq first-win (cl-tmux/model:session-active-window s)))))))))))
