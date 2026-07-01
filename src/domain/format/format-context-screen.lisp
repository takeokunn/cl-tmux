(in-package #:cl-tmux/format)

;;;; Pane-geometry / screen / client section builders for format-context.
;;;;
;;;; Split out of format-context.lisp: these three section builders are more
;;;; mechanical getter tables (pane bounding-box arithmetic, copy-mode/screen
;;;; state, and client/server/host environment lookups) than the model-facing
;;;; session/window/pane-structural builders that remain in format-context.lisp.
;;;; format-context-from-session (format-context.lisp) appends all six slices.

;;; ── Client section helpers ──────────────────────────────────────────────────

(defun %current-time-string ()
  "Return HH:MM string from the system clock."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %short-hostname (h)
  "Return the hostname up to the first dot, or the full string if no dot."
  (subseq h 0 (or (position #\. h) (length h))))

;;; ── Context plist section builders ──────────────────────────────────────────

(defun %pane-geometry-context-plist (pane window)
  "Build the pane-geometry slice of the format-context plist for PANE within WINDOW."
  (list :pane-width           (if pane (cl-tmux/model:pane-width  pane) 0)
        :pane-height          (if pane (cl-tmux/model:pane-height pane) 0)
        :pane-pid             (if pane (cl-tmux/model:pane-pid    pane) 0)
        :pane-left            (if pane (cl-tmux/model:pane-x      pane) 0)
        :pane-top             (if pane (cl-tmux/model:pane-y      pane) 0)
        :pane-right           (if pane (+ (cl-tmux/model:pane-x pane)
                                          (cl-tmux/model:pane-width pane) -1) 0)
        :pane-bottom          (if pane (+ (cl-tmux/model:pane-y pane)
                                          (cl-tmux/model:pane-height pane) -1) 0)
        :pane-at-top          (if (and pane (= (cl-tmux/model:pane-y pane) 0)) "1" "0")
        :pane-at-left         (if (and pane (= (cl-tmux/model:pane-x pane) 0)) "1" "0")
        :pane-at-bottom       (if (and pane window
                                       (= (+ (cl-tmux/model:pane-y    pane)
                                             (cl-tmux/model:pane-height pane))
                                          (cl-tmux/model:window-height window)))
                                  "1" "0")
        :pane-at-right        (if (and pane window
                                       (= (+ (cl-tmux/model:pane-x   pane)
                                             (cl-tmux/model:pane-width pane))
                                          (cl-tmux/model:window-width window)))
                                  "1" "0")))

(defun %screen-context-plist (pane-scr cursor-x cursor-y)
  "Build the screen/copy-mode slice of the format-context plist for PANE-SCR.
   PANE-SCR is the pane's screen object (or NIL); CURSOR-X and CURSOR-Y are
   its pre-computed cursor coordinates."
  (list :cursor-x             cursor-x
        :cursor-y             cursor-y
        :cursor-character
        (if (and pane-scr
                 (< -1 cursor-x (cl-tmux/terminal:screen-width  pane-scr))
                 (< -1 cursor-y (cl-tmux/terminal:screen-height pane-scr)))
            (string (cl-tmux/terminal:cell-char
                     (cl-tmux/terminal:screen-cell pane-scr cursor-x cursor-y)))
            "")
        :pane-in-mode         (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr)) "1" "0")
        :pane-mode            (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr)) "copy-mode" "")
        :scroll-position      (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                  (format nil "~D" (cl-tmux/terminal:screen-copy-offset pane-scr))
                                  "")
        :copy-position        (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                  (format nil "~D" (cl-tmux/terminal:screen-copy-offset pane-scr))
                                  "")
        :copy-position-limit  (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                  (format nil "~D" (length (cl-tmux/terminal:screen-scrollback pane-scr)))
                                  "")
        :selection-active     (if (and pane-scr
                                       (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                       (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                  "1" "0")
        :selection-present    (if (and pane-scr
                                       (cl-tmux/terminal:screen-copy-mode-p pane-scr)
                                       (cl-tmux/terminal:screen-copy-selecting pane-scr))
                                  "1" "0")
        :copy-cursor-x        (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                  (format nil "~D" (cdr (cl-tmux/terminal:screen-copy-cursor pane-scr)))
                                  "")
        :copy-cursor-y        (if (and pane-scr (cl-tmux/terminal:screen-copy-mode-p pane-scr))
                                  (format nil "~D" (car (cl-tmux/terminal:screen-copy-cursor pane-scr)))
                                  "")
        :history-size         (format nil "~D"
                                      (if pane-scr
                                          (length (cl-tmux/terminal:screen-scrollback pane-scr))
                                          0))))

(defun %client-context-plist (client-width client-height client-tty hostname pid-str)
  "Build the client/server/host/environment slice of the format-context plist.
   CLIENT-WIDTH, CLIENT-HEIGHT, and CLIENT-TTY describe the attached client;
   HOSTNAME and PID-STR are pre-computed by the caller."
  (list :client-width         client-width
        :client-height        client-height
        :client-tty           client-tty
        :client-name          client-tty
        :client-termname      (or (ignore-errors (sb-ext:posix-getenv "TERM")) "")
        :client-pid           pid-str
        :client-prefix        (if (ignore-errors
                                    (symbol-value (find-symbol "*PREFIX-ACTIVE*" "CL-TMUX")))
                                  "1" "0")
        :client-last-session  ""
        :server-pid           pid-str
        :version              (cl-tmux/version:version-string)
        :hostname             hostname
        :host                 hostname
        :host-short           (%short-hostname hostname)
        :time                 (%current-time-string)
        :term-program         (or (ignore-errors (sb-ext:posix-getenv "TERM_PROGRAM")) "")
        :colorterm            (or (ignore-errors (sb-ext:posix-getenv "COLORTERM")) "")
        :history-limit        (format nil "~D"
                                      (or (cl-tmux/options:get-option "history-limit") 2000))))
