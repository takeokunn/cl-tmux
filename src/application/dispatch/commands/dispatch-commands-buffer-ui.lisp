(in-package #:cl-tmux)

;;; -- Popup, menu, confirmation, and key-list %cmd-* handlers ------------------

(defconstant +popup-max-width+  60 "Maximum column width of a popup overlay.")
(defconstant +popup-max-height+ 15 "Maximum row height of a popup overlay.")
(defconstant +popup-margin+      4 "Row margin subtracted from terminal height for popups.")

(defun %popup-border-chars ()
  "Return popup border drawing characters for the text overlay."
  (multiple-value-bind (tl tr bl br h v) (cl-tmux/renderer:%popup-border-charset)
    (declare (ignore v))
    (values tl tr bl br h)))

(defun %format-popup-overlay (title output)
  "Format a popup overlay string with box-drawing borders."
  (multiple-value-bind (tl tr bl br h) (%popup-border-chars)
    (format nil "~C~C ~A ~C~C~%~A~%~C~A~C"
            tl h title h tr
            (or output "")
            bl (make-string (+ 2 (length title)) :initial-element h) br)))

(defun %show-popup-command-output (title command width height)
  "Run COMMAND, open a popup sized WIDTH x HEIGHT, and render the command output."
  (let* ((label  (if (plusp (length title)) title command))
         (output (run-shell command)))
    (show-popup (make-popup :title label :width width :height height
                            :screen nil :pane nil))
    (show-overlay (%format-popup-overlay label output))))

(defun %popup-dimension (spec axis-total fallback)
  "Resolve a popup -w/-h dimension SPEC against AXIS-TOTAL."
  (let ((n (cond
             ((null spec) fallback)
             ((and (plusp (length spec))
                   (char= (char spec (1- (length spec))) #\%))
              (let ((pct (parse-integer spec :end (1- (length spec)) :junk-allowed t)))
                (if pct (max 1 (floor (* axis-total pct) 100)) fallback)))
             (t (or (parse-integer spec :junk-allowed t) fallback)))))
    (max 1 (min n axis-total))))

(defun %cmd-display-popup (session args)
  "display-popup [-E] [-w width] [-h height] [-x col] [-y row]
   [-d dir] [-t target] [-c client] [-b border] [-T title] [command]: show a popup."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "whxydtcbTesS")
    (let* ((title   (%popup-title-from-flags flags))
           (command (when positionals (format nil "~{~A~^ ~}" positionals)))
           (width   (%popup-dimension (%popup-width-from-flags flags) *term-cols* +popup-max-width+))
           (height  (%popup-dimension (%popup-height-from-flags flags) *term-rows*
                                      (min +popup-max-height+ (- *term-rows* +popup-margin+))))
           (clamp-w (min width  *term-cols*))
           (clamp-h (min height (max 1 (- *term-rows* +popup-margin+)))))
      (if command
          (%show-popup-command-output title command clamp-w clamp-h)
          (prompt-nonempty "popup command"
                           (lambda (cmd)
                             (%show-popup-command-output title cmd clamp-w clamp-h)))))))

(defun %cmd-display-menu-arg (session args)
  "display-menu [-T title] [-x x] [-y y] [label key command ...]: show an interactive menu."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "TxybcsSt")
    (let* ((title (%menu-title-from-flags flags))
           (menu-x (%parse-flag-int flags #\x))
           (menu-y (%parse-flag-int flags #\y))
           (items (loop for (label key cmd) on positionals by #'cdddr
                        when (and label key cmd)
                        collect (cons (if (and (plusp (length label))
                                               (plusp (length key)))
                                          (format nil "~A [~A]" label key)
                                          label)
                                      cmd))))
      (cond
        ((null positionals)
         (%overlayf "command display-menu: too few arguments (need at least 1)"))
        (items
         (show-menu (make-menu :title title :items items :selected-index 0
                               :x menu-x :y menu-y
                               :keep-open (%flag-present-p flags #\O)))
         (show-overlay (%format-menu *active-menu*)))))))

(defun %cmd-confirm-before-arg (session args)
  "confirm-before [-y] [-p prompt] [-c confirm-key] [-t target] command."
  (with-command-flags+pos (flags positionals args "pct")
    (multiple-value-bind (window pane) (%active-window-pane session)
      (let* ((custom-prompt (%confirm-prompt-from-flags flags))
             (assume-yes    (%flag-present-p flags #\y))
             (cmd-line      (format nil "~{~A~^ ~}" positionals))
             (ctx           (cl-tmux/format:format-context-from-session
                             session window pane))
             (prompt-text   (if custom-prompt
                                (cl-tmux/format:expand-format-safe custom-prompt ctx)
                                (format nil "~A? (y/n)" cmd-line))))
        (when (plusp (length cmd-line))
          (if assume-yes
              (%run-command-line session cmd-line)
              (%confirm-prompt prompt-text
                               (lambda ()
                                 (%run-command-line session cmd-line)))))))))

(defun %cmd-list-keys-arg (session args)
  "list-keys [-1aN] [-P prefix] [-T table] [key]: list key bindings."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "T1P")
    (let* ((table-name (%list-keys-table-name-from-flags flags))
           (key        (first positionals))
           (output     (cond
                         ((%flag-present-p flags #\N)
                          (cl-tmux/config:describe-key-binding-notes
                           table-name (%flag-present-p flags #\a)))
                         (key
                          (cl-tmux/config:describe-key-bindings-for-key table-name key))
                         (t
                          (cl-tmux/config:describe-key-bindings-for-table table-name))))
           (output     (if (%flag-present-p flags #\1)
                           (let ((newline (position #\Newline output)))
                             (if newline
                                 (subseq output 0 newline)
                                 output))
                           output)))
      (show-overlay (if (plusp (length output))
                        output
                        (format nil "(no bindings in table ~A)"
                                (or table-name "all")))))))
