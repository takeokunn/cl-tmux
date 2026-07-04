(in-package #:cl-tmux)

;;;; Key-table binding execution helpers.

(defun %key-table-entry-by-candidates (table candidates)
  "Return the first key-table entry in TABLE that matches one of CANDIDATES."
  (loop for candidate in candidates
        for entry = (key-table-lookup table candidate)
        when entry
          return entry))

(defun %run-key-table-binding (session entry byte)
  "Execute the command bound to a key-table ENTRY.
   BYTE is the originating key byte; pass NIL for synthetic chords."
  (let ((cmd (key-table-command entry)))
    (cond
      ((and (consp cmd) (eq (car cmd) :sequence))
       (dolist (subcmd (cdr cmd))
         (%run-command-tokens session subcmd)))
      ((consp cmd)
       (%run-command-tokens session cmd))
      (t
       (dispatch-command session cmd byte)))))

(defun %run-bound-string-key (session table key-string)
  "Run string KEY-STRING from TABLE and return its entry, or NIL if unbound."
  (let ((entry (and key-string (key-table-lookup table key-string))))
    (when entry
      (%run-key-table-binding session entry nil)
      (setf *dirty* t)
      entry)))

(defun %try-bound-string-key (session table key-string)
  "Run KEY-STRING from TABLE and return true when a binding exists."
  (and (%run-bound-string-key session table key-string) t))

(defun %copy-mode-table-or-nil (session)
  "Return the active copy-mode table when COPY-MODE is enabled, otherwise NIL."
  (and (%copy-mode-active-p session)
       (%active-copy-mode-table)))

(defun %try-bound-string-key-in-order (session key-string &rest tables)
  "Try KEY-STRING against TABLES in order until one binding runs."
  (loop for table in tables
        when (and table (%try-bound-string-key session table key-string))
          return t))

(defun %try-bound-string-key-root-then-copy-mode (session key-string)
  "Try ROOT first, then the active copy-mode table when copy mode is enabled."
  (%try-bound-string-key-in-order session key-string
                                  +table-root+
                                  (%copy-mode-table-or-nil session)))

(defun %try-bound-string-key-copy-mode-then-root (session key-string)
  "Try the active copy-mode table first, then ROOT."
  (%try-bound-string-key-in-order session key-string
                                  (%copy-mode-table-or-nil session)
                                  +table-root+))

(defun %prefix-string-entry-result (entry)
  "Return the CPS outcome/state pair for a prefix string-key ENTRY."
  (if (and entry (key-table-repeatable-p entry))
      (values :repeatable #'%after-prefix-input-state)
      (values nil #'%ground-input-state)))

(defun %dispatch-modifier-arrow (session mod-byte final-byte)
  "Handle the modifier+arrow combination inside ESC [ 1 ; MOD FINAL."
  (let ((key (%modifier-arrow-key-name mod-byte final-byte)))
    (or (%run-bound-string-key session +table-prefix+ key)
        (let ((window (session-active-window session)))
          (when window
            (cond
              ((= mod-byte +byte-csi-mod-ctrl+)
               (case final-byte
                 (#.+byte-arrow-up+    (resize-pane window :up    1))
                 (#.+byte-arrow-down+  (resize-pane window :down  1))
                 (#.+byte-arrow-right+ (resize-pane window :right 1))
                 (#.+byte-arrow-left+  (resize-pane window :left  1))))
              ((= mod-byte +byte-csi-mod-meta+)
               (let ((command (case final-byte
                                (#.+byte-arrow-up+    :resize-up)
                                (#.+byte-arrow-down+  :resize-down)
                                (#.+byte-arrow-right+ :resize-right)
                                (#.+byte-arrow-left+  :resize-left)
                                (otherwise nil))))
                 (when command (dispatch-command session command nil)))))))
        nil)))
