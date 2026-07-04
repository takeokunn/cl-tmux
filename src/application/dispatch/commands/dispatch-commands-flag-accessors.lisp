(in-package #:cl-tmux)

;;; -- Generated flag accessors for dispatch command handlers ------------------

(defmacro define-flag-accessors (&rest specs)
  "Generate flag accessor functions from a fact table.
   Each SPEC is (fn-name doc :value flag-char [default]) or
                (fn-name doc :present flag-char).
   :value   - returns the flag value, or DEFAULT when the flag is absent.
   :present - returns the %flag-present-p result (truthy or NIL)."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (fn-name doc type char &optional default) spec
                   `(defun ,fn-name (flags)
                      ,doc
                      ,(ecase type
                         (:value  (if default
                                      `(or (%flag-value flags ,char) ,default)
                                      `(%flag-value flags ,char)))
                         (:present `(%flag-present-p flags ,char))))))
               specs)))

(define-flag-accessors
  (%buffer-name-from-flags
   "Return the named buffer selected by -b in FLAGS, or NIL when absent."
   :value #\b)
  (%buffer-append-p
   "Return T when the command FLAGS include -a."
   :present #\a)
  (%popup-title-from-flags
   "Return the popup title encoded by FLAGS, or the empty title when absent."
   :value #\T "")
  (%popup-width-from-flags
   "Return the popup width encoded by FLAGS, or NIL when absent."
   :value #\w)
  (%popup-height-from-flags
   "Return the popup height encoded by FLAGS, or NIL when absent."
   :value #\h)
  (%menu-title-from-flags
   "Return the menu title encoded by FLAGS, or the default menu title."
   :value #\T "Menu")
  (%confirm-prompt-from-flags
   "Return the custom confirm prompt encoded by FLAGS, or NIL when absent."
   :value #\p)
  (%list-keys-table-name-from-flags
   "Return the key table encoded by FLAGS, or NIL when absent."
   :value #\T)
  (%copy-mode-scroll-to-top-p
   "Return T when FLAGS request copy-mode to start at the top."
   :present #\u)
  (%copy-mode-exit-on-bottom-p
   "Return T when FLAGS request copy-mode to exit at the bottom."
   :present #\e))
