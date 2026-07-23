;;;; Overlay, prompt, and sequence assertion helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

(defun overlay-text (overlay)
  "Normalize OVERLAY contents to a string suitable for substring checks."
  (cond
    ((null overlay) "")
    ((stringp overlay) overlay)
    ((listp overlay) (format nil "~{~A~%~}" overlay))
    (t (princ-to-string overlay))))

(defmacro assert-overlay-contains (needle overlay &optional (context "overlay"))
  "Assert that an active overlay contains NEEDLE in its rendered text."
  (declare (ignore context))
  `(let ((text (overlay-text ,overlay)))
     (expect (overlay-active-p) :to-be-truthy)
     (expect (search ,needle text))))

(defmacro assert-overlay-contains-all (needles overlay &optional (context "overlay"))
  "Assert that an active overlay contains every string in NEEDLES."
  (declare (ignore context))
  `(let ((text (overlay-text ,overlay)))
     (expect (overlay-active-p) :to-be-truthy)
     (dolist (needle ,needles)
       (expect (search needle text)))))

(defmacro assert-command-args-rejected-without-redraw (form args
                                                       &key
                                                       (message "unsupported argument")
                                                       (context "command"))
  "Assert that FORM rejects ARGS without scheduling a redraw."
  (declare (ignore context args))
  `(progn
     (expect (null ,form))
     (expect cl-tmux::*dirty* :to-be-falsy)
     (assert-overlay-contains ,message cl-tmux::*overlay*)))

(defmacro with-temporary-posix-environment-variable ((name value) &body body)
  "Bind NAME to VALUE in the real process environment for BODY and restore it."
  (let ((old-value (gensym "OLD")))
    `(let ((,old-value (ignore-errors (sb-ext:posix-getenv ,name))))
       (unwind-protect
            (progn
              (if ,value
                  (ignore-errors (sb-posix:setenv ,name ,value 1))
                  (ignore-errors (sb-posix:unsetenv ,name)))
              ,@body)
         (if ,old-value
             (ignore-errors (sb-posix:setenv ,name ,old-value 1))
             (ignore-errors (sb-posix:unsetenv ,name)))))))

(defmacro assert-overlay-not-contains (needle overlay &optional (context "overlay"))
  "Assert that an active overlay does not contain NEEDLE in its rendered text."
  (declare (ignore context))
  `(let ((text (overlay-text ,overlay)))
     (expect (overlay-active-p) :to-be-truthy)
     (expect (null (search ,needle text)))))

(defmacro assert-overlay-active (&rest args)
  "Assert that an overlay is currently active."
  (declare (ignore args))
  `(expect (overlay-active-p) :to-be-truthy))

(defmacro assert-overlay-inactive (&optional (context "overlay"))
  "Assert that an overlay is currently inactive."
  (declare (ignore context))
  `(expect (overlay-active-p) :to-be-falsy))

(defmacro assert-member (needle sequence &key (test '#'equal) (context "sequence"))
  "Assert that SEQUENCE contains NEEDLE under TEST."
  (declare (ignore context))
  `(expect (member ,needle ,sequence :test ,test)))

(defmacro assert-not-member (needle sequence &key (test '#'equal) (context "sequence"))
  "Assert that SEQUENCE does not contain NEEDLE under TEST."
  (declare (ignore context))
  `(expect (null (member ,needle ,sequence :test ,test))))

(defmacro assert-config-directive-rejected (form &optional (context "config directive"))
  "Assert that APPLY-CONFIG-DIRECTIVE rejects FORM and returns NIL."
  (declare (ignore context))
  `(expect (null (apply-config-directive ,form))))

(defmacro assert-config-directive-safe-nil (form &optional (context "config directive"))
  "Assert that APPLY-CONFIG-DIRECTIVE returns NIL without signaling."
  `(let ((result (handler-case (apply-config-directive ,form)
                   (error (e)
                     (fail "~A must not signal, got ~A" ,context e)
                     :signaled))))
     (expect (null result))))

(defmacro assert-config-directive-applied (form &optional (context "config directive"))
  "Assert that APPLY-CONFIG-DIRECTIVE returns T."
  (declare (ignore context))
  `(expect (eq t (apply-config-directive ,form))))

(defmacro assert-overlay-uses-custom-format (needles overlay &optional (context "overlay"))
  "Assert that an overlay shows NEEDLES and does not fall back to the default listing."
  `(progn
     (assert-overlay-contains-all ,needles ,overlay ,context)
     (assert-overlay-not-contains "[" ,overlay
                                  ,(format nil "~A must replace the default listing" context))))

(defmacro with-overlay-session ((session-spec &key context) setup-form &body body)
  "Run SETUP-FORM in a fake session and assert that it opens an overlay."
  (declare (ignore context))
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-fake-session (,session-var ,@session-args)
       (let ((*overlay* nil))
         ,setup-form
         (expect (overlay-active-p) :to-be-truthy)
         ,@body))))

(defmacro with-dispatch-overlay ((session-spec command &key args context)
                                 &body body)
  "Run DISPATCH-COMMAND for COMMAND in a fake session and assert an overlay opens."
  `(with-overlay-session (,session-spec :context ,(or context "dispatch-command must open an overlay"))
       (cl-tmux::dispatch-command ,(if (consp session-spec) (first session-spec) session-spec)
                                  ,command ,args)
     ,@body))

(defmacro with-run-command-line-overlay ((session-spec command &key context)
                                         &body body)
  "Run %RUN-COMMAND-LINE for COMMAND in a fake session and assert an overlay opens."
  (declare (ignore context))
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    (if session-args
        `(with-overlay-session (,session-spec)
             (cl-tmux::%run-command-line ,session-var
                                         ,command)
           ,@body)
        `(let ((*overlay* nil))
           (cl-tmux::%run-command-line ,session-var
                                       ,command)
           (expect (overlay-active-p) :to-be-truthy)
           ,@body))))

(defmacro with-dispatch-prompt ((session-spec command &key args label context)
                                &body body)
  "Run DISPATCH-COMMAND for COMMAND in a fake session and assert a prompt opens."
  (declare (ignore context))
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-fake-session (,session-var ,@session-args)
       (let ((*prompt* nil))
         (cl-tmux::dispatch-command ,session-var ,command ,args)
         (expect (prompt-active-p) :to-be-truthy)
         ,(when label
            `(expect (string= ,label (prompt-label *prompt*))))
         ,@body))))

(defmacro assert-overlay-rejects-before-row (overlay message row-token
                                            &optional (context "overlay"))
  "Assert that OVERLAY reports MESSAGE and does not fall through to ROW-TOKEN."
  (declare (ignore context))
  `(let ((text (overlay-text ,overlay)))
     (expect (overlay-active-p) :to-be-truthy)
     (expect (search ,message text))
     (expect (null (search ,row-token text)))))
