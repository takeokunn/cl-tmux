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
  `(let ((text (overlay-text ,overlay)))
     (is (overlay-active-p)
         "~A must open an overlay" ,context)
     (is (search ,needle text)
         "~A must report ~S (got ~S)" ,context ,needle text)))

(defmacro assert-overlay-contains-all (needles overlay &optional (context "overlay"))
  "Assert that an active overlay contains every string in NEEDLES."
  `(let ((text (overlay-text ,overlay)))
     (is (overlay-active-p)
         "~A must open an overlay" ,context)
     (dolist (needle ,needles)
       (is (search needle text)
           "~A must report ~S (got ~S)" ,context needle text))))

(defmacro assert-command-args-rejected-without-redraw (form args
                                                       &key
                                                       (message "unsupported argument")
                                                       (context "command"))
  "Assert that FORM rejects ARGS without scheduling a redraw."
  `(progn
     (is (null ,form)
         "~A must reject unsupported args: ~S" ,context ,args)
     (is-false cl-tmux::*dirty*
               "~A must not redraw after rejecting: ~S" ,context ,args)
     (assert-overlay-contains ,message cl-tmux::*overlay*
                              (format nil "~A rejection for ~S" ,context ,args))))

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
  `(let ((text (overlay-text ,overlay)))
     (is (overlay-active-p)
         "~A must open an overlay" ,context)
     (is (null (search ,needle text))
         "~A must not report ~S (got ~S)" ,context ,needle text)))

(defmacro assert-overlay-active (&rest args)
  "Assert that an overlay is currently active."
  (let ((message (if args
                     (apply #'format nil "~A must open an overlay" args)
                     "overlay must open an overlay")))
    `(is (overlay-active-p)
         ,message)))

(defmacro assert-overlay-inactive (&optional (context "overlay"))
  "Assert that an overlay is currently inactive."
  `(is (not (overlay-active-p))
       "~A must not open an overlay" ,context))

(defmacro assert-member (needle sequence &key (test #'equal) (context "sequence"))
  "Assert that SEQUENCE contains NEEDLE under TEST."
  `(is (member ,needle ,sequence :test ,test)
       "~A must contain ~S (got ~S)" ,context ,needle ,sequence))

(defmacro assert-not-member (needle sequence &key (test #'equal) (context "sequence"))
  "Assert that SEQUENCE does not contain NEEDLE under TEST."
  `(is (null (member ,needle ,sequence :test ,test))
       "~A must not contain ~S (got ~S)" ,context ,needle ,sequence))

(defmacro assert-members (needles sequence &key (test #'equal) (context "sequence"))
  "Assert that SEQUENCE contains every item in NEEDLES."
  `(dolist (needle ,needles)
     (assert-member needle ,sequence :test ,test :context ,context)))

(defmacro assert-config-directive-rejected (form &optional (context "config directive"))
  "Assert that APPLY-CONFIG-DIRECTIVE rejects FORM and returns NIL."
  `(is (null (apply-config-directive ,form))
       "~A must be rejected (got NIL)" ,context))

(defmacro assert-config-directive-safe-nil (form &optional (context "config directive"))
  "Assert that APPLY-CONFIG-DIRECTIVE returns NIL without signaling."
  `(let ((result (handler-case (apply-config-directive ,form)
                   (error (e)
                     (fail "~A must not signal, got ~A" ,context e)
                     :signaled))))
     (is (null result)
         "~A must return NIL" ,context)))

(defmacro assert-config-directive-applied (form &optional (context "config directive"))
  "Assert that APPLY-CONFIG-DIRECTIVE returns T."
  `(is (eq t (apply-config-directive ,form))
       "~A should return T" ,context))

(defmacro assert-overlay-uses-custom-format (needles overlay &optional (context "overlay"))
  "Assert that an overlay shows NEEDLES and does not fall back to the default listing."
  `(progn
     (assert-overlay-contains-all ,needles ,overlay ,context)
     (assert-overlay-not-contains "[" ,overlay
                                  ,(format nil "~A must replace the default listing" context))))

(defmacro with-overlay-session ((session-spec &key context) setup-form &body body)
  "Run SETUP-FORM in a fake session and assert that it opens an overlay."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-fake-session (,session-var ,@session-args)
       (let ((*overlay* nil))
         ,setup-form
         (is (overlay-active-p)
             ,(or context "overlay must open"))
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
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    (if session-args
        `(with-overlay-session (,session-spec :context ,(or context "%run-command-line must open an overlay"))
             (cl-tmux::%run-command-line ,session-var
                                         ,command)
           ,@body)
        `(let ((*overlay* nil))
           (cl-tmux::%run-command-line ,session-var
                                       ,command)
           (is (overlay-active-p)
               ,(or context "%run-command-line must open an overlay"))
           ,@body))))

(defmacro with-dispatch-prompt ((session-spec command &key args label context)
                                &body body)
  "Run DISPATCH-COMMAND for COMMAND in a fake session and assert a prompt opens."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-fake-session (,session-var ,@session-args)
       (let ((*prompt* nil))
         (cl-tmux::dispatch-command ,session-var ,command ,args)
         (is (prompt-active-p)
             ,(or context "dispatch-command must open a prompt"))
         ,(when label
            `(is (string= ,label (prompt-label *prompt*))
                 ,(format nil "~A prompt label must be ~S" command label)))
         ,@body))))

(defmacro assert-overlay-rejects-before-row (overlay message row-token
                                            &optional (context "overlay"))
  "Assert that OVERLAY reports MESSAGE and does not fall through to ROW-TOKEN."
  `(let ((text (overlay-text ,overlay)))
     (is (overlay-active-p)
         "~A must open an overlay" ,context)
     (is (search ,message text)
         "~A must report ~S (got ~S)" ,context ,message text)
     (is (null (search ,row-token text))
         "~A must not fall through to row output ~S (got ~S)"
         ,context ,row-token text)))
