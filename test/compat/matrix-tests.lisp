(in-package #:cl-tmux/test)

(def-suite compat-suite
  :description "Differential compatibility checks against the local tmux binary")
(in-suite compat-suite)

(defun %compat-matrix-path ()
  (asdf:system-relative-pathname :cl-tmux/test
                                 "test/tmux-compat-matrix.sexp"))

(defun %read-compat-matrix ()
  (with-open-file (stream (%compat-matrix-path)
                          :direction :input)
    (read stream)))

(defstruct compat-run-result stdout stderr exit-code)

(defconstant +compat-command-timeout+ 10
  "Wall-clock timeout, in seconds, for external tmux/cl-tmux compatibility probes.")

(defun %compat-entry (matrix kind name)
  (find-if (lambda (entry)
             (and (eq kind (getf entry :kind))
                  (string= name (getf entry :name))))
           (getf matrix :entries)))

(defun %run-program-result (args)
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program args
                            :output :string
                            :error-output :string
                            :ignore-error-status t
                            :timeout +compat-command-timeout+)
        (make-compat-run-result :stdout out
                                :stderr err
                                :exit-code code))
    (error (c)
      (make-compat-run-result :stdout ""
                              :stderr (princ-to-string c)
                              :exit-code -1))))

(defun %run-program-string (args)
  (handler-case
      (string-right-trim
       '(#\Space #\Tab #\Newline #\Return)
       (uiop:run-program args
                         :output :string
                         :error-output :string
                         :ignore-error-status t
                         :timeout +compat-command-timeout+))
    (error ()
      nil)))

(defun %tmux-clean-socket-name ()
  (format nil "cltmux-compat-~A-~A"
          (get-universal-time)
          (random 1000000)))

(defun %tmux-clean-args (socket args)
  (append (list "tmux" "-L" socket "-f" "/dev/null") args))

(defmacro %with-clean-tmux-server ((socket) &body body)
  `(let ((,socket (%tmux-clean-socket-name)))
     (unwind-protect
          (progn
            (%run-program-result
             (%tmux-clean-args ,socket
                               '("new-session" "-d" "-s" "cltmux_compat_baseline")))
            ,@body)
       (%run-program-result (%tmux-clean-args ,socket '("kill-server"))))))

(defmacro %with-matching-tmux-matrix ((matrix version) &body body)
  "Run BODY only when local tmux is available and matches MATRIX metadata."
  `(let ((,version (%tmux-version)))
     (cond
       ((not ,version)
        (skip "tmux executable unavailable"))
       ((not (string= ,version (getf ,matrix :tmux-version)))
        (skip (format nil "matrix records ~A, local binary is ~A"
                      (getf ,matrix :tmux-version) ,version)))
       (t
        ,@body))))

(defun %tmux-output (&rest args)
  (%run-program-string (cons "tmux" args)))

(defun %tmux-clean-output (socket &rest args)
  (%run-program-string (%tmux-clean-args socket args)))

(defun %cl-tmux-binary ()
  (or (let ((env (sb-ext:posix-getenv "CL_TMUX_COMPAT_BINARY")))
        (and env (plusp (length env)) env))
      (let ((path "result/bin/cl-tmux"))
        (and (probe-file path) path))))

(defun %blank-string-p (value)
  (or (null value)
      (string= "" value)))

(defun %tmux-version ()
  (let ((version (%tmux-output "-V")))
    (unless (%blank-string-p version)
      version)))

(defun %non-empty-lines (text)
  (when text
    (remove-if (lambda (line) (string= line ""))
               (uiop:split-string text :separator '(#\Newline)))))

(defun %sorted-non-empty-lines (text)
  (sort (copy-list (%non-empty-lines text)) #'string<))

(defun %compat-tempdir ()
  (let ((dir (merge-pathnames
              (format nil "cltmux-compat-~A-~A/"
                      (get-universal-time)
                      (random 1000000))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun %cl-tmux-env-args (binary tmpdir args)
  (append (list "env"
                (format nil "TMPDIR=~A"
                        (string-right-trim
                         "/" (namestring (uiop:ensure-directory-pathname tmpdir))))
                binary)
          args))

(defun %cl-tmux-socket-path (tmpdir name)
  (merge-pathnames (format nil "cl-tmux-~A.sock" name)
                   (uiop:ensure-directory-pathname tmpdir)))

(defun %wait-for-cl-tmux-socket (tmpdir name)
  (loop repeat 50
        when (probe-file (%cl-tmux-socket-path tmpdir name))
          return t
        do (sleep 0.1)
        finally (return nil)))

(defmacro %with-clean-cl-tmux-server ((binary tmpdir process name) &body body)
  `(let* ((,tmpdir (%compat-tempdir))
          (,process (uiop:launch-program
                     (%cl-tmux-env-args ,binary ,tmpdir
                                        (list "server" ,name))
                     :output nil
                     :error-output nil)))
     (declare (ignorable ,process))
     (unwind-protect
          (progn
            (is-true (%wait-for-cl-tmux-socket ,tmpdir ,name)
                     "cl-tmux server socket must appear before live probes")
            ,@body)
       (ignore-errors
         (%run-program-result
          (%cl-tmux-env-args ,binary ,tmpdir '("kill-server"))))
       (ignore-errors (uiop:terminate-process ,process))
       (ignore-errors (uiop:wait-process ,process))
       (ignore-errors
         (uiop:delete-directory-tree ,tmpdir
                                     :validate t
                                     :if-does-not-exist :ignore)))))

(defun %tmux-line-count (&rest args)
  (length (%non-empty-lines (apply #'%tmux-output args))))

(defun %tmux-clean-line-count (socket &rest args)
  (length (%non-empty-lines (apply #'%tmux-clean-output socket args))))

(defun %tmux-command-names ()
  (mapcar (lambda (line)
            (first (uiop:split-string line :separator '(#\Space #\Tab))))
          (%non-empty-lines (%tmux-output "list-commands"))))

(defun %tmux-clean-command-names (socket)
  (mapcar (lambda (line)
            (first (uiop:split-string line :separator '(#\Space #\Tab))))
          (%non-empty-lines (%tmux-clean-output socket "list-commands"))))

(defun %cl-tmux-command-inventory-names ()
  (sort
   (remove-duplicates
    (append (mapcar (lambda (symbol)
                      (string-downcase (symbol-name symbol)))
                    cl-tmux/config::*bindable-commands*)
            (mapcan #'copy-list
                    (mapcar #'car cl-tmux::*arg-command-table*)))
    :test #'string=)
   #'string<))

(defun %tmux-clean-hook-names (socket)
  (mapcar (lambda (line)
            (first (uiop:split-string line :separator '(#\Space #\Tab))))
          (%non-empty-lines (%tmux-clean-output socket "show-hooks" "-g"))))

(defparameter +compat-binding-tables+
  '("root" "prefix" "copy-mode" "copy-mode-vi"))

(defparameter +compat-option-scopes+
  '((:global :global-options "show-options" "tmux show-options -g")
    (:window :window-options "show-window-options" "tmux show-window-options -g")))

(defparameter +compat-environment-option-defaults+
  '("default-shell")
  "Option defaults whose tmux value is derived from the caller environment.")

(defun %compat-environment-option-default-p (entry)
  (member (getf entry :name) +compat-environment-option-defaults+
          :test #'string=))

(defun %compat-binding-table-scope-key (table)
  (cdr (assoc table
              '(("root" . :root-bindings)
                ("prefix" . :prefix-bindings)
                ("copy-mode" . :copy-mode-bindings)
                ("copy-mode-vi" . :copy-mode-vi-bindings))
              :test #'string=)))

(defun %tmux-clean-binding-lines (socket table)
  (%non-empty-lines (%tmux-clean-output socket "list-keys" "-T" table)))

(defun %strip-surrounding-quotes (string)
  (if (and (<= 2 (length string))
           (char= #\" (char string 0))
           (char= #\" (char string (1- (length string)))))
      (subseq string 1 (1- (length string)))
      string))

(defun %normalize-tmux-binding-key (key)
  (let ((key (%strip-surrounding-quotes key)))
    (if (and (< 1 (length key))
             (char= #\\ (char key 0)))
        (subseq key 1)
        key)))

(defun %tmux-binding-key-from-line (line)
  (let* ((words (uiop:split-string line :separator '(#\Space #\Tab)))
         (table-marker (member "-T" words :test #'string=))
         (key (third table-marker)))
    (%normalize-tmux-binding-key key)))

(defun %compat-entries (matrix kind)
  (remove-if-not (lambda (entry) (eq kind (getf entry :kind)))
                 (getf matrix :entries)))

(defun %compat-binding-entries (matrix table)
  (remove-if-not (lambda (entry)
                   (and (eq :binding (getf entry :kind))
                        (string= table (getf entry :table))))
                 (getf matrix :entries)))

(defun %compat-option-entries (matrix scope)
  (remove-if-not (lambda (entry)
                   (and (eq :option (getf entry :kind))
                        (eq scope (getf entry :scope))))
                 (getf matrix :entries)))

(defun %compat-entry-names (matrix kind)
  (mapcar (lambda (entry) (getf entry :name))
          (%compat-entries matrix kind)))

(defun %keyword-format-name (keyword)
  (string-downcase
   (substitute #\_ #\- (symbol-name keyword))))

(defun %cl-tmux-format-context-names ()
  (sort
   (remove-if (lambda (name)
                (and (plusp (length name))
                     (char= (char name 0) #\%)))
              (loop for tail on (cl-tmux/format:format-context-from-session nil nil nil)
                    by #'cddr
                    for key = (first tail)
                    collect (%keyword-format-name key)))
   #'string<))

(defun %string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun %cl-tmux-hook-event-names ()
  (let ((names '()))
    (do-external-symbols (symbol (find-package '#:cl-tmux/hooks))
      (when (and (%string-prefix-p "+HOOK-" (symbol-name symbol))
                 (boundp symbol)
                 (stringp (symbol-value symbol)))
        (push (symbol-value symbol) names)))
    (sort (remove-duplicates names :test #'string=) #'string<)))

(defun %cl-tmux-binding-display-key (key)
  (let ((label (cl-tmux/config::key-label key)))
    (cond
      ((and (= 1 (length label))
            (= 2 (char-code (char label 0))))
       "C-b")
      ((string= label " ") "Space")
      ((string= label "PageUp") "PPage")
      ((string= label "PageDown") "NPage")
      (t label))))

(defun %cl-tmux-binding-key-set (table)
  (let ((keys '())
        (cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:initialize-default-key-tables)
    (let ((inner (gethash table cl-tmux/config:*key-tables*)))
      (when inner
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (push (%cl-tmux-binding-display-key key) keys))
                 inner)))
    (sort (remove-duplicates keys :test #'string=) #'string<)))

(defun %duplicate-strings (strings)
  (let ((seen (make-hash-table :test #'equal))
        (duplicates '()))
    (dolist (string strings)
      (if (gethash string seen)
          (pushnew string duplicates :test #'string=)
          (setf (gethash string seen) t)))
    (sort duplicates #'string<)))

(defun %string-set-difference (left right)
  (set-difference left right :test #'string=))

(defun %status-count (matrix status)
  (count-if (lambda (entry) (eq status (getf entry :status)))
            (getf matrix :entries)))

(defun %compat-result-matches-p (left right)
  (and (= (compat-run-result-exit-code left)
          (compat-run-result-exit-code right))
       (string= (compat-run-result-stdout left)
                (compat-run-result-stdout right))
       (string= (compat-run-result-stderr left)
                (compat-run-result-stderr right))))

(defun %normalize-no-server-stderr (text)
  (let ((trimmed (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                    (or text ""))))
    (if (and (search "error connecting to " trimmed)
             (search "(No such file or directory)" trimmed))
        "error connecting to <socket> (No such file or directory)"
        trimmed)))

(defun %no-server-result-matches-p (tmux cl-tmux)
  (and (= (compat-run-result-exit-code tmux)
          (compat-run-result-exit-code cl-tmux))
       (string= (compat-run-result-stdout tmux)
                (compat-run-result-stdout cl-tmux))
       (string= (%normalize-no-server-stderr
                 (compat-run-result-stderr tmux))
                (%normalize-no-server-stderr
                 (compat-run-result-stderr cl-tmux)))))

(defun %option-default-string (value)
  (cond
    ((eq value t) "on")
    ((eq value nil) "off")
    ((stringp value) value)
    (t (princ-to-string value))))

(defun %cl-tmux-option-default (name)
  (let ((spec (gethash name cl-tmux/options:*option-registry*)))
    (when spec
      (%option-default-string
       (cl-tmux/options:option-spec-default spec)))))

(defun %tmux-option-line-name (line)
  (let ((space (position #\Space line)))
    (if space
        (subseq line 0 space)
        line)))

(defun %tmux-unquote-option-value (value)
  (cond
    ((null value) "")
    ((string= value "''") "")
    ((and (<= 2 (length value))
          (char= #\" (char value 0))
          (char= #\" (char value (1- (length value)))))
     (with-output-to-string (out)
       (let ((escaped nil))
         (loop for index from 1 below (1- (length value))
               for char = (char value index)
               do (cond
                    (escaped
                     (write-char char out)
                     (setf escaped nil))
                    ((char= char #\\)
                     (setf escaped t))
                    (t
                     (write-char char out)))))))
    (t value)))

(defun %tmux-option-line-value (line)
  (let ((space (position #\Space line)))
    (%tmux-unquote-option-value
     (and space (subseq line (1+ space))))))

(defun %tmux-option-lines (socket show-command)
  (%non-empty-lines (%tmux-clean-output socket show-command "-g")))

(defun %tmux-option-default (socket entry)
  (let* ((name (getf entry :name))
         (command (getf entry :tmux-command))
         (show-command (if (and command
                                (search "show-window-options" command
                                        :test #'char=))
                           "show-window-options"
                           "show-options"))
         (line (%tmux-clean-output socket show-command "-g" name)))
    (when (and line (string= name (%tmux-option-line-name line)))
      (%tmux-option-line-value line))))

(test compat-matrix-loads
  (let ((matrix (%read-compat-matrix)))
    (is (= 1 (getf matrix :schema-version)))
    (is (string= "2026-06-14" (getf matrix :generated-date)))
    (is (string= "tmux 3.6a" (getf matrix :tmux-version)))
    (is (= 90 (getf (getf matrix :scope) :commands)))
    (is (= 87 (getf (getf matrix :scope) :named-bindings)))
    (is (= 19 (getf (getf matrix :scope) :root-bindings)))
    (is (= 87 (getf (getf matrix :scope) :prefix-bindings)))
    (is (= 74 (getf (getf matrix :scope) :copy-mode-bindings)))
    (is (= 87 (getf (getf matrix :scope) :copy-mode-vi-bindings)))
    (is (= 267 (getf (getf matrix :scope) :default-key-bindings)))
    (is (= 61 (getf (getf matrix :scope) :global-options)))
    (is (= 67 (getf (getf matrix :scope) :window-options)))
    (is (= 57 (getf (getf matrix :scope) :hooks)))
    (is (= 226 (getf (getf matrix :scope) :formats)))
    (is (some (lambda (entry) (eq :partial (getf entry :status)))
              (getf matrix :entries))
        "the compatibility matrix must not claim complete tmux compatibility yet")))

(test compat-status-counts-match-entry-data
  (let* ((matrix (%read-compat-matrix))
         (counts (getf matrix :status-counts)))
    (dolist (status '(:match :partial :missing :intentionally-unsupported :unknown))
      (is (= (getf counts status)
             (%status-count matrix status))
          (format nil "recorded count for ~A must match entry data" status)))))

(test compat-local-tmux-is-reachable
  (let ((version (%tmux-version)))
    (if (not version)
        (skip "tmux executable unavailable")
        (is (and (>= (length version) 5)
                 (string= "tmux " (subseq version 0 5)))
            "tmux -V must return a tmux version string"))))

(test compat-current-tmux-inventory-matches-recorded-baseline
  (let ((matrix (%read-compat-matrix)))
    (%with-matching-tmux-matrix (matrix version)
      (%with-clean-tmux-server (socket)
        (let ((scope (getf matrix :scope)))
          (is (= (getf scope :commands)
                 (%tmux-clean-line-count socket "list-commands"))
              "tmux list-commands count must match the recorded baseline")
          (is (= (getf scope :named-bindings)
                 (%tmux-clean-line-count socket "list-keys" "-N"))
              "tmux named binding count must match the recorded baseline")
          (is (= (getf scope :root-bindings)
                 (%tmux-clean-line-count socket "list-keys" "-T" "root"))
              "tmux root binding count must match the recorded baseline")
          (is (= (getf scope :prefix-bindings)
                 (%tmux-clean-line-count socket "list-keys" "-T" "prefix"))
              "tmux prefix binding count must match the recorded baseline")
          (is (= (getf scope :copy-mode-bindings)
                 (%tmux-clean-line-count socket "list-keys" "-T" "copy-mode"))
              "tmux copy-mode binding count must match the recorded baseline")
          (is (= (getf scope :copy-mode-vi-bindings)
                 (%tmux-clean-line-count socket "list-keys" "-T" "copy-mode-vi"))
              "tmux copy-mode-vi binding count must match the recorded baseline")
          (is (= (getf scope :global-options)
                 (%tmux-clean-line-count socket "show-options" "-g"))
              "tmux global option count must match the recorded baseline")
          (is (= (getf scope :window-options)
                 (%tmux-clean-line-count socket "show-window-options" "-g"))
              "tmux window option count must match the recorded baseline")
          (is (= (getf scope :hooks)
                 (%tmux-clean-line-count socket "show-hooks" "-g"))
              "tmux hook count must match the recorded baseline"))))))

(test compat-default-key-binding-rows-match-current-tmux
  (let ((matrix (%read-compat-matrix)))
    (%with-matching-tmux-matrix (matrix version)
      (%with-clean-tmux-server (socket)
        (let ((scope (getf matrix :scope))
              (all-row-keys '()))
          (dolist (table +compat-binding-tables+)
            (let* ((rows (%compat-binding-entries matrix table))
                   (tmux-lines (%tmux-clean-binding-lines socket table))
                   (tmux-keys (mapcar #'%tmux-binding-key-from-line tmux-lines))
                   (row-keys (mapcar (lambda (entry) (getf entry :key)) rows)))
              (setf all-row-keys
                    (append (mapcar (lambda (key)
                                      (format nil "~A ~A" table key))
                                    row-keys)
                            all-row-keys))
              (is (= (getf scope (%compat-binding-table-scope-key table))
                     (length rows))
                  (format nil "~A binding row count must match scope" table))
              (is (equal tmux-keys row-keys)
                  (format nil "~A binding rows must match tmux list-keys order" table))
              (dolist (entry rows)
                (is (string= version (getf entry :tmux-version))
                    (format nil "~A ~A version must match"
                            table (getf entry :key)))
                (is (string= (format nil "list-keys -T ~A" table)
                             (getf entry :source))
                    (format nil "~A ~A source must match"
                            table (getf entry :key))))))
          (is (= (getf scope :default-key-bindings)
                 (length all-row-keys))
              "default binding row count must match table rows")
          (is (null (%duplicate-strings all-row-keys))
              "default binding rows must not duplicate table/key pairs"))))))

(test compat-default-key-binding-statuses-track-cl-tmux-key-tables
  (let ((matrix (%read-compat-matrix)))
    (dolist (table +compat-binding-tables+)
      (let ((cl-keys (%cl-tmux-binding-key-set table)))
        (dolist (entry (%compat-binding-entries matrix table))
          (let* ((key (getf entry :key))
                 (present (member key cl-keys :test #'string=))
                 (expected-status (if present :partial :missing))
                 (expected-binding (and present t)))
            (is (eq expected-status (getf entry :status))
                (format nil "~A ~A status must track cl-tmux key presence"
                        table key))
            (is (eq expected-binding (getf entry :cl-tmux-binding))
                (format nil "~A ~A cl-tmux-binding flag must match key table"
                        table key))))))))

(test compat-option-rows-match-recorded-inventory
  (let* ((matrix (%read-compat-matrix))
         (scope (getf matrix :scope))
         (all-row-names '()))
    (dolist (spec +compat-option-scopes+)
      (destructuring-bind (scope-key count-key show-command source-command) spec
        (declare (ignore show-command source-command))
        (let* ((entries (%compat-option-entries matrix scope-key))
               (names (mapcar (lambda (entry) (getf entry :name)) entries)))
          (setf all-row-names
                (append (mapcar (lambda (name)
                                  (format nil "~A ~A" scope-key name))
                                names)
                        all-row-names))
          (is (= (getf scope count-key) (length entries))
              (format nil "~A option row count must match scope" scope-key))
          (is (null (%duplicate-strings names))
              (format nil "~A option rows must not duplicate names" scope-key)))))
    (is (null (%duplicate-strings all-row-names))
        "option rows must not duplicate scope/name pairs")))

(test compat-current-tmux-option-rows-match-current-tmux
  (let ((matrix (%read-compat-matrix)))
    (%with-matching-tmux-matrix (matrix version)
      (%with-clean-tmux-server (socket)
        (let ((scope (getf matrix :scope)))
          (dolist (spec +compat-option-scopes+)
            (destructuring-bind (scope-key count-key show-command source-command) spec
              (let* ((entries (%compat-option-entries matrix scope-key))
                     (tmux-lines (%tmux-option-lines socket show-command))
                     (tmux-names (mapcar #'%tmux-option-line-name tmux-lines))
                     (row-names (mapcar (lambda (entry) (getf entry :name))
                                        entries)))
                (is (= (getf scope count-key) (length entries))
                    (format nil "~A option row count must match scope" scope-key))
                (is (equal tmux-names row-names)
                    (format nil "~A option rows must match tmux output order"
                            scope-key))
                (loop for entry in entries
                      for line in tmux-lines
                      do (let* ((name (getf entry :name))
                                (tmux-default (%tmux-option-line-value line)))
                           (is (string= (format nil "~A ~A"
                                                source-command name)
                                        (getf entry :tmux-command))
                               (format nil "~A command must match source"
                                       name))
                           (unless (%compat-environment-option-default-p entry)
                             (is (string= (getf entry :tmux-default)
                                          tmux-default)
                                 (format nil
                                         "~A tmux default must match recorded matrix value"
                                         name)))))))))))))

(test compat-option-default-rows-track-cl-tmux-registry
  (let ((matrix (%read-compat-matrix)))
    (dolist (entry (%compat-entries matrix :option))
      (let* ((name (getf entry :name))
             (tmux-default (getf entry :tmux-default))
             (cl-default (%cl-tmux-option-default name))
             (expected-status (cond
                                ((null cl-default) :missing)
                                ((string= tmux-default cl-default) :match)
                                (t :partial))))
        (if cl-default
            (is (string= cl-default (getf entry :cl-tmux-default))
                (format nil "~A cl-tmux default must match registry value"
                        name))
            (is (null (getf entry :cl-tmux-default))
                (format nil "~A must not record a cl-tmux default" name)))
        (is (eq expected-status (getf entry :status))
            (format nil "~A status must reflect cl-tmux registry coverage"
                    name))))))

(test compat-format-variable-rows-match-recorded-inventory
  (let* ((matrix (%read-compat-matrix))
         (scope (getf matrix :scope))
         (entries (%compat-entries matrix :format))
         (names (%compat-entry-names matrix :format))
         (duplicates (%duplicate-strings names))
         (inventory (%compat-entry matrix :inventory "tmux format variable inventory"))
         (cl-context-names (%cl-tmux-format-context-names)))
    (is (= (getf scope :formats) (length entries)))
    (is-true inventory "format variable inventory row must be present")
    (is (= (length entries) (getf inventory :tmux-count)))
    (is (= (length cl-context-names) (getf inventory :cl-tmux-count)))
    (is (null duplicates)
        (format nil "duplicate format rows: ~{~A~^, ~}" duplicates))))

(test compat-format-variable-rows-track-cl-tmux-context
  (let* ((matrix (%read-compat-matrix))
         (cl-context-names (%cl-tmux-format-context-names))
         (entries (%compat-entries matrix :format)))
    (dolist (entry entries)
      (let* ((name (getf entry :name))
             (present (member name cl-context-names :test #'string=))
             (expected-status (if present :partial :missing))
             (expected-context (and present t)))
        (is (eq expected-status (getf entry :status))
            (format nil "~A status must track cl-tmux format context presence" name))
        (is (eq expected-context (getf entry :cl-tmux-context))
            (format nil "~A cl-tmux-context flag must match actual context" name))))))

(test compat-hook-rows-match-recorded-inventory
  (let* ((matrix (%read-compat-matrix))
         (scope (getf matrix :scope))
         (entries (%compat-entries matrix :hook))
         (names (%compat-entry-names matrix :hook))
         (duplicates (%duplicate-strings names))
         (inventory (%compat-entry matrix :inventory "tmux hook inventory"))
         (cl-hook-names (%cl-tmux-hook-event-names)))
    (is (= (getf scope :hooks) (length entries)))
    (is-true inventory "hook inventory row must be present")
    (is (= (length entries) (getf inventory :tmux-count)))
    (is (= (length cl-hook-names) (getf inventory :cl-tmux-count)))
    (is (= (length (%string-set-difference cl-hook-names names))
           (getf inventory :cl-tmux-only-count)))
    (is (null duplicates)
        (format nil "duplicate hook rows: ~{~A~^, ~}" duplicates))))

(test compat-hook-rows-track-cl-tmux-events
  (let* ((matrix (%read-compat-matrix))
         (cl-hook-names (%cl-tmux-hook-event-names))
         (entries (%compat-entries matrix :hook)))
    (dolist (entry entries)
      (let* ((name (getf entry :name))
             (present (member name cl-hook-names :test #'string=))
             (expected-status (if present :partial :missing))
             (expected-event (and present t)))
        (is (eq expected-status (getf entry :status))
            (format nil "~A status must track cl-tmux hook event presence" name))
        (is (eq expected-event (getf entry :cl-tmux-event))
            (format nil "~A cl-tmux-event flag must match actual hook constants" name))))))

(test compat-current-tmux-hooks-are-all-represented
  (let ((matrix (%read-compat-matrix)))
    (%with-matching-tmux-matrix (matrix version)
      (%with-clean-tmux-server (socket)
        (let* ((tmux-names (%tmux-clean-hook-names socket))
               (matrix-names (%compat-entry-names matrix :hook))
               (missing (%string-set-difference tmux-names matrix-names))
               (extra (%string-set-difference matrix-names tmux-names)))
          (is (= (length tmux-names) (length matrix-names))
              "matrix must have one hook row per tmux show-hooks -g row")
          (is (null missing)
              (format nil "matrix is missing tmux hooks: ~{~A~^, ~}" missing))
          (is (null extra)
              (format nil "matrix has hook rows not present in tmux: ~{~A~^, ~}" extra)))))))

(test compat-current-tmux-commands-are-all-represented
  (let ((matrix (%read-compat-matrix)))
    (%with-matching-tmux-matrix (matrix version)
      (%with-clean-tmux-server (socket)
        (let* ((tmux-names (%tmux-clean-command-names socket))
               (matrix-names (%compat-entry-names matrix :command))
               (missing (%string-set-difference tmux-names matrix-names))
               (extra (%string-set-difference matrix-names tmux-names)))
          (is (= (length tmux-names) (length matrix-names))
              "matrix must have one command row per tmux list-commands row")
          (is (null missing)
              (format nil "matrix is missing tmux commands: ~{~A~^, ~}" missing))
          (is (null extra)
              (format nil "matrix has command rows not present in tmux: ~{~A~^, ~}" extra)))))))

(test compat-command-rows-track-cl-tmux-command-inventory
  (let* ((matrix (%read-compat-matrix))
         (command-entries (%compat-entries matrix :command))
         (inventory (%compat-entry matrix :inventory "tmux command inventory"))
         (cl-command-names (%cl-tmux-command-inventory-names)))
    (is (= 90 (length command-entries)))
    (is-true inventory)
    (is (= (length cl-command-names) (getf inventory :cl-tmux-count)))
    (is (= 90 (getf inventory :cl-tmux-matched-count)))
    (is (= 125 (getf inventory :cl-tmux-extra-count)))
    (is (= 90 (count :partial command-entries
                     :key (lambda (entry) (getf entry :status)))))
    (is (= 0 (count :unknown command-entries
                    :key (lambda (entry) (getf entry :status)))))
    (dolist (entry command-entries)
      (let* ((name (getf entry :name))
             (present (member name cl-command-names :test #'string=))
             (expected-command (and present t)))
        (is-true present)
        (is (eq :partial (getf entry :status))
            (format nil "~A status must stay partial until behavior is proven"
                    name))
        (is (eq expected-command (getf entry :cl-tmux-command))
            (format nil "~A cl-tmux-command flag must match command inventory"
                    name))))))

(test compat-known-window-size-divergence-is-recorded
  (let* ((matrix (%read-compat-matrix))
         (entry (%compat-entry matrix :option "window-size")))
    (is-true entry "window-size default divergence must be recorded")
    (is (eq :partial (getf entry :status)))
    (is (string= "latest" (getf entry :tmux-default)))
    (is (string= "smallest" (getf entry :cl-tmux-default)))))
