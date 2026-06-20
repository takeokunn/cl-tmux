;;;; Test DSL helpers for cl-tmux.
;;;;
;;;; Provides screen builder macros, byte-feeding utilities, grid inspection
;;;; accessors, a table-driven test macro, and layout invariant checkers.

(in-package #:cl-tmux/test)

;;; ── Hooks isolation ─────────────────────────────────────────────────────────

(defmacro with-isolated-hooks (&body body)
  "Run BODY with fresh *hook-registry* and *command-hooks* tables so neither
   lisp-function hooks nor command hooks (set-hook) leak between tests."
  `(let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal))
         (cl-tmux/hooks:*command-hooks* (make-hash-table :test #'equal)))
     ,@body))

;;; ── Config isolation ────────────────────────────────────────────────────────

(defmacro with-isolated-config (&body body)
  "Run BODY with the mutable config specials dynamically rebound to copies,
   so directives applied in a test never leak into other suites.
   Isolates: key-tables, default-shell, status-height, prefix-key-code,
             global-options (copy), server-options (copy)."
  `(let ((cl-tmux/config:*key-tables*  (make-hash-table :test #'equal))
          (cl-tmux/config:*default-shell* cl-tmux/config:*default-shell*)
          (cl-tmux/config:*status-height* cl-tmux/config:*status-height*)
          (cl-tmux/config:*prefix-key-code*  cl-tmux/config:*prefix-key-code*)
          (cl-tmux/config:*prefix2-key-code* cl-tmux/config:*prefix2-key-code*)
          (cl-tmux/options:*global-options*
           (let ((h (make-hash-table :test #'equal)))
             (maphash (lambda (k v) (setf (gethash k h) v))
                      cl-tmux/options:*global-options*)
             h))
          (cl-tmux/options:*server-options*
           (let ((h (make-hash-table :test #'equal)))
             (maphash (lambda (k v) (setf (gethash k h) v))
                      cl-tmux/options:*server-options*)
             h)))
     ;; Re-initialize with fresh key tables: config.lisp defaults PLUS the
     ;; extended prefix bindings installed by events-loop.lisp (C-b z, C-b L,
     ;; etc.).  Without the latter the isolated table would diverge from the live
     ;; image and tests like bind-multichar would not find #\z bound.
     (cl-tmux/config::initialize-default-key-tables)
     (cl-tmux::install-extended-key-bindings)
     ,@body))

(defmacro with-isolated-key-tables (&body body)
  "Run BODY with a fresh *KEY-TABLES* inside WITH-ISOLATED-CONFIG isolation.
   Prevents key-table mutations from leaking between tests that exercise
   bind/unbind directives across multiple key-tables (root, copy-mode, etc.)."
  `(let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
     (with-isolated-config
       ,@body)))

(defmacro with-temp-config-file ((path-var &rest lines) &body body)
  "Write LINES (each a string) to a fresh temporary config file, bind PATH-VAR
   to that file's pathname, run BODY, then delete the file.
   Used by load-config-file and source-file tests to avoid repetition of the
   create->unwind-protect->delete-file scaffold."
  (let ((path-sym (gensym "PATH")))
    `(let ((,path-sym (merge-pathnames
                        (format nil "cl-tmux-test-~D.conf" (random 1000000))
                        (uiop:temporary-directory))))
       (unwind-protect
            (progn
              (with-open-file (out ,path-sym :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create)
                ,@(mapcar (lambda (line) `(write-line ,line out)) lines)
                (finish-output out))
              (let ((,path-var ,path-sym))
                ,@body))
         (when (probe-file ,path-sym)
           (delete-file ,path-sym))))))

;;; ── Screen builder ──────────────────────────────────────────────────────────

(defmacro with-screen ((var w h) &body body)
  "Bind VAR to a fresh screen of width W and height H for BODY."
  `(let ((,var (make-screen ,w ,h))) ,@body))

;;; ── Byte feeding ────────────────────────────────────────────────────────────

(defun octets (string)
  "Convert STRING to an (unsigned-byte 8) vector (Latin-1; each char maps
   directly to its char-code, so #\\Escape = #x1B)."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun feed (screen string)
  "Process STRING (one byte per character) through SCREEN's emulator."
  (screen-process-bytes screen (octets string))
  screen)

(defun copy-mode-screen (&key (w 20) (h 5) (content "") cursor mark selecting)
  "Return a copy-mode screen pre-filled with CONTENT and optional copy state."
  (let ((screen (make-screen w h)))
    (unless (string= content "")
      (feed screen content))
    (cl-tmux/commands::copy-mode-enter screen)
    (when cursor
      (setf (cl-tmux/terminal/types:screen-copy-cursor screen) cursor))
    (when mark
      (setf (cl-tmux/terminal/types:screen-copy-mark screen) mark))
    (when selecting
      (setf (cl-tmux/terminal/types:screen-copy-selecting screen) selecting))
    screen))

(defmacro with-copy-mode-cursor ((screen-var row col &key (w 20) (h 5)) &body body)
  "Bind SCREEN-VAR to a fresh copy-mode screen with cursor at (ROW . COL)."
  `(let ((,screen-var (copy-mode-screen :w ,w :h ,h :cursor (cons ,row ,col))))
     ,@body))

;;; ── Semantic escape sequence builders ──────────────────────────────────────

(defun esc (fmt &rest args)
  "Build an escape sequence string with ESC (char code 27) prefix.
   FMT and ARGS are passed to FORMAT after the ESC character."
  (format nil "~C~?" #\Escape fmt args))

(defun csi (params final)
  "Build the string ESC [ PARAMS FINAL."
  (format nil "~C[~A~A" #\Escape params (string final)))

;;; ── Grid inspection ─────────────────────────────────────────────────────────

(defun row-string (screen y &key (start 0) end)
  "Return the characters of row Y from START to END (default: full width)."
  (let* ((w (screen-width screen))
         (e (or end w)))
    (with-output-to-string (s)
      (loop for x from start below (min e w)
            do (write-char (cell-char (screen-cell screen x y)) s)))))

(defun cell-at  (screen x y) (screen-cell screen x y))
(defun char-at  (screen x y) (cell-char   (screen-cell screen x y)))
(defun fg-at    (screen x y) (cell-fg     (screen-cell screen x y)))
(defun bg-at    (screen x y) (cell-bg     (screen-cell screen x y)))
(defun attrs-at (screen x y) (cell-attrs  (screen-cell screen x y)))

;;; ── Terminal emulator helpers ───────────────────────────────────────────────

(defmacro check-cursor (screen cx cy)
  "Assert that SCREEN's cursor is at column CX, row CY."
  `(progn
     (is (= ,cx (screen-cursor-x ,screen))
         "cursor-x: expected ~D got ~D" ,cx (screen-cursor-x ,screen))
     (is (= ,cy (screen-cursor-y ,screen))
         "cursor-y: expected ~D got ~D" ,cy (screen-cursor-y ,screen))))

(defmacro check-table (rows &key (test #'=))
  "Assert each (ACTUAL EXPECTED DESC) row in ROWS with TEST."
  `(dolist (row ,rows)
     (destructuring-bind (actual expected desc) row
       (is (funcall ,test expected actual)
           "~A: expected ~S got ~S" desc expected actual))))

(defun row-blank-p (screen y)
  "Return T when every cell in row Y of SCREEN contains a space."
  (every (lambda (c) (char= #\Space c))
         (coerce (row-string screen y) 'list)))

(defun utf8-feed (screen lisp-string)
  "Encode LISP-STRING as UTF-8 and feed the bytes to SCREEN."
  (screen-process-bytes screen
                        (babel:string-to-octets lisp-string :encoding :utf-8))
  screen)

(defun feed-lines (screen &rest lines)
  "Feed LINES to SCREEN separated by CR/LF, scrolling as needed.  Returns SCREEN."
  (loop for (line . more) on lines
        do (feed screen line)
        when more do (feed screen (format nil "~C~C" #\Return #\Linefeed)))
  screen)

(defun display-row-string (screen y &key end)
  "Characters of viewport row Y via screen-display-cell (honors copy-offset)."
  (let ((end (or end (screen-width screen))))
    (with-output-to-string (s)
      (loop for x below end
            do (write-char (cell-char (screen-display-cell screen x y)) s)))))

(defun render-pane-output (session pane)
  "Render PANE to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-pane s session pane)))

(defmacro with-copy-mode-render-fixture ((session-var pane-var screen-var w h
                                          &key (content "")
                                               (position-format "")
                                               (options '()))
                                         &body body)
  "Bind a renderer session, its pane, and screen under isolated copy-mode defaults.
   BODY may mutate the screen before calling render-pane-output.
   OPTIONS is a flat list of option-name/value strings; callers may pass it as a
   quoted literal list (e.g. :options '(\"name\" \"value\")), which is unwrapped
   here before being spliced into with-isolated-options."
  (let ((option-pairs (if (and (consp options) (eq (car options) 'quote))
                          (second options)
                          options)))
    `(with-isolated-options ("copy-mode-position-style" "default"
                             "copy-mode-position-format" ,position-format
                             ,@option-pairs)
       (let* ((,session-var (make-renderer-test-session ,w ,h :content ,content))
              (,pane-var (first (window-panes (session-active-window ,session-var))))
              (,screen-var (pane-screen ,pane-var)))
         ,@body))))

(defmacro with-copy-mode-selection-fixture ((session-var pane-var screen-var w h
                                             &key (content "")
                                                  (mark-row nil)
                                                  (mark-col nil)
                                                  (cursor-row nil)
                                                  (cursor-col nil)
                                                  (selecting-p t)
                                                  (copy-mode-p t)
                                                  (position-format "")
                                                  (options '()))
                                            &body body)
  "Bind a copy-mode renderer fixture with selection state preconfigured."
  `(with-copy-mode-render-fixture (,session-var ,pane-var ,screen-var ,w ,h
                                   :content ,content
                                   :position-format ,position-format
                                   :options ,options)
     (setf (screen-copy-mode-p ,screen-var) ,copy-mode-p
           (screen-copy-selecting ,screen-var) ,selecting-p
           (screen-copy-offset ,screen-var) 0
           (screen-copy-mark ,screen-var)
           (and ,mark-row ,mark-col (cons ,mark-row ,mark-col))
           (screen-copy-cursor ,screen-var)
           (and ,cursor-row ,cursor-col (cons ,cursor-row ,cursor-col)))
     ,@body))

(defun render-status-bar-output (sess rows cols &key ((:status-row status-row)
                                                     nil
                                                     status-row-supplied-p))
  "Render the status bar for SESS to a string using the production renderer."
  (with-output-to-string (s)
    (if status-row-supplied-p
        (cl-tmux/renderer::render-status-bar s sess rows cols :status-row status-row)
        (cl-tmux/renderer::render-status-bar s sess rows cols))))

(defun render-overlay-output (width)
  "Render the current overlay to a string using the production renderer."
  (with-output-to-string (buf)
    (cl-tmux/renderer::render-overlay buf width)))

(defun render-popup-output (popup rows cols)
  "Render POPUP to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-popup s popup rows cols)))

(defun render-menu-output (menu rows cols)
  "Render MENU to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-menu s menu rows cols)))

(defun render-tree-borders-output (tree active-pane width)
  "Render TREE borders for ACTIVE-PANE to a string using the production renderer."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-tree-borders s tree active-pane width)))

;;; ── Key translation helpers ────────────────────────────────────────────────

(defun key-name-bytes (name)
  "Return %key-name-to-bytes(NAME) as a list of byte values."
  (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list))

(defun split-key-modifiers-values (name)
  "Return the multiple values from %split-key-modifiers(NAME) as a list."
  (multiple-value-list (cl-tmux/commands::%split-key-modifiers name)))

(defun translate-send-keys-bytes (string)
  "Return %translate-send-keys(STRING) as a list of byte values."
  (coerce (cl-tmux/commands::%translate-send-keys string) 'list))

(defun key-table-command-value (table key)
  "Return the command bound to KEY in TABLE as a list or keyword."
  (let ((command (cl-tmux/config:key-table-command
                  (cl-tmux/config:key-table-lookup table key))))
    (if (and (consp command)
             (eq 'quote (first command))
             (consp (second command))
             (null (cddr command)))
        (second command)
        command)))

(defun copy-mode-x-command-value (name)
  "Return the copy-mode -X command keyword bound to NAME."
  (cdr (assoc name cl-tmux::*copy-mode-x-commands* :test #'string-equal)))

(defun alist-value (key alist &key (test #'eql))
  "Return the value bound to KEY in ALIST."
  (cdr (assoc key alist :test test)))

;;; ── Overlay assertions ─────────────────────────────────────────────────────

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
  "Run DISPATCH-COMMAND for COMMAND in a fake session and assert that it opens
   an overlay.  BODY runs with the session bound and *OVERLAY* still active.
   SESSION-SPEC may be either a session variable, or a list whose first element
   is the session variable and the remaining elements are forwarded to
   WITH-FAKE-SESSION as MAKE-ARGS."
  `(with-overlay-session (,session-spec :context ,(or context "dispatch-command must open an overlay"))
       (cl-tmux::dispatch-command ,(if (consp session-spec) (first session-spec) session-spec)
                                  ,command ,args)
     ,@body))

(defmacro with-run-command-line-overlay ((session-spec command &key context)
                                         &body body)
  "Run %RUN-COMMAND-LINE for COMMAND in a fake session and assert that it opens
   an overlay.  BODY runs with the session bound and *OVERLAY* still active.
   SESSION-SPEC may be either a session variable, or a list whose first element
   is the session variable and the remaining elements are forwarded to
   WITH-FAKE-SESSION as MAKE-ARGS."
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
  "Run DISPATCH-COMMAND for COMMAND in a fake session and assert that it opens
   a prompt.  BODY runs with the session bound and *PROMPT* still active.
   SESSION-SPEC may be either a session variable, or a list whose first element
   is the session variable and the remaining elements are forwarded to
   WITH-FAKE-SESSION as MAKE-ARGS."
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

(defmacro with-session-name ((session-var name) &body body)
  "Assign NAME to SESSION-VAR and continue with BODY."
  `(progn
     (setf (session-name ,session-var) ,name)
     ,@body))

(defmacro with-window-names ((session-var &rest names) &body body)
  "Assign NAMES to the active windows of SESSION-VAR in order."
  (let ((windows (gensym "WINDOWS")))
    `(let ((,windows (session-windows ,session-var)))
       (loop for window in ,windows
             for name in (list ,@names)
             do (setf (window-name window) name))
       ,@body)))

(defmacro with-session-and-window-names ((session-var session-name
                                         &rest window-names)
                                        &body body)
  "Assign SESSION-NAME and WINDOW-NAMES to SESSION-VAR in one step."
  `(with-session-name (,session-var ,session-name)
     (with-window-names (,session-var ,@window-names)
       ,@body)))

(defmacro with-registered-sessions ((&rest session-bindings) &body body)
  "Bind *SERVER-SESSIONS* from SESSION-BINDINGS data.

   Each binding is a (SESSION-NAME SESSION-VAR) pair, keeping registry setup
   separate from the test logic that exercises it."
  `(let ((cl-tmux::*server-sessions*
           (list ,@(loop for (session-name session-var) in session-bindings
                         collect `(cons ,session-name ,session-var)))))
     ,@body))

;;; ── PTY availability probe (test-only) ─────────────────────────────────────
;;;
;;; pty-available-p is a testing artifact: it spawns a real shell and immediately
;;; kills it purely to check PTY access.  It lives here (test helpers) rather
;;; than in production source so the production pty.lisp has no test-only code.

(defun pty-available-p ()
  "Return T if a PTY-backed shell can be spawned on this system, NIL otherwise.
   Used as a skip guard in integration tests that require /dev/ptmx."
  (handler-case
      (multiple-value-bind (fd pid) (forkpty-with-shell 8 20)
        (cl-tmux/pty:pty-close fd pid)
        t)
    (error () nil)))

(defmacro with-pty-available (&body body)
  "Run BODY only when PTY-backed shells are available."
  `(when (pty-available-p)
     ,@body))

(defmacro with-pty-session (session-spec &body body)
  "Run BODY in a fake session only when PTY-backed shells are available."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-available
       (with-fake-session (,session-var ,@session-args)
         ,@body))))

(defmacro with-pty-run-command-line-overlay ((session-spec command &key context)
                                             &body body)
  "Run %RUN-COMMAND-LINE for COMMAND in a fake session only when PTYs exist."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-session (,session-var ,@session-args)
       (with-run-command-line-overlay (,session-var ,command :context ,context)
         ,@body))))

(defmacro with-command-line-rejection-cases ((line-var message-var row-token-var cases)
                                             &body body)
  "Iterate over rejection cases as data, keeping the assertions in BODY."
  `(dolist (case ,cases)
     (destructuring-bind (,line-var ,message-var ,row-token-var) case
       ,@body)))

(defmacro with-pty-command-preserving-focus ((session-spec command &key count-form active-form
                                                           count-context focus-context)
                                              &body body)
  "Run COMMAND in a PTY-backed fake session and assert it changes COUNT-FORM
   while leaving ACTIVE-FORM unchanged."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-session (,session-var ,@session-args)
       (let ((before-count ,count-form)
             (before-active ,active-form))
         (cl-tmux::%run-command-line ,session-var ,command)
         (let ((after-count ,count-form))
           (is (> after-count before-count)
               ,count-context))
         (is (eq before-active ,active-form)
             ,focus-context)
         ,@body))))

(defmacro with-pty-command-increasing-count ((session-spec command &key count-form count-context)
                                             &body body)
  "Run COMMAND in a PTY-backed fake session and assert it increases COUNT-FORM."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-session (,session-var ,@session-args)
       (let ((before-count ,count-form))
         (cl-tmux::%run-command-line ,session-var ,command)
         (is (> ,count-form before-count)
             ,count-context)
         ,@body))))

(defmacro with-pty-shell ((fd-var pid-var &key (rows 24) (cols 80)) &body body)
  "Spawn a shell on a fresh PTY of ROWS×COLS; bind FD-VAR and PID-VAR.
   Closes the PTY via unwind-protect on exit, even if BODY signals."
  `(multiple-value-bind (,fd-var ,pid-var) (forkpty-with-shell ,rows ,cols)
     (unwind-protect
          (progn ,@body)
       (pty-close ,fd-var ,pid-var))))

;;; ── No-PTY pane builder ─────────────────────────────────────────────────────

(defun make-no-pty-pane (id x y w h)
  "Build a pane with no real PTY and a matching virtual screen."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1
             :screen (make-screen w h)))
