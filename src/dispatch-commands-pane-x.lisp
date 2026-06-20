(in-package #:cl-tmux)

;;;; Copy-mode -X command name table (send-keys -X dispatch).

;;; ── Copy-mode command name table (for send-keys -X) ─────────────────────────
;;;
;;; Real tmux's `send-keys -X <name>` dispatches a named copy-mode action.
;;; This table maps name strings to dispatch command keywords so that
;;; `bind -T copy-mode-vi v send-keys -X begin-selection` works.
;;;
;;; define-copy-mode-x-command-table makes the table declarative; each entry is
;;; ("command-name" keyword).

(defmacro define-copy-mode-x-command-table (&rest entries)
  "Build *COPY-MODE-X-COMMANDS* from a declarative name -> keyword table.
   Each ENTRY is (\"command-name\" keyword). The resulting alist is consulted
   by %DISPATCH-SEND-KEYS-X via ASSOC."
  `(defparameter *copy-mode-x-commands*
     (list ,@(mapcar (lambda (e) `(cons ,(first e) ,(second e))) entries))
     "Alist mapping send-keys -X command names to copy-mode dispatch keywords."))

(define-copy-mode-x-command-table
  ("begin-selection"              :copy-mode-begin-selection)
  ("begin-line-selection"         :copy-mode-begin-line-selection)
  ;; clear-selection: drop the selection but stay in copy mode (the default vi
  ;; Escape binding).
  ("clear-selection"              :copy-mode-clear-selection)
  ;; copy-selection copies + clears the selection but STAYS in copy mode (tmux
  ;; REDRAW); copy-selection-and-cancel copies then exits (CANCEL).
  ("copy-selection"               :copy-mode-copy-selection-no-cancel)
  ("copy-selection-and-cancel"    :copy-mode-yank)
  ("copy-selection-no-clear"      :copy-mode-copy-selection-no-clear)
  ("cancel"                       :copy-mode-exit)
  ("cursor-up"                    :copy-mode-cursor-up)
  ("cursor-down"                  :copy-mode-cursor-down)
  ("cursor-left"                  :copy-mode-cursor-left)
  ("cursor-right"                 :copy-mode-cursor-right)
  ("page-up"                      :copy-mode-page-up)
  ("page-down"                    :copy-mode-page-down)
  ("halfpage-up"                  :copy-mode-half-page-up)
  ("halfpage-down"                :copy-mode-half-page-down)
  ("search-again"                 :copy-mode-search-next)
  ("search-reverse"               :copy-mode-search-prev)
  ("search-forward"               :copy-mode-search-forward-prompt)
  ("search-backward"              :copy-mode-search-backward-prompt)
  ;; Incremental (live) search: cursor jumps on every keystroke (C-s / C-r).
  ("search-forward-incremental"   :copy-mode-search-forward-incremental)
  ("search-backward-incremental"  :copy-mode-search-backward-incremental)
  ;; top/middle/bottom-line (vi H/M/L) move WITHIN the viewport, keeping the
  ;; scroll position; history-top/bottom (vi g/G) jump to the scrollback extremes.
  ("top-line"                     :copy-mode-high)
  ("middle-line"                  :copy-mode-middle)
  ("bottom-line"                  :copy-mode-low)
  ("history-top"                  :copy-mode-top)
  ("history-bottom"               :copy-mode-bottom)
  ("next-word"                    :copy-mode-word-forward)
  ("previous-word"                :copy-mode-word-backward)
  ("next-word-end"                :copy-mode-word-end)
  ;; WORD motion (vi W/B/E): whitespace-delimited, spans punctuation.
  ("next-space"                   :copy-mode-space-forward)
  ("previous-space"               :copy-mode-space-backward)
  ("next-space-end"               :copy-mode-space-end)
  ("rectangle-toggle"             :copy-mode-rectangle-toggle)
  ("copy-end-of-line"             :copy-mode-copy-end-of-line)
  ("copy-end-of-line-and-cancel"  :copy-mode-copy-end-of-line-and-cancel)
  ("copy-line"                    :copy-mode-copy-line)
  ("copy-line-and-cancel"         :copy-mode-copy-line-and-cancel)
  ("append-selection"             :copy-mode-append-selection)
  ("append-selection-and-cancel"  :copy-mode-append-selection-and-cancel)
  ("back-to-indentation"          :copy-mode-back-to-indentation)
  ("start-of-line"                :copy-mode-line-start)
  ("end-of-line"                  :copy-mode-line-end)
  ;; scroll variants
  ("scroll-up"                    :copy-mode-scroll-up-line)
  ("scroll-down"                  :copy-mode-scroll-down-line)
  ("scroll-middle"                :copy-mode-scroll-middle)
  ;; emacs-style names
  ("select-word"                  :copy-mode-select-word)
  ;; canonical copy-pipe names: these remain valid `send-keys -X` targets when
  ;; tmux resolves them through keyword dispatch without an explicit argument.
  ("copy-pipe"                    :copy-mode-copy-pipe-no-cancel)
  ("copy-pipe-and-cancel"         :copy-mode-copy-pipe-and-cancel)
  ("copy-pipe-end-of-line-and-cancel"
   :copy-mode-copy-pipe-end-of-line-and-cancel)
  ;; vi-style movement
  ("previous-paragraph"           :copy-mode-prev-paragraph)
  ("next-paragraph"               :copy-mode-next-paragraph)
  ("jump-to-mark"                 :copy-mode-jump-to-mark)
  ("set-mark"                     :copy-mode-set-mark)
  ;; select-line: start a line-granularity selection (same as V in copy-mode-vi)
  ("select-line"                  :copy-mode-begin-line-selection)
  ;; refresh-from-pane: copy mode always reads from pane live in this implementation.
  ("refresh-from-pane"            :copy-mode-refresh-from-pane)
  ;; jump-to-char: vi f/F/t/T repeat/reverse (;/,)
  ;; The char-argument variants (jump-forward, jump-backward, jump-to,
  ;; jump-to-backward) need a char arg and are handled specially in
  ;; %dispatch-send-keys-X; the argless ;/, repeat commands map to keywords.
  ("jump-again"                   :copy-mode-jump-again)
  ("jump-reverse"                 :copy-mode-jump-reverse)
  ;; other-end: swap the two ends of the selection (vi `o`).
  ("other-end"                    :copy-mode-other-end)
  ;; Bracket matching (vi %): jump to matching bracket.
  ("next-matching-bracket"        :copy-mode-next-matching-bracket)
  ("previous-matching-bracket"    :copy-mode-previous-matching-bracket))

;;; Flat records keep explicit-arg lookup and coercion separate:
;;;   (command-name kind handler)
(defparameter +send-keys-x-explicit-arg-specs+
  '(("jump-forward"                  :char copy-mode-jump-forward)
    ("jump-backward"                 :char copy-mode-jump-backward)
    ("jump-to"                       :char copy-mode-jump-to)
    ("jump-to-backward"              :char copy-mode-jump-to-backward)
    ("goto-line"                     :line copy-mode-goto-line)
    ("search-forward-text"           :text copy-mode-search-forward)
    ("search-backward-text"          :text copy-mode-search-backward)
    ("copy-pipe"                     :text copy-mode-copy-pipe-no-cancel)
    ("copy-pipe-and-cancel"          :text copy-mode-copy-pipe)
    ("copy-pipe-end-of-line-and-cancel"
     :text copy-mode-copy-pipe-end-of-line)))

(defun %send-keys-x-explicit-arg-spec (command-name)
  "Return the explicit-argument spec for COMMAND-NAME."
  (dolist (spec +send-keys-x-explicit-arg-specs+)
    (destructuring-bind (name kind handler) spec
      (when (string-equal command-name name)
        (return (values kind handler))))))

(defun %send-keys-x-explicit-arg-string (kind extra-args)
  "Return the explicit argument string for KIND from EXTRA-ARGS."
  (ecase kind
    ((:char :line) (first extra-args))
    (:text (format nil "~{~A~^ ~}" extra-args))))

(defun %send-keys-x-coerce-explicit-arg (kind handler screen arg)
  "Apply KIND-specific coercion to ARG and call HANDLER on SCREEN."
  (when (and screen arg (plusp (length arg)))
    (ecase kind
      (:char (funcall handler screen (char arg 0)))
      (:line (let ((line-number (%parse-integer-or-nil arg)))
               (when line-number
                 (funcall handler screen line-number)
                 t)))
      (:text (funcall handler screen arg) t))))

(defun %dispatch-send-keys-x-explicit-arg (screen command-name extra-args)
  "Dispatch COMMAND-NAME with an explicit positional argument when it has one."
  (multiple-value-bind (kind handler)
      (%send-keys-x-explicit-arg-spec command-name)
    (when handler
      (%send-keys-x-coerce-explicit-arg kind handler screen
                                        (%send-keys-x-explicit-arg-string kind
                                                                         extra-args)))))

(defun %dispatch-send-keys-x-with-temporary-focus (session target-pane target-window thunk)
  "Run THUNK while TARGET-PANE is temporarily focused in TARGET-WINDOW.
   Restores the real session/window focus afterward without delivering focus
   events or updating recency metadata."
  (let ((prev-win  (session-active-window session))
        (prev-pane (and target-window (window-active-pane target-window))))
    (unwind-protect
         (progn
           (setf (session-active session) target-window
                 (window-active target-window) target-pane)
           (funcall thunk))
      (when target-window
        (setf (window-active target-window) prev-pane))
      (setf (session-active session) prev-win))))

(defun %dispatch-send-keys-X (session command-name &optional target-pane target-window extra-args)
  "Dispatch a send-keys -X COMMAND-NAME against TARGET-PANE's copy mode (default:
   the active pane).  Copy-mode -X commands act on the session's ACTIVE screen, so
   when TARGET-PANE is a non-active pane the command runs with a temporary focus
   swap so it operates on the target while leaving the real focus unchanged.
   Returns T when COMMAND-NAME is a recognised copy-mode command.
   EXTRA-ARGS (a list of strings) holds any positional arguments after the command
   name; used by the copy-pipe commands to carry the pipe-command string."
  (let* ((pane   (or target-pane (session-active-pane session)))
         (screen (and pane (cl-tmux/model:pane-screen pane))))
    (cond
      ((and extra-args
            (%dispatch-send-keys-x-explicit-arg screen command-name extra-args))
       t)
      ;; Standard keyword dispatch.
       (t
       (let ((kw (cdr (assoc command-name *copy-mode-x-commands* :test #'string-equal))))
         (when kw
           (if (and target-pane target-window
                    (not (eq target-pane (session-active-pane session))))
               (%dispatch-send-keys-x-with-temporary-focus
                session target-pane target-window
                (lambda ()
                  (dispatch-command session kw nil)))
               (dispatch-command session kw nil))
           t))))))
