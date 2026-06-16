(in-package #:cl-tmux)

;;;; Copy-mode -X command name table (send-keys -X dispatch).

;;; ── Copy-mode command name table (for send-keys -X) ─────────────────────────
;;;
;;; Real tmux's `send-keys -X <name>` dispatches a named copy-mode action.
;;; This table maps name strings to dispatch command keywords so that
;;; `bind -T copy-mode-vi v send-keys -X begin-selection` works.
;;;
;;; define-copy-mode-x-command-table makes the table declarative; each entry is
;;; ("command-name" keyword). The macro removes the dead duplicate stop-selection
;;; entry that previously appeared twice (assoc always returns the first match).

(defmacro define-copy-mode-x-command-table (&rest entries)
  "Build *COPY-MODE-X-COMMANDS* from a declarative name -> keyword table.
   Each ENTRY is (\"command-name\" keyword). The resulting alist is consulted
   by %DISPATCH-SEND-KEYS-X via ASSOC."
  `(defparameter *copy-mode-x-commands*
     (list ,@(mapcar (lambda (e) `(cons ,(first e) ,(second e))) entries))
     "Alist mapping send-keys -X command names to copy-mode dispatch keywords."))

(define-copy-mode-x-command-table
  ("begin-selection"              :copy-mode-begin-selection)
  ("begin-selection-line"         :copy-mode-begin-line-selection)
  ("begin-line-selection"         :copy-mode-begin-line-selection)
  ;; clear-selection: drop the selection but stay in copy mode (the default vi
  ;; Escape binding); stop-selection is tmux's alias for the same action.
  ("clear-selection"              :copy-mode-clear-selection)
  ("stop-selection"               :copy-mode-clear-selection)
  ("copy-selection"               :copy-mode-yank)
  ("copy-selection-and-cancel"    :copy-mode-yank)
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
  ("copy-line"                    :copy-mode-copy-line)
  ("append-selection"             :copy-mode-append-selection)
  ("append-selection-and-cancel"  :copy-mode-append-selection-and-cancel)
  ("back-to-indentation"          :copy-mode-back-to-indentation)
  ("start-of-line"                :copy-mode-line-start)
  ("end-of-line"                  :copy-mode-line-end)
  ;; scroll variants
  ("scroll-up"                    :copy-mode-scroll-up-line)
  ("scroll-down"                  :copy-mode-scroll-down-line)
  ("scroll-middle"                :copy-mode-scroll-middle)
  ("scroll-up-half-page"          :copy-mode-half-page-up)
  ("scroll-down-half-page"        :copy-mode-half-page-down)
  ;; emacs-style names
  ("select-word"                  :copy-mode-select-word)
  ;; copy-pipe: copy + pipe-to-cmd, stays in copy mode (no-cancel).
  ;; copy-pipe-and-cancel: copy + pipe-to-cmd, exits copy mode.
  ;; When send-keys -X copy-pipe "cmd" passes an explicit pipe command via
  ;; extra-args, %dispatch-send-keys-X calls copy-mode-copy-pipe{,-no-cancel}
  ;; directly with the argument.  These keyword fallbacks handle the no-arg case
  ;; (using the copy-command global option).
  ("copy-pipe"                    :copy-mode-copy-pipe-no-cancel)
  ("copy-pipe-and-cancel"         :copy-mode-copy-pipe-and-cancel)
  ("copy-pipe-end-of-line-and-cancel"
   :copy-mode-copy-pipe-end-of-line-and-cancel)
  ;; mouse-wheel support
  ("scroll-mouse"                 :copy-mode-scroll-up-line)
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
  ;; other-end / toggle-position: swap the two ends of the selection (vi `o`).
  ("other-end"                    :copy-mode-other-end)
  ("toggle-position"              :copy-mode-other-end)
  ;; pipe / pipe-and-cancel / pipe-no-clear: same semantics as copy-pipe variants.
  ("pipe"                         :copy-mode-copy-pipe-no-cancel)
  ("pipe-and-cancel"              :copy-mode-copy-pipe-and-cancel)
  ("pipe-no-clear"                :copy-mode-copy-pipe-no-cancel)
  ;; search-forward-text / search-backward-text (tmux 3.2+): scripted search
  ;; with the text passed as an extra-arg instead of an interactive prompt.
  ;; Handled specially in %dispatch-send-keys-X via the extra-args path.
  ("search-forward-text"          :copy-mode-search-forward-text)
  ("search-backward-text"         :copy-mode-search-backward-text)
  ;; Bracket matching (vi %): jump to matching bracket.
  ("next-matching-bracket"        :copy-mode-next-matching-bracket)
  ("previous-matching-bracket"    :copy-mode-previous-matching-bracket))
