(in-package #:cl-tmux/control)

;;;; tmux control mode (-C) wire protocol.
;;;;
;;;; A control client (iTerm2, tmuxp, libtmux, ...) drives tmux over a plain text
;;;; protocol instead of a curses UI: it sends commands as lines on stdin, and the
;;;; server frames each reply in a %begin/%end (or %begin/%error) block and emits
;;;; %-prefixed notifications asynchronously as state changes.
;;;;
;;;; ID sigils (matching real tmux): "$" session, "@" window, "%" pane.
;;;;
;;;; This file is the PURE protocol layer — the line formatters — verified against
;;;; control-mode.c / control-notify.c.  The control-mode REPL (read line → dispatch
;;;; → frame reply) is wired on top of these in a later step.

;;; ── Command-reply framing ───────────────────────────────────────────────────
;;;
;;; A reply is:   %begin <time> <number> <flags>
;;;               <command output, line by line>
;;;               %end   <time> <number> <flags>     (or %error on failure)
;;; TIME is a Unix timestamp, NUMBER a monotonic command counter, FLAGS usually 1.

(defun %control-line (verb time number flags)
  "Format a `%VERB TIME NUMBER FLAGS` control line (the shape of %begin/%end/%error)."
  (format nil "%~A ~D ~D ~D" verb time number flags))

(defun control-begin (number &key (time 0) (flags 1))
  "The %begin line that opens a command reply block for command NUMBER."
  (%control-line "begin" time number flags))

(defun control-end (number &key (time 0) (flags 1))
  "The %end line that closes a successful command reply block."
  (%control-line "end" time number flags))

(defun control-error (number &key (time 0) (flags 1))
  "The %error line that closes a FAILED command reply block."
  (%control-line "error" time number flags))

(defun control-format-reply (number output &key (success t) (time 0) (flags 1))
  "Frame OUTPUT (a possibly-multi-line command result string) as a control-mode
   reply: a %begin line, the output lines, then %end — or %error when SUCCESS is
   NIL.  An empty/NIL OUTPUT yields just the %begin/%end pair."
  (with-output-to-string (s)
    (write-line (control-begin number :time time :flags flags) s)
    (when (and output (plusp (length output)))
      (loop with text = (string-right-trim '(#\Newline) output)
            for start = 0 then (1+ nl)
            for nl = (position #\Newline text :start start)
            do (write-line (subseq text start (or nl (length text))) s)
            while nl))
    (write-string (if success
                      (control-end   number :time time :flags flags)
                      (control-error number :time time :flags flags))
                  s)))

;;; ── Pane output notification ────────────────────────────────────────────────

(defun control-escape-output (data)
  "Escape pane-output DATA for a %output line: bytes outside printable ASCII
   (code < 32 or >= 127) become a 3-digit octal escape \\ooo; printable ASCII
   passes through.  Mirrors tmux's VIS_OCTAL escaping so a control client can
   recover the original bytes."
  (with-output-to-string (s)
    (loop for ch across data
          for code = (char-code ch)
          do (if (<= 32 code 126)
                 (write-char ch s)
                 (format s "\\~3,'0O" code)))))

(defun control-output (pane-id data)
  "A `%output %<pane-id> <escaped-data>` notification carrying pane output."
  (format nil "%output %~D ~A" pane-id (control-escape-output data)))

;;; ── State-change notifications ──────────────────────────────────────────────

(defun control-session-changed (session-id name)
  "`%session-changed $<id> <name>` — the client's current session changed."
  (format nil "%session-changed $~D ~A" session-id name))

(defun control-session-renamed (session-id name)
  "`%session-renamed $<id> <name>`."
  (format nil "%session-renamed $~D ~A" session-id name))

(defun control-window-add (window-id)
  "`%window-add @<id>` — a window was linked into the client's session."
  (format nil "%window-add @~D" window-id))

(defun control-window-close (window-id)
  "`%window-close @<id>` — a window was unlinked/closed."
  (format nil "%window-close @~D" window-id))

(defun control-window-renamed (window-id name)
  "`%window-renamed @<id> <name>`."
  (format nil "%window-renamed @~D ~A" window-id name))

(defun control-layout-change (window-id layout visible-layout raw-flags)
  "`%layout-change @<id> <layout> <visible-layout> <raw-flags>`."
  (format nil "%layout-change @~D ~A ~A ~A" window-id layout visible-layout raw-flags))

(defun control-window-pane-changed (window-id pane-id)
  "`%window-pane-changed @<window-id> %<pane-id>` — the active pane within a window
   changed (tmux control_notify_window_pane_changed)."
  (format nil "%window-pane-changed @~D %~D" window-id pane-id))

(defun control-session-window-changed (session-id window-id)
  "`%session-window-changed $<session-id> @<window-id>` — a session's active window
   changed (tmux control_notify_session_window_changed)."
  (format nil "%session-window-changed $~D @~D" session-id window-id))

(defun control-unlinked-window-add (window-id)
  "`%unlinked-window-add @<id>` — a window was linked but NOT to the client's session."
  (format nil "%unlinked-window-add @~D" window-id))

(defun control-client-session-changed (client session-id name)
  "`%client-session-changed <client> $<id> <name>` — another client's session changed."
  (format nil "%client-session-changed ~A $~D ~A" client session-id name))

(defun control-exit (&optional reason)
  "`%exit` (optionally with a REASON) — the control client is detaching."
  (if reason (format nil "%exit ~A" reason) "%exit"))
