(in-package #:cl-tmux/options)

;;;; Registered tmux option specs (name/type/default triples).
;;;;
;;;; This is pure DATA fed to the define-tmux-options / define-server-options
;;;; macros defined in options.lisp; it is loaded immediately after that file so
;;;; the macros are already available.  Keeping the tables in their own file
;;;; mirrors this project's convention for large data blocks (see
;;;; commands-keys-data.lisp) and keeps options.lisp itself as pure macro/logic.

;;; Registered options

(define-tmux-options
  ;; tmux 2.9+: `status` is a CHOICE/number {off,on,2,3,4,5}, NOT a boolean — an
  ;; integer N selects an N-row multi-line status bar (capped at 5).  Registered
  ;; :string so "2".."5"/"on"/"off" survive set-option unchanged; status-line-count
  ;; (renderer) and the *status-height* side-effect both parse the string.
  ("status"                   :string  "on")
  ("status-position"          :string  "bottom")
  ("status-interval"          :integer 15)
  ("status-left"              :string  "[#{session_name}]")
  ("status-right"             :string  "#{time}")
  ("status-left-length"       :integer 40)
  ("status-right-length"      :integer 40)
  ("status-style"             :string  "")
  ("status-justify"           :string  "left")
  ("window-status-format"     :string  " #{window_index}:#{window_name} ")
  ("window-status-current-format" :string " #{window_index}:#{window_name}* ")
  ("window-status-style"      :string  "")
  ("window-status-current-style" :string "reverse")
  ;; Pane background/foreground defaults: window-style applies to inactive panes,
  ;; window-active-style to the active pane.  Empty = no override (the common
  ;; "dim inactive panes" idiom sets a dimmer bg on window-style).
  ("window-style"             :string  "")
  ("window-active-style"      :string  "")
  ;; Copy-mode selection highlight.  "reverse" (default) → reverse-video; a
  ;; colour-based value (e.g. "bg=colour172") recolours the selection instead.
  ("mode-style"               :string  "reverse")
  ;; Alert-state window-tab styles (applied to a non-active window in that state):
  ;; bell takes priority over activity, then last (previously active) window.
  ("window-status-activity-style" :string "reverse")
  ("window-status-bell-style"     :string "reverse")
  ("window-status-last-style"     :string "")
  ("window-status-separator"  :string  " ")
  ("history-limit"            :integer 2000)
  ("escape-time"              :integer 10)
  ("base-index"               :integer 0)
  ("pane-base-index"          :integer 0)
  ("mouse"                    :boolean nil)
  ("default-command"          :string  "")
  ("default-shell"            :string  "/bin/sh")
  ("exit-unattached"          :boolean nil)
  ("pane-border-style"        :string  "")
  ("pane-active-border-style" :string  "fg=green")
  ;; pane-border-indicators: how the active pane's border is indicated —
  ;; "colour" (default) colours it, "arrows" draws arrow glyphs pointing at the
  ;; active pane, "both" does both, "off" disables all indicators (tmux).
  ("pane-border-indicators"   :string  "colour")
  ;; Border line glyphs: single (default light box-drawing), double, heavy,
  ;; simple (ASCII), padded (blank).  "number" uses single glyphs and writes
  ;; the adjacent pane's number into the border.
  ("pane-border-lines"        :string  "single")
  ("synchronize-panes"        :boolean nil)
  ("word-separators"          :string  " -_@")
  ("automatic-rename"         :boolean t)
  ("automatic-rename-format"  :string  "#{pane_current_command}")
  ("bell-action"              :string  "any")
  ;; visual-* are tmux off/on/both enums: "on"/"both" show the message overlay,
  ;; "off"/"both" keep the audible bell (visual-bell only).
  ("visual-bell"              :string  "off")
  ("visual-activity"          :string  "off")
  ("visual-silence"           :string  "off")
  ("monitor-activity"         :boolean nil)
  ;; monitor-silence is an INTEGER (seconds of PTY silence before a window
  ;; alerts); 0 = off.  The %check-monitor-silence path (runtime.lisp) — set
  ;; window-silence-flag + fire alert-silence (+ visual-silence overlay) — reads
  ;; this option; registering it gives the correct default (0) and lists it in
  ;; show-options, completing the monitor-* family alongside monitor-activity.
  ("monitor-silence"          :integer 0)
  ("monitor-bell"             :boolean t)
  ("activity-action"          :string  "other")
  ("silence-action"           :string  "other")
  ("buffer-limit"             :integer 50)
  ("focus-events"             :boolean nil)
  ("copy-command"             :string  "")
  ("set-titles"               :boolean nil)
  ("set-titles-string"        :string  "#S:#I:#W")
  ("remain-on-exit"           :boolean nil)
  ;; Text shown (reverse-video) in a pane kept open by remain-on-exit.
  ;; #{pane_dead_status/signal/time} ARE tracked (pane death record) and
  ;; resolve in this format; the registered default is kept simple while
  ;; users may set tmux's richer conditional default verbatim.
  ("remain-on-exit-format"    :string  "Pane is dead")
  ("renumber-windows"         :boolean nil)
  ;; Max messages kept in the message log, and max command-prompt history entries.
  ("message-limit"            :integer 1000)
  ("prompt-history-limit"     :integer 100)
  ;; Which status line row messages appear on (tmux 0..4, clamped to the
  ;; status height).  Honoured by render-overlay's message path: single-line
  ;; messages draw over the status area; multi-line pagers stay top-anchored.
  ("message-line"             :integer 0)
  ;; Milliseconds within which consecutive keys are assumed to be a paste and
  ;; root-table key bindings are skipped (tmux default 1).  cl-tmux relies on
  ;; bracketed paste (DECSET 2004, supported) for paste detection; registered
  ;; for set/show-options fidelity.
  ("assume-paste-time"        :integer 1)
  ("message-style"            :string  "")
  ("update-environment"       :string  "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_CONNECTION WINDOWID XAUTHORITY")
  ;; Display options
  ("display-time"             :integer 750)    ; ms to show messages / pane numbers
  ("display-panes-time"       :integer 1000)   ; ms to show pane numbers (display-panes)
  ("display-panes-colour"     :string  "blue")
  ("display-panes-active-colour" :string "red")
  ;; Resize and timing
  ("repeat-time"              :integer 500)    ; ms window for repeatable bindings
  ("initial-repeat-time"      :integer 0)      ; ms window for the FIRST repeat; 0 = use repeat-time (tmux 3.5+)
  ("double-click-time"        :integer 500)    ; ms window for double/triple mouse clicks
  ("lock-after-time"          :integer 0)      ; 0 = disabled
  ;; Terminal settings
  ("default-terminal"         :string  "screen")
  ("terminal-overrides"       :string  "")
  ;; Window/pane defaults
  ("allow-rename"             :boolean t)
  ("alternate-screen"         :boolean t)
  ;; scroll-on-clear: when on (tmux default), clearing the whole screen (ED 2 /
  ;; the `clear` command) first scrolls the visible content into the history.
  ("scroll-on-clear"          :boolean t)
  ;; main-horizontal / main-vertical layout: size of the main (first) pane.
  ("main-pane-width"          :integer 80)
  ("main-pane-height"         :integer 24)
  ;; ...and the OTHER (non-main) region: when non-zero it overrides main-pane-*,
  ;; giving the other panes this size and the main pane the rest (0 = unset).
  ("other-pane-width"         :integer 0)
  ("other-pane-height"        :integer 0)
  ;; Status bar extras
  ("status-keys"              :string  "emacs")  ; emacs or vi
  ("mode-keys"                :string  "emacs")  ; emacs or vi copy-mode keys (tmux default emacs; vi-autodetected from $VISUAL/$EDITOR at startup)
  ("status-left-style"        :string  "")
  ("status-right-style"       :string  "")
  ;; Pane border status line (top / bottom / off)
  ("pane-border-status"       :string  "off")
  ("pane-border-format"       :string  " #{pane_index} ")
  ;; Clock display
  ("clock-mode-colour"        :string  "blue")
  ("clock-mode-style"         :integer 24)      ; 12 or 24 hour
  ;; Copy mode search
  ("wrap-search"              :boolean t)        ; wrap search in copy-mode
  ("copy-mode-line-numbers"   :string  "off")
  ("copy-mode-line-number-style" :string "")
  ("copy-mode-current-line-number-style" :string "")
  ("copy-mode-current-match-style" :string "bg=magenta")
  ("copy-mode-match-style"    :string  "bg=green")
  ("copy-mode-position-format" :string  "#[align=right]#{t/p:top_line_time}#{?#{e|>:#{top_line_time},0}, ,}[#{copy_position}/#{copy_position_limit}]#{?search_timed_out, (timed out),#{?search_count, (#{search_count}#{?search_count_partial,+,} results),}}")
  ("copy-mode-position-style" :string  "#{E:mode-style}")
  ("copy-mode-selection-style" :string  "#{E:mode-style}")
  ("copy-mode-mark-style"     :string  "bg=red,fg=black")
  ;; Session lifecycle
  ("destroy-unattached"       :boolean nil)     ; destroy session when no clients
  ;; detach-on-destroy: off / on (default) / no-detached / previous / next.
  ;; A choice option (string), NOT boolean — controls what the client does when the
  ;; session it is viewing is destroyed.
  ("detach-on-destroy"        :string  "on")
  ;; Window sizing
  ("default-size"             :string  "80x24") ; default WxH for new sessions
  ;; Multi-client sizing policy: smallest / largest / latest / manual.
  ;; cl-tmux broadcasts ONE shared frame to all clients, so "smallest" (every
  ;; client can display it) is the safe default — diverging from tmux's "latest"
  ;; default, which in a shared-frame model could overflow smaller terminals.
  ("window-size"              :string  "smallest")
  ;; Input handling
  ("extended-keys"            :string  "off")   ; off / on / always
  ("key-table"                :string  "prefix") ; accepted; default key table (INERT —
                                                 ; the active table is driven by the prefix
                                                 ; and switch-client -T, not this option)
  ("prefix2"                  :string  "")      ; secondary prefix key
  ;; History / logging
  ("history-file"             :string  "")      ; save command-prompt history here (wired)
  ;; status-format (tmux 3.2+ array-style).  status-format[0], when set, IS now
  ;; honoured: render-status-bar expands it and composes #[align=…] regions
  ;; (see %compose-aligned-line) instead of the procedural left/window-list/right
  ;; path.  This bare "status-format" key is the registry default; the per-row
  ;; values are stored under the array keys status-format[0..N].
  ("status-format"            :string  "")
  ;; Popup defaults
  ("popup-border-lines"       :string  "single")
  ("popup-border-style"       :string  "")
  ;; popup-style colours the popup interior (the empty body of a text popup).
  ("popup-style"              :string  "")
  ;; Passthrough: forward DCS/OSC sequences from pane to outer terminal.
  ;; Values: "off" (default), "on" (non-nested), "all" (always).
  ("allow-passthrough"        :string  "off")
  ;; Command-prompt style (used when a : command-prompt overlay is active).
  ("message-command-style"    :string  "")
  ;; display-menu appearance: menu-style applies to the menu items, menu-selected-
  ;; style to the highlighted item.  Empty (default) = no colour (the ▶ indicator
  ;; alone marks the selection), so applying these is opt-in.
  ("menu-style"               :string  "")
  ("menu-selected-style"      :string  "")
  ;; menu-border-lines selects the menu box glyphs (single/rounded/double/heavy/
  ;; simple/padded/none); menu-border-style colours the border.
  ("menu-border-lines"        :string  "single")
  ("menu-border-style"        :string  ""))

(define-server-options
  ("escape-time"          :integer 10)
  ("exit-empty"           :boolean t)
  ("exit-unattached"      :boolean nil)
  ("focus-events"         :boolean nil)  ; enable focus-events reporting (server-wide)
  ("set-clipboard"        :string  "on") ; external, on, or off
  ("terminal-features"    :string  "")
  ("terminal-overrides"   :string  "")
  ("default-terminal"     :string  "screen")
  ("buffer-limit"         :integer 50)
  ;; The byte the client's Backspace key (DEL, 0x7f) is translated to before it
  ;; reaches the pane PTY — tmux key syntax (C-? default = identity, C-h = 8).
  ;; Honoured in %forward-octets-synchronized (events-loop-timers.lisp).
  ("backspace"            :string  "C-?")
  ;; Editor used for edit-buffer-style commands.  tmux resolves $EDITOR at
  ;; runtime with vi as the fallback; cl-tmux has no buffer-editing subsystem
  ;; yet, so this is registered for set/show-options fidelity only.
  ("editor"               :string  "vi"))
