(in-package #:cl-tmux/options)

;;; Global option storage

(defvar *global-options* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to their current values.")

(defvar *server-options* (make-hash-table :test #'equal)
  "Hash-table for server-scoped options (set-option -s).
   Keys: escape-time, exit-empty, exit-unattached.")

;;; Option specification

(defstruct option-spec
  "Describes one tmux option: its name, type keyword, and default value."
  name
  type
  default)

(defvar *option-registry* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to OPTION-SPEC instances.")

;;; ── Unified option-table registration macro ───────────────────────────────
;;;
;;; define-option-table encapsulates the two-phase expand pattern shared by
;;; define-tmux-options and define-server-options: registering spec metadata
;;; (immutable after load) and initialising runtime default values.
;;;
;;; Parameters:
;;;   REGISTRY-VAR  — the *-option-registry* hash-table to receive specs
;;;   STORAGE-VAR   — the *-options* hash-table to receive runtime defaults
;;;   SPECS         — list of (name type default) triples

(defmacro define-option-table (registry-var storage-var &rest specs)
  "Register option specs in REGISTRY-VAR and initialise STORAGE-VAR with defaults.
   Each SPEC has the form (name type default) where TYPE is :boolean, :integer,
   or :string.  Phase 1 stores spec metadata; phase 2 stores runtime defaults."
  `(progn
     ;; Phase 1: register spec metadata (immutable after load)
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name type default) spec
                   `(setf (gethash ,name ,registry-var)
                          (make-option-spec :name ,name
                                            :type ,type
                                            :default ,default))))
               specs)
     ;; Phase 2: initialise runtime default values
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name _type default) spec
                   (declare (ignore _type))
                   `(setf (gethash ,name ,storage-var) ,default)))
               specs)))

;;; ── Convenience wrappers for the two standard option tables ───────────────

(defmacro define-tmux-options (&rest specs)
  "Register tmux options in *OPTION-REGISTRY* (spec metadata) and initialise
   *GLOBAL-OPTIONS* with their defaults (runtime state).  Each SPEC has the form
   (name type default) where TYPE is :boolean, :integer, or :string."
  `(define-option-table *option-registry* *global-options* ,@specs))

(defmacro define-server-options (&rest specs)
  "Register server options in *SERVER-OPTION-REGISTRY* and initialise
   *SERVER-OPTIONS* with their defaults.  Each SPEC has the form
   (name type default)."
  `(define-option-table *server-option-registry* *server-options* ,@specs))

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
  ("escape-time"              :integer 500)
  ("base-index"               :integer 0)
  ("pane-base-index"          :integer 0)
  ("mouse"                    :boolean nil)
  ("default-command"          :string  "")
  ("default-shell"            :string  "/bin/sh")
  ("exit-unattached"          :boolean nil)
  ("pane-border-style"        :string  "")
  ("pane-active-border-style" :string  "fg=green")
  ;; pane-border-indicators: how the active pane's border is indicated — "colour"
  ;; (default), "both", and "arrows" colour it (cl-tmux does not draw the arrow
  ;; glyphs, so "arrows" degrades to colour); "off" disables the highlight.
  ("pane-border-indicators"   :string  "colour")
  ;; Border line glyphs: single (default light box-drawing), double, heavy,
  ;; simple (ASCII).  number/padded fall back to single (glyph-only support).
  ("pane-border-lines"        :string  "single")
  ("synchronize-panes"        :boolean nil)
  ("word-separators"          :string  " -_@")
  ("automatic-rename"         :boolean t)
  ("automatic-rename-format"  :string  "#{pane_current_command}")
  ("bell-action"              :string  "any")
  ("visual-bell"              :boolean nil)
  ("visual-activity"          :boolean nil)
  ("visual-silence"           :boolean nil)
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
  ;; Text shown (reverse-video) in a pane kept open by remain-on-exit.  tmux's
  ;; default references #{pane_dead_status/signal/time}, which cl-tmux does not
  ;; track, so the default is simplified to a plain "Pane is dead".
  ("remain-on-exit-format"    :string  "Pane is dead")
  ("renumber-windows"         :boolean nil)
  ;; Max messages kept in the message log, and max command-prompt history entries.
  ("message-limit"            :integer 1000)
  ("prompt-history-limit"     :integer 100)
  ("message-style"            :string  "")
  ("update-environment"       :string  "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_CONNECTION WINDOWID XAUTHORITY")
  ;; Display options
  ("display-time"             :integer 750)    ; ms to show messages / pane numbers
  ("display-panes-time"       :integer 1000)   ; ms to show pane numbers (display-panes)
  ("display-panes-colour"     :string  "blue")
  ("display-panes-active-colour" :string "red")
  ;; Resize and timing
  ("repeat-time"              :integer 500)    ; ms window for repeatable bindings
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
  ("mode-keys"                :string  "vi")     ; vi or emacs copy-mode keys
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
  ("copy-mode-current-match-style" :string "bg=magenta")
  ("copy-mode-match-style"    :string  "bg=green")
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

;;; Server-option registry and defaults

(defvar *server-option-registry* (make-hash-table :test #'equal)
  "Specs for server-scoped options (set with set-option -s).")

(define-server-options
  ("escape-time"          :integer 500)
  ("exit-empty"           :boolean t)
  ("exit-unattached"      :boolean nil)
  ("focus-events"         :boolean nil)  ; enable focus-events reporting (server-wide)
  ("set-clipboard"        :string  "on") ; external, on, or off
  ("terminal-features"    :string  "")
  ("terminal-overrides"   :string  "")
  ("command-alias"        :string  "")   ; array stored as single string for simplicity
  ("default-terminal"     :string  "screen")
  ("buffer-limit"         :integer 50))

