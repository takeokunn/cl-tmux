(in-package #:cl-tmux/renderer)

;;;; Terminal renderer: composites all pane screens onto the real terminal.
;;;;
;;;; Uses raw ANSI/VT100 escape sequences only — no curses dependency.
;;;; Each render call does a full repaint, buffered in a string stream and
;;;; flushed in one write to minimise flicker.
;;;;
;;;; File layout (all in the cl-tmux/renderer package):
;;;;
;;;;   renderer-format.lisp   — ANSI escape-code primitives (move-to, SGR, cursor)
;;;;   renderer-style.lisp    — Style-string parsing and SGR dispatch tables
;;;;   renderer-pane.lisp     — Pane and split-tree border rendering
;;;;   renderer-overlay.lisp  — Popup and menu box-drawing
;;;;   renderer-statusbar.lisp — Status bar composition (pure, no session knowledge)
;;;;   renderer-compose.lisp  — Session frame compositing, lock-screen, mouse,
;;;;                            render-session-to-string / render-session entry points
;;;;
;;;; This file is intentionally empty: it exists only for ASDF load-order
;;;; documentation.  All renderer code lives in the files listed above.
