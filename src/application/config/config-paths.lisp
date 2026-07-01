(in-package #:cl-tmux/config)

;;; -- Config-file path resolution + top-level file loading ---------------------
;;;
;;; %config-path-from / config-file-path resolve the cl-tmux config path (XDG
;;; Base Directory spec, with a $CL_TMUX_CONF override).  %tmux-conf-paths lists
;;; the traditional tmux.conf fallback locations.  load-config-file is the
;;; top-level entry point that opens a path (or auto-detects one) and hands the
;;; stream to load-config-from-stream (config-preprocessor.lisp).

(defun %env-set-p (env-string)
  "True when environment variable string ENV-STRING is set and non-empty."
  (and env-string (plusp (length env-string))))

(defun %config-path-from (override xdg home)
  "Resolve the config-file path from environment values (OVERRIDE = $CL_TMUX_CONF,
   XDG = $XDG_CONFIG_HOME, each a string or NIL) and HOME (a directory pathname).

   Precedence (XDG Base Directory spec):
     1. $CL_TMUX_CONF                              — explicit override
     2. $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf
     3. ~/.config/cl-tmux/cl-tmux.conf             — XDG default when unset
   Empty strings are treated as unset.  Pure: no I/O, no environment access."
  (if (%env-set-p override)
      (pathname override)
      (let ((base (if (%env-set-p xdg)
                      xdg
                      (namestring (merge-pathnames ".config/" home)))))
        (pathname (format nil "~A/cl-tmux/cl-tmux.conf"
                          (string-right-trim "/" base))))))

(defun %tmux-conf-paths (home)
  "Return a list of candidate .tmux.conf paths in priority order:
     1. $XDG_CONFIG_HOME/tmux/tmux.conf
     2. ~/.config/tmux/tmux.conf  (XDG default)
     3. ~/.tmux.conf              (traditional location)"
  (let* ((xdg  (sb-ext:posix-getenv "XDG_CONFIG_HOME"))
         (base (if (%env-set-p xdg)
                   xdg
                   (namestring (merge-pathnames ".config/" home)))))
    (list (pathname (format nil "~A/tmux/tmux.conf"
                            (string-right-trim "/" base)))
          (merge-pathnames ".tmux.conf" home))))

(defun config-file-path ()
  "Path to the user config file, honoring $CL_TMUX_CONF then the XDG Base
   Directory spec ($XDG_CONFIG_HOME, default ~/.config).  See %config-path-from."
  (%config-path-from (sb-ext:posix-getenv "CL_TMUX_CONF")
                     (sb-ext:posix-getenv "XDG_CONFIG_HOME")
                     (user-homedir-pathname)))

(defun load-config-file (&optional (path (config-file-path)))
  "Load and apply the config file at PATH if it exists (returns the count of
   directives applied), or NIL when no file is found.
   PATH defaults to the XDG/cl-tmux path; pass NIL to auto-detect, which also
   searches the standard .tmux.conf locations."
  (if path
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in (load-config-from-stream in)))
      ;; Auto-detect: try each candidate path in priority order.
      (let ((home (user-homedir-pathname)))
        (dolist (candidate (cons (config-file-path)
                                 (%tmux-conf-paths home)))
          (with-open-file (in candidate :direction :input :if-does-not-exist nil)
            (when in
              (return (load-config-from-stream in))))))))
