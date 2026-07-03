(in-package #:cl-tmux)

;;;; Popup / menu overlay handlers split out from dispatch-handlers-b.lisp.

(defun %step-menu (menu step)
  "Advance MENU's selected index by STEP (positive = forward), wrapping around, then redisplay."
  (let ((n (length (menu-items menu))))
    (setf (menu-selected-index menu)
          (mod (+ (menu-selected-index menu) step) n))
    (show-overlay (%format-menu menu))))

(defun %execute-menu-cmd (session byte cmd)
  "Dispatch CMD chosen from a menu in SESSION.
   Keywords dispatch directly; strings run via command-line; lists encode
   structured commands (:select-window N, :switch-client name, or raw tokens)."
  (cond
    ((keywordp cmd)
     (dispatch-command session cmd byte))
    ((stringp cmd)
     (%run-command-line session cmd))
    ((and (consp cmd) (keywordp (first cmd)))
     (case (first cmd)
       (:select-window
        (%with-window-focus-transition (session)
          (select-window-by-number session (second cmd))))
       (:switch-client
        (let ((target (server-find-session (second cmd))))
          (when target (%switch-to-session target))))
       (otherwise
        (%run-command-tokens session cmd))))))

(define-command-handlers
  ;; ── Popup / menu overlays ──────────────────────────────────────────────────
  (:display-popup
   (prompt-nonempty "popup command"
                    (lambda (cmd)
                      (%show-popup-command-output
                       cmd cmd
                       (min +popup-max-width+  *term-cols*)
                       (min +popup-max-height+ (- *term-rows* +popup-margin+))))))
  (:display-popup-dismiss
   (close-popup))
  (:display-menu
   (let ((items (list (cons "New Window"    :new-window)
                      (cons "Next Window"   :next-window)
                      (cons "Prev Window"   :prev-window)
                      (cons "Kill Pane"     :kill-pane)
                      (cons "Kill Window"   :kill-window)
                      (cons "Zoom Toggle"   :zoom-toggle)
                      (cons "List Sessions" :list-sessions)
                      (cons "Detach"        :detach))))
     (%show-jk-menu "Menu" items)))
  (:menu-next   (when *active-menu* (%step-menu *active-menu*  1)))
  (:menu-prev   (when *active-menu* (%step-menu *active-menu* -1)))
  (:menu-select
   (when *active-menu*
     (let* ((idx  (menu-selected-index *active-menu*))
            (cmd  (cdr (nth idx (menu-items *active-menu*))))
            ;; display-menu -O: keep the menu open after the selection runs.
            (keep (cl-tmux/prompt:menu-keep-open *active-menu*)))
       (unless keep
         (close-menu)
         (clear-overlay))
       (when cmd (%execute-menu-cmd session byte cmd)))))
  (:menu-dismiss
   (close-menu)
   (clear-overlay)))
