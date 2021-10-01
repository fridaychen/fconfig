;;; fc-ergo-seg.el --- Ergo modeline segments -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun fc-modeline-extra-state (state)
  "Mode-line info func."
  (concat (if fc-modal-mode (if *is-colorful* "" "M") "-")
          (if *fc-ergo-prefix* "P" "-")
          (if *fc-dev-mode* "D" "-")
          state
          (upcase (fc-layout-current))))

(defun fc-modeline-proj-name ()
  "Get project name of current buffer."
  (and (boundp 'fc-proj-name) fc-proj-name))

(defconst *fc-menu*
  (fc-create-pop-menu
   "Start"
   '(
     (fc-user-select-control-mode "Control")
     (fc-user-select-project "Projects"))))

(defun fc--menu-seg ()
  "Menu segment."
  (and (boundp '*fc-project-name*)
       (fc-text (format (if *is-colorful* "⟨%s⟩" "{%s}")
                        *fc-project-name*)
                :face fc--modeline-hi-face
                :keys (fc-make-keymap
                       `(([mode-line mouse-1]
                          ,(lambda () (interactive) (fc-eval-pop-menu *fc-menu*))))))))

(add-to-list '*fc-modeline-most-right-string* '(t (:eval (fc--menu-seg))))

(defun fc--tomato-modeline ()
  "Returns the tomate status."
  (when (fc--wide-window-p)
    *fc-tomato-bar*))

(add-to-list 'global-mode-string '(t (:eval (fc--tomato-modeline))))

(provide 'fc-ergo-seg)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ergo-seg.el ends here
