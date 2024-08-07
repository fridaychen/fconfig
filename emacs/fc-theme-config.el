;;; fc-theme.el --- setup theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-current-theme* nil "Current theme.")

(defun fc-reset-theme ()
  "Reset color theme."
  (interactive)

  (fc--do-load-theme))

(defun fc--do-load-theme ()
  "Load color theme."
  (mapc #'disable-theme custom-enabled-themes)

  (fc-run-hook '*fc-before-theme-hook*)
  (load-theme *fc-current-theme* t)
  (fc-run-hook '*fc-after-theme-hook*)

  (force-mode-line-update)

  (message "Load theme %s" *fc-current-theme*))

(cl-defun fc-load-theme (theme)
  "Load theme.
THEME: new theme."
  (unless theme
    (cl-return-from fc-load-theme))

  (cond
   ((symbolp theme)
    (setq *fc-current-theme* theme))

   ((consp theme)
    (setq *fc-current-theme* (car theme)))

   (t
    (message "Unknown theme type")
    (cl-return-from fc-load-theme)))

  (fc--do-load-theme))

(defun fc-theme-auto-select (themes)
  "Auto select and load theme from THEMES.
THEMES: list of themes."
  (let ((theme (cl-loop
                with theme = nil
                do
                (setf theme (seq-random-elt themes))
                while (and (> (length themes) 1)
                           (if (consp theme)
                               (eq (car theme) *fc-current-theme*)
                             (eq theme *fc-current-theme*)))
                finally return theme)))
    (fc-load-theme theme)))

(defun fc-dark-theme-p ()
  "Test if the current theme is dark."
  (color-dark-p
   (color-name-to-rgb
    (face-attribute 'default :background))))

(defvar *fc-theme-deep-dark-diff-threshold* 65000)

(cl-defun fc-deep-dark-theme-p (&optional (threshold *fc-theme-deep-dark-diff-threshold*))
  "Test if the current theme is deep dark."
  (and (fc-dark-theme-p)
       (> threshold
          (fc-color-difference (fc-get-face 'default :foreground)
                               (fc-get-face 'default :background)))))

(when *is-mac*
  (setf ns-use-srgb-colorspace nil))

(provide 'fc-theme-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-theme-config.el ends here
