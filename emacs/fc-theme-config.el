;;; fc-theme.el --- setup theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-current-theme* nil "Current theme.")

(defun fc-reset-theme ()
  "Reset color theme."
  (interactive)

  (load-theme *fc-current-theme* t)

  (fc-run-hook '*fc-after-theme-hook*))

(defun fc--do-load-theme ()
  "Load color theme."
  (-map #'disable-theme custom-enabled-themes)
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
    (setq *fc-current-theme* theme)
    (fc--do-load-theme))

   ((consp theme)
    (setq *fc-current-theme* (car theme))
    (fc--do-load-theme))

   (t
    (message "Unknown theme type"))))

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

(defvar *fc-theme-deep-dark-diff-threshold* 70000)

(cl-defun fc-deep-dark-theme-p (&optional (threshold *fc-theme-deep-dark-diff-threshold*))
  "Test if the current theme is deep dark."
  (and (fc-dark-theme-p)
       (> threshold
          (fc-color-difference (fc-get-face-attribute 'default :foreground)
                               (fc-get-face-attribute 'default :background)))))

(when *is-mac*
  (setf ns-use-srgb-colorspace nil))

(provide 'fc-theme-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-theme-config.el ends here
