;;; fc-theme.el --- setup theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-dark-theme* 'wombat "Prefer dark theme.")
(defvar *fc-deep-dark-theme* 'wombat "Prefer deep dark theme.")
(defvar *fc-light-theme* 'tango "Prefer light theme.")
(defvar *fc-current-theme* nil "Current theme.")

(defun fc-reset-theme ()
  "Reset color theme."
  (interactive)

  (load-theme *fc-current-theme* t)

  (fc-run-hook '*fc-after-theme-hook*))

(defun fc--after-load-theme ()
  "Load color theme."
  (load-theme *fc-current-theme* t)

  (fc-run-hook '*fc-after-theme-hook*
               (if *fc-booting* 2 1)))

(cl-defun fc-load-theme (theme)
  "Load theme.
THEME: new theme."
  (when theme
    (-map #'disable-theme custom-enabled-themes)

    (cond
     ((symbolp theme)
      (setf *fc-current-theme* theme)
      (fc--after-load-theme))

     ((consp theme)
      (setf *fc-current-theme* (car theme))
      (fc--after-load-theme))

     (t
      (message "Unknown theme type")
      (cl-return-from fc-load-theme)))))

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
    (fc-load-theme theme)
    (message "Load theme %s" theme)))

(defun fc-light-theme ()
  "Randomly select a light theme."
  (interactive)

  (fc-theme-auto-select *fc-light-theme*))

(defun fc-dark-theme ()
  "Randomly select a dark theme."
  (interactive)

  (fc-theme-auto-select *fc-dark-theme*))

(defun fc-deep-dark-theme ()
  "Randomly select a deep dark theme."
  (interactive)

  (fc-theme-auto-select *fc-deep-dark-theme*))

(defun fc-dark-theme-p ()
  "Test if the current theme is dark."
  (color-dark-p
   (color-name-to-rgb
    (face-attribute 'default :background))))

(defvar *fc-theme-deep-dark-diff-threshold* 70000)

(defun fc-deep-dark-theme-p ()
  "Test if the current theme is deep dark."
  (and (fc-dark-theme-p)
       (> *fc-theme-deep-dark-diff-threshold*
          (fc-color-difference (fc-get-face-attribute 'default :foreground)
                               (fc-get-face-attribute 'default :background)))))

(when *is-mac*
  (setf ns-use-srgb-colorspace nil))

(let ((install-theme (lambda (x)
                       (and (consp x)
                            (fc-install (cdr x))))))
  (-each *fc-dark-theme* install-theme)
  (-each *fc-deep-dark-theme* install-theme)
  (-each *fc-light-theme* install-theme))

(provide 'fc-theme-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-theme-config.el ends here
