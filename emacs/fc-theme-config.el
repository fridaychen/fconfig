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
  (cl-loop
   with theme = nil
   do
   (setf theme (seq-random-elt themes))
   while (and (> (length themes) 1)
              (if (consp theme)
                  (eq (car theme) *fc-current-theme*)
                (eq theme *fc-current-theme*)))
   finally do (fc-load-theme theme)))

(defun fc-dark-face-p (face)
  (color-dark-p
   (color-name-to-rgb
    (fc-get-face face :background))))

(defun fc-dark-theme-p ()
  "Test if the current theme is dark."
  (fc-dark-face-p 'default))

(defvar *fc-theme-deep-dark-diff-threshold* 65000)

(cl-defun fc-deep-dark-theme-p (&optional (threshold *fc-theme-deep-dark-diff-threshold*))
  "Test if the current theme is deep dark."
  (and (fc-dark-theme-p)
       (> threshold
          (color-distance (fc-get-face 'default :foreground)
                          (fc-get-face 'default :background)))))

(cl-defun fc--get-color-light (color-name)
  (seq-elt
   (apply #'color-rgb-to-hsl (color-name-to-rgb color-name))
   2))

(cl-defun fc--set-color-light (color-name light)
  (apply #'fc-apply-chain
         (list #'color-rgb-to-hsl
               #'(lambda (h s _) (list h s light ))
               #'color-hsl-to-rgb
               #'color-rgb-to-hex
               )
         (color-name-to-rgb color-name)))

(cl-defun fc--set-face-fg-light (face light)
  (fc-set-face face nil
               :foreground
               (fc--set-color-light (fc-get-face face :foreground) light)))

(cl-defun fc--set-face-bg-light (face light)
  (fc-set-face face nil
               :background
               (fc--set-color-light (fc-get-face face :background) light)))

(defun fc--adjust-face-fg-light (face delta)
  (let* ((color (fc-get-face face :foreground))
         (light (fc--get-color-light color))
         (new-light (+ light delta)))

    (unless (>= 1 new-light 0)
      (error "Wrong light"))

    (fc-set-face face nil
                 :foreground (fc--set-color-light
                              color
                              new-light))))

(defun fc--adjust-face-bg-light (face delta)
  (let* ((color (fc-get-face face :background))
         (light (fc--get-color-light color))
         (new-light (+ light delta)))

    (unless (>= 1 new-light 0)
      (error "Wrong light"))

    (fc-set-face face nil
                 :background (fc--set-color-light
                              color
                              new-light))))

(cl-defun fc--get-face-contrast (face)
  (let* ((bg (fc-get-face face :background))
         (fg (fc-get-face face :foreground)))
    (abs (-
          (fc--get-color-light fg)
          (fc--get-color-light bg)))))

(cl-defun fc--set-face-contrast (face contrast &optional (adjust-fg t))
  (let* ((current-contrast (fc--get-face-contrast face)))
    (if adjust-fg
        (fc--adjust-face-fg-light face
                                  (* (- contrast current-contrast)
                                     (if (fc-dark-face-p face)
                                         1
                                       -1)))
      (fc--adjust-face-bg-light face
                                (* (- contrast current-contrast)
                                   (if (fc-dark-face-p face)
                                       -1
                                     1))))))

(cl-defun fc--adjust-face-contrast (face contrast)
  (let* ((current-contrast (fc--get-face-contrast face)))
    (when (> current-contrast contrast)
      (cl-return-from fc--adjust-face-contrast))

    (fc--adjust-face-fg-light face
                              (* (abs (- contrast current-contrast))
                                 (if (fc-dark-face-p face)
                                     1
                                   -1)))))

(when *is-mac*
  (setf ns-use-srgb-colorspace nil))

(provide 'fc-theme-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-theme-config.el ends here
