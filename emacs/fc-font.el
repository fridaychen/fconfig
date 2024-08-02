;;; fc-font.el --- setup font -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-font-height* 180)
(defvar *fc-font-width* 'normal)
(defvar *fc-default-font* "Monaco")
(defvar *fc-read-font* "Monaco")
(defvar *fc-font* '(('iso-8859-1 . "Monaco")))
(defvar *fc-font-weight-of-default* 'regular)
(defvar *fc-use-another-font-for-mode-line* nil)
(defvar *fc-mode-line-font* nil)

(cl-defun fc-create-fontset (fontset-name family &optional (extra-props ""))
  (let* ((font (format "%s:weight=normal:slant=normal:%s"
                       family
                       extra-props)))
    (create-fontset-from-ascii-font
     font
     nil
     fontset-name)))

(cl-defun fc-setup-font-spec (fontset charset-specs)
  (mapc (lambda (n)
          (dolist (charset (car n))
            (set-fontset-font
             fontset
             charset
             (apply #'font-spec
                    (cdr n)))))

        charset-specs))

(defun fc-setup-font ()
  "Setup font."
  (fc-set-face
   'default
   nil
   :height *fc-font-height*
   :weight *fc-font-weight-of-default*
   :width *fc-font-width*
   *fc-default-font*)

  (let ((italic-font (apply #'font-spec
                            :slant 'italic
                            *fc-default-font*)))
    (when (find-font italic-font)
      (set-face-attribute 'italic
                          nil
                          :font italic-font
                          :height *fc-font-height*)))

  (let ((bold-italic-font (apply #'font-spec
                                 :slant 'italic
                                 :weight 'bold
                                 *fc-default-font*)))
    (when (find-font bold-italic-font)
      (set-face-attribute 'bold-italic
                          nil
                          :font bold-italic-font
                          :height *fc-font-height*)))

  (fc-setup-font-spec (frame-parameter nil 'font) *fc-font*)

  (when *fc-use-another-font-for-mode-line*
    (fc-set-face 'mode-line
                 nil
                 *fc-mode-line-font*
                 :height (+ *fc-font-height*
                            *fc-font-mode-line-delta*))))

(cl-defun fc-reset-buffer-font ()
  (apply #'buffer-face-set
         :height *fc-font-height*
         :weight *fc-font-weight-of-default*
         :width *fc-font-width*
         *fc-default-font*))

(cl-defun fc-adjust-font (delta)
  (cl-incf *fc-font-height* delta)
  (fc-setup-font))

(defun fc-increase-font ()
  "Increase font size."
  (interactive)

  (fc-adjust-font 2))

(defun fc-decrease-font ()
  "Decrease font size."
  (interactive)

  (fc-adjust-font -2))

(defun fc-font-exists-p (font)
  "Test is a font exists.
FONT: to be tested."
  (find-font (apply #'font-spec font)))

(defun fc-select-font-family ()
  "Select a font family from system."
  (fc-select "Select font family : "
      (delete-dups
       (sort (font-family-list) #'string<))))

(cl-defun fc-config-font ()
  "Allow user config FONT."
  (interactive)

  (let ((family (fc-select-font-family))
        (height (string-to-number
                 (read-string "Height"
                              (fc-string *fc-font-height*))))
        (weight (intern (fc-select
                            "Weight"
                            '("ultra-light"
                              "extra-light"
                              "light"
                              "semi-light"
                              "normal"
                              "semi-bold"
                              "bold"
                              "extra-bold"
                              "ultra-bold"))))
        (width (intern (fc-select
                           "Width"
                           '("extra-condensed"
                             "condensed"
                             "semi-condensed"
                             "normal"
                             "semi-expanded"
                             "expanded"
                             "extra-expanded"
                             "ultra-expanded")))))
    (when (> 96 height)
      (message "Font height too small.")
      (cl-return-from fc-config-font))
    (when (fc-void-p family)
      (message "Font dose not exists.")
      (cl-return-from fc-config-font))

    (setf *fc-default-font* (list :family family)
          *fc-font-height* height
          *fc-font-width* width
          *fc-font-weight-of-default* weight)
    (fc-setup-font)))

(cl-defun fc-nerd-icon (rune &rest rest)
  (fc-text (char-to-string rune)
           :face `(:family "Symbols Nerd Font Mono" ,@rest)))

;; configration
(setf font-lock-maximum-decoration t
      text-scale-mode-step 1.1
      inhibit-compacting-font-caches t)

(provide 'fc-font)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-font.el ends here
