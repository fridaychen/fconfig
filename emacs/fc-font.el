;;; fc-font.el --- setup font -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-font-height* 180)
(defvar *fc-default-font* "Monaco")
(defvar *fc-read-font* "Monaco")
(defvar *fc-font* '(('iso-8859-1 . "Monaco")))
(defvar *fc-font-weight-of-default* 'regular)
(defvar *fc-use-another-font-for-mode-line* nil)

(defun fc-setup-font ()
  "Setup font."
  (apply #'set-face-attribute 'default 'nil
         :height *fc-font-height*
         :weight *fc-font-weight-of-default*
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

  (let ((font (frame-parameter nil 'font)))
    (-map (lambda (n)
            (dolist (charset (car n))
              (set-fontset-font
               font
               charset
               (apply #'font-spec
                      (cdr n)))))

          *fc-font*))

  (when (and *fc-use-another-font-for-mode-line*
             (fontset-info "fontset-fc"))
    (set-face-attribute 'mode-line nil
                        :fontset "fontset-fc"))

  (set-face-attribute 'mode-line nil
                      :height (+ *fc-font-height*
                                 *fc-font-mode-line-delta*)))

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
  (fc-user-select "Select font family : "
                  (font-family-list)))

;; configration
(setf font-lock-maximum-decoration t
      text-scale-mode-step 1.1
      inhibit-compacting-font-caches t)

;;(global-font-lock-mode)

(provide 'fc-font)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-font.el ends here
