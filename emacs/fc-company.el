;;; fc-company.el --- setup company -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-company-delay* 0.2)
(defvar *fc-company-minimum-prefix-len* 3)

(fc-load 'company
  :after (progn
           (setf company-backends '(company-elisp company-ispell)
                 company-idle-delay *fc-company-delay*
                 company-minimum-prefix-length *fc-company-minimum-prefix-len*
                 company-show-numbers t
                 company-auto-complete t)

           (defun fc--company-enable ()
             (global-company-mode))

           (defun fc--company-disable ()
             (global-company-mode -1)))

  :bind '((nil
           ("M-/" company-complete))
          (company-active-map
           ("C-h" company-abort)
           ("M-i" company-select-previous)
           ("M-k" company-select-next)
           ("C-j" company-select-next)
           ("C-k" company-select-previous)
           ("S-<SPC>" company-select-previous)
           ("C-<SPC>" company-select-next)
           ("M-/" company-select-next))))

(fc-load 'company-c-headers
  :after (add-to-list 'company-backends 'company-c-headers))

(fc-load 'company-quickhelp
  :idle t
  :after (company-quickhelp-mode 1))

(fc-load 'company-posframe
  :enable (<= 26 emacs-major-version)
  :idle t
  :after (company-posframe-mode 1))

(provide 'fc-company)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-company.el ends here
