;;; fc-company.el --- setup company -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'company
  :after (progn
           (setf company-backends '(company-elisp company-ispell)
                 company-idle-delay 0.8
                 company-minimum-prefix-length 3
                 company-auto-complete t)

           (defun fc--company-enable-dev ()
             (global-company-mode)
             (setf company-idle-delay 0.8))

           (defun fc--company-disable-dev ()
             (global-company-mode -1)
             (setf company-idle-delay 600))

           (add-hook '*fc-enable-dev-hook* #'fc--company-enable-dev)
           (add-hook '*fc-disable-dev-hook* #'fc--company-disable-dev))
  :bind '((nil
           ("M-/" company-complete))
          (company-active-map
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
