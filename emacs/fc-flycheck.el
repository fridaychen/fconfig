;;; fc-flycheck.el --- setup flycheck -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'flycheck
  :idle t
  :after (progn
           (setf flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
                 flycheck-emacs-lisp-initialize-packages 'auto
                 flycheck-standard-error-navigation t)
           (setq-default flycheck-emacs-lisp-load-path 'inherit)

           (fc-add-next-error-mode 'flycheck-error-list-mode
                                   #'flycheck-next-error
                                   #'flycheck-previous-error)))

(provide 'fc-flycheck)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-flycheck.el ends here
