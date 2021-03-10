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
                 flycheck-standard-error-navigation nil)
           (setq-default flycheck-emacs-lisp-load-path 'inherit)

           (add-hook '*fc-enable-dev-hook*
                     #'global-flycheck-mode)
           (add-hook '*fc-disable-dev-hook*
                     #'(lambda ()
                         (global-flycheck-mode -1)))))

(provide 'fc-flycheck)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-flycheck.el ends here
