;;; fc-lsp.el --- control lsp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'lsp-mode
  :autoload t
  :after (progn
           (require 'lsp)
           (require 'lsp-mode)

           (setq lsp-headerline-breadcrumb-enable nil
                 lsp-progress-via-spinner nil)))

;; (fc-load 'lsp-ui
;;      :after (progn
;;               (setf lsp-ui-sideline-global nil)))

(provide 'fc-lsp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-lsp.el ends here
