;;; fc-lsp.el --- control lsp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-lsp-mode* t)

(fc-load 'lsp-bridge
  :local t
  :after (progn
           (global-lsp-bridge-mode)
           (setf *fc-lsp-mode* nil)))

;; (fc-load 'eglot
;;   :local t
;;   :after (progn
;;            (setf *fc-lsp-mode* nil)))

(fc-load 'lsp-mode
  :enable *fc-lsp-mode*
  :after (progn
           (require 'lsp)
           (require 'lsp-mode)

           (setq lsp-headerline-breadcrumb-enable nil
                 lsp-progress-via-spinner nil
                 lsp-enable-on-type-formatting nil)))

(fc-load 'lsp-ui
  :after (progn
           (setf lsp-ui-sideline-global t
                 lsp-ui-sideline-delay 2
                 lsp-ui-doc-enable t
                 lsp-ui-doc-show-with-cursor nil
                 lsp-ui-doc-show-with-mouse t
                 lsp-ui-doc-alignment 'window
                 lsp-ui-doc-delay 0.1)))

(provide 'fc-lsp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-lsp.el ends here
