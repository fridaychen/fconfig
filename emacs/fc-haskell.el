;;; fc-haskell.el --- Haskell env -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-haskell-map*
  (fc-make-keymap
   `(
     )
   "fc-haskell-map"
   *fc-func-mode-map*)
  "E: org edit exit.")

(defun fc-haskell-mode-func ()
  (fc-modal-head-key "Haskell" '*fc-haskell-map*))

(fc-load 'haskell-mode
  :after (progn
           (add-hook 'haskell-mode-hook #'haskell-doc-mode)
           ))

(fc-install 'lsp-haskell)

(provide 'fc-haskell)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-haskell.el ends here
