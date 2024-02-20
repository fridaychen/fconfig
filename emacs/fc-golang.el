;;; fc-golang.el --- setup golang environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(fc-load 'go-playground)
(fc-install 'flycheck-golangci-lint)

(defconst *fc-golang-map*
  (fc-make-keymap
   `(("a i" go-import-add)
     ("g i" go-goto-imports)
     ("r" ,(fc-manual (unless go-playground-mode
                        (go-playground-mode))
                      (go-playground-exec)))
     )
   "fc-go-map"
   *fc-func-mode-map*)
  "KEYS a i: add import  g i: goto imports  r: run.")

(defun fc--go-mode-func ()
  "Mode func."
  (fc-modal-head-key "Golang" '*fc-golang-map*))

(fc-load 'go-mode
  :after (progn
           (fc-add-mode-name 'go-mode "🎱")
           (fc-add-mode-name 'go-ts-mode "🎱")

           (cl-defun fc--go-setup ()
             (fc--lsp-enable))

           (add-hook 'go-mode-hook #'fc--go-setup)

           (fc-add-fmt 'go-mode nil #'gofmt)
           (fc-add-fmt 'go-ts-mode nil #'gofmt)))

(provide 'fc-golang)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-golang.el ends here
