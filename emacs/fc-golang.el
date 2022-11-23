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
           (add-hook 'go-mode-hook #'lsp)

           (fc-add-fmt 'go-mode nil #'gofmt)))

;; tag
(defclass fc-tag-go (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-go) id)
  (fc-funcall #'godef-jump))

(cl-defmethod fc-tag--find-aproposs ((x fc-tag-go) pattern)
  )

(cl-defmethod fc-tag--find-references ((x fc-tag-go) id)
  )

(cl-defmethod fc-tag--open-project ((x fc-tag-go) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-go))
  )

(defvar *fc-tag-go* (make-instance 'fc-tag-go))

(fc-add-tag 'go-mode *fc-tag-go*)

(provide 'fc-golang)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-golang.el ends here
