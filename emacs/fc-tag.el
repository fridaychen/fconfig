;;; fc-tag.el --- source tagging interface -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc-tag* (make-hash-table))

;; base class
(defclass fc-tag ()
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag) id)
  (message "find-definitions is not implemented"))

(cl-defmethod fc-tag--find-apropos ((x fc-tag) pattern)
  (message "find aprops is not implemented"))

(cl-defmethod fc-tag--find-references ((x fc-tag) id)
  (message "find references is not implemented"))

(cl-defmethod fc-tag--open-file ((x fc-tag))
  (message "open file is not implemented"))

(cl-defmethod fc-tag--open-project ((x fc-tag) proj-dir src-dirs)
  (message "open project is not implemented"))

;; xref tag
(defclass fc-tag-xref (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-xref) id)
  (xref--find-definitions id nil))

(cl-defmethod fc-tag--find-aproposs ((x fc-tag-xref) pattern)
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-xref) id)
  (xref--find-xrefs id 'references id nil))

(cl-defmethod fc-tag--open-project ((x fc-tag-xref) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-xref))
  )

(defvar *fc-tag-xref* (make-instance 'fc-tag-xref))

;; global tag
(defclass fc-tag-basic (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-basic) id)
  (ggtags-find-tag-dwim id))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-basic) pattern)
  (ggtags-find-tag-dwim pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-basic) id)
  (ggtags-find-reference id))

(cl-defmethod fc-tag--open-project ((x fc-tag-basic) proj-dir src-dirs)
  (setenv "GTAGSLIBPATH"
          (s-join ":"
                  (-map #'expand-file-name
                        src-dirs)))
  (ggtags-visit-project-root proj-dir))

(cl-defmethod fc-tag--open-file ((x fc-tag-basic))
  (let ((compile-json-file (fc-exists-file-in-path "compile_commands.json"))
        (gtags-file (fc-exists-file-in-path "compile_commands.json")))
    (when gtags-file
      (ggtags-mode t))

    (cond
     (compile-json-file
      (lsp t)
      (add-to-list 'company-backends 'company-capf))

     (gtags-file
      (add-to-list 'company-backends 'company-gtags)))))

(defvar *fc-tag-basic* (make-instance 'fc-tag-basic))

;; cquery tag
(defclass fc-tag-cquery (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-cquery) id)
  (xref--find-definitions id nil))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-cquery) pattern)
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-cquery) id)
  (xref--find-xrefs id 'references id nil))

(cl-defmethod fc-tag--open-project ((x fc-tag-cquery) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-cquery))
  (lsp)

  ;; (lsp-ui--toggle nil)
  ;; (lsp-ui-doc-mode -1)
  ;; (lsp-ui-sideline-mode -1)

  ;; (if (fboundp 'lsp-ui-sideline-enable)
  ;;     (lsp-ui-sideline-enable nil))

  ;; (add-to-list 'company-backends 'company-lsp))
  )

(defvar *fc-tag-cquery* (make-instance 'fc-tag-cquery))

;; lsp tag
(defclass fc-tag-lsp (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-lsp) id)
  (setq id (propertize id 'identifier-at-point t))
  (xref--find-definitions id nil))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-lsp) pattern)
  (setq pattern (propertize pattern 'identifier-at-point t))
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-lsp) id)
  (setq id (propertize id 'identifier-at-point t))
  (xref--find-xrefs id 'references id nil))

(cl-defmethod fc-tag--open-project ((x fc-tag-lsp) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-lsp))
  (if (member major-mode '(c-mode c++mode python-mode))
      (lsp)

    (add-to-list 'company-backends 'company-capf)))

(defvar *fc-tag-lsp* (make-instance 'fc-tag-lsp))

(cl-defun fc-find-tag ()
  (let ((instance (gethash major-mode *fc-tag*)))
    (when instance
      (cl-return-from fc-find-tag instance)))

  (cond
   ((not (boundp 'fc-proj-tag))
    *fc-tag-basic*)

   ((eq fc-proj-tag 'global)
    *fc-tag-basic*)

   ((eq fc-proj-tag 'xref)
    *fc-tag-xref*)

   ((member fc-proj-tag '(lsp cquery ccls))
    *fc-tag-lsp*)

   (t *fc-tag-basic*)))

(defun fc-tag-find-definitions (id)
  (fc-tag--find-definitions (fc-find-tag) id))

(defun fc-tag-find-apropos (id)
  (fc-tag--find-apropos (fc-find-tag) id))

(defun fc-tag-find-references (id)
  (fc-tag--find-references (fc-find-tag) id))

(defun fc-tag-open-project (proj-dir src-dirs)
  (fc-tag--open-project (fc-find-tag) proj-dir src-dirs))

(defun fc-tag-open-file ()
  (let ((tag (fc-find-tag)))
    (when tag
      (fc-tag--open-file tag))))

(cl-defun fc-add-tag (mode tag-instance)
  (puthash mode tag-instance *fc-tag*))

(fc-add-to-hook 'after-change-major-mode-hook
                #'fc-tag-open-file
                #'hack-local-variables
                )
(puthash 'emacs-lisp-mode *fc-tag-xref* *fc-tag*)

;; (add-hook 'text-mode-hook 'fc-tag-open-file)
;; (add-hook 'c-mode-hook 'fc-tag-open-file)
;; (add-hook 'c++-mode-hook 'fc-tag-open-file)

(provide 'fc-tag)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag.el ends here
