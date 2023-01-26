;;; fc-tag.el --- source tagging interface -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc--tag-map* (make-hash-table))

(cl-defun fc-find-definitions (&key apropos)
  (interactive)

  (when (not apropos)
    (let* ((sym (fc-current-thing :ask nil)))
      (when sym
        (fc-tag-find-definitions sym)
        (cl-return-from fc-find-definitions))))

  (fc-tag-find-apropos (fc-current-thing :confirm t)))

(cl-defun fc-find-references ()
  (interactive)

  (let* ((sym (fc-current-thing)))
    (when sym
      (fc-tag-find-references sym))))

;; base class
(defclass fc-tag ()
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag) id)
  (message "find-definitions is not implemented"))

(cl-defmethod fc-tag--find-apropos ((x fc-tag) pattern)
  (message "find apropos is not implemented"))

(cl-defmethod fc-tag--find-references ((x fc-tag) id)
  (message "find references is not implemented"))

(cl-defmethod fc-tag--open-file ((x fc-tag))
  (message "open file is not implemented"))

(cl-defmethod fc-tag--open-project ((x fc-tag) proj-dir src-dirs)
  (message "open project is not implemented"))

(cl-defmethod fc-tag--list ((x fc-tag))
  (message "list tags is not implemented"))

;; xref tag
(defclass fc-tag-xref (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-xref) id)
  (xref--find-definitions id nil))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-xref) pattern)
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-xref) id)
  (xref--find-xrefs id 'references id nil))

(cl-defmethod fc-tag--open-project ((x fc-tag-xref) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-xref))
  )

(cl-defmethod fc-tag--list ((x fc-tag-xref))
  (fc-funcall #'xref-find-definitions))

(defvar *fc-tag-xref* (make-instance 'fc-tag-xref))

;; global tag
(defclass fc-tag-global (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-global) id)
  (ggtags-find-tag-dwim id))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-global) pattern)
  (ggtags-find-tag-dwim pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-global) id)
  (ggtags-find-reference id))

(cl-defmethod fc-tag--open-project ((x fc-tag-global) proj-dir src-dirs)
  (setenv "GTAGSLIBPATH"
          (s-join ":"
                  (-map #'expand-file-name
                        src-dirs)))
  (ggtags-visit-project-root proj-dir))

(cl-defmethod fc-tag--open-file ((x fc-tag-global))
  (let ((compile-json-file (fc-exists-file-in-path "compile_commands.json"))
        (gtags-file (fc-exists-file-in-path "GTAGS")))
    (cond
     (compile-json-file
      (fc--lsp-enable)
      (add-to-list 'company-backends 'company-capf))

     (gtags-file
      (ggtags-mode t))

     (gtags-file
      (add-to-list 'company-backends 'company-gtags)))))

(cl-defmethod fc-tag--list ((x fc-tag-global))
  (fc-funcall #'counsel-gtags-find-symbol))

(defvar *fc-tag-global* (make-instance 'fc-tag-global))

;; lsp tag
(defclass fc-tag-lsp (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-lsp) id)
  (setq id (propertize id 'identifier-at-point t))
  (fc--lsp-find-definitions))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-lsp) pattern)
  (fc--lsp-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-lsp) id)
  (setq id (propertize id 'identifier-at-point t))

  (fc--lsp-find-references))

(cl-defmethod fc-tag--open-project ((x fc-tag-lsp) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-lsp))
  (if (derived-mode-p 'prog-mode)
      (fc--lsp-enable)
    (add-to-list 'company-backends 'company-capf)))

(cl-defmethod fc-tag--list ((x fc-tag-lsp))
  (fc--lsp-list-tag))

(defvar *fc-tag-lsp* (make-instance 'fc-tag-lsp))

(cl-defun fc-auto-select-tag ()
  (let* ((root (fc-proj-root)))
    (cond
     ((not root)
      *fc-tag-global*)

     ((file-exists-p (format "%s/compile_commands.json" root))
      *fc-tag-lsp*)

     ((file-exists-p (format "%s/GTAGS" root))
      *fc-tag-global*)

     (t
      *fc-tag-global*))))

(cl-defun fc-find-tag ()
  (let ((instance (gethash major-mode *fc--tag-map*)))
    (when instance
      (cl-return-from fc-find-tag instance)))

  (cond
   ((not (boundp 'fc-proj-tag))
    *fc-tag-global*)

   ((eq fc-proj-tag 'global)
    *fc-tag-global*)

   ((eq fc-proj-tag 'xref)
    *fc-tag-xref*)

   ((member fc-proj-tag '(lsp cquery ccls))
    *fc-tag-lsp*)

   (t (fc-auto-select-tag))))

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

(defun fc-tag-list ()
  (fc-tag--list (fc-find-tag)))

(cl-defun fc-add-tag (mode tag-instance)
  (puthash mode tag-instance *fc--tag-map*))

(fc-add-to-hook 'after-change-major-mode-hook
                #'(lambda ()
                    (when buffer-file-name
                      (fc-tag-open-file)))
                #'(lambda ()
                    (unless (or
                             (eq major-mode 'minibuffer-mode)
                             (eq major-mode 'minibuffer-inactive-mode))
                      (hack-local-variables))))

(puthash 'emacs-lisp-mode *fc-tag-xref* *fc--tag-map*)

(provide 'fc-tag)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag.el ends here
