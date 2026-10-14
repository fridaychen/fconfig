;;; fc-tag-eglot.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load 'eglot
  :local t
  :enable *fc-lsp-enable*
  :after (progn
           (message "Enabled eglot")

           (setf *fc-lsp-enable* nil
                 *fc-lsp-eglot-enable* t
                 eglot-max-file-watches nil)

           (fc-add-mode-name 'eglot-hierarchy-mode "🇪")

           (setf eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                     :documentRangeFormattingProvider
                                                     :documentOnTypeFormattingProvider
                                                     :inlayHintProvider))

           (fc-set-face 'eglot-semantic-function nil
                        :inherit 'font-lock-function-call-face
                        :extend t
                        :overline nil)
           (fc-set-face 'eglot-semantic-method nil
                        :inherit 'font-lock-function-call-face
                        :extend t
                        :overline nil)
           (fc-set-face 'eglot-semantic-operator nil
                        :inherit 'font-lock-operator-face
                        :extend t
                        :overline nil)
           (fc-set-face 'eglot-semantic-macro nil
                        :overline nil)
           (fc-set-face 'eglot-semantic-readonly nil
                        :overline nil)

           (fc-load 'eglot-booster
             :enable (executable-find "emacs-lsp-booster")
             :raw "https://github.com/jdtsmith/eglot-booster.git"
             :after (progn
                      (setf eglot-booster-io-only t)

                      (eglot-booster-mode 1)))

           (defun fc--setup-eglot ()
             (let ((buf (current-buffer)))
               (fc-delay
                 (with-current-buffer buf
                   (flymake-mode 1)))))

           (add-hook 'eglot-managed-mode-hook #'fc--setup-eglot)

           (defun fc--lsp-enable ()
             (eglot-ensure))))

(defclass fc-tag-eglot (fc-tag-xref)
  ())

(cl-defmethod fc-tag--open-project ((x fc-tag-eglot) proj-dir src-dirs)
  "Open project.
X: tag object.
PROJ-DIR: project root dir.
SRC-DIRS: others source code dirs."
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-eglot))
  "Open file.
X: tag object."
  (when (derived-mode-p 'prog-mode)
    (eglot-ensure)))

(cl-defmethod fc-tag--describe-at-point ((x fc-tag-eglot))
  "Open describe object at point.
X: tag object."
  (fc-funcall #'eldoc-box-help-at-point))

(cl-defmethod fc-tag--info ((x fc-tag-eglot))
  "Open show tag info.
X: tag object."
  (eglot-describe-connection))

(cl-defmethod fc-tag--rename ((x fc-tag-eglot))
  "Rename object at point.
X: tag object."
  (fc-funcall #'eglot-rename))

(cl-defmethod fc-tag--call-hierarchy ((x fc-tag-eglot))
  "Show call hierarchy of function at point.
X: tag object."
  (fc-funcall #'eglot-show-call-hierarchy))

(defvar *fc-tag-eglot* (make-instance 'fc-tag-eglot))

(provide 'fc-tag-eglot)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-eglot.el ends here
