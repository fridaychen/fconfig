;;; fc-tag-global.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(fc-load 'gtags-mode
  :after (progn
           ))

(defclass fc-tag-global (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-global) id)
  (xref--find-definitions id nil))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-global) pattern)
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-global) id)
  (xref--find-xrefs id 'references id nil))

(cl-defmethod fc-tag--open-project ((x fc-tag-global) proj-dir src-dirs)
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-global))
  (gtags-mode 1))

(cl-defmethod fc-tag--list ((x fc-tag-global))
  (fc-funcall #'xref-find-definitions))

(defvar *fc-tag-global* (make-instance 'fc-tag-global))

(provide 'fc-tag-global)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-global.el ends here
