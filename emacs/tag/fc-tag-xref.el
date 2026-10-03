;;; fc-tag-xref.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defclass fc-tag-xref (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-xref) id)
  (xref-find-definitions id))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-xref) pattern)
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-xref) id)
  (xref-find-references id))

(cl-defmethod fc-tag--list ((x fc-tag-xref))
  (fc-funcall #'xref-find-definitions))

(defvar *fc-tag-xref* (make-instance 'fc-tag-xref))

(provide 'fc-tag-xref)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-xref.el ends here
