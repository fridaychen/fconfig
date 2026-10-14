;;; fc-tag-xref.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defclass fc-tag-xref (fc-tag)
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag-xref) id)
  "Find the definition of the identifier at point.
X: tag object.
ID: identifier."
  (xref-find-definitions id))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-xref) pattern)
  "Find all meaningful symbols that match PATTERN.
X: tag object.
PATTERN: apropos pattern."
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-xref) id)
  "Find references to the identifier at point.
X: tag object.
ID: identifier."
  (xref-find-references id))

(cl-defmethod fc-tag--list ((x fc-tag-xref))
  "List all tags.
X: tag objects."
  (fc-funcall #'xref-find-definitions))

(defvar *fc-tag-xref* (make-instance 'fc-tag-xref))

(provide 'fc-tag-xref)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-xref.el ends here
