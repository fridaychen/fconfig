;;; fc-tag-base.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defclass fc-tag ()
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag) id)
  (message "find-definitions is not implemented"))

(cl-defmethod fc-tag--find-apropos ((x fc-tag) pattern)
  (message "find apropos is not implemented"))

(cl-defmethod fc-tag--find-references ((x fc-tag) id)
  (message "find references is not implemented"))

(cl-defmethod fc-tag--open-file ((x fc-tag))
  )

(cl-defmethod fc-tag--open-project ((x fc-tag) proj-dir src-dirs)
  (message "open project is not implemented"))

(cl-defmethod fc-tag--list ((x fc-tag))
  (message "list tags is not implemented"))

(cl-defmethod fc-tag--describe ((x fc-tag))
  (message "describe is not implemented"))

(cl-defmethod fc-tag--info ((x fc-tag))
  (message "info is not implemented"))

(provide 'fc-tag-base)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-base.el ends here
