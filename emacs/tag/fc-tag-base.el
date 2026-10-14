;;; fc-tag-base.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defclass fc-tag ()
  ())

(cl-defmethod fc-tag--find-definitions ((x fc-tag) id)
  "Find the definition of the identifier at point.
X: tag object.
ID: identifier."
  (message "find-definitions is not implemented"))

(cl-defmethod fc-tag--find-apropos ((x fc-tag) pattern)
  "Find all meaningful symbols that match PATTERN.
X: tag object.
PATTERN: apropos pattern."
  (message "find apropos is not implemented"))

(cl-defmethod fc-tag--find-references ((x fc-tag) id)
  "Find references to the identifier at point.
X: tag object.
ID: identifier."
  (message "find references is not implemented"))

(cl-defmethod fc-tag--open-file ((x fc-tag))
  "Open file.
X: tag object."
  )

(cl-defmethod fc-tag--open-project ((x fc-tag) proj-dir src-dirs)
  "Open project.
X: tag object.
PROJ-DIR: project root dir.
SRC-DIRS: others source code dirs."
  (message "open project is not implemented"))

(cl-defmethod fc-tag--list ((x fc-tag))
  "List all tags.
X: tag objects."
  (message "list tags is not implemented"))

(cl-defmethod fc-tag--describe-at-point ((x fc-tag))
  "Open describe object at point.
X: tag object."
  (message "describe is not implemented"))

(cl-defmethod fc-tag--info ((x fc-tag))
  "Open show tag info.
X: tag object."
  (message "info is not implemented"))

(cl-defmethod fc-tag--rename ((x fc-tag))
  "Rename object at point.
X: tag object."
  (message "rename is not implemented"))

(cl-defmethod fc-tag--call-hierarchy ((x fc-tag))
  "Show call hierarchy of function at point.
X: tag object."
  (message "call-hierarchy is not implemented"))

(cl-defmethod fc-tag--update ((x fc-tag))
  "Update tag database.
X: tag object."
  (message "update is not implemented"))

(provide 'fc-tag-base)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-base.el ends here
