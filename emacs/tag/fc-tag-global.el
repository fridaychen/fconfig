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
  "Find the definition of the identifier at point.
X: tag object.
ID: identifier."
  (xref--find-definitions id nil))

(cl-defmethod fc-tag--find-apropos ((x fc-tag-global) pattern)
  "Find all meaningful symbols that match PATTERN.
X: tag object.
PATTERN: apropos pattern."
  (xref-find-apropos pattern))

(cl-defmethod fc-tag--find-references ((x fc-tag-global) id)
  "Find references to the identifier at point.
X: tag object.
ID: identifier."
  (xref--find-xrefs id 'references id nil))

(cl-defmethod fc-tag--open-project ((x fc-tag-global) proj-dir src-dirs)
  "Open project.
X: tag object.
PROJ-DIR: project root dir.
SRC-DIRS: others source code dirs."
  )

(cl-defmethod fc-tag--open-file ((x fc-tag-global))
  "Open file.
X: tag object."
  (gtags-mode 1))

(cl-defmethod fc-tag--list ((x fc-tag-global))
  "List all tags.
X: tag objects."
  (fc-funcall #'xref-find-definitions))

(defvar *fc-tag-global* (make-instance 'fc-tag-global))

(provide 'fc-tag-global)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-global.el ends here
