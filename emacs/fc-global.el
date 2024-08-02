;;; fc-global.el --- setup ggtags (global) -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'ggtags
  :autoload t
  :before (progn
            (autoload #'ggtags-visit-project-root "ggtags"))

  :after (progn
           (setf ggtags-update-on-save (not *is-cygwin*)
                 ggtags-oversize-limit 1024
                 ggtags-mode-line-project-name nil
                 ggtags-use-sqlite3 nil)

           (fc-unbind-keys '("M-." "M-,") ggtags-mode-map)))

(defun fc-global-open-tags (proj-dir src-dirs)
  "Open projects tags.
PROJ-DIR: project path.
SRC-DIRS: source code directories."
  (setenv "GTAGSLIBPATH"
          (s-join ":"
                  (mapcar #'expand-file-name
                          src-dirs)))
  (ggtags-visit-project-root proj-dir))

(defun fc-global-update-tag ()
  "Update project tag."
  (interactive)

  (ggtags-create-tags (fc-proj-root)))

(provide 'fc-global)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-global.el ends here
