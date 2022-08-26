;;; fc-patch.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun replace-regexp-in-string-test-empty (args)
  (when (null (nth 2 args))
    (setf (nth 2 args) ""))
  args)

(advice-add 'replace-regexp-in-string :filter-args
            #'replace-regexp-in-string-test-empty)

(provide 'fc-patch)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-patch.el ends here
