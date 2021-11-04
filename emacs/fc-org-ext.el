;;; fc-org-ext.el --- org-mode extension -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(org-link-set-parameters "music"
                         :follow #'fc--music-open)

(defun fc--music-open (path _)
  (fc-exec-command "quodlibet" "--query" path)
  (sit-for 1)
  (fc-exec-command "quodlibet" "--next"))

(provide 'fc-org-ext)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org-ext.el ends here
