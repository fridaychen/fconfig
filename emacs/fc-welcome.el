;;; fc-welcome.el --- welcome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setf inhibit-startup-message t)

(org-mode)

(insert (emacs-init-time))

(provide 'fc-welcome)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-welcome.el ends here
