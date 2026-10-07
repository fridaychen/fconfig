;;; fc-welcome.el --- welcome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setf inhibit-startup-message t)

(text-mode)
(setq-local line-spacing 0)
(text-scale-set -5)

(insert-file (expand-file-name "welcome.txt" user-emacs-directory))

(provide 'fc-welcome)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-welcome.el ends here
