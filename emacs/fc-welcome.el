;;; fc-welcome.el --- welcome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setf inhibit-startup-message t)

(text-mode)
(setq-local line-spacing 0)
(text-scale-set -5)

(insert-file "~/.emacs.d/welcome.txt")

(provide 'fc-welcome)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-welcome.el ends here
