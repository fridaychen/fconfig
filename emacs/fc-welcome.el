;;; fc-welcome.el --- welcome -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setf inhibit-startup-message t)

(text-mode)

(insert (emacs-init-time))

(provide 'fc-welcome)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-music.el ends here
