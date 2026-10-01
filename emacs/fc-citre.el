;;; fc-citre.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load 'citre
  :after (progn
           (require 'citre-config)

           (add-hook '*fc-ergo-restore-hook* #'citre-peek-abort)

           (setf citre-auto-enable-citre-mode-modes
                 '(c-ts-mode c-mode c++-mode c++-ts-mode)
                 citre-completion-use-cache t)))

(provide 'fc-citre)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-citre.el ends here
