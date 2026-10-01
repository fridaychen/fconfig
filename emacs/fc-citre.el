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
                 citre-completion-use-cache t)

           (fc-bind-keys `(("<mouse-4>" citre-peek-prev-line)
                           ("<mouse-5>" citre-peek-next-line)
                           ("<wheel-up>" citre-peek-prev-line)
                           ("<wheel-down>" citre-peek-next-line)
                           )
                         citre-peek-keymap)
           ))

(provide 'fc-citre)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-citre.el ends here
