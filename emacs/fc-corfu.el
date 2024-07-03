;;; fc-corfu.el --- Corfu -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load 'corfu
  :after (progn
           (setf corfu-auto t
                 corfu-auto-delay 0
                 corfu-auto-prefix 1
                 corfu-cycle t
                 corfu-on-exact-match nil
                 tab-always-indent 'complete
                 )

           (global-corfu-mode 1))
  :bind '((corfu-map
           ("C-j" corfu-next)
           ("C-k" corfu-previous)
           ("M-i" corfu-previous)
           ("M-k" corfu-next)
           ("M-j" corfu-scroll-down)
           ("M-l" corfu-scroll-up)
           ("TAB" corfu-insert))))

(provide 'fc-corfu)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-corfu.el ends here
