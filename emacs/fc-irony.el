;;; fc-irony.el --- control irony -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'irony
  :after (progn
           (add-hook 'c++-mode-hook #'irony-mode)
           (add-hook 'c-mode-hook #'irony-mode)
           ;; (add-hook 'objc-mode-hook 'irony-mode)

           (when *is-cygwin*
             ;; Windows performance tweaks
             ;;
             (when (boundp 'w32-pipe-read-delay)
               (setq w32-pipe-read-delay 0))
             ;; Set the buffer size to 64K on Windows (from the original 4K)
             (when (boundp 'w32-pipe-buffer-size)
               (setq irony-server-w32-pipe-buffer-size (* 64 1024))))))

(fc-load 'company-irony
  :after (progn
           (add-to-list 'company-backends 'company-irony t)))

(fc-load 'flycheck-irony
  :after (progn
           (add-hook 'flycheck-mode-hook
                     #'flycheck-irony-setup)))

(setf company-backends '(company-irony
                         company-elisp))

flycheck-checker
(provide 'fc-irony)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-irony.el ends here
