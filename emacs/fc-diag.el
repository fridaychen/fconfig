;;; fc-diag.el --- setup flycheck -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'flycheck
  :idle t
  :after (progn
           (setf flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
                 flycheck-emacs-lisp-initialize-packages 'auto
                 flycheck-global-modes '(not python-ts-mode)
                 flycheck-mode-line-prefix "🚧"
                 flycheck-standard-error-navigation t)
           (setq-default flycheck-emacs-lisp-load-path 'inherit)

           (fc-add-next-error-mode 'flycheck-error-list-mode
                                   #'flycheck-next-error
                                   #'flycheck-previous-error)))

(fc-load 'flymake
  :after (progn
           (defun flymake--diagnostics-buffer-name ()
             "*Flymake errors*")

           (setf flymake-start-on-flymake-mode t
                 flymake-inline-diagnostics nil
                 flymake-no-changes-timeout 3)

           (fc-add-next-error-mode 'flymake-diagnostics-buffer-mode
                                   #'flymake-goto-next-error
                                   #'flymake-goto-prev-error)))

(cl-defun fc-diag-enable ()
  "Enable diag."
  (fc-with-each-buffer
   :buffers (fc-list-buffer :mode '(prog-mode))
   (flycheck-mode 1))

  (setf flymake-inline-diagnostics 'fancy)

  (fc-each-visible-buffer '(:mode prog-mode)
    (revert-buffer t t))

  (force-window-update))

(cl-defun fc-diag-disable ()
  "Disable diag."
  (fc-with-each-buffer
   :buffers (fc-list-buffer :mode '(prog-mode))
   (flymake-mode -1))

  (setf flymake-inline-diagnostics nil)
  (force-window-update))

(cl-defun fc-diag-show ()
  "Show diag buffer."
  (interactive)

  (cond
   (flymake-mode
    (fc-flymake))

   (flycheck-mode
    (fc-flycheck))

   (t
    (message "No diagnostic method."))))

(provide 'fc-diag)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-diag.el ends here
