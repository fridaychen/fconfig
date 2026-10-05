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
                 flymake-no-changes-timeout 3)

           (fc-add-next-error-mode 'flymake-diagnostics-buffer-mode
                                   #'flymake-goto-next-error
                                   #'flymake-goto-prev-error)))

(cl-defun fc-diag-enable ()
  (fc-with-each-buffer
   :buffers (fc-list-buffer :mode '(prog-mode))
   (if (bound-and-true-p eglot--managed-mode)
       (flymake-mode 1)
     (flycheck-mode 1))))

(cl-defun fc-diag-disable ()
  (fc-with-each-buffer
   :buffers (fc-list-buffer :mode '(prog-mode))
   (flycheck-mode -1)
   (flymake-mode -1)))

(cl-defun fc-diag-show ()
  (interactive)

  (cond
   (flymake-mode
    (fc-flymake))

   (flycheck-mode
    (fc-flycheck))

   (t
    ("No diadnostic method."))))

(provide 'fc-diag)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-diag.el ends here
