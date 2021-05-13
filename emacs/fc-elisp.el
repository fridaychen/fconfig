;;; fc-elisp.el --- setup elisp environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-emacs-lisp-map*
  (fc-make-keymap
   `(
     ("e" eval-last-sexp)
     ("f" eval-defun)
     ("l" ,(fc-manual (when buffer-file-name
                        (load buffer-file-name))))
     ("SPC" fc-elisp-portal)
     )
   "fc-emacs-lisp-map"
   *fc-func-mode-map*)
  "KEYS e: eval  f: eval fun  l: load current file  E: org edit exit  SPC: portal.")

(defun fc-elisp-portal ()
  "Show elisp portal."
  (fc-user-select-func
   "Elisp"
   `(
     ("Add header and footer" . fc-add-elisp-header-footer)
     )))

(defun fc-add-elisp-header-footer ()
  "Add elisp header and footer."
  (let ((fname (file-name-nondirectory buffer-file-name)))
    (fc-buffer
      (goto-char (point-min))
      (insert ";;; " fname " --- DESCRIPTION -*- lexical-binding: t -*-\n\n"
              ";;; Commentary:\n"
              ";;\n\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert "\n(provide '"  (file-name-sans-extension fname) ")\n\n"
              ";; Local Variables:\n"
              ";; byte-compile-warnings: (not free-vars unresolved)\n"
              ";; End:\n\n"
              ";;; " fname " ends here\n"))))

(defun fc-emacs-lisp-mode-func ()
  "Mode func."
  (fc-modal-head-key "Elisp" '*fc-emacs-lisp-map*))

(fc-load 'adjust-parens
  :after (progn
           (fc-add-hook 'emacs-lisp-mode-hook
             (local-set-key (kbd "TAB")
                            (fc-manual (fc-tab-key
                                        #'lisp-indent-adjust-parens)))
             (local-set-key (kbd "<backtab>")
                            #'lisp-dedent-adjust-parens))))

(fc-load 'elisp-mode
  :local t

  :after (progn
           (cl-defun fc-format-emacs-lisp ()
             (whitespace-cleanup)
             (fc-remove-empty-line)
             (indent-region (point-min) (point-max)))

           (fc-add-fmt 'emacs-lisp-mode nil #'fc-format-emacs-lisp)

           (add-hook 'emacs-lisp-mode-hook (lambda ()
                                             (setf indent-tabs-mode nil)))
           (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
           (add-hook 'after-save-hook #'check-parens nil t)))

(provide 'fc-elisp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-elisp.el ends here
