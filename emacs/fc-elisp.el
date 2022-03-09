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
     ("x" transpose-sexps)
     ("SPC" fc-elisp-portal)
     )
   "fc-emacs-lisp-map"
   *fc-func-mode-map*)
  "KEYS e: eval  f: eval fun  l: load current file  x: transpose sexps  E: org edit exit  SPC: portal.")

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
    (fc-whole-buffer
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
             (indent-region (point-min) (point-max))

             (fc--default-fmt))

           (fc-unbind-keys '("C-M-i") emacs-lisp-mode-map)

           (fc-add-fmt 'emacs-lisp-mode nil #'fc-format-emacs-lisp)

           (defun fc--setup-elisp-mode ()
             (setf indent-tabs-mode nil)
             (add-hook 'write-contents-functions 'check-parens nil t))

           (add-hook 'emacs-lisp-mode-hook #'fc--setup-elisp-mode)
           (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)))

(when (eq major-mode 'emacs-lisp-mode)
  (fc--setup-elisp-mode))

(provide 'fc-elisp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-elisp.el ends here
