;;; fc-python.el --- setup python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'python-mode
  :after (progn
           (fc-add-mode-name 'python-mode "🐍")
           (fc-add-mode-name 'python-ts-mode "🐍")
           (setf py-comment-fill-column 88
                 py-docstring-fill-column 88)

           (setf flycheck-python-flake8-executable
                 (expand-file-name "~/.emacs.d/site/python/bin/pflake8"))

           (require 'python)
           (fc-add-fmt 'python-mode
                       '("fc-fmt-python.sh")
                       nil)
           (fc-add-fmt 'python-ts-mode
                       '("fc-fmt-python.sh")
                       nil)

           (add-hook 'python-mode-hook #'highlight-indent-guides-mode)
           (add-hook 'python-ts-mode-hook #'highlight-indent-guides-mode)))

(defconst *fc-python-map*
  (fc-make-keymap
   `(("E" org-edit-src-exit)
     )
   "fc-python-map"
   *fc-func-mode-map*)
  "KEYS E: org edit exit  F: format.")

(cl-defun fc--python-mode-func ()
  (fc-modal-head-key "Python" '*fc-python-map*))

(provide 'fc-python)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-python.el ends here
