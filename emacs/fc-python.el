;;; fc-python.el --- setup python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'python
  :after (progn
           (fc-add-mode-name 'python-ts-mode "🐍")

           (setf flycheck-python-flake8-executable
                 (expand-file-name "~/.emacs.d/site/python/bin/pflake8"))

           (fc-add-fmt 'python-ts-mode
                       '("fc-fmt-python.sh")
                       nil)

           (add-hook 'python-ts-mode-hook #'highlight-indent-guides-mode)))

(defconst *fc-python-map*
  (fc-make-keymap
   `(("E" org-edit-src-exit)
     )
   "fc-python-map"
   *fc-func-mode-map*)
  "KEYS E: org edit exit  F: format.")

(cl-defun fc--python-ts-mode-func ()
  (fc-modal-head-key "Python" '*fc-python-map*))

(provide 'fc-python)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-python.el ends here
