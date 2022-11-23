;;; fc-python.el --- setup python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'python-mode
  :after (progn
           (require 'python)
           (fc-add-fmt 'python-mode
                       `("black" "-l" ,(fc-string py-comment-fill-column) "-")
                       nil)))

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
