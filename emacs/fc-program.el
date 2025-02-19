;;; fc-program.el --- programs -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-program-path* (format "%s/emacs/programs/" *fc-home*))

(defun fc-program ()
  "Select program to run."
  (interactive)

  (when-let* ((prog (fc-select "Programs"
                        (mapcar
                         (lambda (x)
                           (cons (capitalize
                                  (replace-regexp-in-string
                                   "-"
                                   " "
                                   (file-name-sans-extension x)))
                                 x))
                         (directory-files *fc-program-path* nil "el$")))))
    (load-file (concat *fc-program-path* prog))))

(provide 'fc-program)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-program.el ends here
