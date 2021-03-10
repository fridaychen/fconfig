;;; fc-lisp.el --- lisp utility -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'slime
  :autoload t
  :after (setf inferior-lisp-program
               (fc-file-first-exists
                '("/usr/bin/sbcl"
                  "/usr/local/bin/sbcl"
                  "/usr/bin/clisp"
                  "/usr/local/bin/clisp"))))

(defun fc-lisp-find-tag ()
  "Find tag."
  (interactive)

  (xrefc-push-marker-stack)
  (call-interactively 'find-function-at-point))

(provide 'fc-lisp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-lisp.el ends here
