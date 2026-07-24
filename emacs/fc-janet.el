;;; fc-janet.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load 'flymake-janet
  :raw "https://github.com/torusjkl/flymake-janet")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(janet-ts-mode . ("janet-lsp" "--stdio"))))

(provide 'fc-janet)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-janet.el ends here
