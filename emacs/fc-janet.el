;;; fc-janet.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load 'janet-ts-mode
  :raw "https://github.com/sogaiu/janet-ts-mode")

(fc-load 'flymake-janet
  :raw "https://github.com/torusjkl/flymake-janet")

(provide 'fc-janet)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-janet.el ends here
