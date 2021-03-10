;;; fc-math.el --- mathmatic -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(fc-install 'matlab-mode 'rpn-calc)

(setf calc-language 'c)

(provide 'fc-math)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-math.el ends here
