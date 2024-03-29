;;; fc-ttl.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setq *fc-ttl-highlights*
      `(
        (,(regexp-opt '("if" "then" "else" "endif"
                        "while" "endwhile"
                        "include"
                        "call" "return" "exit"))
         . font-lock-keyword-face)
        (";.+" . font-lock-comment-face)
        ("^:.+" . font-lock-function-name-face)))

(define-derived-mode ttl-mode prog-mode "TeraTerm macro(ttl)"
  "Major mode for ttl files."
  (setq-local comment-start ";")
  (setq-local indent-tabs-mode nil)
  (setq font-lock-defaults '(*fc-ttl-highlights*))
  )

(add-to-list 'auto-mode-alist '("\\.ttl$" . ttl-mode))

(provide 'fc-ttl)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
