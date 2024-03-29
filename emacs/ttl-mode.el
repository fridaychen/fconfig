;;; ttl-mode.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'smie)

(setq *fc-ttl-highlights*
      `(
        (";.+" . font-lock-comment-face)
        ("^:.+" . font-lock-function-name-face)
        (,(regexp-opt '("if" "then" "else" "endif"
                        "while" "endwhile"
                        "for" "next"
                        "intdim" "strdim"
                        "include"
                        "call" "return" "exit")
                      'words)
         . font-lock-keyword-face)
        ))

(defun ttl-indent-line ()
  4)

(define-derived-mode ttl-mode prog-mode "TTL"
  "Major mode for ttl files."
  (setq-local comment-start ";"
              tab-width 4
              indent-tabs-mode nil
              indent-line-function #'ttl-indent-line)
  (setq font-lock-defaults '(*fc-ttl-highlights*))
  )

(add-to-list 'auto-mode-alist '("\\.ttl$" . ttl-mode))

(provide 'ttl-mode)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
