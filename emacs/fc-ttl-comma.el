;;; fc-ttl-comma.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'smie)

(setq smie-ttl-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2
        '((insts (insts ";" insts) (inst))
          (inst ("if" inst "then" insts "endif")
                ("while" inst "then" insts "endwhile")
                ))
        '((assoc ";")))))

(defun smie-ttl-rules (kind token)
  (message "##%s####%s##(%s)##" kind token (type-of token))

  (pcase (list kind token)
    ('(:after "endif")
     (smie-rule-parent))
    ('(:after "endwhile")
     (smie-rule-parent))
    ('(:elem arg) 1)
    ))

(define-derived-mode ttl-comma-mode prog-mode "TTL(comma)"
  "Major mode for TeraTerm macro files."
  :syntax-table nil
  (modify-syntax-entry ?\# "<")
  (modify-syntax-entry ?\n ">")

  (setq-local comment-start "#"
              comment-end "")

  (font-lock-add-keywords nil `(
                                (,(regexp-opt '("if" "then" "else" "endif"
                                                "do" "while" "endwhile"
                                                "for" "next"
                                                "include"
                                                "call" "return" "exit"))
                                 . font-lock-keyword-face)
                                ("^:.+" . font-lock-function-name-face)))

  (smie-setup smie-ttl-grammar #'smie-ttl-rules))

(provide 'fc-ttl-comma)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
