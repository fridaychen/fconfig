;;; fc-ttl-nocomma.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'smie)

(setq smie-ttl-no-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2
        '((insts (insts "\n" insts) (inst))
          (inst ("if" inst "then" insts "endif")))
        '((assoc "\n")))))

(defun smie-ttl-no-rules (kind token)
  (message "##%s####%s##(%s)##" kind token (type-of token))

  (pcase (list kind token)
    ('(:after "endif")
     (smie-rule-parent))
    ('(:elem arg) 1)))

(define-derived-mode ttl-nocomma-mode prog-mode "TTL(no comma)"
  "Major mode for TeraTerm macro files."
  :syntax-table nil
  (modify-syntax-entry ?\n ".")

  (setq-local comment-start "#"
              comment-end "")

  (setq smie-blink-matching-triggers '(?\s))

  (smie-setup smie-ttl-no-grammar #'smie-ttl-no-rules))

(provide 'fc-ttl-nocomma)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
