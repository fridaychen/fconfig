;;; smie-sample.el --- TeraTerm macro major mode -*- lexical-binding: t -*-
(require 'smie)

;; We are using SMIE's default lexer.
;; A token is:
;; 1. any sequence of characters that have word or symbol syntax
;; 2. any sequence of characters that have punctuation syntax

(defvar smie-sample-grammar nil
  "Sample BNF grammar for `smie'.")

(setq smie-sample-grammar
      (smie-prec2->grammar
       (smie-bnf->prec2
        `((insts (insts ";" insts) (inst))
          (inst ("AttributeBegin" insts "AttributeEnd")))
        '((assoc ";")))))

(defun smie-sample-rules (kind token)
  "Perform indentation of KIND on TOKEN using the `smie' engine."
  (message "==%s--%s==" kind token)
  (pcase (list kind token)
    ('(:after "AttributeEnd")
     (smie-rule-parent))
    ;; ('(:elem arg) 1)
    ))

(define-derived-mode smie-sample-mode prog-mode "ExSMIE"
  "Usage example for the Simple Minded Indentation Engine."
  :syntax-table nil
  (modify-syntax-entry ?\# "<")
  (modify-syntax-entry ?\n ">")
  (setq comment-start "#"
        comment-end "")
  (smie-setup smie-sample-grammar #'smie-sample-rules)
  (font-lock-add-keywords nil '(("AttributeBegin" . font-lock-keyword-face)
                                ("AttributeEnd" . font-lock-keyword-face)))
  (font-lock-mode)
  (font-lock-ensure (point-min) (point-max)))

(provide 'smie-sample)
