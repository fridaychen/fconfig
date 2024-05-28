;;; fc-ttl.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defface *fc-ttl-function-name-face*
  '((t :inherit font-lock-function-name-face
       :weight bold
       :underline t
       ))
  "TTL function name face.")

(defvar *fc-ttl-function-name-face* '*fc-ttl-function-name-face*)

(defconst *ttl-imenu-generic-expression*
  (list (list "Subroutine" "^:\\([^_][^ \t\n]+\\)$" 1)))

(cl-defun --ttl-find-previous-statement ()
  (beginning-of-line)

  (while (> (point) 1)
    (forward-line -1)

    (cond
     ((looking-at " *$")
      )

     ((looking-at "^:")
      (cl-return-from --ttl-find-previous-statement 4))

     ((looking-at " *\\(if.+then$\\|else\\|for\\|while\\|until\\)")
      (beginning-of-line-text)
      (cl-return-from --ttl-find-previous-statement 4))

     ((looking-at " *;")
      )

     (t
      (cl-return-from --ttl-find-previous-statement 0))))

  0)

(cl-defun -ttl-find-regex-table (table &optional user-regex)
  (beginning-of-line)

  (let ((regex (or user-regex
                   (concat " *" (funcall #'regexp-opt
                                         (cl-loop for x in table collect (car x)))))))
    (while (> (point) 1)
      (forward-line -1)

      (when (looking-at regex)
        (beginning-of-line-text)

        (--each table
          (when (looking-at (car it))
            (cl-return-from -ttl-find-regex-table (cdr it))))))))

(cl-defun fc-ttl-indent-line ()
  (interactive)

  (beginning-of-line)
  (when (looking-at " *;;")
    (indent-line-to 0)
    (cl-return-from fc-ttl-indent-line))

  (beginning-of-line-text)

  (when (looking-at ":")
    (indent-line-to 0)
    (cl-return-from fc-ttl-indent-line))

  (let ((indent 0)
        (pos (point))
        (last (cond
               ((looking-at "\\(endif\\|else\\)")
                (-ttl-find-regex-table '(("if.+then$" . 0)
                                         ("endif" . -4))
                                       ".*\\(if.+then$\\|endif\\)"))

               ((looking-at "endwhile")
                (-ttl-find-regex-table '(("while" . 0)
                                         ("endwhile" . -4))))

               ((looking-at "enduntil")
                (-ttl-find-regex-table '(("until" . 0)
                                         ("enduntil" . -4))))

               ((looking-at "next")
                (-ttl-find-regex-table '(("for" . 0)
                                         ("next" . -4))))

               (t
                (--ttl-find-previous-statement)))))

    (when (null last)
      (goto-char pos)
      (indent-line-to 0)
      (cl-return-from fc-ttl-indent-line))

    (unless (looking-at " *;")
      (beginning-of-line-text))
    (setq indent (current-column))

    (goto-char pos)
    (indent-line-to (+ indent last))))

(define-derived-mode fc-ttl-mode prog-mode "TTL"
  "Major mode for TeraTerm macro files."
  :syntax-table nil
  (modify-syntax-entry ?\; "<")
  (modify-syntax-entry ?\n ">")

  (setq-local comment-start ";"
              comment-end ""
              imenu-generic-expression *ttl-imenu-generic-expression*)

  (setf indent-tabs-mode nil
        tab-width 4)

  (font-lock-add-keywords nil
                          `(
                            (,(regexp-opt '("if" "then" "else" "elseif" "endif"
                                            "do" "loop"
                                            "while" "endwhile"
                                            "until" "enduntil"
                                            "for" "next"
                                            "break" "continue"
                                            "goto"
                                            "include")
                                          'words)
                             . font-lock-keyword-face)
                            (,(regexp-opt '("call" "return" "end" "exit") 'words)
                             . font-lock-function-name-face)
                            ("^:[^_].+" . ,*fc-ttl-function-name-face*)
                            ("^:[_].+" . font-lock-function-name-face)))

  (setq-local indent-line-function #'fc-ttl-indent-line))

(fc-add-fmt 'fc-ttl-mode nil #'fc--default-fmt-with-indent)

(provide 'fc-ttl)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
