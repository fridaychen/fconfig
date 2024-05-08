;;; fc-ttl.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(cl-defun -ttl-find-last ()
  (beginning-of-line)

  (while (> (point) 1)
    (forward-line -1)

    (cond
     ((looking-at " *$")
      )

     ((looking-at "^:")
      (cl-return-from -ttl-find-last 4))

     ((looking-at " *\\(if.+then$\\|else\\|for\\|while\\)")
      (beginning-of-line-text)
      (cl-return-from -ttl-find-last 4))

     ((looking-at " *;")
      )

     (t
      (cl-return-from -ttl-find-last 0))))

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
  (when (looking-at " *;")
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

               ((looking-at "next")
                (-ttl-find-regex-table '(("for" . 0)
                                         ("next" . -4))))

               (t
                (-ttl-find-last)))))

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
              comment-end "")

  (setf indent-tabs-mode nil
        tab-width 4)

  (font-lock-add-keywords nil `(
                                (,(regexp-opt '("if" "then" "else" "endif"
                                                "do" "while" "endwhile"
                                                "for" "next"
                                                "include"
                                                "call" "goto" "return" "exit"))
                                 . font-lock-keyword-face)
                                ("^:.+" . font-lock-function-name-face)))

  (setq-local indent-line-function #'fc-ttl-indent-line))

(add-to-list 'auto-mode-alist '("\\.ttl" . fc-ttl-mode))

(fc-add-fmt 'fc-ttl-mode nil #'fc--default-fmt-with-indent)

(provide 'fc-ttl)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
