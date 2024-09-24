;;; fc-ttl.el --- TeraTerm macro major mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defface *fc-ttl-function-name-face*
  '((t :inherit font-lock-function-name-face
       :weight bold
       :overline t
       ))
  "TTL function name face.")

(defvar *fc-ttl-function-name-face* '*fc-ttl-function-name-face*)

(defface *fc-ttl-function-exit-face*
  '((t :inherit font-lock-function-name-face
       :weight bold
       :overline nil
       ))
  "TTL function exit face.")

(defvar *fc-ttl-function-exit-face* '*fc-ttl-function-exit-face*)

(defconst *ttl-imenu-generic-expression*
  (list (list "Subroutine" "^:\\([^_][^ \t\n]+\\)$" 1)))

(defconst *fc-ttl-function-regex* "^:[^_].+")

(cl-defun --ttl-find-previous-statement ()
  "Find previous statement for calculating indentation."
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

        (fc-each table
          (when (looking-at (car it))
            (cl-return-from -ttl-find-regex-table (cdr it))))))))

(defun --indent-to (origin-pos col)
  (goto-char origin-pos)

  (if (and (zerop (current-column))
           (looking-at-p "$"))
      (indent-line-to col)
    (save-excursion
      (indent-line-to col))))

(cl-defun fc-ttl-indent-line ()
  "Indent current line."
  (interactive)

  (let ((origin-pos (point)))
    (beginning-of-line)
    (when (looking-at " *;;")
      (--indent-to origin-pos 0)
      (cl-return-from fc-ttl-indent-line))

    (beginning-of-line-text)

    (when (looking-at ":")
      (--indent-to origin-pos 0)
      (cl-return-from fc-ttl-indent-line))

    (let ((indent 0)
          (last (cond
                 ((looking-at "\\(endif\\|else\\)")
                  (-ttl-find-regex-table '(("if.+then$" . 0)
                                           ("endif\\|endwhile\\|endutil\\|next" . -4))
                                         ".*\\(if.+then$\\|endif\\|endwhile\\|endutil\\|next\\)"))

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
        (--indent-to origin-pos 0)
        (cl-return-from fc-ttl-indent-line))

      (unless (looking-at " *;")
        (beginning-of-line-text))
      (setq indent (current-column))

      (--indent-to origin-pos (+ indent last)))))

(defun fc-ttl--beginning-of-defun ()
  (re-search-backward *fc-ttl-function-regex*))

(defun fc-ttl--end-of-defun ()
  (forward-line 1)
  (if (re-search-forward *fc-ttl-function-regex* (point-max) t)
      (goto-char (1- (match-beginning 0)))
    (goto-char (point-max))))

(define-derived-mode fc-ttl-mode prog-mode "TTL"
  "Major mode for TeraTerm macro files."
  :syntax-table nil
  (modify-syntax-entry ?\; "<")
  (modify-syntax-entry ?\n ">")

  (add-hook 'xref-backend-functions #'xref-ttl-xref-backend)

  (setq-local comment-start ";"
              comment-end ""
              imenu-generic-expression *ttl-imenu-generic-expression*
              outline-regexp *fc-ttl-function-regex*)

  (setq-local indent-tabs-mode nil
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
                                            "call" "include")
                                          'words)
                             . font-lock-keyword-face)
                            (,(regexp-opt '("end" "exit" "return") 'words)
                             . ,*fc-ttl-function-exit-face*)
                            (,*fc-ttl-function-regex* . ,*fc-ttl-function-name-face*)
                            ("^:[_].+" . font-lock-function-name-face)))

  (setq-local indent-line-function #'fc-ttl-indent-line
              beginning-of-defun-function #'fc-ttl--beginning-of-defun
              end-of-defun-function #'fc-ttl--end-of-defun)

  (save-excursion
    (outline-minor-mode 1)
    (goto-char (point-min))
    (fc-hs-toggle-all))

  (fc-hs-toggle))

(defun xref-ttl-xref-backend ()
  "TTL backend for Xref."
  (and (eq major-mode 'fc-ttl-mode)
       'xref-ttl))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-ttl)))
  nil)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-ttl)))
  (fc-current-thing))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-ttl)) symbol)
  (or
   (fc--ttl-find-def-in-current-buf symbol)
   (fc--ttl-find-def-in-proj symbol)))

(cl-defun fc--ttl-find-def-in-current-buf (symbol)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^:%s" symbol) (point-max) t)
      (list (xref-make "function"
                       (xref-make-file-location
                        (buffer-file-name)
                        (line-number-at-pos (point))
                        0))))))

(cl-defun fc--ttl-find-def-in-proj (symbol)
  (when-let ((buf (fc--text-retrieve :pattern (format "^:%s_ENTRY$" symbol)
                                     :file-types '(code)))
             (regex "^\\([^:]+\\):\\([0-9]+\\):")
             (bound (point-max)))
    (with-current-buffer buf
      (cl-loop
       while (re-search-forward regex bound t)
       collect (xref-make "function"
                          (xref-make-file-location
                           (match-string 1)
                           (match-string 2)
                           0))
       finally (kill-buffer buf)))))

(cl-defun fc--ttl-function-p (symbol)
  (let ((case-fold-search nil))
    (eq 0 (string-match "^:?[a-z0-9_]+$" symbol))))

(cl-defun fc--ttl-find-ref-in-current-buf (symbol)
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     with func = (if (eq (seq-elt symbol 0) ?:)
                     (cl-subseq symbol 0)
                   symbol)
     with name = (format "call %s" symbol)
     while (re-search-forward (format "^ +call +%s" symbol) (point-max) t)
     collect (xref-make name
                        (xref-make-file-location
                         (buffer-file-name)
                         (line-number-at-pos (point))
                         0)))))

(cl-defun fc--ttl-find-ref-in-proj (symbol)
  (when-let* ((func (if (string-suffix-p "_ENTRY" symbol)
                        (cl-subseq symbol 0 -6)
                      symbol))
              (buf (fc--text-retrieve :pattern (format "include %s$" func)
                                      :dir (fc-proj-root)
                                      :file-types '(code)))
              (regex "^\\([^:]+\\):\\([0-9]+\\):")
              (bound (point-max)))
    (with-current-buffer buf
      (cl-loop
       with name = (format "include %s" func)
       while (re-search-forward regex bound t)
       collect (xref-make name
                          (xref-make-file-location
                           (concat (fc-proj-root) (match-string 1))
                           (string-to-number (match-string 2))
                           0))
       finally (kill-buffer buf)))))

(cl-defmethod xref-backend-references ((_backend (eql xref-ttl)) symbol)
  (if (fc--ttl-function-p symbol)
      (fc--ttl-find-ref-in-current-buf symbol)
    (fc--ttl-find-ref-in-proj symbol)))

(fc-add-fmt 'fc-ttl-mode nil #'fc--default-fmt-with-indent)
(add-to-list '*fc-doc-modes* 'fc-ttl-mode)
(fc-add-tag 'fc-ttl-mode *fc-tag-xref*)

(provide 'fc-ttl)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ttl.el ends here
