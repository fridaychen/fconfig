;;; fc-latex.el --- setup latex environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-ex-scale* 1)

(fc-load 'latex
  :autoload t
  :package 'auctex

  :after (progn
           (defun fc--latex-setup ()
             (fc-each '("\\\\postil{[^{}]+}"
                        "\\\\postil_[^{}]+{[^{}]+}"
                        "\\\\footnote{[^{}]+}")
               (highlight-regexp it 'font-lock-comment-face))

             (tex-fold-mode 1)
             (TeX-fold-buffer))

           (add-hook 'LaTeX-mode-hook #'fc--latex-setup))

  :bind '((LaTeX-mode-map
           ("C-M-j" backward-word)
           ("C-M-l" forward-word)
           ("C-M-i" scroll-down-command)
           ("C-M-k" scroll-up-command)
           ("S-M-s" fc-toggle-hide-show-all)
           ("M-s" fc-toggle-hide-show))))

(defun fc-chapter-zh-to-number ()
  "Convert Chinese chapter number to arabic number."
  (fc-replace-regexp "{第\\([零一二两三四五六七八九十百千万]+\\)章"
                     #'(lambda ()
                         (replace-match (concat "{第"
                                                (number-to-string (fc-zh-to-number (match-string 1)))
                                                "章")))))

(defun fc-latex-generate-fake-part ()
  "Insert fake part to avoid large file."
  (interactive)
  (when (search-forward "\\chapter")
    (forward-line -1)
    (insert (format "\\part{start}")))

  (let ((n 50))
    (while (search-forward "\\chapter" nil nil 50)
      (move-beginning-of-line nil)
      (insert (format "\\part{%d}\n" n))
      (cl-incf n 50)
      (move-end-of-line nil))))

(defun fc-latex-generate-chapters (n-from n-to)
  "Insert chapters."
  (interactive (list (read-number "From: ")
                     (read-number "To: ")))
  (cl-loop for x from n-from to n-to
           do (insert (format "\\chapter{%d}\n\n\n\n" x))))

(defun fc-latex-clean-cmd-space ()
  (save-excursion
    (fc-replace-regexp " +}" "}"))
  (save-excursion
    (fc-replace-regexp "{ +" "{")))

(defun fc--latex-chapter-mark (level title)
  "Find chapter and insert latex commands."
  (interactive)
  (format "%s{%s}"
          (elt ["\\\\part"
                "\\\\chapter"
                "\\\\section"
                "\\\\subsection"
                "\\\\subsubsection"]
               level)
          title))

(cl-defun fc-latex-fix-sectionname-space ()
  "Replace space in the section commands with escape-space."
  (interactive)

  (cl-loop
   (let* ((start (re-search-forward "\\(\\\\part\\|\\\\chapter\\|\\\\section\\)"))
          (end (if start (re-search-forward "$") nil)))
     (if (null start)
         (cl-return))
     (narrow-to-region start end)
     (fc-replace-regexp "\\([^\\\\]\\) " "\\1\\\\ " :from-start t)
     (widen)
     (goto-char end))))

(defun fc-latex-add-zhverse (start end)
  "Add chinese verse.
START: start point.
END: end point."
  (interactive "r")

  (fc-region start end
    (fc-replace-regexp "

+" "
" :from-start t)
    (fc-replace-string "
" "\\\\\\\\
" :from-start t)
    (goto-char (point-min))
    (insert "\\begin{zhverse}\n")
    (goto-char (point-max))
    (insert "\\end{zhverse}\n")
    (indent-region (point-min) (point-max))))

(defun fc-latex-portal ()
  "Show latex portal."
  (fc-select-func
   "Latex"
   (append
    `(("Chapter number zh to Arabic" . fc-chapter-zh-to-number)
      ("Fix space in title"          . fc-latex-fix-sectionname-space)
      ("Fix single quote"            . fc-fix-single-qoute)
      ("Generate fake part"          . fc-latex-generate-fake-part)
      ("Generate fake chapter"       . fc-latex-generate-chapters)
      )
    *fc-book-func-list*)))

(defconst *fc-latex-map*
  (fc-make-keymap
   `(
     ("c" ,(fc-decorate-region "\\chapter{" "}"))
     ("e" ,(fc-decorate-region "\\emph{" "}"))
     ("s" ,(fc-decorate-region "\\section{" "}"))

     ("x" fc-reading-toggle)
     ("v" fc-latex-add-zhverse)
     ("SPC" fc-latex-portal))
   "fc-latex-map"
   *fc-punctuation-map*)
  "KEYS c: chapter  e: emphasize  s: section  x: reading  v: zh verse  SPC: portal.")

(defun fc--latex-mode-func ()
  "Run latex mode func."
  (fc-modal-head-key "Latex" '*fc-latex-map*))

(provide 'fc-latex)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-latex.el ends here
