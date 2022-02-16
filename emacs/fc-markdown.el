;; fc-markdown.el --- setup markdown environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar-local *fc-md-scale* 1.2)

(fc-load 'markdown-mode
  :after
  (progn
    (setf markdown-hr-strings
          '("---"))

    (defun fc--setup-markdown-mode ()
      (markdown-display-inline-images)

      (outline-hide-sublevels 3)

      (let ((buf (current-buffer)))
        (fc-delay-task #'(lambda ()
                           (with-current-buffer buf
                             (text-scale-set *fc-md-scale*)))
                       0.1))

      (eldoc-mode -1))

    (defun markdown-fontify-hrs (_last)
      )

    (defun fc--md-toggle-viewer ()
      (when (derived-mode-p 'markdown-mode)
        (markdown-toggle-markup-hiding (if fc-viewer-minor-mode 1 -1)))

      (when (and fc-viewer-minor-mode
                 (equal (fc--md-lang) "en-US"))
        (fc-modal-visual-feedback)))

    (add-hook '*fc-viewer-hook* #'fc--md-toggle-viewer)
    (add-hook 'markdown-mode-hook #'fc--setup-markdown-mode)))

(fc-load 'markdown-changelog)

(fc-load 'markdown-toc)

(cl-defun fc--md-lang ()
  "Get current language."
  (or
   (fc-search "language: *\\(.+\\)" :begin t :sub 1 :bound 128 :default nil)
   (if (gethash 'han (fc-detect-char-script (buffer-substring 1 128)))
       "zh-CN"
     "en-US")))

(cl-defun fc-md-add-header (&optional title author date lang)
  "Add header.
TITLE: title.
AUTHOR: author.
DATE: date.
LANG: language."
  (goto-char (point-min))

  (insert "---\n"
          "title: " (or title (read-string "Title : ")) "\n"
          "author: " (or author (read-string "Author : ")) "\n"
          "date: " (or date (read-string "Date : ")) "\n"
          "language: " (or lang
                           (fc-user-select "Language"
                                           `("en-US"
                                             "jp-JP"
                                             "zh-CN")))
          "\n"
          "---\n"))

(cl-defun fc-md-mark-chapter (level)
  "Find chapter and insert md commands.
LEVEL: chapter level."
  (interactive "nLever: ")

  (save-excursion
    (fc--toc-replace-regexp
     "^ *\\([第章]\\) *\\([0-9零一二三四五六七八九十两百千]\\{1,8\\}\\) *\\([章节回幕]\\{0,1\\}\\) *\\([^。\n]\\{0,40\\}\\)$"
     (concat "\n"
             (s-repeat level "#")
             " \\1\\2\\3 \\4"))))

(cl-defun fc-md-mark-section (level)
  "Find section and insert md commands.
LEVEL: chapter level."
  (interactive "nLever: ")

  (save-excursion
    (fc--toc-replace-regexp
     "^\\(第\\{0,1\\}\\)\\([0-9零一二三四五六七八九十]\\{1,4\\}\\)\\(节\\{0,1\\}\\) *\\([^。\n]\\{0,40\\}\\)$"
     (concat "
"
             (s-repeat level "#")
             " \\1\\2\\3 \\4"))))

(defun fc-md-chapter-zh-to-number ()
  "Convert Chinese chapter number to arabic number."
  (fc-replace-regexp
   "第\\([零一二两三四五六七八九十百千万]+\\)\\([章节回幕]\\)"
   #'(lambda ()
       (replace-match (concat "第"
                              (number-to-string (fc-zh-to-number (match-string 1)))
                              "\\2")))))

(cl-defun fc-md-convert-latex ()
  "Convert latex to markdown."
  (save-excursion
    (fc-md-add-header
     (fc-search "\\title{\\(.+\\)}" :begin t :sub 1 :bound 128)
     (fc-search "\\author{\\(.+\\)}" :begin t :sub 1 :bound 128)
     (fc-search "\\date{\\(.+\\)}" :begin t :sub 1 :bound 128))

    (save-excursion
      (fc-funcall #'fc-md-convert-latex-footnote))

    (save-excursion
      (--each (if (fc-search "\\\\part{" :begin t :sub 0 :bound 20480)
                  '(("\\part{" "# ")
                    ("\\chapter{" "## ")
                    ("\\chapter*{" "## ")
                    ("\\section{" "### ")
                    ("\\section*{" "### ")
                    ("\\subsection{" "#### "))
                '(("\\chapter{" "# ")
                  ("\\chapter*{" "# ")
                  ("\\section{" "## ")
                  ("\\section*{" "## ")
                  ("\\subsection{" "### ")))
        (fc-replace-string (cl-first it) (cl-second it) :from-start t)))

    (save-excursion
      (--each '(("^ +\\\\sopening{" "")
                ("^\\\\documentclass.+" "")
                ("^\\\\usepackage.+" "")
                ("^\\\\title.+" "")
                ("^\\\\author.+" "")
                ("^\\\\date.+" ""))
        (fc-replace-regexp (cl-first it) (cl-second it) :from-start t)))

    (save-excursion
      (--each '(("\\begin{document}" "")
                ("\\zhbook" "")
                ("\\end{document}" "")
                ("\\begin{sletter}" "```")
                ("\\end{sletter}" "```")
                ("\\begin{verse}" "```")
                ("\\end{verse}" "```")
                ("\\begin{zhverse}" "```")
                ("\\end{zhverse}" "```")
                ("\\begin{flushright}" "")
                ("\\end{flushright}" "")
                ("\\begin{flushleft}" "")
                ("\\end{flushleft}" "")
                ("\\footnote{" "")
                ("\\end{document}" "")
                ("\\sclosing{" "")
                ("\\sps{" "")
                ("\\" "")
                ("}" ""))
        (fc-replace-string (cl-first it) (cl-second it) :from-start t)))))

(cl-defun fc--md-add-footnote (regex)
  "Add footnote.
REGEX: regex."
  (fc-replace-regexp regex
                     #'(lambda ()
                         (set-mark (match-beginning 1))
                         (goto-char (match-end 1))
                         (fc-funcall 'fc-md-insert-footnote)
                         (deactivate-mark))))

(cl-defun fc-md-convert-latex-footnote ()
  "Convert latex footnote to markdown footnote."
  (fc--md-add-footnote "\\\\\\{0,1\\}footnote{\\([^}]+\\)}"))

(cl-defun fc-md-convert-footnote ()
  "Convert footnote."
  (fc--md-add-footnote (read-string "Footnote regex : ")))

(cl-defun fc-md-fix-headline-spacing ()
  "Fix headline spacing."
  (save-excursion
    (fc-replace-regexp "\\([^\n]\\)\n+#"
                       "\\1\n\n#" :from-start t)

    (fc-replace-regexp "^#\\([^\n]+\\)\n+\\([^#\n]\\)"
                       "#\\1\n\n\\2" :from-start t)))

(cl-defun fc-markdown-mode-whitespace-cleanup ()
  "Clean whitespace."
  (save-excursion
    (fc-replace-regexp "\\([^ ]\\) $" "\\1" :from-start t)))

(cl-defun fc-md-update-local-var ()
  (add-dir-local-variable 'markdown-mode '*fc-md-scale* (read-number "Markdown scale" *fc-md-scale*)))

(defun fc-md-portal ()
  "Show md portal."
  (fc-user-select-func
   "Markdown"
   `(
     ("Add header"                      .       fc-md-add-header)
     ("Chapter number zh to Arabic"     .       fc-md-chapter-zh-to-number)
     ("Convert footnote"                .       fc-md-convert-footnote)
     ("Convert Latex"                   .       fc-md-convert-latex)
     ("Convert Latex footnote"          .       fc-md-convert-latex-footnote)
     ("Fix headline spacing"            .       fc-md-fix-headline-spacing)
     ("Fix zh single quote"             .       fc-fix-zh-single-qoute)
     ("Format"                          .       fc-book-format)
     ("Init"                            .       fc-book-init)
     ("Init book var"                   .       fc-md-update-local-var)
     ("Mark chapter"                    .       fc-md-mark-chapter)
     ("Mark section"                    .       fc-md-mark-section)
     ("Merge lines"                     .       fc-merge-short-line)
     ("Recheck"                         .       fc-recheck-book)
     ("Remove extra space"              .       fc-remove-extra-whitespace)
     ("Remove suffix space"             .       fc-markdown-mode-whitespace-cleanup)
     )))

(cl-defun fc-md-quote (start end)
  "Quote region.
START: start of region.
END: end of region."
  (interactive "r")

  (fc-region start end
    (goto-char (point-min))
    (insert "```\n")
    (fc-replace-regexp "^—" "\t—" :from-start t)
    (goto-char (point-max))
    (insert "```\n")))

(cl-defun fc--md-create-footnote (pos text)
  "Create footnote.
POS: position.
TEXT: text of footnote."
  (save-excursion
    (let ((fn (markdown-footnote-counter-inc)))
      (goto-char pos)
      (insert (format "[^%d]" fn))

      (goto-char (point-max))
      (insert (format "\n[^%d]: " fn)
              text
              "\n"))))

(cl-defun fc-md-convert-to-footnote (regex)
  "Convert to footnote.
REGEX: regular expression."
  (save-excursion
    (fc-replace-regexp regex
                       #'(lambda ()
                           (let ((pos (match-beginning 0))
                                 (text (match-string 1)))
                             (replace-match "")
                             (fc--md-create-footnote pos text)))
                       :from-start t)))

(cl-defun fc-md-insert-footnote ()
  "Inert footnote."
  (interactive)

  (if (use-region-p)
      (save-excursion
        (let ((note (buffer-substring (region-beginning)
                                      (region-end))))
          (fc-funcall 'delete-region)

          (fc-funcall 'markdown-insert-footnote)
          (insert note)))
    (fc-funcall 'markdown-insert-footnote)))

(cl-defun fc-md-make-list ()
  "Make list."
  (interactive)

  (unless (region-active-p)
    (fc-funcall 'mark-paragraph))

  (save-excursion
    (let ((start (region-beginning))
          (end (region-end)))
      (fc-region start end
        (fc-replace-string "```
" "" :from-start t)
        (fc-replace-regexp "\n+" "\n" :from-start t)
        (fc-replace-regexp "^\\([^\n]+\\)" "- \\1" :from-start t)
        (goto-char (point-min))
        (indent-region (point-min) (point-max))))))

(cl-defun fc-md-add-verse ()
  "Add verse.
START: start point.
END: end point."
  (interactive)

  (unless (region-active-p)
    (fc-funcall 'mark-paragraph)
    (fc-funcall 'next-line))

  (let ((start (region-beginning))
        (end (region-end)))
    (fc-region start end
      (fc-replace-string "```
" "" :from-start t)
      (fc-replace-regexp "

+" "
" :from-start t)
      (fc-replace-string "
" "\x20\x20
" :from-start t)
      (goto-char (point-min))
      (indent-region (point-min) (point-max)))))

(cl-defun fc-md-style ()
  "Apply MD style."
  (minibuffer-message (keymap-prompt markdown-mode-style-map))
  (set-transient-map markdown-mode-style-map))

(defconst *fc-md-map*
  (fc-make-keymap
   `(
     ("1" ,(fc-decorate-region "# " "" :mark #'fc-mark-line))
     ("2" ,(fc-decorate-region "## " "" :mark #'fc-mark-line))
     ("3" ,(fc-decorate-region "### " "" :mark #'fc-mark-line))
     ("4" ,(fc-decorate-region "#### " "" :mark #'fc-mark-line))
     ("5" ,(fc-decorate-region "##### " "" :mark #'fc-mark-line))
     ("6" ,(fc-decorate-region "###### " "" :mark #'fc-mark-line))

     ("a" fc-md-style)
     ("b" fc-md-insert-footnote)

     ("f b" ,(fc-decorate-region "**" "**"))
     ("f i" ,(fc-decorate-region "*" "*"))
     ("f u" ,(fc-decorate-region "__" "__"))

     ("l" fc-md-make-list)

     ("q" fc-md-quote)
     ("s" markdown-insert-hr)

     ("t" fc-book-search-verse)

     ("v" fc-md-add-verse)
     ("u" markdown-do)

     ("L" markdown-changelog-new)
     ("T" markdown-toc-generate-or-refresh-toc)
     ("SPC" fc-md-portal))
   "fc-md-map"
   *fc-func-mode-map*)
  "KEYS  a: apply style  b: footnote  l: make list  q: quote  t: search verse  v: add verse  u: do  SPC: portal.")

(defun fc-markdown-mode-func ()
  "Run markdown mode func."
  (fc-modal-head-key "Markdown" '*fc-md-map*))

(provide 'fc-markdown)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-markdown.el ends here
