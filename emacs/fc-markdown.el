;; fc-markdown.el --- setup markdown environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc--markdown-use-reading-face-for-book* 'true)

(defun fc--markdown-book-info ()
  (list :title (fc-search "title: \\(.+\\)" :begin t :sub 1 :bound 128)
        :author (fc-search "author: \\(.+\\)" :begin t :sub 1 :bound 128)
        :date (fc-search "date: \\(.+\\)" :begin t :sub 1 :bound 128)))

(cl-defun fc--markdown-theme-changed ()
  (fc-set-face 'markdown-header-face-1 nil :height 1.2)
  (fc-set-face 'markdown-header-face-2 nil :height 1.1)
  (fc-set-face 'markdown-header-face-3 nil :height 1.1)
  (fc-set-face 'markdown-header-face-4 nil :height 1.0)
  (fc-set-face 'markdown-header-face-5 nil :height 1.0)
  (fc-set-face 'markdown-header-face-6 nil :height 1.0)

  (when-let* ((has-face (facep 'markdown-header-delimiter-face))
              (no-color (not (color-defined-p (face-attribute
                                               'markdown-header-delimiter-face
                                               :foreground))))
              (fg (fc-get-face 'font-lock-keyword-face
                               :foreground)))
    (fc-set-face 'markdown-header-delimiter-face nil
                 :foreground fg)))

(fc-load 'markdown-mode
  :after (progn
           (fc-add-mode-name 'markdown-mode
             (fc-visible (fc-nerd-icon ?\xf48a :foreground "red3") "Ⓜ️"))

           (setf markdown-hr-strings
                 '("-----"))

           (defun fc--markdown-setup ()
             (when (and *is-gui* (not (fc-big-buffer-p)))
               (markdown-display-inline-images))

             (setq prettify-symbols-alist '(("#" . ?⏹)
                                            ("##" . ?●)
                                            ("###" . ?○)
                                            ("####" . ?▶)
                                            ("#####" . ?▷)
                                            ("[ ]" . ?☐)
                                            ("[X]" . ?☑ )
                                            ("[-]" . ?☒ )))
             (prettify-symbols-mode)

             (fc-idle-delay-task (lambda ()
                                   (when *fc-auto-hide*
                                     (outline-hide-sublevels 3)
                                     (unless (zerop (fc-line-num))
                                       (fc-hs-toggle))))
                                 0.2)

             (eldoc-mode -1))

           (defun markdown-fontify-hrs (_last)
             )

           (defun fc--md-toggle-viewer ()
             (when (derived-mode-p 'markdown-mode)
               (markdown-toggle-markup-hiding (if fc-viewer-minor-mode 1 -1)))

             (when (and fc-viewer-minor-mode
                        (equal (fc--md-lang) "en-US"))
               (fc-modal-visual-feedback)))

           (when (eq (fc-get-face 'markdown-header-delimiter-face :foreground)
                     'unspecified)
             (fc-set-face 'markdown-header-delimiter-face nil
                          :foreground (fc-get-face 'font-lock-keyword-face :foregound)))

           (add-hook '*fc-viewer-hook* #'fc--md-toggle-viewer)
           (add-hook 'markdown-mode-hook #'fc--book-setup)
           (add-hook 'markdown-mode-hook #'fc--markdown-setup)))

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
                           (fc-select "Language"
                               `("en-US"
                                 "jp-JP"
                                 "zh-CN")))
          "\n"
          "---\n"))

(defun fc--markdown-chapter-mark (level title)
  (concat (make-string level ?#)
          " " title))

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
      (fc-each (if (fc-search "\\\\part{" :begin t :sub 0 :bound 20480)
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
      (fc-each '(("^ +\\\\sopening{" "")
                 ("^\\\\documentclass.+" "")
                 ("^\\\\usepackage.+" "")
                 ("^\\\\title.+" "")
                 ("^\\\\author.+" "")
                 ("^\\\\date.+" ""))
        (fc-replace-regexp (cl-first it) (cl-second it) :from-start t)))

    (save-excursion
      (fc-each '(("\\begin{document}" "")
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
                         (fc-funcall #'fc-md-insert-footnote)
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

(cl-defun fc--markdown-whitespace-cleanup ()
  "Clean whitespace."
  (save-excursion
    (fc-replace-regexp "\\([^ ]\\) $" "\\1" :from-start t)))

(defun fc-md-portal ()
  "Show md portal."
  (fc-select-func
   "Markdown"
   (append
    `(
      ("Add header"             . fc-md-add-header)
      ("Convert footnote"       . fc-md-convert-footnote)
      ("Convert Latex"          . fc-md-convert-latex)
      ("Convert Latex footnote" . fc-md-convert-latex-footnote)
      ("Fix headline spacing"   . fc-md-fix-headline-spacing)
      )
    *fc-book-func-list*)))

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
          (fc-funcall #'delete-region)

          (fc-funcall #'markdown-insert-footnote)
          (insert note)))
    (fc-funcall #'markdown-insert-footnote)))

(cl-defun fc-md-make-list ()
  "Make list."
  (interactive)

  (unless (region-active-p)
    (fc-funcall #'mark-paragraph))

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
    (fc-funcall #'mark-paragraph)
    (fc-funcall #'next-line))

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

(defun fc--markdown-mode-func ()
  "Run markdown mode func."
  (fc-modal-head-key "Markdown" '*fc-md-map*))

(provide 'fc-markdown)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-markdown.el ends here
