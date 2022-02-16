;;; fc-org.el --- setup org -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-org-latex-preview-scale* 2.2)
(defvar *fc-org-dir* "~/org/")
(defconst *fc-org-captrue-template*
  `(
    ("b" "Book" "book.org" "Inbox"
     "* Book <<%^{Title of book}>>\n  %T")
    ("j" "Journal" "journal.org" "Inbox"
     "* %^{Journal}\n  %T" :immediate-finish t)
    ("k" "Knowledge" "knowledge.org" "Inbox"
     "* %?\n  # Wrote on %U")
    ("t" "Todo" "remind.org" "Inbox"
     "* TODO %?\n  %a\n  %T")
    ))

(defconst *fc-org-captrue-raw-templates*
  `(
    ("B" "Bookmark" plain (file+headline "bookmarks.org" "Inbox") "[[%(org-cliplink-clipboard-content)][%^{Title}]]\n")
    ))

(defvar *fc-org-user-capture-templates* nil)

(defvar *fc-org-trust-babel-modes* '("blockdiag"
                                     "gnuplot"
                                     "packetdiag"
                                     "plantuml"
                                     "shell"))

(defvar *fc-org-image-background* nil)

(fc-install 'blockdiag-mode
            'gnuplot
            'ob-blockdiag
            'org-cliplink
            'org-link-beautify
            'org-superstar
            'valign)

(cl-defun fc--org-theme-changed ()
  "Update color after theme changed."
  (fc-set-face-attribute 'org-footnote
                         nil
                         :height (- *fc-font-height* 20))

  (setf *fc-org-image-background* (if (fc-dark-theme-p)
                                      "cornsilk2"
                                    nil))

  (plist-put org-format-latex-options
             :foreground
             (fc-get-face-attribute 'font-lock-keyword-face :foreground)))

(fc-load 'org
  :after
  (progn
    (setf org-hide-emphasis-markers t
          org-log-done t
          org-export-with-sub-superscripts nil
          org-src-ask-before-returning-to-edit-buffer nil
          org-image-actual-width nil
          org-preview-latex-image-directory "output/"
          org-startup-indented nil
          org-fontify-quote-and-verse-blocks t
          )

    (plist-put org-format-latex-options :scale *fc-org-latex-preview-scale*)
    (plist-put org-format-latex-options :foreground (fc-get-face-attribute 'font-lock-keyword-face :foreground))

    (fc-set-face-attribute 'org-footnote
                           nil
                           :height (- *fc-font-height* 20))

    (fc-add-fmt 'org-mode nil 'fc-format-org)

    (defun create-image-with-background-color (args)
      "Specify background color of Org-mode inline image through modify `ARGS'."
      (let* ((file (car args))
             (type (cadr args))
             (data-p (caddr args))
             (props (cdddr args)))
        ;; Get this return result style from `create-image'.
        (append (list file type data-p)
                (when (eq major-mode 'org-mode)
                  (list :background (or *fc-org-image-background*
                                        (face-background 'default))))
                props)))

    (advice-add 'create-image :filter-args
                #'create-image-with-background-color)

    (require 'org-agenda)
    (require 'org-capture)
    (require 'ob-blockdiag)
    (require 'ob-gnuplot)
    (require 'ob-octave)
    (require 'ob-python)
    (require 'ob-shell)
    (require 'ol-man)

    (require 'fc-org-ext)

    (cl-defun fc--org-hide-all ()
      (org-content)
      (org-hide-drawer-all)
      (org-hide-block-all)
      (org-block-map (lambda ()
                       (when (looking-at-p "#\\+BEGIN_\\(EXAMPLE\\|NOTE\\|VERSE\\)")
                         (forward-char 1)
                         (org-cycle)))))

    (cl-defun fc--setup-org-mode ()
      (electric-indent-local-mode -1)

      (org-superstar-mode 1)
      (org-link-beautify-mode -1)

      (fc--org-hide-all)

      (add-hook 'write-contents-functions
                (lambda () (org-update-statistics-cookies t)) nil t))

    (cl-defun fc--capture-copy-region ()
      (save-excursion
        (let ((data nil))
          (with-current-buffer (plist-get org-capture-plist :original-buffer)
            (when (region-active-p)
              (setf data (buffer-substring (region-beginning)
                                           (region-end)))
              (deactivate-mark)))
          (when data
            (goto-char (point-max))
            (when (/= (current-column) 0)
              (insert "\n"))
            (insert data)))))

    (cl-defun fc--capture-tag ()
      (when (org-roam-capture-p)
        (cl-return-from fc--capture-tag))

      (let ((tags
             (with-current-buffer (plist-get org-capture-plist :original-buffer)
               (when (boundp 'fc-capture-tags)
                 fc-capture-tags))))
        (org-set-tags (fc-string tags))))

    (cl-defun fc--capture-edit ()
      (fc-modal-disable))

    (add-hook 'org-capture-mode-hook #'fc--capture-edit)
    (add-hook 'org-capture-mode-hook #'fc--capture-copy-region)
    (add-hook 'org-capture-mode-hook #'org-align-all-tags)
    (add-hook 'org-capture-mode-hook #'fc--capture-tag)

    (add-hook 'org-capture-after-finalize-hook #'fc-modal-disable)
    (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
    (add-hook 'org-mode-hook #'fc--setup-org-mode)
    (add-hook 'org-mode-hook #'valign-mode)

    (add-hook 'after-save-hook #'check-parens nil t)))

(cl-defun fc--org-insert-title ()
  "Insert title."
  (let ((title (read-string "Org file title")))
    (insert "#+title: " title  "\n"
            "\n")))

(cl-defun fc-org-add-block (type &key ask pre-format (copy t))
  "Add block.
TYPE: type of block.
ASK: allow user to input parameter of block.
PRE-FORMAT: format the block content."
  (let* (start end edit last-pos)
    (cond
     ((region-active-p)
      (setf start (region-beginning)
            end (region-end)
            edit nil))

     (copy
      (setf start (point))
      (yank)
      (setf end (point)
            edit t))

     (t
      (mark-paragraph)
      (forward-line)
      (setf start (region-beginning)
            end (region-end)
            edit nil)))

    (goto-char end)
    (when pre-format
      (fc-region start end
        (fc-funcall pre-format)))

    (when (/= (current-column) 0)
      (insert "\n"))

    (insert "#+END_" type "\n")
    (unless (looking-at "\n")
      (insert "\n"))

    (unless edit
      (setq last-pos (point-marker)))

    (goto-char start)
    (when (/= (current-column) 0)
      (insert "\n\n"))

    (insert (fc--text " "
                      (concat "#+BEGIN_" type)
                      (fc-ask ask))
            "\n")

    (when last-pos
      (goto-char last-pos))))

(cl-defun fc--org-fix-headline-spacing ()
  "Fix headline spacing."
  (save-excursion
    (fc-replace-regexp "\\([^\n]\\)\n+\\*"
                       "\\1\n\n*" :from-start t)

    (fc-replace-regexp "^\\*\\([^\n]+\\)\n+\\([^*\n]\\)"
                       "*\\1\n\n\\2" :from-start t)))

(cl-defun fc--org-convert-mk-verse ()
  "Convert markdown verse."
  (interactive)

  (save-excursion
    (save-restriction
      (goto-char (point-min))

      (while (re-search-forward "[^
 ]+  $")
        (goto-char (match-beginning 0))
        (mark-paragraph)
        (forward-char)
        (fc-org-add-block "VERSE" :pre-format #'fc--org-format-verse)
        (deactivate-mark)))))

(cl-defun fc--org-fmt-verse ()
  "Format verse."
  (org-block-map (lambda ()
                   (when (looking-at-p "#\\+BEGIN_VERSE")
                     (let (start end)
                       (forward-line)
                       (setf start (point))

                       (goto-char (match-end 0))
                       (forward-line -1)
                       (end-of-line)
                       (setf end (point))

                       (fc-region start end
                         (fc--org-format-verse)))))))

(cl-defun fc-format-org ()
  (fc--org-fix-headline-spacing)
  (fc--org-fmt-verse)

  (fc--default-fmt))

(defun fc--org-find-oneline-footnote (fn)
  (when (re-search-forward (concat "^[[:space:]]*" fn "\\([^
]+\\)"))
    (match-string 1)))

(defun fc--org-convert-inline-fontnote (regex)
  (while (re-search-forward regex)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (note (fc--org-find-oneline-footnote (match-string 0))))
      (goto-char start)
      (if (zerop (current-column))
          (goto-char end)
        (delete-region start end)
        (insert "[fn:: " note "]")))))

(cl-defun fc--org-add-header (&optional title author date lang)
  "Add header.
TITLE: title.
AUTHOR: author.
DATE: date.
LANG: language."
  (goto-char (point-min))

  (insert "#+TITLE: " (or title (read-string "Title : ")) "\n"
          "#+AUTHOR: " (or author (read-string "Author : ")) "\n"
          "#+DATE: " (or date (read-string "Date : ")) "\n"
          "#+LANGUAGE: " (or lang
                             (fc-user-select "Language"
                                             `("en-US"
                                               "jp-JP"
                                               "zh-CN")))
          "\n"))

(cl-defun fc--org-convert-latex ()
  "Convert latex to org."
  (save-excursion
    (fc--org-add-header
     (fc-search "\\title{\\(.+\\)}" :begin t :sub 1 :bound 1024)
     (fc-search "\\author{\\(.+\\)}" :begin t :sub 1 :bound 1024)
     (fc-search "\\date{\\([^}]+\\)}" :begin t :sub 1 :bound 1024))

    (save-excursion
      (fc--org-add-footnote "\\\\footnote{\\([^}]+\\)}"))

    (save-excursion
      (--each (if (fc-search "\\\\part{" :begin t :sub 0 :bound 20480)
                  '(("\\part{" "* ")
                    ("\\chapter{" "** ")
                    ("\\chapter*{" "** ")
                    ("\\section{" "*** ")
                    ("\\section*{" "*** ")
                    ("\\subsection{" "**** "))
                '(("\\chapter{" "* ")
                  ("\\chapter*{" "* ")
                  ("\\section{" "** ")
                  ("\\section*{" "** ")
                  ("\\subsection{" "*** ")))
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
                ("\\end{document}" "")
                ("\\sclosing{" "")
                ("\\sps{" "")
                ("\\" "")
                ("}" ""))
        (fc-replace-string (cl-first it) (cl-second it) :from-start t)))))

(cl-defun fc-org-portal ()
  "Show org portal."
  (fc-user-select-func
   "Org portal"
   `(
     ("Add header"                 . fc--org-add-header)
     ("Convert latex footnote"     . ,(fc-manual (fc--org-add-footnote "\\\\footnote{\\([^}]+\\)}")))
     ("Convert markdown verse"     . fc--org-convert-mk-verse)
     ("Convert latex"              . fc--org-convert-latex)
     ("Convert to inline footnote" . ,(fc-manual (fc--org-convert-inline-fontnote (read-string "Regex for mark footnote"))))
     ("Conervt to table"           . fc--org-convert-table)
     ("Fix zh single quote"        . fc-fix-zh-single-qoute)
     ("Org ctrl-c-minus"           . org-ctrl-c-minus)
     ("Org Sort"                   . org-sort)
     ("Publish to html"            . org-html-export-to-html)
     ("Publish to markdown"        . org-md-export-to-markdown)
     ("Roam sync"                  . org-roam-db-sync)
     ("Redisplay inline image"     . org-redisplay-inline-images)
     ("Update dblock"              . org-update-all-dblocks)
     ("Update source block"        . org-babel-execute-buffer)
     )))

(cl-defun fc--org-ctrl-c-ctrl-c ()
  "Org ctrl-c ctrl-c wrapper."
  (cond
   ((and (boundp 'org-agenda-mode)
         org-agenda-mode)
    (org-agenda-ctrl-c-ctrl-c))

   ((and (boundp 'org-capture-mode)
         org-capture-mode)
    (org-capture-finalize))

   (t
    (org-ctrl-c-ctrl-c))))

(cl-defun fc--org-do-intert-item ()
  "Insert item."
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p " +- \\[[ X]\\]"))
      (org-insert-item t)
    (org-insert-item))

  (fc-modal-disable))

(cl-defmacro fc--org-smart-action (default &rest body)
  "Smart action according to current position.
DEFAULT: defaul function.
BODY: usually a pcase block."
  (declare (indent 1))

  `(let* ((context (org-context))
          (1st-elt (caar context))
          (2nd-elt (caadr context))
          (elt (cond
                ((null 1st-elt) nil)
                ((and (eq 1st-elt :item-bullet)
                      (eq 2nd-elt :item))
                 :item-bullet)
                ((null 2nd-elt) 1st-elt)
                (t 2nd-elt))))
     (when (looking-at-p "\\$[^\\$]+\\$")
       (setq elt :latex-fragment))

     (when (fc--org-looking-over-footnote)
       (setq elt :footnote))

     (if (null elt)
         (fc-funcall ,default)
       ,@body)))

(cl-defun fc--org-looking-over-footnote ()
  "Test if current point is over a footnote."
  (or (looking-at-p "\\[fn:")
      (save-excursion
        (skip-chars-backward "a-zA-Z -:" (max (point-min) (- (point) 10)))
        (backward-char 1)
        (looking-at-p "\\[fn:"))))

(cl-defun fc--org-do ()
  "Smart do."
  (fc--org-smart-action #'org-ctrl-c-ctrl-c
    (pcase elt
      (:checkbox (org-ctrl-c-ctrl-c))
      (:footnote (org-footnote-action))
      (:headline (org-insert-heading-respect-content)
                 (fc-modal-disable))
      (:item (fc--org-do-intert-item))
      (:item-bullet (org-ctrl-c-minus))
      ((or :latex-fragment :latex-preview)
       (org-latex-preview))
      (:link (org-open-at-point))
      (:src-block (org-ctrl-c-ctrl-c))
      (:tags (org-set-tags-command))
      (:timestamp (fc-funcall #'org-time-stamp))
      (:todo-keyword (org-todo))
      (_ (message "context: %s elt: %s" context elt)))))

(defun fc--org-beginning ()
  "Goto the beginning of the current block."
  (fc--org-smart-action nil
    (pcase elt
      (:src-block (re-search-backward "^ *#\\+BEGIN"))
      (_ (message "context: %s" context)))))

(defun fc--org-end ()
  "Goto the end of the current block."
  (fc--org-smart-action nil
    (pcase elt
      (:src-block (re-search-forward "^ *#\\+END"))
      (_ (message "context: %s" context)))))

(defun fc--org-current-cell ()
  "Get the content of current table cell."
  (org-table-get (org-table-current-line)
                 (org-table-current-column)))

(defun fc--org-copy ()
  "Copy the content of current table cell."
  (fc--org-smart-action nil
    (pcase elt
      (:table (kill-new (fc--org-current-cell)))
      (_ (message "context: %s" context)))))

(defun fc-org-mode-mouse-func (_event)
  "Handle mouse event."
  (fc--org-do))

(defvar *fc--org-last-year* "")

(defun fc--org-convert ()
  "Smart format converter."
  (or
   (fc-replace-looking-text "\\([0-9]+\\)[年/-]\\([0-9]+\\)[月/-]\\([0-9]+\\)[日号]?"
     (setf *fc--org-last-year* (string-to-number (match-string 1)))
     (format "[%d-%02d-%02d]"
             (string-to-number (match-string 1))
             (string-to-number (match-string 2))
             (string-to-number (match-string 3))))

   (fc-replace-looking-text "\\([0-9]+\\)[年/-]\\([0-9]+\\)[月]"
     (setf *fc--org-last-year* (string-to-number (match-string 1)))
     (format "[%d-%02d]"
             (string-to-number (match-string 1))
             (string-to-number (match-string 2))))

   (fc-replace-looking-text "\\([0-9]+\\)[月/-]\\([0-9]+\\)[日号]?"
     (format "[%d-%02d-%02d]"
             *fc--org-last-year*
             (string-to-number (match-string 1))
             (string-to-number (match-string 2))))))

(defun fc--org-occur ()
  "Wrapper of org-occur."
  (org-occur (fc-current-thing :ask t :regq t :confirm "Org match")))

(defun fc--org-sparse-tree ()
  "Smart sparse tree."
  (fc--org-smart-action #'org-sparse-tree
    (pcase elt
      (:headline (fc--org-occur))
      (:tags (fc-funcall #'org-match-sparse-tree))
      (:todo-keyword (fc-funcall #'org-show-todo-tree))
      (_ (fc-funcall #'org-sparse-tree)))))

(defun fc--org-insert-formula ()
  "Insert latex formula."
  (let (last-point
        (displayed (zerop (current-column))))
    (unless (looking-back " " 1)
      (insert " "))
    (insert (if displayed "\\[" "$"))
    (setq last-point (point))
    (insert (if displayed "\\] " "$ "))
    (goto-char last-point)
    (fc-modal-disable)))

(defun fc--org-convert-table ()
  "Convert multi line text to table."
  (interactive)

  (fc-region (region-beginning) (region-end)
    (let ((columns (read-number "Org table column")))
      (fc-replace-regexp "\n\\{1,\\}"
                         "\n"
                         :from-start t)

      (fc-replace-regexp "^\\(.\\)" "|\\1" :from-start t)

      (goto-char (point-min))

      (cl-loop
       do
       (join-line columns)
       (forward-line)
       until (or (eq (point) (point-max))
                 (looking-at-p "^$"))))))

(defun fc--org-format-verse ()
  "Format a verse."
  (goto-char (point-min))
  (fc-replace-regexp "^[[:space:]]*\\([^ ]\\)"
                     "  \\1" :from-start t)
  (fc-whitespace-clean)
  (fc-replace-regexp "^\n+"
                     "\n"
                     :from-start t))

(cl-defun fc--org-add-footnote (regex)
  "Add footnote.
REGEX: regex."
  (let ((no (read-number "Footnote number start from")))
    (fc-replace-regexp regex
                       #'(lambda ()
                           (let ((footnote (match-string 1)))
                             (replace-match "")
                             (fc--org-insert-footnote no footnote))
                           (setq no (1+ no))))))

(defun fc--org-insert-footnote (label content)
  "Insert a footnote.
LABEL: label of new footnote.
CONTENT: content of new footnote."
  (save-excursion
    (insert (format "[fn:%s]" label))
    (goto-char (point-max))
    (insert (format "\n[fn:%s] %s\n" label content))))

(defconst *fc-org-map*
  (fc-make-keymap
   `(
     ("9" org-promote)
     ("0" org-demote)

     ("1" ,(fc-decorate-region "* " "" :mark #'fc-mark-line))
     ("2" ,(fc-decorate-region "** " "" :mark #'fc-mark-line))
     ("3" ,(fc-decorate-region "*** " "" :mark #'fc-mark-line))
     ("4" ,(fc-decorate-region "**** " "" :mark #'fc-mark-line))
     ("5" ,(fc-decorate-region "***** " "" :mark #'fc-mark-line))
     ("6" ,(fc-decorate-region "****** " "" :mark #'fc-mark-line))

     ("a" fc--org-beginning)
     ("b" org-emphasize)
     ("c" fc--org-ctrl-c-ctrl-c)
     ("e" fc--org-end)

     ("f b" ,(fc-decorate-region "*" "*"))
     ("f i" ,(fc-decorate-region "/" "/"))
     ("f u" ,(fc-decorate-region "_" "_"))

     ("g" fc--org-copy)

     ("i c" org-cliplink)
     ("i d" org-insert-drawer)
     ("i f" fc--org-insert-formula)
     ("i i" fc--org-convert)
     ("i n" org-roam-node-insert)
     ("i q" ,(fc-manual (fc-org-add-block "QUOTE")))
     ("i t" org-time-stamp)
     ("i u" ,(fc-manual (fc-org-add-block "SRC" :ask '("Output file" "plantuml :file output/"))))
     ("i v" ,(fc-manual (fc-org-add-block "VERSE" :pre-format #'fc--org-format-verse :copy nil)))
     ("i C" ,(fc-manual (fc-org-add-block "COMMENT")))
     ("i E" ,(fc-manual (fc-org-add-block "EXAMPLE" :copy nil)))
     ("i N" ,(fc-manual (fc-org-add-block "NOTE")))
     ("i T" fc--org-insert-title)

     ("l" org-insert-link)
     ("m" org-mark-element)
     ("o" org-open-at-point)
     ("s" ,(fc-manual (fc-org-add-block "SRC" :ask "Programming language")))
     ("t" org-todo)
     ("u" fc--org-do)
     ("v t" ,(fc-manual (org-tags-view t)))
     ("v T" org-tags-view)
     ("y" ,(fc-cond-key :normal 'fc--org-sparse-tree
                        :region 'fc--org-occur))
     ("A" org-archive-subtree)
     ("C i" org-clock-in)
     ("C o" org-clock-out)
     ("D" org-deadline)
     ("L" org-todo-list)
     ("S" org-schedule)
     ("T" org-set-tags-command)
     ("[" ,(fc-decorate-region "[[" "]]"))
     ("<" ,(fc-decorate-region "<<<" ">>>"))
     ("SPC" fc-org-portal))
   "fc-org-map"
   *fc-func-mode-map*)
  "KEYS b: emphasize  c: C-c C-c  i c: clip link  i d: drawer  i f: formula  i n: roam node  i q: quote  i t: timestamp  i u: uml  i N: note  i T: insert title  l: link  m: mark element  o: open  t: todo  s: add src  t: todo  v t:  view tags  v T: view tags TODO  y: show todo tree  C i: clock in  C o: clock out  A: archive  D: deadline  S: schedule  T: set tag  -: C-c minus  ^: sort.")

(cl-defun fc-org-mode-func ()
  "Mode func."
  (fc-modal-head-key "Org" '*fc-org-map*))

(defconst *fc-org-agenda-map*
  (fc-make-keymap
   `(
     ("M" org-agenda-month-view)
     )
   "fc-org-agenda-map"
   *fc-org-map*)
  "KEYS M: month.")

(cl-defun fc-org-agenda-mode-func ()
  "FC org-agenda-mode func."
  (fc-modal-head-key "Org Agenda" '*fc-org-agenda-map*))

(cl-defun fc--org-init-dir ()
  "Init org directory."
  (unless (fc-dir-exists-p *fc-org-dir*)
    (make-directory *fc-org-dir*)))

(cl-defun fc--org-gen-template (template)
  "Generate standard org capture template.
TEMPLATE: fconfig format template."
  (seq-concatenate 'list
                   `(,(cl-first template)
                     ,(cl-second template)
                     entry
                     (file+headline
                      ,(concat *fc-org-dir* (cl-third template))
                      ,(cl-fourth template))
                     ,(cl-fifth template))
                   (nthcdr 5 template)))

(cl-defun fc-org-add-capture-template (templates)
  "Add fconfig templates to org.
TEMPLATES: fconfig templates."
  (--each templates
    (add-to-list 'org-capture-templates (fc--org-gen-template it))))

(cl-defun fc-org-autoconfig ()
  "Auto config org."
  (fc--org-init-dir)

  (--each '(org-agenda
            org-agenda-list)
    (advice-add it :before #'fc--before-agenda))

  (setf org-agenda-files (directory-files *fc-org-dir* t "org$")
        org-capture-templates nil
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "REMIND(r)"
                                      "|"
                                      "DONE(d)" "SOMEDAY(s)"))
        org-confirm-babel-evaluate #'fc--org-confirm-babel-evaluate)

  (fc-org-add-capture-template *fc-org-captrue-template*)
  (fc-org-add-capture-template *fc-org-user-capture-templates*)

  (--each *fc-org-captrue-raw-templates*
    (add-to-list 'org-capture-templates it)))

(cl-defun fc--before-agenda (&rest _rest)
  "Wrapper function."
  (setf org-agenda-files (directory-files *fc-org-dir* t "^[^#].+org$")))

(cl-defun fc--org-toggle-special-edit ()
  "Toggle block editor mode."
  (cond
   ((org-src-edit-buffer-p)
    (if *fc-ergo-prefix*
        (org-edit-src-abort)
      (org-edit-src-exit)))

   ((equal major-mode 'org-mode)
    (org-edit-special))))

(defun fc--org-confirm-babel-evaluate (lang _body)
  "Trust all mode in *fc-org-trust-babel-modes.
LANG: language of babel."
  (not (member lang *fc-org-trust-babel-modes*)))

(fc-load 'org-roam
  :before (setf org-roam-v2-ack t)
  :after
  (progn
    (setf org-roam-directory "~/org/roam")

    (org-roam-db-autosync-mode)))

(when (eq major-mode 'org-mode)
  (fc--setup-org-mode))

(provide 'fc-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org.el ends here
