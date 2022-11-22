;;; fc-org.el --- setup org -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-org-latex-preview-scale* 2.2)
(defvar *fc-org-dir* "~/org/")
(defconst *fc-org-capture-template*
  `(
    ("b" "Book" "book.org" "Inbox"
     "* TODO Book <<%?>> :book:\n")
    ("j" "Journal" "journal.org" "Inbox"
     "* %^{Journal}\n  %T" :immediate-finish t)
    ("s" "Study" "study.org" "Inbox"
     "* TODO Study %? :study:\n")
    ("t" "Todo" "remind.org" "Inbox"
     "* TODO %?\n  %a\n")
    ))

(defconst *fc-org-capture-raw-templates*
  `(
    ("B" "Bookmark" plain (file+headline "bookmarks.org" "Inbox") "[[%(org-cliplink-clipboard-content)][%^{Title}]]\n")
    ))

(defvar *fc-org-user-capture-templates* nil)

(defvar *fc-org-trust-babel-modes* '("blockdiag"
                                     "elisp"
                                     "emacs-lisp"
                                     "gnuplot"
                                     "octave"
                                     "packetdiag"
                                     "plantuml"
                                     "python"
                                     "shell"))

(defvar *fc-org-image-background* nil)

(defvar *fc-org-dwell-timer* nil)
(defvar *fc-org-pop-footnote* nil)

(defvar *fc--org-get-elt* nil)
(defvar *fc-agenda-list* nil)
(defvar *fc-org-no-tag-captures* '("Book" "Study"))

(fc-install 'blockdiag-mode
            'gnuplot
            'ob-blockdiag
            'org-cliplink
            'org-link-beautify
            'org-superstar
            'valign)

(cl-defun fc--org-theme-changed ()
  "Update color after theme changed."
  (fc-set-face-attribute 'org-block nil
                         :family
                         (fc-get-face-attribute 'default :family))

  (if (not (fc-get-face-attribute 'org-level-1 :overline))
      (cond
       ((fc-dark-theme-p)
        (fc-set-face-attribute 'org-level-1 nil
                               :overline "#efcab2"
                               :foreground "#c7c3cb"
                               :background "#3d2a2d"
                               :height 1.1)
        (fc-set-face-attribute 'org-level-2 nil
                               :overline "#efcab2"
                               :foreground "#efcab2"
                               :background "#3d2a2d"
                               :height 1.05))

       (t
        (fc-set-face-attribute 'org-level-1 nil
                               :overline "#A7A7A7"
                               :foreground "#3C3C3C"
                               :background "#F0F0F0"
                               :height 1.1)
        (fc-set-face-attribute 'org-level-2 nil
                               :overline "#123555"
                               :foreground "#123555"
                               :background "#E5F4FB"
                               :height 1.05))))

  (fc-set-face-attribute 'org-level-1 nil :height 1.1)
  (fc-set-face-attribute 'org-level-2 nil :height 1.05)
  (fc-set-face-attribute 'org-level-3 nil :height 1.01)

  (--each '(org-quote org-block)
    (let ((default-bg (fc-get-face-attribute 'default :background))
          (target-bg (fc-get-face-attribute it :background)))
      (when (or
             (equal target-bg default-bg)
             (> (fc-color-difference target-bg default-bg) 8000))
        (fc-set-face-attribute it nil
                               :background
                               (color-darken-name
                                default-bg
                                (if (fc-dark-theme-p) -5 5))))))

  (fc-set-face-attribute 'org-footnote
                         nil
                         :height (- *fc-font-height* 30))

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
          org-superstar-headline-bullets-list '(?◉ ?🞛 ?○ ?▷)
          org-imenu-depth 4
          )

    (plist-put org-format-latex-options :scale *fc-org-latex-preview-scale*)
    (plist-put org-format-latex-options :foreground (fc-get-face-attribute 'font-lock-keyword-face :foreground))

    (fc--org-theme-changed)

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
    (require 'ob-plantuml)
    (require 'ob-gnuplot)
    (require 'ob-octave)
    (require 'ob-python)
    (require 'ob-shell)
    (require 'ol-man)
    (require 'ox-publish)

    (require 'fc-org-ext)

    (add-to-list 'org-babel-default-header-args:plantuml
                 (cons :java "-Djava.awt.headless=true"))

    (cl-defun fc--org-hide-all ()
      (org-content)
      (org-hide-drawer-all)
      (org-hide-block-all)
      (org-block-map (lambda ()
                       (when (looking-at-p "#\\+BEGIN_\\(EXAMPLE\\|QUOTE\\|VERSE\\)")
                         (forward-char 1)
                         (org-cycle)))))

    (cl-defun fc--setup-org-mode ()
      (when (and *is-gui*
                 (fboundp #'pixel-scroll-precision-mode))
        (pixel-scroll-precision-mode 1))

      (electric-indent-local-mode -1)

      (org-superstar-mode 1)
      (org-link-beautify-mode -1)

      (when *fc-reading-face*
        (setf buffer-face-mode-face *fc-reading-face*)
        (buffer-face-mode 1))

      (unless (fc--org-capture-p)
        (fc--org-hide-all)
        (fc-idle-delay-task #'fc-hs-toggle 0.1))

      (fc-dwell-enable #'fc--org-dwell)
      (add-hook 'pre-command-hook #'fc--org-hide-footnote)

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
      (when (or (org-roam-capture-p)
                (member (plist-get org-capture-plist :description)
                        *fc-org-no-tag-captures*))
        (cl-return-from fc--capture-tag))

      (when-let* ((buf (plist-get org-capture-plist :original-buffer))
                  (tags (with-current-buffer buf
                          (bound-and-true-p fc-capture-tags))))
        (org-set-tags (fc-string tags))))

    (cl-defun fc--capture-edit ()
      (fc-modal-disable))

    (cl-defun fc--org-get-property (name)
      (when-let* ((keys (org-collect-keywords '("PROPERTY")))
                  (props (cdar keys))
                  (pair (--first
                         (when (cl-equalp name (car (split-string it)))
                           t)
                         props)))
        (string-join (cdr (split-string pair)) " ")))

    (cl-defun fc--auto-ingest ()
      (when-let* ((correct-mode (eq major-mode 'org-mode))
                  (value (fc--org-get-property "AUTOINGEST"))
                  (flag (string-equal "true" (downcase value))))
        (org-babel-lob-ingest buffer-file-name)))

    (add-hook 'org-capture-mode-hook #'fc--capture-edit)
    (add-hook 'org-capture-mode-hook #'fc--capture-copy-region)
    (add-hook 'org-capture-mode-hook #'fc--capture-tag)

    (add-hook 'org-capture-after-finalize-hook #'org-align-tags)
    (add-hook 'org-capture-after-finalize-hook #'fc-modal-enable)

    (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
    (add-hook 'org-mode-hook #'fc--setup-org-mode)
    (add-hook 'org-mode-hook #'valign-mode)

    (add-hook 'after-save-hook #'fc--auto-ingest)
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
                       "*\\1\n\n\\2" :from-start t)

    (fc-replace-regexp "^\\*\\([^\n]+\\)\n+\\([[:alpha:]]+:\\|:PROPERTIES\\|[[:space:]]*CLOSED:\\)"
                       "*\\1\n\\2" :from-start t)))

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
  (save-excursion
    (when (re-search-forward (concat "^[[:space:]]*" (regexp-quote fn) "[[:space:]]*" "\\([^
]+\\)"))
      (match-string 1))))

(defun fc--org-convert-inline-fontnote (regex)
  (while (re-search-forward regex nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (fn (match-string 1)))
      (goto-char start)
      (if (zerop (current-column))
          (goto-char end)
        (delete-region start end)
        (insert "[fn:: " (s-trim (fc--org-find-oneline-footnote fn)) "]")))))

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

(cl-defun fc--org-convert-from-latex ()
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
                ("\\begin{sletter}" "#+BEGIN_QUOTE")
                ("\\end{sletter}" "#+END_QUOTE")
                ("\\begin{verse}" "#+BEGIN_VERSE")
                ("\\end{verse}" "#+END_VERSE")
                ("\\begin{zhverse}" "#+BEGIN_VERSE")
                ("\\end{zhverse}" "#+END_VERSE")
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

(defun fc--org-convert-from-markdown ()
  "Convert latex to org."
  (save-excursion
    (fc--org-add-header
     (fc-search "^title: \\(.+\\)" :begin t :sub 1 :bound 1024)
     (fc-search "^author: \\(.+\\)" :begin t :sub 1 :bound 1024)
     (fc-search "^date: \\(.+\\)" :begin t :sub 1 :bound 1024)
     (fc-search "^language: \\(.+\\)" :begin t :sub 1 :bound 1024))

    (fc-replace-regexp "\\[^\\([^\]\n]+\\)\\]" "[fn:\\1]" :from-start t)

    (fc-replace-regexp "^\\(\\[fn:[^
]+\\]\\): " "\\1 " :from-start t)

    (--each '(("^#### " "**** ")
              ("^### " "*** ")
              ("^## " "** ")
              ("^# " "* "))
      (fc-replace-regexp (cl-first it) (cl-second it) :from-start t))

    (--each '(("\n\n```" "\n\n#+BEGIN_QUOTE")
              ("```\n\n" "#+END_QUOTE\n\n"))
      (fc-replace-string (cl-first it) (cl-second it) :from-start t))

    (fc--org-convert-mk-verse)))

(cl-defun fc--org-fix-fn-number ()
  (let ((ref-num 0)
        (def-num 0))
    (while (re-search-forward "\\[fn:\\([0-9]+\\)\\]" nil t)
      (let ((num (string-to-number (match-string 1))))
        (if (zerop (- (current-column) (- (match-end 0) (match-beginning 0))))
            (progn
              (when (>= def-num num)
                (setq num (1+ def-num))
                (replace-match (format "[fn:%d]" num)))
              (setq def-num num))
          (when (>= ref-num num)
            (setq num (1+ ref-num))
            (replace-match (format "[fn:%d]" num)))
          (setq ref-num num))))))

(cl-defun fc--org-merge-fn-sections ()
  (let (max s)
    (save-excursion
      (goto-char (point-max))
      (setq max (point-marker)))

    (while (re-search-forward "^\\[fn:.+$")
      (when (> (point) (marker-position max))
        (cl-return-from fc--org-merge-fn-sections))

      (setq s (match-string 0))
      (replace-match "")
      (save-excursion
        (goto-char (point-max))
        (insert s "\n\n")))))

(cl-defun fc--org-validate-fn-number ()
  (let ((ref-num 0)
        (def-num 0))
    (while (re-search-forward "\\[fn:\\([0-9]+\\)\\]" nil t)
      (let ((num (string-to-number (match-string 1))))
        (if (zerop (- (current-column) (- (match-end 0) (match-beginning 0))))
            (progn
              (unless (= (- num def-num) 1)
                (unless (y-or-n-p (format "Continue : def-num error %d" num))
                  (cl-return)))
              (setq def-num num))
          (unless (= (- num ref-num) 1)
            (unless (y-or-n-p (format "Continue : ref-num error %d" num))
              (cl-return)))
          (setq ref-num num))))
    (message "done")))

(defun fc--org-fix-fn ()
  (save-excursion
    (fc--org-fix-fn-number))
  (fc--org-merge-fn-sections))

(cl-defun fc-org-portal ()
  "Show org portal."
  (fc-user-select-func
   "Org portal"
   (append
    `(
      ("Add header"			. fc--org-add-header)
      ("Convert footnote (from inline)"	. ,(fc-manual (fc--org-add-footnote
                                                       (read-string
                                                        "Confirm"
                                                        (fc-user-select
                                                         "Footnote regex"
                                                         '("\\[fn:: \\([^\]]+\\)\\]"
                                                           "\\\\footnote{\\([^}]+\\)}"
                                                           "[〔【<\[]注?\\([^\]]+\\)[\]>】〕]"))))))
      ("Convert footnote (to inline)"	. ,(fc-manual (fc--org-convert-inline-fontnote
                                                       (read-string
                                                        "Confirm"
                                                        (fc-user-select
                                                         "Footnote regex"
                                                         '("\\([①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳⑴⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂⒃⒄⒅⒆⒇]\\)"
                                                           "[（(〔【<\[]\\(注?[0-9]+\\)[\]>】〕)）]"))))))
      ("Convert from latex"		. fc--org-convert-from-latex)
      ("Convert from markdown"		. fc--org-convert-from-markdown)
      ("Convert markdown verse"		. fc--org-convert-mk-verse)
      ("Conervt to table"		. fc--org-convert-table)
      ("Fix footnote"			. fc--org-fix-fn)
      ("Ingest"                         . ,(fc-manual (org-babel-lob-ingest buffer-file-name)))
      ("Org ctrl-c-minus"		. org-ctrl-c-minus)
      ("Org Sort"			. org-sort)
      ("Publish"                        . fc--org-publish)
      ("Publish to html"		. org-html-export-to-html)
      ("Publish to markdown"		. org-md-export-to-markdown)
      ("Redisplay inline image"		. org-redisplay-inline-images)
      ("Update dblock"			. org-update-all-dblocks)
      ("Update source block"		. org-babel-execute-buffer)
      ("Validate footnote number"	. fc--org-validate-fn-number)
      )
    *fc-book-func-list*)))

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

(cl-defun fc--org-current-elt ()
  "Current org element."
  (or
   (fc-run-command-chain *fc--org-get-elt*)
   (let* ((context (org-context))
          (1st-elt (caar context))
          (2nd-elt (caadr context))
          (elt (cond
                ((null 1st-elt) nil)
                ((and (eq 1st-elt :item-bullet)
                      (eq 2nd-elt :item))
                 :item-bullet)
                ((null 2nd-elt) 1st-elt)
                (t 2nd-elt))))
     elt)))

(push #'(lambda () (and (looking-at-p "\\$[^\\$]+\\$") :latex-fragment)) *fc--org-get-elt*)
(push #'(lambda () (and (fc--org-looking-over-footnote) :footnote)) *fc--org-get-elt*)
(push #'(lambda () (and (fc--org-looking-include) :include)) *fc--org-get-elt*)

(cl-defmacro fc--org-smart-action (default &rest body)
  "Smart action according to current position.
DEFAULT: defaul function.
BODY: usually a pcase block."
  (declare (indent 1))

  `(when (eq major-mode 'org-mode)
     (let ((elt (fc--org-current-elt)))
       (if (null elt)
           (when ,default
             (fc-funcall ,default))
         ,@body))))

(cl-defun fc--org-looking-over-footnote ()
  "Test if current point is over a footnote."
  (org-in-regexp "\\[fn:[^\]]+\\]"))

(cl-defun fc--org-looking-include ()
  "Test if current point is over a include."
  (org-in-regexp "^#\\+include: .+"))

(cl-defun fc--org-show-footnote ()
  (when-let ((fn (org-footnote-at-reference-p))
             (fn-def (and (car fn)
                          (org-footnote-get-definition (car fn))))
             (text (nth 3 fn-def)))
    (setf *fc-org-pop-footnote* t)
    (fc-popup-tip text)))

(cl-defun fc--org-hide-footnote ()
  (when *fc-org-pop-footnote*
    (setf *fc-org-pop-footnote* nil)
    (fc-popup-tip-hide)))

(cl-defun fc--org-dwell ()
  (fc--org-smart-action nil
    (pcase elt
      (:footnote (fc--org-show-footnote))
      (:link (when-let* ((msg (not (current-message)))
                         (range (alist-get :link (org-context)))
                         (cell-text (fc--org-current-cell))
                         (text (or (unless (zerop (length cell-text))
                                     (fc-remove-properties cell-text)
                                     cell-text)
                                   (apply #'buffer-substring-no-properties
                                          range)))
                         (match (string-match "\\[\\[\\([^\]]+\\)" text))
                         (link (substring text (match-beginning 1) (match-end 1))))
               (message "Link: %s" link))))))

(cl-defun fc--org-do ()
  "Smart do."
  (fc--org-smart-action #'org-ctrl-c-ctrl-c
    (pcase elt
      (:checkbox (org-ctrl-c-ctrl-c))
      (:footnote (org-footnote-action))
      (:headline (org-insert-heading-respect-content)
                 (fc-modal-disable))
      (:include (fc--org-toggle-special-edit))
      (:item (fc--org-do-intert-item))
      (:item-bullet (org-ctrl-c-minus))
      ((or :latex-fragment :latex-preview)
       (org-latex-preview))
      (:link (org-open-at-point))
      (:src-block (org-ctrl-c-ctrl-c))
      (:tags (org-set-tags-command))
      (:timestamp (fc-funcall #'org-time-stamp))
      (:todo-keyword (fc--org-todo))
      (_ (message "context: %s elt: %s" context elt)))))

(cl-defun fc--org-todo ()
  "Change todo state."
  (let ((org-use-fast-todo-selection (when *fc-ergo-prefix*
                                       'export)))
    (org-todo)))

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

(defun fc--org-toggle-hideshow ()
  "Toggle hideshow by org context."
  (fc--org-smart-action nil
    (pcase elt
      (:src-block
       (save-excursion
         (unless (org-at-block-p)
           (re-search-backward "^ *#\\+BEGIN"))
         (org-cycle)
         t))

      (_ nil))))

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

(defun fc--org-insert-portal ()
  (fc-user-select-func
   "Org insert"
   `(
     ("clock in" . org-clock-in)
     ("clock out" . org-clock-out)
     ("deadline"  . org-deadline)
     ("schedule" . org-schedule)
     )))

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
     ("b" ,(fc-manual
            (unless (region-active-p)
              (er/mark-symbol))
            (fc-funcall #'org-emphasize)))
     ("c" fc--org-ctrl-c-ctrl-c)
     ("d" ,(fc-manual
            (if (org-at-encrypted-entry-p)
                (fc-funcall #'org-decrypt-entry)
              (fc-funcall #'org-encrypt-entry))))
     ("e" fc--org-end)

     ("f b" ,(fc-decorate-region "*" "*"))
     ("f i" ,(fc-decorate-region "/" "/"))
     ("f u" ,(fc-decorate-region "_" "_"))

     ("g" fc--org-copy)

     ("i c" ,(fc-cond-key :normal 'org-cliplink
                          :region (fc-manual (fc-org-add-block "COMMENT"))))
     ("i d" org-insert-drawer)
     ("i e" ,(fc-cond-key :normal nil
                          :region (fc-manual (fc-org-add-block "EXAMPLE"))))
     ("i f" fc--org-insert-formula)
     ("i g" ,(fc-manual (insert "[fn:: ") (yank) (insert "]")))
     ("i i" fc--org-convert)
     ("i n" ,(fc-cond-key :normal 'org-roam-node-insert
                          :region (fc-manual (fc-org-add-block "NOTE"))))
     ("i p" org-priority)
     ("i q" ,(fc-manual (fc-org-add-block "QUOTE")))
     ("i t" org-time-stamp)
     ("i u" ,(fc-manual (fc-org-add-block "SRC" :ask '("Output file" "plantuml :file output/"))))
     ("i v" ,(fc-manual (fc-org-add-block "VERSE" :pre-format #'fc--org-format-verse :copy nil)))
     ("i C" ,(fc-manual (fc-org-add-block "COMMENT")))
     ("i E" ,(fc-manual (fc-org-add-block "EXAMPLE" :copy nil)))
     ("i G" ,(fc-manual (fc-funcall #'org-footnote-new) (insert " ")))
     ("i N" ,(fc-manual (fc-org-add-block "NOTE")))
     ("i T" fc--org-insert-title)
     ("i SPC" fc--org-insert-portal)

     ("l" org-insert-link)
     ("m" ,(fc-cond-key :normal #'org-mark-element
                        :region #'org-ctrl-c-minus))
     ("o" org-open-at-point)
     ("p" org-set-property)
     ("s" ,(fc-manual (fc-org-add-block "SRC" :ask "Programming language")))
     ("t" fc--org-todo)
     ("u" fc--org-do)
     ("v t" ,(fc-manual (org-tags-view t)))
     ("v T" org-tags-view)
     ("y" ,(fc-cond-key :normal 'fc--org-sparse-tree
                        :region 'fc--org-occur))
     ("x c" org-cut-subtree)
     ("x o" org-copy-subtree)
     ("x p" org-paste-subtree)
     ("A" org-archive-subtree)
     ("B" org-roam-buffer-toggle)
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
  "KEYS b: emphasize  c: C-c C-c  d: de/en-crypt  i c: clip link  i d: drawer  i f: formula  i n: roam node  i q: quote  i t: timestamp  i u: uml  i N: note  i T: insert title  l: link  m: mark element  o: open  s: add src  t: todo  v t:  view tags  v T: view tags TODO  y: show todo tree  C i: clock in  C o: clock out  A: archive  B: backlink  D: deadline  S: schedule  T: set tag  -: C-c minus  ^: sort.")

(cl-defun fc-org-mode-func ()
  "Mode func."
  (fc-modal-head-key "Org" '*fc-org-map*))

(defconst *fc-org-agenda-map*
  (fc-make-keymap
   `(
     ("c" org-agenda-columns)
     ("m" org-agenda-month-view)
     ("t" org-agenda-todo)
     ("w" org-agenda-week-view)
     )
   "fc-org-agenda-map"
   *fc-org-map*)
  "KEYS c: columns  m: month  t: todo  w: week.")

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

  (setf org-agenda-files `(,*fc-org-dir*)
        org-todo-keywords '((sequence "TODO(t!)" "NEXT(n)" "|" "DONE(d!/!)" "CANCELLED(c!/!)" "DELEGATED(D!/!)" )
                            (sequence "REMIND(r)" "|" "DONE")
                            (sequence "SOMEDAY(s)" "TODO" "|")
                            (type "WAITING" "|"))
        org-use-fast-todo-selection 'export
        org-confirm-babel-evaluate #'fc--org-confirm-babel-evaluate
        org-agenda-block-separator "───────────────────────────────────────────")

  (when (fboundp #'fc-user-org-config)
    (fc-user-org-config))

  (setf org-capture-templates nil)

  (fc-org-add-capture-template *fc-org-capture-template*)
  (fc-org-add-capture-template *fc-org-user-capture-templates*)

  (--each *fc-org-capture-raw-templates*
    (add-to-list 'org-capture-templates it)))

(cl-defun fc--org-capture-p ()
  (when-let* ((plist org-capture-plist)
              (marker (plist-get plist :begin-marker)))
    (eq (current-buffer) (marker-buffer marker))))

(cl-defun fc-org-agenda-customize (&key project)
  (setf *fc-agenda-list*
        (cl-loop
         for x in project
         append
         `((tags-todo ,x
                      ((org-agenda-overriding-header ,(format "Project <<%s>>:" x)))))))

  (setf org-agenda-custom-commands
        `(("x" "Study and Book-reading"
           ((agenda "")
            (tags-todo "study"
                       ((org-agenda-overriding-header "Study:")))
            (tags-todo "book"
                       ((org-agenda-overriding-header "Book-reading:")))))
          ("X" "My Agenda"
           ((agenda "")
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions:")))
            ,@*fc-agenda-list*)))))

(cl-defun fc-org-agenda ()
  (interactive)

  (org-agenda nil "X"))

(cl-defun fc-org-study-agenda ()
  (interactive)

  (org-agenda nil "x"))

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

(defun fc--org-mode-chapter-mark (level)
  (s-repeat level "*"))

(defun fc--org-run-src-block (name)
  (fc-whole-buffer
    (org-babel-goto-named-src-block name)
    (when (looking-at-p "#\\+BEGIN_SRC")
      (org-ctrl-c-ctrl-c))))

(cl-defun fc--org-publish (&optional (output-dir (fc--org-get-property "publish"))
                                     (base-dir default-directory))
  (unless output-dir
    (message "Cannot find publish directory")
    (cl-return-from fc--org-publish))

  (let ((ob-async-no-async-languages-alist '("plantuml" "python" "shell" "elisp")))
    (setq org-publish-project-alist
          `(
            ("org-notes"
             :base-directory ,base-dir
             :base-extension "org"
             :publishing-directory ,output-dir
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4
             :auto-preamble t
             )

            ("org-static"
             :base-directory ,base-dir
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|svg"
             :publishing-directory ,output-dir
             :recursive t
             :publishing-function org-publish-attachment
             )

            ("org" :components ("org-notes" "org-static"))))

    (org-publish-current-project)))

(when (eq major-mode 'org-mode)
  (fc--setup-org-mode))

(provide 'fc-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org.el ends here
