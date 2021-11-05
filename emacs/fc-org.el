;;; fc-org.el --- setup org -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

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
     "* TODO %? %a\n  %T")
    ))

(defconst *fc-org-captrue-raw-templates*
  `(
    ("B" "Bookmark" plain (file+headline "bookmarks.org" "Inbox") "[[%(org-cliplink-clipboard-content)][%^{Title}]]\n")
    ))

(defvar *fc-org-user-capture-templates* nil)

(defvar *fc-org-trust-babel-modes* '("plantuml"))

(fc-install 'gnuplot
            'org-cliplink
            'org-link-beautify
            'org-plus-contrib
            'org-superstar)

(fc-load 'org
  :after
  (progn
    (setf org-hide-emphasis-markers t
          org-log-done t
          org-export-with-sub-superscripts nil
          org-src-ask-before-returning-to-edit-buffer nil
          )

    (require 'org-agenda)
    (require 'org-capture)
    (require 'ob-gnuplot)
    (require 'ob-octave)
    (require 'ob-python)
    (require 'ob-shell)
    (require 'ol-man)

    (cl-defun fc--setup-org-mode ()
      (org-superstar-mode 1)
      (org-link-beautify-mode -1)
      (visual-line-mode 1)
      (when (= (buffer-size) 0)
        (fc-org-add-header)))

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
      (let ((tags
             (with-current-buffer (plist-get org-capture-plist :original-buffer)
               (when (boundp 'fc-capture-tags)
                 fc-capture-tags))))
        (org-set-tags (fc-string tags))))

    (cl-defun fc--capture-edit ()
      (insert " ")
      (fc-modal-global-mode -1))

    (add-hook 'org-capture-mode-hook #'fc--capture-edit)
    (add-hook 'org-capture-mode-hook #'fc--capture-copy-region)
    (add-hook 'org-capture-mode-hook #'org-align-all-tags)
    (add-hook 'org-capture-mode-hook #'fc--capture-tag)

    (add-hook 'org-capture-after-finalize-hook #'fc-modal-global-mode)
    (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
    (add-hook 'org-mode-hook #'fc--setup-org-mode)))

(cl-defun fc-org-add-header ()
  "Add header."
  (goto-char (point-min))

  (insert "#+TITLE " (read-string "Org file title")  "\n"
          "\n"))

(cl-defun fc-org-add-var ()
  "Add var."
  (save-excursion
    (goto-char (point-max))

    (when (/= (current-column) 0)
      (end-of-line)
      (newline))

    (insert "
#+STARTUP: content
#+STARTUP: hideblocks
#+STARTUP: inlineimages
#+STARTUP: overview
#+STARTUP: showall
#+STARTUP: showeverything
")))

(cl-defun fc-org-add-block (type &key ask)
  "Add block.
TYPE: type of block.
PARAM: parameter of block."
  (when (and (not (region-active-p))
             (/= (current-column) 0))
    (end-of-line)
    (insert "\n\n"))

  (let ((content (when (region-active-p)
                   (kill-region (region-beginning)
                                (region-end))
                   t))
        (point-of-content nil))
    (insert (fc--text " "
                      (concat "#+BEGIN_" type)
                      (fc-ask ask))
            "\n")
    (if content
        (yank)
      (setf point-of-content (point))
      (insert "\n"))
    (insert "#+END_" type "\n")

    (when point-of-content
      (goto-char point-of-content))))

(cl-defun fc-org-portal ()
  "Show org portal."
  (fc-user-select-func
   "Org portal"
   `(
     ("Add header"              .       fc-org-add-header)
     ("Init org var"            .       fc-org-add-var)
     ("Publish to html"         .       org-html-export-to-html)
     ("Publish to markdownad"   .       org-md-export-to-markdown)
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

  (fc-modal-global-mode-off))

(cl-defmacro fc--org-smart-action (default &rest body)
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
     (if (null elt)
         (progn
           (fc-funcall ,default)
           (cl-return-from fc--org-smart-action))
       ,@body)))

(cl-defun fc--org-do ( )
  (fc--org-smart-action #'org-ctrl-c-ctrl-c
    (pcase elt
      (:checkbox (org-ctrl-c-ctrl-c))
      (:headline (org-insert-heading-respect-content))
      (:item (fc--org-do-intert-item))
      (:item-bullet (org-ctrl-c-minus))
      (:link (org-open-at-point))
      (:src-block (org-ctrl-c-ctrl-c))
      (:tags (org-set-tags-command))
      (:timestamp (fc-funcall #'org-time-stamp))
      (:todo-keyword (org-todo))
      (_ (message "context: %s" context)))))

(defun fc-org-mode-mouse-func (_event)
  (fc--org-do))

(defvar *fc--org-last-year* "")

(defun fc--org-convert ()
  (or
   (fc-replace-looking-text "\\([0-9]+\\)[年/-]\\([0-9]+\\)[月/-]\\([0-9]+\\)[日号]?"
     (setf *fc--org-last-year* (string-to-number (match-string 1)))
     (format "<%d-%02d-%02d>"
             (string-to-number (match-string 1))
             (string-to-number (match-string 2))
             (string-to-number (match-string 3))))

   (fc-replace-looking-text "\\([0-9]+\\)[年/-]\\([0-9]+\\)[月]"
     (setf *fc--org-last-year* (string-to-number (match-string 1)))
     (format "<%d-%02d>"
             (string-to-number (match-string 1))
             (string-to-number (match-string 2))))

   (fc-replace-looking-text "\\([0-9]+\\)[月/-]\\([0-9]+\\)[日号]?"
     (format "<%d-%02d-%02d>"
             *fc--org-last-year*
             (string-to-number (match-string 1))
             (string-to-number (match-string 2))))))

(defun fc--org-occur ()
  (org-occur (fc-current-thing :ask t :regq t :confirm "Org match")))

(defun fc--org-sparse-tree ()
  (fc--org-smart-action #'org-sparse-tree
    (pcase elt
      (:headline (fc--org-occur))
      (:tags (fc-funcall #'org-match-sparse-tree))
      (:todo-keyword (fc-funcall #'org-show-todo-tree))
      (_ (fc-funcall #'org-sparse-tree)))))

(defconst *fc-org-map*
  (fc-make-keymap
   `(
     ("9" org-promote)
     ("0" org-demote)
     ("b" org-emphasize)
     ("c" fc--org-ctrl-c-ctrl-c)
     ("i c" org-cliplink)
     ("i d" org-insert-drawer)
     ("i i" fc--org-convert)
     ("i n" org-roam-node-insert)
     ("i q" ,(fc-manual (fc-org-add-block "QUOTE")))
     ("i t" org-time-stamp)
     ("i u" ,(fc-manual (fc-org-add-block "SRC" :ask '("Output file" "plantuml :file "))))
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
     ("-" org-ctrl-c-minus)
     ("^" org-sort)
     ("SPC" fc-org-portal))
   "fc-org-map"
   *fc-func-mode-map*)
  "KEYS b: emphasize  c: C-c C-c  i c: clip link  i d: drawer  i q: quote  i t: timestamp  i u: uml  l: link  m: mark element  o: open  t: todo  s: add src  t: todo  v t:  view tags  v T: view tags TODO  y: show todo tree  C i: clock in  C o: clock out  A: archive  D: deadline  S: schedule  T: set tag  -: C-c minus  ^: sort.")

(cl-defun fc-org-mode-func ()
  "FC org-mode func."
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

  (setf org-capture-templates nil)

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
  "Trust all mode in *fc-org-trust-babel-modes."
  (not (member lang *fc-org-trust-babel-modes*)))

(fc-load 'org-roam
  :after
  (progn
    (setf org-roam-directory "~/org/roam"
          org-roam-v2-ack t)
    (org-roam-db-autosync-mode)
    ))

(when (eq major-mode 'org-mode)
  (fc--setup-org-mode))

(provide 'fc-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org.el ends here
