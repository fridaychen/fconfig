;;; fc-org.el --- setup org -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-org-dir* "~/org/")
(defvar *fc-org-captrue-template*
  `(
    ("t" "Todo" "remind.org" "Inbox"
     "* TODO %?\n  %T")
    ("k" "Knowledge" "knowledge.org" "Inbox"
     "* %?\n  # Wrote on %U")
    ))

(fc-install 'org-superstar)

(fc-load 'org
  :after
  (progn
    (setf org-hide-emphasis-markers t
          org-log-done t
          )

    (require 'org-agenda)
    (require 'org-capture)
    (require 'ob-gnuplot)
    (require 'ob-octave)

    (cl-defun fc--setup-org-mode ()
      (org-superstar-mode 1)
      (visual-line-mode 1))

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

    (add-hook 'org-mode-hook #'fc--setup-org-mode)))

(cl-defun fc-org-add-header ()
  "Add header."
  (goto-char (point-min))

  (insert "#+auther: " (read-string "Author : ") "\n"
          "#+date <" (read-string "Date : ") ">\n"
          "#+title " (read-string "Title : ")  "\n"))

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

(cl-defun fc-org-add-block (type param)
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
    (insert "#+BEGIN_" type " " param "\n")
    (if content
        (yank)
      (setf point-of-content (point))
      (insert "\n"))
    (insert "#+END_" type "\n")

    (when point-of-content
      (goto-char point-of-content))))

(cl-defun fc-org-add-source-block ()
  "Add source block."
  (let ((lang (read-string "Programming language : ")))
    (fc-org-add-block "SRC" lang)))

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

(cl-defun fc--org-do ()
  "Execute action according to current context."
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
    (when (null elt)
      (cl-return-from fc--org-do))

    (message "context: %s  elt: %s" context elt)
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

(defconst *fc-org-map*
  (fc-make-keymap
   `(
     ("c" fc--org-ctrl-c-ctrl-c)
     ("i d" org-insert-drawer)
     ("i t" org-time-stamp)
     ("l" org-insert-link)
     ("o" org-open-at-point)
     ("s" fc-org-add-source-block)
     ("t" org-todo)
     ("u" fc--org-do)
     ("v t" ,(fc-manual (org-tags-view t)))
     ("v T" org-tags-view)
     ("A" org-archive-subtree)
     ("C i" org-clock-in)
     ("C o" org-clock-out)
     ("D" org-deadline)
     ("E" org-edit-special)
     ("L" org-todo-list)
     ("S" org-schedule)
     ("T" org-set-tags-command)
     ("-" org-ctrl-c-minus)
     ("^" org-sort)
     ("SPC" fc-org-portal))
   "fc-org-map"
   *fc-func-mode-map*)
  "KEYS c: C-c C-c  i d: drawer  i t: timestamp  l: link  o: open  t: todo  s: add src  t: todo  v t:  view tags  v T: view tags TODO  C i: clock in  C o: clock out  A: archive  D: deadline  E: edit special  S: schedule  T: set tag  -: C-c minus  ^: sort.")

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
                                      "DONE(d)" "SOMEDAY(s)")))

  (--each *fc-org-captrue-template*
    (add-to-list 'org-capture-templates
                 `(,(cl-first it)
                   ,(cl-second it)
                   entry
                   (file+headline
                    ,(concat *fc-org-dir* (cl-third it))
                    ,(cl-fourth it))
                   ,(cl-fifth it)))))

(cl-defun fc--before-agenda (&rest _rest)
  "Wrapper function."
  (setf org-agenda-files (directory-files *fc-org-dir* t "^[^#].+org$")))

(when (eq major-mode 'org-mode)
  (fc--setup-org-mode))

(provide 'fc-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org.el ends here
