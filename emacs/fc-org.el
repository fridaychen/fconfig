;;; fc-org.el --- setup org -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-install 'org-superstar)

(fc-load 'org
  :after
  (progn
    (setf org-hide-emphasis-markers t
          )

    (require 'ob-gnuplot)
    (require 'ob-octave)

    (cl-defun fc--setup-org-mode ()
      (org-superstar-mode 1)
      (visual-line-mode 1))

    (add-hook 'org-mode-hook #'fc--setup-org-mode)))

(cl-defun fc-org-add-header ()
  (goto-char (point-min))

  (insert "#+auther: " (read-string "Author : ") "\n"
          "#+date <" (read-string "Date : ") ">\n"
          "#+title " (read-string "Title : ")  "\n"))

(cl-defun fc-org-add-var ()
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

(defconst *fc-org-map*
  (fc-make-keymap
   `(
     ("c" org-ctrl-c-ctrl-c)
     ("d" org-insert-drawer)
     ("i T" org-time-stamp)
     ("l" org-insert-link)
     ("o" org-open-at-point)
     ("s" fc-org-add-source-block)
     ("t" org-todo)
     ("v t" ,(fc-manual (org-tags-view t)))
     ("v T" org-tags-view)
     ("E" org-edit-special)
     ("C i" org-clock-in)
     ("C o" org-clock-out)
     ("T" org-set-tags-command)
     ("SPC" fc-org-portal))
   "fc-org-map"
   *fc-punctuation-map*)
  "KEYS c: C-c C-c  d: drawer  e: edit special  l: link  o: open  t: todo  s: add src  t: todo  v t:  view tags  v T: view tags TODO  C i: clock in  C o:clock out  T: set tag.")

(cl-defun fc-org-mode-func ()
  (fc-modal-head-key "Org" '*fc-org-map*))

(cl-defun fc-org-agenda-mode-func ()
  (fc-modal-head-key "Org" '*fc-org-map*))

(let* ((org-dir "~/org/")
       (remind-file (concat org-dir "remind.org"))
       (knowledge-file (concat org-dir "knowledge.org")))

  (cl-defun fc--org-init-dir ()
    (unless (fc-dir-exists-p org-dir)
      (make-directory org-dir))

    (fc-create-file-if-not-exists remind-file
                                  "# remind file\n")

    (fc-create-file-if-not-exists knowledge-file
                                  "# knowledge file\n"))

  (cl-defun fc-org-autoconfig ()
    (fc--org-init-dir)

    (setf org-agenda-files
          (directory-files "~/org" t "org$"))

    (setf org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w)" "REMIND(r)"
                      "|"
                      "DONE(d)" "SOMEDAY(s)")))

    (setf org-capture-templates
          `(("t" "Todo" entry (file+headline
                               ,remind-file
                               "Capture")
             "* TODO %? (wrote on %U)")
            ("k" "Knowledge" entry (file+headline
                                    ,knowledge-file
                                    "TOP")
             "* %?\n  # Wrote on %U")))))

(when (eq major-mode 'org-mode)
  (fc--setup-org-mode))

(provide 'fc-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org.el ends here
