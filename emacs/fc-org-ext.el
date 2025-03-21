;;; fc-org-ext.el --- org-mode extension -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc-mybook-candidates* (list
                                "~/Documents/"
                                "~/Google Drive/My Drive/"))

(defvar *fc-mybook-homes* nil)

(fc-each *fc-mybook-candidates*
  (when (fc-dir-exists-p it)
    (add-to-list '*fc-mybook-homes* (expand-file-name it) t)))

(org-link-set-parameters "music"
                         :follow #'fc--music-open)

(defun fc--music-open (path _)
  (fc-quodlibet-cmd (format "query %s" path))
  (sit-for 1)
  (fc-quodlibet-cmd "next"))

(org-link-set-parameters "mybook"
                         :follow #'fc--mybook-open)

(defun -find-book (filename)
  "Find book by filename."
  (car
   (string-split (string-trim
                  (fc-exec-command-to-string "ff"
                    (list
                     "-nocolor"
                     filename)))
                 "\n")))

(cl-defun fc--mybook-parse (link)
  (cond
   ((string-match "\\(.+\\)\\(::.+\\)" link)
    (list (substring link
                     (match-beginning 1)
                     (match-end 1))
          (substring link
                     (+ 2 (match-beginning 2))
                     (match-end 2))))
   (t
    (list link nil))))

(cl-defun fc--mybook-open-pdf (path arg)
  (cond (*is-linux*
         (fc-exec-command "evince" (format "--page-label=%s" arg) path))

        (*is-mac*
         (fc-exec-command "previewtopage.sh" path arg))))

(cl-defun fc--mybook-open (link _)
  "Open file in mybook."

  (seq-let (path arg) (fc--mybook-parse link)
    (fc-each *fc-mybook-homes*
      (let* ((default-directory it)
             (filename (-find-book path))
             (ext (downcase (or (file-name-extension filename) "")))
             (full-path (format "%s/%s" it filename)))
        (cond
         ((string-blank-p filename))

         ((and ext (member ext '("md" "org" "txt")))
          (find-file full-path)
          (cl-return-from fc--mybook-open))

         ((and (equal ext "pdf") arg)
          (fc--mybook-open-pdf full-path arg)
          (cl-return-from fc--mybook-open))

         (t
          (message "open with a external app %s" filename)
          (fc-exec-command "fj" "--open" full-path)
          (cl-return-from fc--mybook-open)))))

    (message "Mybook %s is not found" path)))

(defvar org-babel-default-header-args:packetdiag
  '(
    (:results . "file")
    (:exports . "results")
    (:tool    . "packetdiag")
    (:transparency . t)
    (:antialias . t)
    (:font    . nil)
    (:size    . nil)
    (:type    . "png"))
  "Default arguments for drawing a packetdiag image.")

(add-to-list 'org-src-lang-modes '("packetdiag" . blockdiag))
(defalias 'org-babel-execute:packetdiag 'org-babel-execute:blockdiag)

(provide 'fc-org-ext)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org-ext.el ends here
