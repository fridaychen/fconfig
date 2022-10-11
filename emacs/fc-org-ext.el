;;; fc-org-ext.el --- org-mode extension -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc-mybook-home* (expand-file-name "~/Documents/mybook"))

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
  (s-trim
   (fc-exec-command-to-string
    "ff"
    (list
     "-nocolor"
     filename))))

(defun fc--mybook-open (path _)
  "Open file in mybook."
  (let* ((default-directory *fc-mybook-home*)
         (filename (-find-book path))
         (ext (downcase (file-name-extension filename))))
    (cond
     ((s-blank? filename)
      (message "Mybook %s is not found" path))

     ((member ext '("md" "org" "txt"))
      (find-file (format "%s/%s" *fc-mybook-home* filename)))

     (t
      (message "open with a external app %s" filename)
      (fc-exec-command "fj" "--open" filename)))))

(defvar org-babel-default-header-args:packetdiag
  '(
    (:results . "file")
    (:exports . "results")
    (:tool    . "packetdiag")
    (:transparency . t)
    (:antialias . nil)
    (:font    . nil)
    (:size    . nil)
    (:type    . "svg"))
  "Default arguments for drawing a packetdiag image.")

(add-to-list 'org-src-lang-modes '("packetdiag" . blockdiag))
(defalias 'org-babel-execute:packetdiag 'org-babel-execute:blockdiag)

(provide 'fc-org-ext)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-org-ext.el ends here
