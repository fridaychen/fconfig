;;; fc-org-ext.el --- org-mode extension -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc-mybook-home* (expand-file-name "~/Documents/mybook"))

(org-link-set-parameters "music"
                         :follow #'fc--music-open)

(defun fc--music-open (path _)
  (fc-exec-command "quodlibet" "--query" path)
  (sit-for 1)
  (fc-exec-command "quodlibet" "--next"))

(org-link-set-parameters "mybook"
                         :follow #'fc--mybook-open)

(defun -find-book (filename)
  (let ((default-directory *fc-mybook-home*))
    (s-trim
     (fc-exec-command-to-string
      "ff"
      (list
       "-nocolor"
       filename)))))

(defun fc--mybook-open (path _)
  (let ((file (-find-book path)))
    (if (s-blank? file)
        (message "Mybook %s is not found" path)
      (find-file (format "%s/%s" *fc-mybook-home* file)))))

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
