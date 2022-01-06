;;; fc-org-ext.el --- org-mode extension -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(org-link-set-parameters "music"
                         :follow #'fc--music-open)

(defun fc--music-open (path _)
  (fc-exec-command "quodlibet" "--query" path)
  (sit-for 1)
  (fc-exec-command "quodlibet" "--next"))

(defvar org-babel-default-header-args:packetdiag
  '(
    (:results . "file")
    (:exports . "results")
    (:tool    . "packetdiag3")
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
