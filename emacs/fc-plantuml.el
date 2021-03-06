;;; fc-plantuml.el --- setup plantuml environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defconst *fc-plant-uml-map*
  (fc-make-keymap
   `(
     ("c" plantuml-preview)
     ("U" plantuml-download-jar)
     )
   "fc-plant-uml-map"
   *fc-func-mode-map*)
  "KEYS c: preview  E: org edit exit  U: update jar.")

(cl-defun fc-plantuml-mode-func ()
  (fc-modal-head-key "Plant-uml" '*fc-plant-uml-map*))

(fc-load 'plantuml-mode
  :after (progn
           (let ((jar (format "%s/site/resource/plantuml.jar" *fc-home*)))
             (setf plantuml-jar-path jar
                   plantuml-default-exec-mode 'jar
                   org-plantuml-jar-path jar))

           (unless (file-exists-p plantuml-jar-path)
             (plantuml-download-jar))

           (add-to-list 'auto-mode-alist
                        '("\\.plantuml\\'" . plantuml-mode))

           (with-eval-after-load 'org
             (require 'ob-plantuml))))

(fc-load 'flycheck-plantuml
  :after (progn
           (flycheck-plantuml-setup)))

(provide 'fc-plantuml)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-plantuml.el ends here
