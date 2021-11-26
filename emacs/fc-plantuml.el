;;; fc-plantuml.el --- setup plantuml environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defconst *fc-plant-uml-map*
  (fc-make-keymap
   `(
     ("c" plantuml-preview)
     ("D" plantuml-download-jar)
     )
   "fc-plant-uml-map"
   *fc-func-mode-map*)
  "KEYS c: preview  D: update jar  E: org edit exit.")

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

(cl-defun fc--insert-note ()
  "return note command."
  (let* ((pos (fc-user-select "Note type" '("left" "over" "right")))
         (arg (when (string= pos "over")
                (concat " " (read-string "Actor/Participant")))))
    (concat pos arg)))

(provide 'fc-plantuml)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-plantuml.el ends here
