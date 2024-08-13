;;; fc-plantuml.el --- setup plantuml environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defconst *fc-plant-uml-map*
  (fc-make-keymap
   `(
     ("c" plantuml-preview)

     ("f b" ,(fc-decorate-region "**" "**"))
     ("f i" ,(fc-decorate-region "//" "//"))
     ("f u" ,(fc-decorate-region "__" "__"))

     ("D" plantuml-download-jar)
     )
   "fc-plant-uml-map"
   *fc-func-mode-map*)
  "KEYS c: preview  D: update jar  E: org edit exit.")

(cl-defun fc--plantuml-mode-func ()
  (fc-modal-head-key "Plant-uml" '*fc-plant-uml-map*))

(fc-load 'plantuml-mode
  :after (progn
           (fc-add-mode-name 'plantuml-mode "🛸")

           (setenv "PLANTUML_SECURITY_PROFILE" "UNSECURE")

           (let ((jar (format "%s/site/resource/plantuml.jar" *fc-home*)))
             (setf plantuml-jar-path jar
                   plantuml-default-exec-mode 'jar
                   org-plantuml-jar-path jar))

           (unless (file-exists-p plantuml-jar-path)
             (plantuml-download-jar))

           (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
           (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

           (cl-defun fc--plantuml-setup ()
             (setq indent-tabs-mode nil
                   plantuml-indent-level 4))

           (add-hook 'plantuml-mode-hook #'fc--plantuml-setup)
           (fc-add-fmt 'plantuml-mode nil #'fc--default-fmt-with-indent)

           (with-eval-after-load 'org
             (require 'ob-plantuml))))

(fc-load 'flycheck-plantuml
  :after (progn
           (flycheck-plantuml-setup)))

(cl-defun fc--insert-note ()
  "Return note command."
  (let* ((pos (fc-select "Note type" '("left" "over" "right")))
         (arg (when (string= pos "over")
                (concat " " (read-string "Actor/Participant")))))
    (concat pos arg)))

(cl-defun fc--insert-component-note ()
  "Return note command."
  (fc-select "Note type" '("top" "left" "right" "bottom")))

(fc-load 'ob-async
  :after (add-hook 'ob-async-pre-execute-src-block-hook
                   `(lambda ()
                      (require 'ob-plantuml)
                      (setq org-plantuml-jar-path ,org-plantuml-jar-path))))

(provide 'fc-plantuml)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-plantuml.el ends here
