;;; fc-yasnippet.el --- setup yasnippet -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-install 'yasnippet-snippets)

(fc-load 'yasnippet
  :after (progn
           (add-to-list 'yas-snippet-dirs
                        (fc-home-path "fconfig/snippets"))

           (fc-unbind-keys '("TAB" "<tab>") yas-minor-mode-map)

           (yas-global-mode +1)))

(defun fc-expand-snippet (name)
  (interactive)

  (insert name)
  (fc-modal-disable)
  (yas-expand))

(defun fc-expand-snippet-file (filename)
  (interactive)

  (let ((snippet (fc-file-to-string filename)))
    (yas-expand-snippet snippet)))

(provide 'fc-yasnippet)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-yasnippet.el ends here
