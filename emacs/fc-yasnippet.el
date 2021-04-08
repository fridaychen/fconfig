;;; fc-yasnippet.el --- setup yasnippet -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-install 'yasnippet)

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs
               (expand-file-name
                "~/.emacs.d/fconfig/snippets"))

  (fc-unbind-keys '("TAB" "<tab>") yas-minor-mode-map)

  (yas-global-mode +1))

(fc-install 'yasnippet-snippets)

(defun fc-expand-snippet (name)
  (interactive)

  (insert name)
  (fc-modal-global-mode -1)
  (yas-expand))

(provide 'fc-yasnippet)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-yasnippet.el ends here
