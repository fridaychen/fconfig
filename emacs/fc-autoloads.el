;;; fc-autoload.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; buildin
(autoload #'xref--find-definitions "xref")
(autoload #'xref--find-xrefs "xref")

;; app
(autoload #'fc-load-desktop "fc-app")
(autoload #'fc-save-desktop "fc-app")

;; calendar
(autoload #'fc-calendar "fc-calendar")

;; computer
(autoload #'fc-auto-config "fc-computer")

;; config
(autoload #'fc-conf-new "fc-config")
(autoload #'fc-conf-open "fc-config")

;; dict
(autoload #'fc-dict-lookup "fc-dict")
(autoload #'fc-make-keymap "fc-keymaps")

;; font
(autoload #'fc-setup-font "fc-font")

;; format
(defvar-local *fc-format-at-save* t)

(autoload #'fc-fmt-buffer "fc-format")
(autoload #'fc-add-fmt "fc-format")

;; ifdef
(autoload #'mark-ifdef "ifdef")

;; layout
(autoload #'fc-layout-save "fc-layout")
(autoload #'fc-layout-switch "fc-layout")

;; modal
(autoload #'fc-modal-global-mode "fc-modal")

;; program
(autoload #'fc-program "fc-program")

;; theme
(autoload #'fc-load-theme "fc-theme-config")
(autoload #'fc-right-bottom-window-p "fc-theme-config")
(autoload #'fc-light-theme "fc-theme-config")
(autoload #'fc-dark-theme "fc-theme-config")
(autoload #'fc-deep-dark-theme "fc-theme-config")

;; util
(autoload #'fc-has-battery "fc-util")

;; viewer
(autoload #'fc-viewer-mode-p "fc-viewer")

;; git
(autoload #'magit-get-current-branch "magit")

;; gitignore
(dolist (pattern (list "/\\.gitignore\\'"
                       "/info/exclude\\'"
                       "/git/ignore\\'"
                       "/\\.ignore\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'fgitignore-mode)))

(autoload #'fgitignore-mode "fc-gitignore-mode")

(autoload #'mark-ifdef "ifdef")

;; org
(autoload #'org-agenda "fc-org")

;; extra
(autoload #'2048-game "fc-extra")

(provide 'fc-autoload)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-autoload.el ends here
