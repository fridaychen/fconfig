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
(autoload #'fc-translate "fc-dict")

;; keymaps
(autoload #'fc-make-keymap "fc-keymaps")

;; font
(autoload #'fc-setup-font "fc-font")

;; format
(defvar-local *fc-format-at-save* t)

(autoload #'fc-fmt-buffer "fc-format")
(autoload #'fc-add-fmt "fc-format")
(autoload #'fc-indent-all "fc-format")

;; ifdef
(autoload #'mark-ifdef "ifdef")

;; layout
(autoload #'fc-layout-save "fc-layout")
(autoload #'fc-layout-switch "fc-layout")

;; modal
(autoload #'fc-modal-enable "fc-modal")
(autoload #'fc-modal-disable "fc-modal")

;; program
(autoload #'fc-program "fc-program")

;; theme
(defvar *fc-after-theme-hook* nil "After theme hook.")

(autoload #'fc-load-theme "fc-theme-config")
(autoload #'fc-theme-auto-select "fc-theme-config")

;; util
(autoload #'fc-has-battery "fc-util")

;; viewer
(defvar-local fc-viewer-minor-mode nil)
(autoload #'fc-viewer-toggle "fc-viewer")
(autoload #'fc-viewer-list-buffer "fc-viewer")

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

;; yas
(autoload #'yas-expand "yasnippet")
(autoload #'yas-describe-tables "yasnippet")

;; user interface
(autoload #'fc-user-select "fc-ui")
(autoload #'fc-user-select-func "fc-ui")
(autoload #'fc-user-confirm "fc-ui")
(autoload #'fc-create-simple-pop-menu "fc-ui")
(autoload #'fc-create-pop-menu "fc-ui")
(autoload #'fc-show-pop-menu "fc-ui")
(autoload #'fc-eval-pop-menu "fc-ui")

;; lanugage
(autoload #'fc-next-input-method "fc-language")
(autoload #'fc-zh-to-number "fc-language")
(autoload #'fc-detect-char-script "fc-language")
(autoload #'fc-detect-has-wide-char "fc-language")
(autoload #'fc-detect-buf-has-wide-char "fc-language")

;; modeline
(autoload #'fc-modeline-mode "fc-modeline")

(provide 'fc-autoload)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-autoload.el ends here
