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

(defvar *fc-common-fast-act-hook* nil "Common fast action hook.")

;; face
(autoload #'fc-get-face "fc-face")
(autoload #'fc-set-face "fc-face")
(autoload #'fc-set-faces "fc-face")

;; font
(autoload #'fc-setup-font "fc-font")
(autoload #'fc-nerd-icon "fc-font")

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
(autoload #'fc-side-window-p "fc-layout")
(autoload #'fc-layout-setup-style "fc-layout")
(autoload #'fc-layout-spotlight "fc-layout")

;; modal
(autoload #'fc-modal-enable "fc-modal")
(autoload #'fc-modal-disable "fc-modal")

;; program
(autoload #'fc-program "fc-program")

;; theme
(defvar *fc-theme-mode* 'dark)

(defvar *fc-before-theme-hook* nil "Before theme hook.")
(defvar *fc-after-theme-hook* nil "After theme hook.")

(autoload #'fc-load-theme "fc-theme-config")
(autoload #'fc-theme-auto-select "fc-theme-config")
(autoload #'fc-dark-theme-p "fc-theme-config")
(autoload #'fc--set-face-contrast "fc-theme-config")
(autoload #'fc--enhance-face-contrast "fc-theme-config")

;; theme beautify
(autoload #'fc--beautify-soothe-face "fc-theme-beautify")
(autoload #'fc-beautify-theme "fc-theme-beautify")
(autoload #'fc-beautify-theme-before "fc-theme-beautify")

;; util
(autoload #'fc-has-battery "fc-util")
(autoload #'fc-color-complement "fc-util")

;; viewer
(defvar *fc-reading-face* nil)
(defvar *fc-reading-fontset* nil)
(defvar-local fc-viewer-minor-mode nil)
(autoload #'fc-viewer-toggle "fc-viewer")

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
(defvar-local *fc-auto-hide* t)
(autoload #'org-agenda "fc-org")

;; extra
(autoload #'2048-game "fc-extra")
(autoload #'speed-type-text "fc-extra")

;; yas
(autoload #'yas-expand "yasnippet")
(autoload #'yas-describe-tables "yasnippet")

;; user interface
(autoload #'fc-select "fc-ui")
(autoload #'fc-select-func "fc-ui")
(autoload #'fc-select-buffer "fc-ui")
(autoload #'fc-select-color "fc-ui")
(autoload #'fc-user-confirm "fc-ui")
(autoload #'fc-create-simple-pop-menu "fc-ui")
(autoload #'fc-create-pop-menu "fc-ui")
(autoload #'fc-show-pop-menu "fc-ui")
(autoload #'fc-eval-pop-menu "fc-ui")
(autoload #'fc-popup-tip "fc-ui")
(autoload #'fc-popup-info "fc-ui")

;; lanugage
(autoload #'fc-next-input-method "fc-language")
(autoload #'fc-zh-to-number "fc-language")
(autoload #'fc-detect-char-script "fc-language")
(autoload #'fc-detect-has-wide-char "fc-language")
(autoload #'fc-detect-buf-has-wide-char "fc-language")

;; modeline
(autoload #'fc-modeline-mode "fc-modeline")

(autoload #'fc-modeline-proj-name "fc-ergo-seg")
(autoload #'fc-mode-name "fc-icon")
(autoload #'fc-add-mode-name "fc-icon")
(autoload #'fc-battery-icon "fc-icon")

;; book
(autoload #'fc--book-p "fc-book")
(autoload #'fc--book-setup "fc-book")

;; ttl
(autoload #'fc-ttl-mode "fc-ttl")
(add-to-list 'auto-mode-alist '("\\.ttl" . fc-ttl-mode))

;; info
(defvar *fc-info-buffer* nil)
(defvar *fc-info-system* nil)

(autoload #'fc-info-show "fc-info")

(provide 'fc-autoload)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-autoload.el ends here
