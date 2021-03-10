;;; fc-setup.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'eieio)

(defconst *is-linux* (eql system-type 'gnu/linux))
(defconst *is-mac* (eql system-type 'darwin))
(defconst *is-unix* (or *is-linux* *is-mac*))
(defconst *is-cygwin* (eql system-type 'cygwin))
(defconst *is-windows* (eql system-type 'windows-nt))
(defconst *is-gui* window-system)

(require 'fc-facility)
(require 'fc-package)

(message "\n### refresh packages\n")
(package-refresh-contents)

(message "\n### install basic packages\n")

(apply #'fc-install
       '(ace-window
         aggressive-indent
         auctex
         beacon
         color-identifiers-mode
         color-theme-sanityinc-tomorrow
         company
         company-posframe
         company-quickhelp
         counsel
         fantom-theme
         flycheck
         fireplace
         ggtags
         go-mode
         google-this
         gruvbox-theme
         haskell-mode
         helm
         ivy
         lsp-mode
         magit
         markdown-mode
         material-theme
         mlso-theme
         mode-icons
         monokai-theme
         monokai-pro-theme
         mozc
         neotree
         nyan-mode
         rainbow-delimiters
         rainbow-mode
         saveplace
         srcery-theme
         tuareg
         org
         powerline
         projectile
         python-mode
         spaceline
         yasnippet
         yasnippet-snippets
         zenburn-theme))

(provide 'fc-setup)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-setup.el ends here
