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
         ayu-theme
         beacon
         csv-mode
         color-identifiers-mode
         color-theme-sanityinc-tomorrow
         counsel
         ef-themes
         emacsql-sqlite-builtin
         faff-theme
         fantom-theme
         flycheck
         fireplace
         ggtags
         google-this
         gruvbox-theme
         haskell-mode
         helm
         ivy
         leuven-theme
         lsp-mode
         magit
         markdown-mode
         material-theme
         monokai-theme
         monokai-pro-theme
         mozc
         neotree
         rainbow-delimiters
         rainbow-mode
         saveplace
         srcery-theme
         tuareg
         org
         projectile
         vimrc-mode
         yasnippet
         yasnippet-snippets
         yaml-mode
         zenburn-theme))

(provide 'fc-setup)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-setup.el ends here
