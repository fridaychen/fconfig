;;; fc-edit.el --- setup editor -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-enable-treesit* nil)

(defvar ns-command-modifier)
(defvar *fc-dev-mode* nil)
(defvar *fc-text-scale-modes* '(markdown-mode latex-mode))

(defvar *fc-column-limit* 80)
(defvar *fc-basic-line-spacing* 0)
(defvar *fc-prog-line-spacing* 1)
(defvar *fc-text-line-spacing* 4)

(setf mouse-autoselect-window nil
      make-pointer-invisible t)

;; large file
(setf large-file-warning-threshold 30000000)

;; setup coding
(set-selection-coding-system 'utf-8)
(set-next-selection-coding-system 'ctext)

;; basic edit
(transient-mark-mode t)
(show-paren-mode 1)
(setf show-paren-style 'parenthesis)

(setq-default line-spacing 0)

(fc-load 'ligature
  :after
  (progn
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures
     'prog-mode
     '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
       "\\\\" "://"))))

(ignore-errors
  (fc-load 'xclip
    :disable (or *is-gui* *is-cygwin*)
    :idle t
    :after (xclip-mode)))

(defun fc--set-visual-line-mode ()
  (let((has-wide-char (fc-detect-buf-has-wide-char)))
    (if has-wide-char
        (progn
          (visual-line-mode -1)
          (toggle-truncate-lines -1))
      (visual-line-mode 1))))

(defun fc--setup-line-spacing ()
  (setf line-spacing
        (cond
         ((derived-mode-p 'prog-mode)
          *fc-prog-line-spacing*)

         ((derived-mode-p 'text-mode)
          *fc-text-line-spacing*)

         (t
          *fc-basic-line-spacing*))))

(defun fc--setup-file-buffer ()
  (when (fc-member major-mode *fc-doc-modes*)
    (fc-call-mode-func "set-visual-line-mode"
                       #'fc--set-visual-line-mode))

  (when (derived-mode-p 'prog-mode)
    (ligature-mode)
    (fc-delay
      (color-identifiers-mode-maybe)))

  (fc--setup-line-spacing)
  ;; hightlight ending whitespace
  (highlight-regexp "[ \t]+$" 'whitespace-trailing))

(add-hook 'find-file-hook #'fc--setup-file-buffer)

;; enable narrow
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq-default case-fold-search t)

(setf auto-save-interval 2500
      auto-save-timeout nil)
(auto-compression-mode 1)

(setf auto-revert-verbose nil)
(global-auto-revert-mode t)

(global-visual-line-mode -1)
(global-eldoc-mode -1)
(add-hook 'prog-mode-hook #'eldoc-mode)

(defun fc-highlight-comment-keywords ()
  (highlight-phrase "\\(FIXME\\|ToDo\\|TODO\\|MEMO\\) ?:"))

(add-hook 'prog-mode-hook #'fc-highlight-comment-keywords)

(setq-default indent-tabs-mode nil)

(fc-load 'recentf
  :local t
  :autoload t
  :before (setf recentf-max-menu-items 1000
                recentf-max-saved-items 1000)

  :after (progn
           (fc-add-to-list 'recentf-exclude
                           "\\.nosync"
                           "\\.noindex")
           (recentf-mode 1)
           (fc-add-idle-hook #'recentf-save-list)))

(fc-install 'csv-mode)

(fc-load 'highlight-indentation
  :autoload t
  :after (progn
           ;; make highlight-indentation-face to inherit mode-line-inactive
           ;; because I want use fringe as visual effect of modal mode
           (set-face-attribute 'highlight-indentation-face
                               nil
                               :inherit 'mode-line-inactive)))

;; multiple cursor
(fc-install 'iedit)
(autoload 'iedit-mode "iedit")
(autoload 'iedit-rectangle-mode "iedit-rect")

;; alt key as meta, leave esc alone
(setf x-alt-keysym (if *is-mac* 'meta 'super))

;; make super/command equal meta
(cond
 (*is-mac*
  (setf ns-command-modifier 'meta))

 (*is-linux*
  (setq x-super-keysym 'meta)))

(fc-load 'saveplace
  :local t
  :autoload t
  :after (progn
           (setq-default save-place t)
           (if (>= emacs-major-version 25)
               (save-place-mode 1))))

;; modes
(when (fboundp #'electric-pair-mode)
  (electric-pair-mode))

(fc-load 'rainbow-mode
  :idle t
  :after (progn
           (setf rainbow-x-colors nil)
           (add-hook 'prog-mode-hook #'(lambda ()
                                         (when (memq major-mode '(emacs-lisp-mode mhtml-mode))
                                           (rainbow-mode))))))

(fc-load 'rainbow-delimiters
  :idle t
  :after (progn
           (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(fc-load 'color-identifiers-mode
  :after (add-hook '*fc-after-theme-hook* #'color-identifiers:regenerate-colors))

(fc-load 'which-func
  :local t
  :after (which-function-mode))

(fc-load 'aggressive-indent
  :idle t)

(fc-load 'diff-mode
  :local t
  :after (progn
           (defconst *fc-diff-map*
             (fc-make-keymap
              `(
                ("d" diff-file-kill)
                ("h" diff-refine-hunk)
                ("o" diff-goto-source)
                )
              "fc-diff-map"
              *fc-func-mode-map*)
             "KEYS d: delete file  h: refine  o: open source.")

           (defun fc--diff-mode-func ()
             "Mode func."
             (fc-modal-head-key "Diff" '*fc-diff-map*))))

(fc-load 'gnuplot
  :local t
  :bind '((gnuplot-mode-map
           ("TAB" fc-tab-key))))

(fc-load 'fish-mode
  :after (progn
           (setf fish-enable-auto-indent t)

           (fc-add-mode-name 'fish-mode "🐟")

           (add-hook 'fish-mode-hook
                     (lambda ()
                       (add-hook 'before-save-hook 'fish_indent-before-save)))))

(fc-load 'treesit
  :local t
  :enable *fc-enable-treesit*
  :after (progn
           (setf treesit-font-lock-level 3)

           (--each '(c go html python)
             (when (treesit-ready-p it)
               (treesit-parser-create it)))

           (treesit-major-mode-setup)))

(fc-load 'ebnf-mode)

(fc-load 'imenu
  :after (progn
           (cl-defun fc--imenu-after-jump ()
             (fc-hs-show-block)
             (recenter 1))

           (add-hook 'imenu-after-jump-hook #'fc--imenu-after-jump t)))

(fc-load 'xref
  :after (progn
           (cl-defun fc--xref-after-jump ()
             (fc-hs-show-block))

           (add-hook 'xref-after-jump-hook #'fc--xref-after-jump t)))

(provide 'fc-edit)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-edit.el ends here
