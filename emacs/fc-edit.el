;;; fc-edit.el --- setup editor -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-enable-anzu* nil)
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

(ignore-errors
  (fc-load 'xclip
    :disable (or *is-gui* *is-cygwin*)
    :idle t
    :after (xclip-mode)))

(defun fc--set-visual-line-mode ()
  (visual-line-mode (if (fc-detect-buf-has-wide-char) -1 1)))

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
  (setf enable-dir-local-variables t)

  (when (fc-member major-mode *fc-doc-modes*)
    (fc-call-mode-func "set-visual-line-mode"
                       #'fc--set-visual-line-mode))

  (when (derived-mode-p 'prog-mode)
    (fc-delay
      (color-identifiers-mode)))

  (fc--setup-line-spacing)
  ;; hightlight ending whitespace
  (highlight-regexp "[ \t]+$" 'whitespace-trailing))

(add-hook 'find-file-hook #'fc--setup-file-buffer)

;; enable narrow
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq-default case-fold-search t)

(fc-load 'anzu
  :autoload t
  :enable *fc-enable-anzu*
  :after (global-anzu-mode -1))

(setf auto-save-interval 2500
      auto-save-timeout nil)
(auto-compression-mode 1)

(setf auto-revert-verbose nil)
(global-auto-revert-mode t)

(global-visual-line-mode -1)
(global-eldoc-mode -1)
(add-hook 'prog-mode-hook #'eldoc-mode)

(fc-load 'recentf
  :local t
  :autoload t
  :before (setf recentf-max-menu-items 200
                recentf-max-saved-items 200)

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
(defvar iedit-mode nil "Autoload workaround.")

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
  :autoload t)

(fc-load 'which-func
  :local t
  :after (which-function-mode))

(fc-load 'aggressive-indent
  :idle t)

(fc-load 'diff-mode
  :local t
  :after
  (progn
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

(provide 'fc-edit)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-edit.el ends here
