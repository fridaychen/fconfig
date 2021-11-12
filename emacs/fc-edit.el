;;; fc-edit.el --- setup editor -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-input-methods* nil)
(defvar *fc-enable-anzu* nil)
(defvar ns-command-modifier)
(defvar *fc-dev-mode* nil)
(defvar *fc-text-scale-modes* '(markdown-mode latex-mode))

(defvar *fc-column-limit* 80)
(defvar *fc-basic-line-spacing* 0)

(setf mouse-autoselect-window nil
      make-pointer-invisible t)

;; large file
(setf large-file-warning-threshold 30000000)

;; setup coding
(set-selection-coding-system 'utf-8)
(set-next-selection-coding-system 'ctext)

(fc-load 'mozc
  :enable (executable-find "mozc_emacs_helper")
  :idle t
  :after (add-to-list '*fc-input-methods* 'japanese-mozc))

(defun fc-next-input-method ()
  "Switch to next input method."
  (interactive)

  (unless (null *fc-input-methods*)
    (setf *fc-input-methods* (-rotate 1 *fc-input-methods*))
    (set-input-method (cl-first *fc-input-methods*))))

;; basic edit
(transient-mark-mode t)
(show-paren-mode 1)
(setf show-paren-style 'parenthesis)

(ignore-errors
  (fc-load 'xclip
    :disable (or *is-gui* *is-cygwin*)
    :idle t
    :after (xclip-mode)))

(defun fc--set-visual-line-mode ()
  (visual-line-mode (if (fc-detect-buf-has-wide-char) -1 1)))

(fc-add-hook 'find-file-hook
  (when (member major-mode *fc-doc-modes*)
    (fc--set-visual-line-mode))

  ;; setup default line-space
  (setf line-spacing *fc-basic-line-spacing*)
  ;; hightlight ending whitespace
  (highlight-regexp "[ \t]+$" 'whitespace-trailing))

;; text-scale-mode
(fc-load 'face-remap
  :idle t
  :local t
  :after (progn
           (fc-add-hook 'find-file-hook
             (when (member major-mode *fc-text-scale-modes*)
               (setf text-scale-mode-amount *fc-reading-scale*)
               (text-scale-mode)))))

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
(setf x-alt-keysym 'meta)

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
  :autoload t
  :after (global-color-identifiers-mode))

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

    (defun fc-diff-mode-func ()
      "Mode func."
      (fc-modal-head-key "Diff" '*fc-diff-map*))))

(provide 'fc-edit)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-edit.el ends here
