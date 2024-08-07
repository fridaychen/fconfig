;;; fc-helm.el --- setup helm -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-install 'helm-core)

(fc-load 'helm
  :autoload t
  :before (progn
            (when (eq *fc-completion* 'helm)
              (helm-mode)

              (defalias 'fc-find-files 'helm-find-files)
              (defalias 'fc-buffers-list 'helm-buffers-list)
              (defalias 'fc-recentf 'helm-recentf)
              (defalias 'fc-occur 'occur)
              (defalias 'fc-M-x 'helm-M-x)
              (defalias 'fc-bookmark 'helm-bookmarks)

              (defun fc-imenu ()
                (interactive)

                (let ((helm-full-frame nil)
                      (helm-always-two-windows t))
                  (fc-funcall #'helm-semantic-or-imenu)))))

  :after (progn
           (setf helm-boring-buffer-regexp-list
                 '("\\` "
                   "\\*ehsell"
                   "\\*helm"
                   "\\*helm-mode"
                   "\\*Echo Area"
                   "\\*tramp"
                   "\\*Minibuf"
                   "\\*epc"))

           (setf helm-ff-skip-boring-files t
                 helm-ff-file-name-history-use-recentf t
                 helm-full-frame nil
                 helm-always-two-windows t)

           (setf helm-boring-file-regexp-list *fc-ignore-file*)

           (require 'helm-files)
           (fc-each (list helm-generic-files-map
                          helm-find-files-map
                          helm-read-file-map)
             (fc-unbind-keys '("M-i" "M-k" "M-j" "M-l" "C-i" "C-<backspace>") it))

           (require 'helm-buffers)
           (fc-each (list helm-buffer-map)
             (fc-unbind-keys '("M-i" "M-k" "M-j" "M-l" "C-i" "C-<backspace>") it)))

  :bind '((helm-map
           ("M-i" helm-previous-line)
           ("M-k" helm-next-line)
           ("M-j" helm-previous-page)
           ("M-l" helm-next-page)

           ("M-<" helm-beginning-of-buffer)
           ("M->" helm-end-of-buffer)

           ("<escape>" minibuffer-keyboard-quit)

           ("C-j" helm-next-line)
           ("C-k" helm-previous-line)
           ("C-d" helm-previous-page)
           ("C-f" helm-next-page)

           ("C-i" helm-execute-persistent-action)
           ("C-<backspace>" backward-kill-word))
          (nil
           ("M-x" helm-M-x)
           ("M-y" helm-show-kill-ring)
           ("C-x C-f" helm-find-files)
           ("C-x b" helm-buffers-list))))

(fc-load 'helm-flycheck
  :autoload t
  :before (when (eq *fc-completion* 'helm)
            (defalias 'fc-flycheck 'helm-flycheck)))

(fc-load 'helm-projectile
  :autoload t
  :before (progn
            (when (eq *fc-completion* 'helm)
              (defalias 'fc-projectile-buffers-list 'helm-browse-project)
              (defalias 'fc-projectile-grep 'helm-projectile-grep)))

  :after (progn
           (fc-unbind-keys '("M-i" "M-k" "M-j" "M-l" "C-i" "C-<backspace>")
                           helm-projectile-find-file-map))

  :bind '((nil
           ("C-x C-b" helm-projectile-switch-to-buffer))))

(fc-load 'helm-gtags
  :autoload t
  :before (when (eq *fc-completion* 'helm)
            (defalias 'fc-gtags-select 'helm-gtags-select)))

(provide 'fc-helm)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-helm.el ends here
