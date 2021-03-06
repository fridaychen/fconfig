;;; fc-ivy.el --- setup ivy -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'ivy
  :autoload t
  :before (when (eq *fc-completion* 'ivy)
            (ivy-mode)
            (defalias 'fc-buffers-list 'ivy-switch-buffer))

  :bind '((ivy-minibuffer-map
           ("M-k" ivy-next-line)
           ("M-i" ivy-previous-line)
           ("M-l" ivy-scroll-up-command)
           ("M-j" ivy-scroll-down-command)

           ("C-j" ivy-next-line)
           ("C-k" ivy-previous-line)
           ("C-f" ivy-scroll-up-command)
           ("C-d" ivy-scroll-down-command)

           ("<escape>" minibuffer-keyboard-quit))

          (ivy-switch-buffer-map
           ("C-j" ivy-next-line)
           ("C-k" ivy-previous-line)
           ("C-q" ivy-switch-buffer-kill))))

(fc-load 'counsel
  :autoload t
  :before (progn
            (when (eq *fc-completion* 'ivy)
              (defalias 'fc-find-files 'counsel-find-file)
              (defalias 'fc-recentf 'counsel-recentf)
              (defalias 'fc-imenu 'counsel-imenu)
              (defalias 'fc-M-x 'counsel-M-x)
              (defalias 'fc-bookmark 'counsel-bookmark)))

  :after (progn
           (setf counsel-find-file-ignore-regexp
                 (s-chop-suffix "\\|"
                                (concat
                                 (--reduce-from (concat acc it "\\|")
                                                ""
                                                *fc-ignore-file*)
                                 (--reduce-from (concat acc it "/\\|")
                                                ""
                                                *fc-ignore-dir*))))))

(fc-load 'counsel-gtags
  :autoload t
  :before (when (eq *fc-completion* 'ivy)
            (defalias 'fc-gtags-select 'counsel-gtags-find-symbol)))

(fc-load 'counsel-projectile
  :autoload t
  :before (when (eq *fc-completion* 'ivy)
            (defalias 'fc-projectile-buffers-list 'counsel-projectile-switch-to-buffer)))

(when (eq *fc-completion* 'ivy)
  (defalias 'fc-flycheck 'flycheck-list-errors)
  (defalias 'fc-occur 'occur))

(provide 'fc-ivy)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ivy.el ends here
