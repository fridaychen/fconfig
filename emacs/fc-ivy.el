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

           ("C-<return> "ivy-immediate-done)

           ("<escape>" minibuffer-keyboard-quit))

          (ivy-switch-buffer-map
           ("C-j" ivy-next-line)
           ("C-k" ivy-previous-line)
           ("C-q" ivy-switch-buffer-kill))))

(fc-load 'ivy-posframe
  :after (progn
           (cl-defun fc--posframe-theme-changed ()
             (fc-set-face 'ivy-posframe-border nil
                          :background
                          (if (fc-dark-theme-p)
                              "#FEBA07"
                            "SkyBlue3")))

           (defun fc--posframe-get-size ()
             "Return posframe size."
             (list
              :height ivy-posframe-height
              :width ivy-posframe-width
              :min-height (or ivy-posframe-min-height
                              (let ((height (+ ivy-height 1)))
                                (min height (or ivy-posframe-height height))))
              :min-width (round (* 0.7 (frame-width)))))

           (setf ivy-posframe-size-function #'fc--posframe-get-size
                 ivy-posframe-style 'frame-bottom-center
                 ivy-posframe-border-width 6)

           (add-hook '*fc-after-theme-hook* #'fc--posframe-theme-changed)
           (fc--posframe-theme-changed)

           (ivy-posframe-mode 1)))

(fc-load 'orderless
  :after (progn
           (setq completion-styles '(orderless basic)
                 ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
           (add-to-list 'ivy-highlight-functions-alist
                        '(orderless-ivy-re-builder . orderless-ivy-highlight))))

(fc-load 'counsel
  :autoload t
  :before (progn
            (when (eq *fc-completion* 'ivy)
              (defalias 'fc-find-files 'counsel-find-file)
              (defalias 'fc-recentf 'counsel-recentf)
              (defalias 'fc-imenu 'counsel-imenu)
              (defalias 'fc-yank-pop 'counsel-yank-pop)
              (defalias 'fc-M-x 'counsel-M-x)
              (defalias 'fc-bookmark 'counsel-bookmark)
              (defalias 'fc-outline 'counsel-outline)))

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
