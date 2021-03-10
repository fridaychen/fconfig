;;; fc-boot.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load (if (and *is-gui*
                  (<= 26 emacs-major-version))
             'posframe
           'popup))
(fc-load 'fullframe :autoload t)
(when *is-mac*
  (fc-load 'osx-lib :autoload t))

(fc-load 'fc-hideshow
  :local t

  :after(progn
          (add-hook 'find-file-hook #'fc-hide-show-init)))

(apply #'fc-install *fc-extra-packages*)
(fc-install 'auctex 'go-mode 'haskell-mode 'markdown-mode 'tuareg 'org 'python-mode)

(cl-defun fc-load-mode-config ()
  "Load config for current mode."
  (pcase major-mode
    ((or 'c-mode 'c++-mode) (fc-require 'fc-clang))
    ('emacs-lisp-mode (fc-require 'fc-elisp))
    ('go-mode (fc-require 'fc-golang))
    ('haskell-mode (fc-require 'fc-haskell))
    ('latex-mode (fc-require 'fc-latex t))
    ('lisp-mode (fc-require 'fc-lisp))
    ('markdown-mode (fc-require 'fc-markdown))
    ('ocaml-mode (fc-require 'fc-ocaml))
    ('org-mode (unless (featurep 'fc-org)
                 (fc-load 'fc-org
                   :local t
                   :after (fc-org-autoconfig))
                 (fc-require 'fc-plantuml)))
    ('plantuml-mode (fc-require 'fc-plantuml))
    ('python-mode (fc-require 'fc-python))))

(add-hook 'find-file-hook #'fc-load-mode-config)

(fc-require 'fc-ergo)
(fc-require 'fc-edit)

(when (eq *fc-completion* 'helm)
  (fc-require 'fc-helm t))
(fc-require 'fc-ivy t)
(fc-require 'fc-buffer t)

(fc-load 'ido
  :local t
  :run t

  :bind '((ido-common-completion-map
           ("<tab>" ido-next-match)
           ("C-j" ido-next-match)
           ("C-k" ido-prev-match))))

;; (fc-require 'fc-bookmark t)

;; midnight
(fc-load 'midnight
  :local t
  :enable (not *fc-lightweight*)

  :after (progn
           (setf midnight-period 10
                 clean-buffer-list-delay-special 20)

           (add-to-list 'clean-buffer-list-kill-regexps
                        "\\`\\*Customize .*\\*\\'")))

(fc-load 'fc-tomato
  :local t
  :run t

  :after (progn
           (fc-tomato-init)

           (fc-add-to-hook '*fc-tomato-rest-hook*
                           (lambda ()
                             (fc-speak "take a break")
                             (fc-popup-tip "Take a break !"
                                           :title "Tomato"
                                           :timeout 3)))

           (fc-add-to-hook '*fc-tomato-start-hook*
                           #'(lambda ()
                               (fc-speak "tomato started")))

           (fc-add-to-hook '*fc-tomato-done-hook*
                           #'fc-job-done
                           (lambda ()
                             (fc-speak "tomato done")
                             (fc-popup-tip "Circle done !"
                                           :title "Tomato")))

           (when *fc-enable-screen-saver*
             (add-hook '*fc-tomato-start-hook*
                       (lambda ()
                         (setf *fc-enable-screen-saver* nil)))
             (add-hook '*fc-tomato-done-hook*
                       (lambda ()
                         (setf *fc-enable-screen-saver* t))))))

(fc-load 'exec-path-from-shell
  :before (setf exec-path-from-shell-check-startup-files nil)
  :after (exec-path-from-shell-initialize))

(fc-require 'fc-app t)
(fc-require 'fc-book t)

(fc-load 'fc-player
  :local t
  :after (progn
           (fc-init-user-player)))

;; (fc-require 'fc-welcome)

(fc-layout-save "a")
(fc-layout-save "s")
(fc-layout-save "d")
(fc-layout-save "f")
(fc-layout-switch "a")

(fc-require 'fc-vc t)
(fc-require 'fc-buffer t)

(fc-require 'fc-proj t)
(fc-require 'fc-dired t)

(fc-require 'fc-yasnippet t)
(fc-require 'fc-global)
(fc-require 'fc-lsp)
(fc-require 'fc-tag)

(fc-require 'fc-math t)

(fc-require 'fc-flycheck)
(fc-require 'fc-company)

(setf *fc-booting* nil)

(provide 'fc-boot)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-boot.el ends here
