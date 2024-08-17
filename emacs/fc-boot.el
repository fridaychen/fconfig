;;; fc-boot.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-require 'fc-patch)
(fc-load (if (and *is-gui*
                  (<= 26 emacs-major-version))
             'posframe
           'popup)
  :after (progn
           (add-hook '*fc-after-theme-hook* #'posframe-delete-all)))

(fc-load 'fullframe :autoload t)
(when *is-mac*
  (fc-load 'osx-lib :autoload t))

(fc-load 'fc-hideshow
  :local t

  :after(progn
          (add-hook 'find-file-hook #'fc-hs-init)))

(apply #'fc-install *fc-extra-packages*)

(fc-load 'fc-org
  :local t
  :after (progn
           (add-hook '*fc-after-theme-hook* #'fc--org-theme-changed)
           (fc-org-autoconfig)
           (add-hook '*fc-common-fast-act-hook* #'fc--org-toggle-special-edit)
           (fc-require 'fc-plantuml)))

(defvar *fc--mode-config-map* (fc-make-hash-table
                               '(
                                 (c-mode . fc-clang)
                                 (c-ts-mode . fc-clang)
                                 (c++-mode . fc-clang)
                                 (emacs-lisp-mode . fc-elisp)
                                 (go-mode . fc-golang)
                                 (haskell-mode . fc-haskell)
                                 (latex-mode . fc-latex)
                                 (lisp-mode . fc-lisp)
                                 (markdown-mode . fc-markdown)
                                 (ocaml-mode . fc-ocaml)
                                 (plantuml-mode . fc-plantuml)
                                 (python-mode . fc-python)
                                 (python-ts-mode . fc-python))))

(cl-defun fc--load-mode-config ()
  "Load config for current mode."
  (when-let ((name (gethash major-mode *fc--mode-config-map*)))
    (unless (featurep name)
      (fc-load name
        :local t
        :after (progn
                 (fc-call-mode-func "setup" nil)
                 (fc-call-mode-func "patch-theme" nil))))))

(add-hook 'find-file-hook #'fc--load-mode-config)

(fc-require 'fc-buffer t)
(fc-require 'fc-control)
(fc-load 'fc-next-error
  :local t
  :after (add-hook '*fc-ergo-restore-hook* #'fc--clear-next-error-buffer))
(fc-require 'fc-ergo)
(fc-require 'fc-ergo-seg)
(fc-load 'fc-edit
  :local t
  :after (fc-add-hook-func '*fc-after-theme-hook* #'fc--setup-line-spacing))

(pcase *fc-completion*
  ('helm (fc-require 'fc-helm t))
  ('vertico (fc-require 'fc-vertico t))
  ('ivy (fc-require 'fc-ivy t)))

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
           (fc-add-to-hook '*fc-tomato-rest-hook*
                           #'fc--org-clock-out
                           #'(lambda ()
                               (fc-speak "take a break")
                               (fc-popup-info "Take a break !"
                                              :title "Tomato"
                                              :timeout 3)))

           (fc-add-to-hook '*fc-tomato-start-hook*
                           #'(lambda ()
                               (fc-speak "tomato started")))

           (fc-add-to-hook '*fc-tomato-done-hook*
                           #'fc--org-clock-out
                           #'(lambda ()
                               (fc-job-done :voice "tomato done"
                                            :msg "Tomato circle done!")))

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

(fc-load 'fc-app
  :local t
  :after (progn
           (add-hook '*fc-after-theme-hook* #'fc--setup-eshell-after-theme-changed)))

(fc-require 'fc-book t)

(fc-load 'fc-player
  :local t
  :after (progn
           (fc-init-user-player)))

(fc-require 'fc-welcome)

(fc-layout-save "a")
(fc-layout-save "s")
(fc-layout-save "d")
(fc-layout-save "f")
(fc-layout-switch "a")

(fc-require 'fc-vc)
(fc-require 'fc-buffer t)

(fc-require 'fc-proj t)
(fc-require 'fc-dired t)

(fc-require 'fc-yasnippet t)
(fc-require 'fc-global)
(fc-load 'fc-lsp
  :local t
  :after (when *fc-lsp-mode-enable*
           (add-hook '*fc-ergo-restore-hook* #'lsp-ui-doc-hide)))
(fc-require 'fc-tag)

(fc-require 'fc-math t)

(fc-load 'fc-flycheck
  :local t
  :after (progn
           (add-hook '*fc-enable-dev-hook*
                     #'global-flycheck-mode)
           (add-hook '*fc-disable-dev-hook*
                     #'(lambda ()
                         (global-flycheck-mode -1)))))

(fc-load 'fc-corfu
  :local t
  :disable *fc-enable-company*)

(fc-load 'fc-company
  :local t
  :enable *fc-enable-company*
  :after (fc--company-enable))

(fc-load 'fc-language
  :local t
  :autoload t
  :after (add-hook '*fc-modal-hook* #'fc--auto-toggle-input-method))

(setf *fc-booting* nil
      gc-cons-threshold (* 20 1024 1024))

(provide 'fc-boot)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-boot.el ends here
