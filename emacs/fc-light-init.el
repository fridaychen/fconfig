;;; fc-init.el --- init fconfig -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'iso-transl)

(add-to-list 'load-path "~/.emacs.d/fconfig")

(setf enable-local-variables :all)

(defconst *is-linux* (eql system-type 'gnu/linux))
(defconst *is-mac* (eql system-type 'darwin))
(defconst *is-unix* (or *is-linux* *is-mac*))
(defconst *is-cygwin* (eql system-type 'cygwin))
(defconst *is-windows* (eql system-type 'windows-nt))
(defconst *is-gui* window-system)
(defvar *fc-enable-sound* (if *is-unix* t nil))

(setf *fc-packages* (append '(s dash popup fullframe osx-lib)
                            *fc-packages*))

(require 'fc-package)
;; Load modules
(fc-load 'fc-util
  :local t

  :after (defconst *is-laptop* (fc-has-battery)))
(require 'fc-lang)

(fc-load 'fc-computer
  :after (fc-auto-config))

(require 'fc-facility)

(fc-load 'fc-modal
  :after (fc-modal-global-mode))
(require 'fc-theme-config)
(require 'fc-config)
(require 'fc-screen)
(require 'fc-edit)

(when *is-gui*
  (require 'fc-font))

(require 'fc-app)

(require 'fc-ergo)

(fc-delay
  (require 'fc-flycheck)
  (require 'fc-company)
  (require 'fc-helm)
  (require 'fc-ivy)
  (require 'fc-python)

  (when *is-gui*
    (require 'fc-calendar))

  (require 'fc-program)
  (require 'fc-yasnippet)
  (require 'fc-buffer)
  (require 'fc-lisp)
  (require 'fc-org)
  (require 'fc-dired)
  (require 'fc-project)
  (require 'fc-global)
  (require 'fc-vc))

(fc-dark-theme)
;; disable server mode
(server-mode -1)

(fc-show-welcome)

(provide 'fc-light-init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-light-init.el ends here
