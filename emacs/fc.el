;;; fc.el --- init fconfig -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'eieio)
(require 'iso-transl)

(when (and (featurep 'nativecomp)
           (native-comp-available-p))
  (setf package-native-compile t))

(setf enable-local-variables :all
      warning-minimum-level :error
      inhibit-startup-message t
      gc-cons-threshold 104857600
      *fc-booting* t)

(defconst *fc-home* (getenv "FCHOME"))
(defconst *fc-resource* (format "%s/emacs/resource/" *fc-home*))

(defconst *is-linux* (eql system-type 'gnu/linux))
(defconst *is-mac* (eql system-type 'darwin))
(defconst *is-unix* (or *is-linux* *is-mac*))
(defconst *is-cygwin* (eql system-type 'cygwin))
(defconst *is-windows* (eql system-type 'windows-nt))
(defconst *is-gui* window-system)
(defconst *is-colorful* (or *is-gui* (not (string= (getenv "TERM") "linux"))))

(defvar *fc-boot* t)
(defvar *fc-restart* nil)
(defvar *fc-lightweight* nil)
(defvar *fc-enable-sound* (if *is-unix* t nil))
(defvar *fc-extra-packages* nil)

(defconst *fc-config-prompt* "Welcome back Master")

(defvar *fc-enable-dev-hook* nil)
(defvar *fc-disable-dev-hook* nil)

(require 'fc-facility)
(require 'fc-package)
(fc-require 'fc-autoloads)
(fc-require 'fc-lang)

;; first step initilization
(let ((has-battery (fc-has-battery)))
  (setf *is-laptop* has-battery
        *fc-enable-screen-saver* (not has-battery)))

(when *is-gui*
  (fc-require 'fc-font))

(fc-auto-config)

(fc-add-env-path (concat (getenv "FCHOME") "/python") nil "PYTHONPATH")

(fc-require 'fc-screen)
(fc-modal-global-mode)

(add-hook '*fc-after-theme-hook* #'fc-modal-visual-feedback)
(when *is-gui*
  (add-hook '*fc-after-theme-hook* #'fc-setup-font))
(add-hook '*fc-after-theme-hook* #'fc-patch-theme)

(unless *is-mac*
  (fc-dark-theme))

(cl-defun fc-after-restart ()
  (with-eval-after-load 'fc-boot
    (fc-load-desktop)))

(fc-delay-task
 (lambda ()
   (when *is-gui*
     (fc-setup-font))
   (fc-dark-theme)

   (when *fc-boot*
     (fc-require 'fc-boot))
   ;; enable server mode
   (server-mode 1))
 0.01)

(provide 'fc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc.el ends here
