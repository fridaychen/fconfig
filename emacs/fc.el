;;; fc.el --- init fconfig -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'eieio)
(require 'iso-transl)

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setf package-native-compile t))

(setf enable-local-variables :all
      warning-minimum-level :error
      inhibit-startup-message t
      gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024)
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

(load-theme 'leuven t)

(require 'fc-facility)
(require 'fc-package)
(fc-require 'fc-autoloads)
(fc-require 'fc-lang)

;; first step initilization
(let ((has-battery (fc-has-battery)))
  (setf *is-laptop* has-battery
        *fc-enable-screen-saver* (not has-battery)))

(when *is-gui*
  (fc-require 'fc-font)
  (fc-add-hook-func '*fc-after-theme-hook* #'fc-setup-font))

(fc-auto-config)

(when *is-gui*
  (fc-setup-font))

(fc-add-env-path (concat (getenv "FCHOME") "/python") nil "PYTHONPATH")
(fc-add-env-path (expand-file-name "~/.local/bin") nil "PATH")

(fc-load 'fc-screen
  :local t
  :after
  (progn
    (when *is-gui*
      (add-hook '*fc-after-theme-hook* #'fc--setup-fringe))
    (add-hook '*fc-project-hook* #'fc--setup-window-title)))

(fc-require 'fc-xpm)
(fc-load 'fc-modeline
  :local t
  :after
  (progn
    (add-to-list '*fc-after-theme-hook* #'fc-modeline-mode)))
(fc-modal-enable)

(fc-add-hook-func '*fc-after-theme-hook*
  #'fc-modal-after-theme-change
  #'fc-patch-theme)

(cl-defun fc-after-restart ()
  "After restart, load desktop."
  (with-eval-after-load 'fc-boot
    (fc-load-desktop)))

(fc-delay-task
 (lambda ()
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
