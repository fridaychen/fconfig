;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun fc-user-config ()
  (setf *fc-font-mode-line-delta* 0
        *fc-extra-packages* '()
        *fc-basic-line-spacing* 4
        *fc-reading-fill* 50
        *fc-reading-scale* 1.2)

  (when *is-gui*
    ;;(fc-play-sound 'welcome)
    (cl-incf *fc-font-height* 2)
    (setf *fc-enable-nyan* nil))

  (defconst *fc-location-gateway*
    '((work . "mac-addr")
      (home . "mac-addr"))))

(setenv "FCHOME" "${FCHOME}")

(add-to-list 'load-path "~/.emacs.d/fconfig")
(require 'fc)
