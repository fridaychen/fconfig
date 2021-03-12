;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
                                        ;(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/col-number ((t (:inherit sml/global :weight normal))))
 '(sml/line-number ((t (:inherit sml/global :weight normal)))))

(defun fc-user-config ()
  (setf *fc-font-mode-line-delta* 0)
  (fc-require 'fc-modeline)

  (setf *fc-extra-packages* '(vimrc-mode
                              cmake-mode
                              csv-mode
                              yaml-mode)
        *fc-basic-line-spacing* 4
        *fc-reading-fill* 50
        *fc-reading-scale* 1.2)
  (when *is-gui*
    ;;(fc-play-sound 'welcome)
    (cl-incf *fc-font-height* 2)
    (setf *fc-enable-nyan* nil))

  (defconst *fc-work-gateway-mac* '("00:25:36:b7:5c:09")))

(setenv "FCHOME" "${FCHOME}")

(add-to-list 'load-path "~/.emacs.d/fconfig")
(require 'fc)
