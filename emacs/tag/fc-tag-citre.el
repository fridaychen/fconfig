;;; fc-tag-citre.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc-citre-tag-dir* (expand-file-name "citre/tags/" user-emacs-directory))
(defvar *fc-citre-global-dir* (expand-file-name "citre/gtags/" user-emacs-directory))

(fc-load 'citre
  :before (progn
            (mkdir *fc-citre-tag-dir* t)
            (mkdir *fc-citre-global-dir* t)
            (setenv "GTAGSOBJDIRPREFIX" *fc-citre-global-dir*))

  :after (progn
           (require 'citre-config)

           (add-hook '*fc-ergo-restore-hook* #'citre-peek-abort)

           (setf citre-auto-enable-citre-mode-modes nil
                 citre-completion-use-cache t
                 citre-default-create-tags-file-location 'global-cache
                 citre-tags-file-global-cache-dir *fc-citre-tag-dir*)

           (setq-default citre--global-dbpath *fc-citre-global-dir*
                         citre-enable-imenu-integration nil)

           (fc-bind-keys `(("<mouse-4>" citre-peek-prev-line)
                           ("<mouse-5>" citre-peek-next-line)
                           ("<wheel-up>" citre-peek-prev-line)
                           ("<wheel-down>" citre-peek-next-line)
                           )
                         citre-peek-keymap)
           ))

(defclass fc-tag-citre (fc-tag-xref)
  ())

(cl-defmethod fc-tag--open-file ((x fc-tag-citre))
  (citre-mode 1))

(cl-defmethod fc-tag--find-references ((x fc-tag-citre) id)
  (cl-call-next-method
   x
   (propertize id 'citre-xref-symbol-buffer (current-buffer))))

(cl-defmethod fc-tag--describe-at-point ((x fc-tag-citre))
  (fc-funcall #'citre-peek))

(cl-defmethod fc-tag--info ((x fc-tag-citre))
  (message "global: %s, tags: %s, eglot: %s"
           (citre-backend-usable-p 'global)
           (citre-backend-usable-p 'tags)
           (citre-backend-usable-p 'eglot)))

(cl-defmethod fc-tag--update ((x fc-tag-citre))
  (citre-create-tags-file)
  (citre-global-update-file))

(defvar *fc-tag-citre* (make-instance 'fc-tag-citre))

(provide 'fc-tag-citre)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag-citre.el ends here
