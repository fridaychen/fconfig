;;; fc-tag.el --- source tagging interface -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(add-to-list 'load-path (concat *fc-home* "/emacs/tag"))

(require 'fc-tag-base)
(require 'fc-tag-xref)
(require 'fc-tag-citre)
(require 'fc-tag-eglot)
(require 'fc-tag-global)

(defvar *fc--mode-tag-map* (make-hash-table))
(defconst *fc--name-tag-map* (fc-make-hash-table
                              `((citre ,*fc-tag-citre*)
                                (eglot ,*fc-tag-eglot*)
                                (global ,*fc-tag-global*)
                                )))

(cl-defun fc-find-definitions (&key apropos)
  (interactive)

  (when (not apropos)
    (let* ((sym (fc-current-thing :ask nil)))
      (when sym
        (fc-tag-find-definitions sym)
        (cl-return-from fc-find-definitions))))

  (fc-tag-find-apropos (fc-current-thing :confirm t)))

(cl-defun fc-find-references ()
  (interactive)

  (let* ((sym (fc-current-thing)))
    (when sym
      (fc-tag-find-references sym))))

(cl-defun fc-find-tag ()
  (when-let* ((instance (gethash major-mode *fc--mode-tag-map*)))
    (cl-return-from fc-find-tag instance))

  (when-let* ((use-tag (boundp 'fc-proj-tag))
              (tag (gethash fc-proj-tag *fc--name-tag-map*)))
    (cl-return-from fc-find-tag (car tag))))

(defun fc-tag-find-definitions (id)
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--find-definitions tag id)))

(defun fc-tag-find-apropos (id)
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--find-apropos tag id)))

(defun fc-tag-find-references (id)
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--find-references tag id)))

(defun fc-tag-open-project (proj-dir src-dirs)
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--open-project tag proj-dir src-dirs)))

(defun fc-tag-open-file ()
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--open-file tag)))

(defun fc-tag-list ()
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--list tag)))

(defun fc-tag-describe-at-point ()
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--describe-at-point tag)))

(defun fc-tag-info ()
  (when-let* ((tag (fc-find-tag)))
    (fc-tag--info tag)))

(cl-defun fc-tag-rename ()
  (when-let* ((tag (fc-find-tag)))
    (cl-return-from fc-tag-rename (fc-tag--rename tag)))
  nil)

(cl-defun fc-add-tag (mode tag-instance)
  (puthash mode tag-instance *fc--mode-tag-map*))

(fc-add-to-hook 'after-change-major-mode-hook
                #'(lambda ()
                    (when buffer-file-name
                      (fc-tag-open-file)))
                #'(lambda ()
                    (unless (or
                             (eq major-mode 'minibuffer-mode)
                             (eq major-mode 'minibuffer-inactive-mode))
                      (hack-local-variables))))

(puthash 'emacs-lisp-mode *fc-tag-xref* *fc--mode-tag-map*)

(provide 'fc-tag)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tag.el ends here
