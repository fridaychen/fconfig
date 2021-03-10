;;; fc-hideshow.el --- setup hideshow -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-enable-hideshowvis* nil)

(defvar *fc-prog-modes* nil)
(defvar *fc-doc-modes* nil)

(defvar *fc-show-all-state* t)
(make-local-variable '*fc-show-all-state*)

(fc-load 'hideshowvis
  :enable *fc-enable-hideshowvis*

  :after (progn
           (autoload 'hideshowivs-enable "hideshowvis" "Highlight foldable regions")
           (hideshowvis-symbols)))

(cl-flet ((is-hs () (and (boundp 'hs-minor-mode)
                         hs-minor-mode))
          (is-outline ()
                      (and (boundp 'outline-minor-mode)
                           outline-minor-mode)))
  (defun fc-hide-show-init ()
    (cond
     ((member major-mode *fc-prog-modes*)
      (hs-minor-mode 1)
      (when *fc-enable-hideshowvis*
        (hideshowvis-enable)))

     ((member major-mode *fc-doc-modes*)
      (outline-minor-mode 1))))

  (defun fc-show-all ()
    "Show all levels."
    (cond
     ((is-hs)
      (hs-show-all))

     ((is-outline)
      (outline-show-all))))

  (defun fc-hide-all ()
    "Hide leafs at too level."
    (cond
     ((is-hs)
      (hs-hide-all))

     ((is-outline)
      (outline-hide-body))))

  (defun fc-toggle-hide-show-all ()
    "Toggle show and hide at top level."
    (interactive)

    (if *fc-show-all-state*
        (fc-hide-all)
      (fc-show-all))

    (setq-local *fc-show-all-state* (not *fc-show-all-state*)))

  (defun fc-toggle-hide-show ()
    "Toggle show/hide at current leaf."
    (interactive)

    (cond
     ((is-hs)
      (hs-toggle-hiding))

     ((is-outline)
      (outline-toggle-children))

     ((eq major-mode 'org-mode)
      (org-cycle)))))

(fc-require 'hideshow)

(provide 'fc-hideshow)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-hideshow.el ends here
