;;; fc-hideshow.el --- setup hideshow -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-hs-init-hide-all-modes* '(markdown-mode org-mode))

(defvar *fc-doc-modes* nil)

(defvar *fc-hs-show-all* t)
(make-local-variable '*fc-hs-show-all*)

(fc-require 'hideshow)
(fc-require 'outline)

(defun fc-hs--is-hideshow ()
  (bound-and-true-p hs-minor-mode))

(defun fc-hs--is-outline ()
  (bound-and-true-p outline-minor-mode))

(defun fc-hs-init ()
  (when (member major-mode *fc-hs-init-hide-all-modes*)
    (setf *fc-hs-show-all* nil))

  (cond
   ((derived-mode-p 'prog-mode)
    (hs-minor-mode 1))

   ((member major-mode *fc-doc-modes*)
    (outline-minor-mode 1))))

(defun fc-hs--show-all ()
  "Show all levels."
  (cond
   ((eq major-mode 'org-mode)
    (org-show-all '(headings)))

   ((fc-hs--is-hideshow)
    (hs-show-all))

   ((fc-hs--is-outline)
    (outline-show-all))))

(defun fc-hs--hide-all ()
  "Hide leafs at too level."
  (cond
   ((eq major-mode 'org-mode)
    (fc--org-hide-all))

   ((fc-hs--is-hideshow)
    (hs-hide-all))

   ((fc-hs--is-outline)
    (outline-hide-body))))

(defun fc-hs-toggle-all ()
  "Toggle show and hide at top level."
  (interactive)

  (if *fc-hs-show-all*
      (fc-hs--hide-all)
    (fc-hs--show-all))

  (setf *fc-hs-show-all* (not *fc-hs-show-all*)))

(defun fc-hs-toggle ()
  "Toggle show/hide at current leaf."
  (interactive)

  (cond
   ((fc-hs--is-hideshow)
    (hs-toggle-hiding))

   ((eq major-mode 'org-mode)
    (unless (fc--org-toggle-hideshow)
      (ignore-errors
        (outline-toggle-children))))

   ((fc-hs--is-outline)
    (ignore-errors
      (outline-toggle-children)))))

(provide 'fc-hideshow)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-hideshow.el ends here
