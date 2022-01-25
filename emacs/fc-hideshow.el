;;; fc-hideshow.el --- setup hideshow -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-hs-init-hide-all-modes* '(org-mode))

(defvar *fc-doc-modes* nil)

(defvar *fc-hs-show-all* t)
(make-local-variable '*fc-hs-show-all*)

(fc-require 'hideshow)
(fc-require 'outline-mode)

(defun fc-hs--is-hideshow ()
  (and (boundp 'hs-minor-mode)
       hs-minor-mode))

(defun fc-hs--is-outline ()
  (and (boundp 'outline-minor-mode)
       outline-minor-mode))

(defun fc-hs-init ()
  (when (member major-mode *fc-hs-init-hide-all-modes*)
    (setf *fc-hs-show-all* nil))

  (cond
   ((eq major-mode 'org-mode)
    )

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
    (org-content)
    (org-hide-block-all)
    (org-hide-drawer-all))

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
   ((eq major-mode 'org-mode)
    (org-cycle))

   ((fc-hs--is-hideshow)
    (hs-toggle-hiding))

   ((fc-hs--is-outline)
    (outline-toggle-children))))

(provide 'fc-hideshow)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-hideshow.el ends here
