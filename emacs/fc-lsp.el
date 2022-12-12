;;; fc-lsp.el --- control lsp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-lsp-enable* t)
(defvar *fc-lsp-bridge-enable* nil)
(defvar *fc-lsp-mode-enable* nil)

(fc-load 'lsp-bridge
  :local t
  :enable *fc-lsp-enable*
  :after (progn
           (message "Enabled lsp-bridge")

           (setf *fc-lsp-enable* nil
                 *fc-lsp-bridge-enable* t
                 lsp-headerline-breadcrumb-enable nil)

           (fc-add-next-error-mode 'lsp-bridge-ref-mode
                                   #'lsp-bridge-ref-jump-next-keyword
                                   #'lsp-bridge-ref-jump-prev-keyword)

           (defun fc--lsp-enable ()
             (lsp-bridge-mode 1))))

;; (fc-load 'eglot
;;   :local t
;;   :after (progn
;;            (setf *fc-lsp-enable* nil)))

(fc-load 'lsp-mode
  :enable *fc-lsp-enable*
  :after (progn
           (require 'lsp)
           (require 'lsp-mode)

           (message "Enabled lsp-mode")

           (setq *fc-lsp-enable* nil
                 *fc-lsp-mode-enable* t
                 lsp-headerline-breadcrumb-enable nil
                 lsp-progress-via-spinner nil
                 lsp-enable-on-type-formatting nil)

           (defun fc--lsp-enable ()
             (lsp-mode 1))))

(fc-load 'lsp-ui
  :after (progn
           (setf lsp-ui-sideline-global t
                 lsp-ui-sideline-delay 2
                 lsp-ui-doc-enable t
                 lsp-ui-doc-show-with-cursor nil
                 lsp-ui-doc-show-with-mouse t
                 lsp-ui-doc-alignment 'window
                 lsp-ui-doc-delay 0.1)))

(cl-defun fc--lsp-descripbe-function ()
  "Describe function."
  (cond
   (lsp-bridge-mode
    (lsp-bridge-popup-documentation)
    t)

   (lsp-mode
    (lsp-ui-doc-show)
    t)

   (t)))

(cl-defun fc--lsp-find-definitions ()
  (cond
   (lsp-bridge-mode
    (lsp-bridge-find-def))

   (t
    (xref--find-definitions id nil))))

(cl-defun fc--lsp-find-references ()
  (cond
   (lsp-bridge-mode
    (lsp-bridge-find-references))

   (t
    (xref--find-xrefs id 'references id nil))))

(cl-defun fc--lsp-find-apropos (pattern)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-workspace-list-symbols pattern))

   (t
    (xref-find-apropos pattern))))

(cl-defun fc--lsp-list-tag ()
  (cond
   (lsp-bridge-mode
    (fc-funcall #'lsp-bridge-workspace-list-symbols))

   (t
    (fc-funcall #'lsp-ivy-workspace-symbol))))

(cl-defun fc--lsp-active-p ()
  (or lsp-bridge-mode lsp-mode))

(cl-defun fc--lsp-rename ()
  (cond
   (lsp-bridge-mode
    (lsp-bridge-rename)
    t)

   (lsp-mode
    (when (lsp--capability :renameProvider)
      (progn
        (lsp-rename)
        t)))

   (t)))

(provide 'fc-lsp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-lsp.el ends here
