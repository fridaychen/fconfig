;;; fc-lsp.el --- control lsp -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-lsp-enable* t)
(defvar *fc-lsp-bridge-enable* nil)
(defvar *fc-lsp-eglot-enable* nil)
(defvar *fc-lsp-mode-enable* nil)

(fc-load 'lsp-bridge
  :local t
  :enable *fc-lsp-enable*
  :after (progn
           (message "Enabled lsp-bridge")

           (setf *fc-lsp-enable* nil
                 *fc-lsp-bridge-enable* t
                 lsp-bridge-complete-manually nil
                 lsp-bridge-enable-signature-help nil)

           (fc-add-next-error-mode 'lsp-bridge-ref-mode
                                   #'lsp-bridge-ref-jump-next-keyword
                                   #'lsp-bridge-ref-jump-prev-keyword)

           (defun fc-lsp-bridge-check ()
             (unless (lsp-bridge-epc-live-p lsp-bridge-epc-process)
               (message "restart lsp bridge")
               (lsp-bridge-restart-process)))

           (defun fc--lsp-hide ()
             (lsp-bridge-hide-signature-tooltip)
             (lsp-bridge-hide-doc-tooltip))

           (add-hook '*fc-ergo-restore-hook* #'fc--lsp-hide)

           (defun fc--lsp-enable ()
             (setq company-backends
                   (remove #'company-ispell company-backends))
             (lsp-bridge-mode 1))))

(fc-load 'eglot
  :local t
  :enable *fc-lsp-enable*
  :after (progn
           (message "Enabled eglot")

           (setf *fc-lsp-enable* nil
                 *fc-lsp-eglot-enable* t)

           (setf eglot-ignored-server-capabilities '(:documentFormattingProvider
                                                     :documentRangeFormattingProvider
                                                     :documentOnTypeFormattingProvider
                                                     :inlayHintProvider))

           (fc-set-face 'eglot-semantic-function nil
                        :inherit 'font-lock-function-call-face
                        :extend t
                        :overline nil)
           (fc-set-face 'eglot-semantic-method nil
                        :inherit 'font-lock-function-call-face
                        :extend t
                        :overline nil)
           (fc-set-face 'eglot-semantic-operator nil
                        :inherit 'font-lock-operator-face
                        :extend t
                        :overline nil)
           (fc-set-face 'eglot-semantic-readonly nil
                        :overline nil)

           (defun fc--setup-eglot ()
             (let ((buf (current-buffer)))
               (fc-delay
                 (with-current-buffer buf
                   (flymake-mode 1)))))

           (add-hook 'eglot-managed-mode-hook #'fc--setup-eglot)

           (defun fc--lsp-enable ()
             (eglot-ensure))))

(fc-load 'lsp-mode
  :enable *fc-lsp-enable*
  :before (setq lsp-modeline-code-actions-enable nil
                lsp-modeline-diagnostics-enable nil
                lsp-modeline-workspace-status-enable nil
                lsp-headerline-breadcrumb-enable nil
                lsp-progress-via-spinner nil
                lsp-enable-on-type-formatting nil
                lsp-completion-provider :none)
  :after (progn
           (require 'lsp)
           (require 'lsp-mode)

           (message "Enabled lsp-mode")

           (setq *fc-lsp-enable* nil
                 *fc-lsp-mode-enable* t)

           (defun fc--lsp-enable ()
             (lsp-deferred))))

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
   ((and *fc-lsp-bridge-enable* lsp-bridge-mode)
    (fc-lsp-bridge-check)
    (lsp-bridge-popup-documentation)
    t)

   ((and *fc-lsp-eglot-enable* (eglot-managed-p))
    (fc-funcall #'eldoc-box-help-at-point)
    t)

   ((and *fc-lsp-mode-enable* lsp-mode)
    (lsp-ui-doc-show)
    t)))

(cl-defun fc--lsp-find-definitions ()
  (cond
   ((and *fc-lsp-bridge-enable* lsp-bridge-mode)
    (fc-lsp-bridge-check)
    (lsp-bridge-find-def))

   (t
    (let ((id (fc-current-thing)))
      (set-text-properties 0 1 '(identifier-at-point t) id)
      (xref-find-definitions id)))))

(cl-defun fc--lsp-find-references ()
  (cond
   ((and *fc-lsp-bridge-enable* lsp-bridge-mode)
    (fc-lsp-bridge-check)
    (lsp-bridge-find-references))

   (t
    (let ((id (fc-current-thing)))
      (set-text-properties 0 1 '(identifier-at-point t) id)
      (xref-find-references id)))))

(cl-defun fc--lsp-find-apropos (pattern)
  (cond
   ((and *fc-lsp-bridge-enable* lsp-bridge-mode)
    (fc-lsp-bridge-check)
    (lsp-bridge-workspace-list-symbols pattern))

   (t
    (xref-find-apropos pattern))))

(cl-defun fc--lsp-list-tag ()
  (cond
   ((and *fc-lsp-bridge-enable* lsp-bridge-mode)
    (fc-lsp-bridge-check)
    (fc-funcall #'lsp-bridge-workspace-list-symbols))

   (t
    (fc-funcall #'lsp-ivy-workspace-symbol))))

(cl-defun fc--lsp-active-p ()
  (or (and *fc-lsp-bridge-enable* lsp-bridge-mode)
      (and *fc-lsp-eglot-enable* (eglot-managed-p))
      (and *fc-lsp-mode-enable* lsp-mode)))

(cl-defun fc--lsp-rename ()
  (cond
   ((and *fc-lsp-bridge-enable* lsp-bridge-mode)
    (fc-lsp-bridge-check)
    (lsp-bridge-rename)
    t)

   ((and *fc-lsp-eglot-enable* (eglot-managed-p))
    (fc-funcall #'eglot-rename)
    t)

   ((and *fc-lsp-mode-enable* lsp-mode)
    (when (lsp--capability :renameProvider)
      (progn
        (call-interactively #'lsp-rename)
        t)))

   (t)))

(cl-defun fc--lsp-open-project (proj-dir)
  )

(provide 'fc-lsp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-lsp.el ends here
