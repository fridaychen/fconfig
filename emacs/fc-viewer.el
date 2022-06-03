;;; fc-viewer.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar-local *fc-reading-line-spacing* 10)
(defconst *fc-reading-title-limit* 22)
(defvar-local *fc-bak-line-spacing* nil)

(defvar *fc-viewer-hook* nil "After viewer mode toggled hook.")

(defvar *fc-viewer-keymap*
  (fc-make-keymap nil "fc-viewer")
  "Keymap of viewer mode.")

(cl-defun -fc-viewer-adjust-width ()
  "Adjust viewer buffer width."
  (when (and (not (window-combined-p))
             (> (window-width) *fc-reading-fill*))
    (fc-set-window-width *fc-reading-fill*)))

(cl-defun -fc-viewer-display ()
  "View display function."
  (when (and fc-viewer-minor-mode
             (<= (- (frame-height) (window-height)) 3))
    (hl-line-mode 1)
    (-fc-viewer-adjust-width)))

(cl-defun fc-viewer-list-buffer ()
  "List viewer buffer."
  (fc-list-buffer :filter
                  (lambda ()
                    fc-viewer-minor-mode)))

(defun fc-viewer-toggle ()
  "Toggle viewer mode."
  (interactive)

  (if fc-viewer-minor-mode
      (fc--viewer-quit)
    (fc--viewer-enter))

  (fc-run-hook '*fc-viewer-hook*))

(defvar *fc--viewer-mark* (fc-visible "📜" "RD"))

(defun fc--viewer-seg ()
  "Generate viewer state string."
  (fc-text (list *fc--viewer-mark*
                 (which-function)
                 (file-name-sans-extension
                  (buffer-name)))
           :face `(:foreground ,(color-complement-hex
                                 (fc-get-face-attribute (fc--modeline-base-face) :background))
                               :inherit ,(fc--modeline-base-face))
           :limit (- (window-width) 10)
           :separator " :"
           :keys *fc-buffer-id-keymap*))

(defun fc--viewer-enter ()
  "Enter viewer mode."
  (interactive)

  (setf fc-viewer-minor-mode t
        *fc-enable-state-seg* nil
        *fc-enable-major-mode-seg* nil
        *fc-buffer-title-seg* #'fc--viewer-seg)

  (-fc-viewer-adjust-width)

  (hl-line-mode 1)
  (read-only-mode 1)

  (fc-modal-set-cursor-shape nil)

  (setf *fc-bak-line-spacing* line-spacing
        line-spacing *fc-reading-line-spacing*)

  (unless buffer-display-table
    (setq-local buffer-display-table #^[nil nil display-table nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 92 nil nil nil nil]))

  (set-display-table-slot buffer-display-table 'wrap ?\x20))

(defun fc--viewer-quit ()
  "Quit viewer mode."
  (interactive)

  (setf fc-viewer-minor-mode nil
        *fc-enable-state-seg* t
        *fc-enable-major-mode-seg* t
        *fc-buffer-title-seg* nil)

  (hl-line-mode -1)
  (read-only-mode -1)

  (fc-modal-visual-feedback)

  (setf line-spacing *fc-bak-line-spacing*)

  (set-display-table-slot buffer-display-table 'wrap ?\\))

(define-minor-mode fc-viewer-minor-mode
  "Viewer minor mode."
  :global nil
  :lighter " Viewer"
  :keymap *fc-viewer-keymap*
  (fc-viewer-toggle))

(when (< *fc-reading-fill* window-min-width)
  (setf *fc-reading-fill* window-min-width))

(fc-add-display-hook #'-fc-viewer-display)

(provide 'fc-viewer)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-viewer.el ends here
