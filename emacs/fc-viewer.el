;;; fc-viewer.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar-local *fc-reading-line-spacing* 10)
(defvar-local *fc-reading-changed-font* nil)

(defconst *fc-reading-title-limit* 22)
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
  (list
   *fc--viewer-mark*
   (fc-text
    (substring-no-properties (or (which-function) ""))
    :limit (- (window-width) 10)
    :keys *fc-buffer-id-keymap*)))

(defun fc--viewer-enter ()
  "Enter viewer mode."
  (interactive)

  (setf fc-viewer-minor-mode t
        *fc-enable-state-seg* nil
        *fc-enable-major-mode-seg* nil
        *fc-buffer-title-seg* #'fc--viewer-seg)

  (-fc-viewer-adjust-width)

  (hl-line-mode -1)
  (read-only-mode 1)

  (setf line-spacing *fc-reading-line-spacing*)

  (when (and (not buffer-face-mode) *fc-reading-face*)
    (setf buffer-face-mode-face *fc-reading-face*
          *fc-reading-changed-font* t)
    (buffer-face-mode 1))

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

  (when *fc-reading-changed-font*
    (setf *fc-reading-changed-font* nil)
    (buffer-face-mode -1))

  (hl-line-mode -1)
  (read-only-mode -1)

  (fc-modal-visual-feedback)

  (fc--setup-line-spacing)

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
