;;; fc-viewer.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar-local *fc-viewer-mode* nil)
(defvar-local *fc-reading-line-spacing* 10)
(defconst *fc-reading-title-limit* 22)
(defvar-local *fc-bak-line-spacing* nil)

(defvar *fc-viewer-hook* nil "After viewer mode toggled hook.")

(cl-defun -fc-viewer-adjust-width ()
  "Adjust viewer buffer width."
  (when (> (window-width) *fc-reading-fill*)
    (fc-set-window-width :width *fc-reading-fill*)))

(cl-defun -fc-viewer-display ()
  "View display function."
  (when *fc-viewer-mode*
    (hl-line-mode 1)
    (-fc-viewer-adjust-width)))

(defun fc-viewer-enter ()
  "Enter reading mode."
  (interactive)

  (fc-set-window-width :width *fc-reading-fill*)

  (hl-line-mode 1)
  (read-only-mode 1)

  (setf *fc-bak-line-spacing* line-spacing
        line-spacing *fc-reading-line-spacing*)

  (setq-local *fc-show-line-col-mode* nil)

  (unless buffer-display-table
    (setq-local buffer-display-table #^[nil nil display-table nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 92 nil nil nil nil]))

  (set-display-table-slot buffer-display-table 'wrap ?\x20))

(defun fc-viewer-quit ()
  "Quit viewer mode."
  (interactive)

  (hl-line-mode -1)
  (read-only-mode -1)

  (setf line-spacing *fc-bak-line-spacing*)
  (setq-local *fc-show-line-col-mode* t)

  (set-display-table-slot buffer-display-table 'wrap ?\\))

(defun fc-viewer-toggle ()
  "Toggle viewer mode."
  (interactive)

  (if *fc-viewer-mode*
      (fc-viewer-quit)
    (fc-viewer-enter))

  (setq-local *fc-viewer-mode* (not *fc-viewer-mode*))
  (fc-run-hook '*fc-viewer-hook*))

(cl-defun fc-viewer-list ()
  "List viewer buffer."
  (fc-list-buffer :filter
                  (lambda ()
                    *fc-viewer-mode*)))

(cl-defun fc-viewer-mode-p ()
  "Test if current buffer is under viewer mode."
  (and (boundp '*fc-viewer-mode*)
       *fc-viewer-mode*))

(fc-add-display-hook #'-fc-viewer-display)

(provide 'fc-viewer)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-viewer.el ends here
