;;; fc-next-error.el --- Next error wrapper -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defvar *fc--next-error-map* (make-hash-table))

(fc-load 'simple
  :local t
  :after (progn
           (defun fc--next-error-buffer-p (buf &rest rest)
             (gethash (buffer-local-value 'major-mode (get-buffer buf))
                      *fc--next-error-map*))

           (defun fc--find-visible-next-error-buffer ()
             "Find visible next-error buffer by major-mode."
             (cdr (fc-first-window
                   (fc--next-error-buffer-p (cdr it)))))

           (defun fc--next-error-find-buffer (&rest _args)
             (or (fc--find-visible-next-error-buffer)
                 (and (buffer-name next-error-last-buffer)
                      next-error-last-buffer)))

           (defun fc--clear-next-error-buffer ()
             (setf next-error-last-buffer nil))

           (defun fc-switch-next-error-buffer ()
             "Switch a next-error buffer."
             (fc-select-buffer
              "Next-error buffer"
              (list :not-file t
                    :mode *fc--next-error-map*
                    :no-curr t)
              :error-msg "No navigatable buffer found."
              :pop (not (fc--next-error-buffer-p (current-buffer)))))

           (setf next-error-find-buffer-function #'fc--next-error-find-buffer)))

(cl-defun fc-add-next-error-mode (mode next prev)
  "Add mode support next-prev.
MODE: major mode.
NEXT: next function.
PREV: previous function."
  (puthash mode (list next prev) *fc--next-error-map*))

(cl-defun fc-next-error ()
  "Goto next error."
  (when-let* ((buf (fc--next-error-find-buffer))
              (ops (gethash (buffer-local-value 'major-mode buf)
                            *fc--next-error-map*))
              (next (cl-first ops)))
    (call-interactively next)
    t))

(cl-defun fc-prev-error ()
  "Goto previous error."
  (when-let* ((buf (fc--next-error-find-buffer))
              (ops (gethash (buffer-local-value 'major-mode buf)
                            *fc--next-error-map*))
              (prev (cl-second ops)))
    (call-interactively prev)
    t))

(--each '(compilation-mode
          ggtags-navigation-mode
          grep-mode
          occur-mode
          xref--xref-buffer-mode)
  (fc-add-next-error-mode it #'next-error #'previous-error))

(provide 'fc-next-error)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-next-error.el ends here
