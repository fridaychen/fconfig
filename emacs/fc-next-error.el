;;; fc-next-error.el --- Next error wrapper -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defvar *fc--next-error-modes* nil)
(defvar *fc--next-error-map* (make-hash-table))

(fc-load 'simple
  :local t
  :after (progn
           (defun fc--next-error-buffer-p (buf)
             (member (buffer-local-value 'major-mode buf)
                     *fc--next-error-modes*))

           (defun fc--find-visible-next-error-buffer ()
             "Find visible next-error buffer by major-mode."
             (let ((buf (cdr (fc-first-window
                              (fc--next-error-buffer-p (cdr it))))))
               (if (and buf
                        (eq (buffer-local-value 'major-mode buf)
                            'flycheck-error-list-mode))
                   (if next-error-function
                       (current-buffer)
                     nil)
                 buf)))

           (defun fc--next-error-find-buffer (&rest _args)
             (or (fc--find-visible-next-error-buffer)
                 (and (buffer-name next-error-last-buffer)
                      next-error-last-buffer)))

           (defun fc--clear-next-error-buffer ()
             (setf next-error-last-buffer nil))

           (defun fc-switch-next-error-buffer ()
             "Switch a next-error buffer."
             (fc-switch-to-buffer
              "Next-error buffer"
              (fc-list-buffer :not-file t
                              :filter
                              (lambda ()
                                (member major-mode *fc--next-error-modes*)))
              :error-msg "No navigatable buffer found."
              :pop (not (fc--next-error-buffer-p (current-buffer)))))

           (setf next-error-find-buffer-function #'fc--next-error-find-buffer)))

(cl-defun fc-add-next-error-mode (mode next prev)
  "Add mode support next-prev.
MODE: major mode.
NEXT: next function.
PREV: previous function."
  (add-to-list '*fc--next-error-modes* mode)
  (puthash mode (list next prev) *fc--next-error-map*))

(cl-defun fc-next-error ()
  "Goto next error."
  (when-let* ((buf (fc--next-error-find-buffer))
              (ops (gethash (buffer-local-value 'major-mode buf)
                            *fc--next-error-map*))
              (next (cl-first ops)))
    (with-current-buffer buf
      (call-interactively next)
      t)))

(cl-defun fc-prev-error ()
  "Goto previous error."
  (when-let* ((buf (fc--next-error-find-buffer))
              (ops (gethash (buffer-local-value 'major-mode buf)
                            *fc--next-error-map*))
              (prev (cl-second ops)))
    (with-current-buffer buf
      (call-interactively prev)
      t)))

(--each '(compilation-mode
          flycheck-error-list-mode
          ggtags-navigation-mode
          grep-mode
          xref--xref-buffer-mode)
  (fc-add-next-error-mode it #'next-error #'previous-error))

(provide 'fc-next-error)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-next-error.el ends here
