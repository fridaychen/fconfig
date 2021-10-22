;;; fc-next-error.el --- Next error wrapper -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defconst *fc-navi-buffer-modes* '(compilation-mode
                                   flycheck-error-list-mode
                                   ggtags-navigation-mode
                                   grep-mode
                                   xref--xref-buffer-mode))

(fc-load 'simple
  :local t
  :after (progn
           (defun fc--find-visible-next-error-buffer ()
             "Find visible next-error buffer by major-mode."
             (cdr (fc-first-window (with-current-buffer (cdr it)
                                     (member major-mode *fc-navi-buffer-modes*)))))

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
                                (member major-mode *fc-navi-buffer-modes*)))
              :error-msg "No navigatable buffer found."
              :pop t))

           (setf next-error-find-buffer-function #'fc--next-error-find-buffer)))

(provide 'fc-next-error)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-next-error.el ends here
