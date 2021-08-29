;;; fc-control.el --- control mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defvar *fc-control-mode* '(no touch mouse))

(cl-defun fc-mouse-turn-page (event)
  "Turning page.
EVENT: mouse evnet."
  (let* ((posn (elt event 1))
         (start (fc-point-to-line (window-start)))
         (end (fc-point-to-line (window-end)))
         (middle (/ (- end start) 2)))
    (if (or (= start (fc-line-num))
            (< (fc-line-num) middle))
        (fc-funcall #'scroll-down-command)
      (fc-funcall #'scroll-up-command))))

(defun fc-mouse-func (event)
  "Entry for all mouse events.
EVENT: mouse event."
  (interactive "e")

  (pcase (fc-current-control-mode)
    ('mouse
     (mouse-drag-region event))

    ('touch
     (let ((f (intern (format "fc-%s-mouse-func"
                              (symbol-name major-mode)))))
       (when (and
              (fboundp f)
              (apply f (list event)))
         (cl-return-from fc-mouse-func))

       (fc-mouse-turn-page event)))))

(defun fc-current-control-mode ()
  "Get current control mode."
  (car *fc-control-mode*))

(defun fc-next-control-mode ()
  "Toggle control mode."
  (interactive)

  (setf *fc-control-mode* (-rotate 1 *fc-control-mode*)))

(provide 'fc-control)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-control.el ends here
