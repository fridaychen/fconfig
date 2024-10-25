;;; fc-control.el --- control mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defvar *fc-control-mode* 'no)
(defvar *fc-control-modes* '(no touch mouse))

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

  ;; (message "mouse event %s" event)
  (pcase *fc-control-mode*
    ('mouse
     (mouse-drag-region event))

    ('touch
     (message "touch mode")
     (fc-call-mode-func "mouse-func"
                        (lambda (evt) (fc-mouse-turn-page evt))
                        event))))

(defun fc-user-select-control-mode ()
  "Allow user to select control mode."
  (when-let* ((mode (fc-select (format "Control mode <%s>" *fc-control-mode*)
                        *fc-control-modes*)))
    (setf *fc-control-mode* mode)))

(provide 'fc-control)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-control.el ends here
