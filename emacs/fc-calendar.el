;;; fc-calendar.el --- setup calendar -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(require 'calendar)
(require 'holidays)

(fc-load 'japanese-holidays
  :after (progn
           (setf calendar-holidays (append japanese-holidays
                                           holiday-local-holidays
                                           holiday-other-holidays)

                 calendar-mark-holidays-flag t

                 japanese-holiday-weekend '(0 6)

                 japanese-holiday-weekend-marker '(holiday
                                                   nil
                                                   nil
                                                   nil
                                                   nil
                                                   nil
                                                   japanese-holiday-saturday))

           (fc-add-to-hook 'calendar-today-visible-hook
                           #'japanese-holiday-mark-weekend
                           #'calendar-mark-today)
           (add-hook 'calendar-today-invisible-hook
                     #'japanese-holiday-mark-weekend)))

(fc-install 'calfw 'calfw-cal)

(defvar fc-calendar-ics "")

(defun fc-calendar ()
  "Show calendar."
  (interactive)
  (cfw:open-calendar-buffer))

(provide 'fc-calendar)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-calendar.el ends here
