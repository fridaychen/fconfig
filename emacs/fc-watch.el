;;; fc-watch.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(let ((start (current-time)))
  (defun fc--time-delta (b a)
    (* 1000.0 (float-time (time-subtract b a))))

  (defun fc-watch-start ()
    (setq start (current-time)))

  (defun fc-watch-record (log)
    (message "##### %.2fms : %S"
             (fc--time-delta (current-time) start)
             log))

  (add-hook 'after-init-hook
            (lambda ()
              (message "##### %.2fms : init completed in"
                       (fc--time-delta after-init-time
                                       before-init-time)))))

(provide 'fc-watch)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-watch.el ends here
