;;; fc-player-quodlibet.el --- Linux Quod Libet player -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defclass fc-player-quodlibet (fc-player)
  ((play-state :initform 'Paused
               :type symbol)
   (meta :initform nil
         :type list)
   (app :initform (symbol-value '*fc-quodlibet*))))

(cl-defmethod initialize-instance :after ((x fc-player-quodlibet) &rest args)
  )

(cl-defmethod fc-player--play-pause ((x fc-player-quodlibet))
  (fc-quodlibet-cmd "play-pause")
  (fc-delay
    (oset x play-state
          (intern
           (capitalize
            (car
             (split-string
              (fc-exec-command-to-string (oref x app) "--status"))))))
    (run-hooks '*fc-player-hook*))
  (cl-call-next-method x))

(cl-defmethod fc-player--next ((x fc-player-quodlibet))
  (fc-quodlibet-cmd "next")
  (cl-call-next-method x))

(cl-defmethod fc-player--previous ((x fc-player-quodlibet))
  (fc-quodlibet-cmd "previous")
  (cl-call-next-method x))

(cl-defmethod fc-player--get-play-status ((x fc-player-quodlibet))
  (oref x play-state))

(cl-defmethod fc-player--get-volume ((x fc-player-quodlibet))
  (oref x volume))

(cl-defmethod fc-player--set-volume ((x fc-player-quodlibet) vol)
  (oset x volume vol)
  (fc-quodlibet-cmd (format "--volume=%d" vol)))

(cl-defmethod fc-player--get-metadata ((x fc-player-quodlibet))
  (fc-exec-command (oref x app) "--print-playing" "<artist> \\| <album> \\| <title>"))

(cl-defmethod fc-player--show-metadata ((x fc-player-quodlibet))
  (let* ((data (fc-exec-command-to-string
                (oref x app)
                "--print-playing" "<artist> \\| <album> \\| <title>")))
    (apply
     #'cl-call-next-method x
     (split-string data " | "))))

(defun fc-quodlibet-cmd (cmds)
  (f-write-text
   (fc-text cmds :separator "\n")
   'utf-8
   *fc-quodlibet-control*))

(provide 'fc-player-quodlibet)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player-quodlibet.el ends here
