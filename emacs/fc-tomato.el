;;; fc-tomato.el --- tomato -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-tomato-bar* nil)

(defvar *fc-tomato-work-interval* (* 60 25)
  "Interval of time you will be working, in seconds.")
(defvar *fc-tomato-rest-interval* (* 60 5)
  "Interval of time you will be resting, in seconds.")

(defvar *fc-tomato-start-hook* nil)
(defvar *fc-tomato-rest-hook* nil)
(defvar *fc-tomato-done-hook* nil)

(let* ((tomato-steps 8.0)
       (tomato-workbar-interval (/ *fc-tomato-work-interval* tomato-steps))
       (tomato-restbar-interval (/ *fc-tomato-rest-interval* tomato-steps))
       (tomato-start-time nil)
       (tomato-timer nil)
       (tomato-bars
        `((,tomato-workbar-interval "█" "#ffff66")
          (,tomato-workbar-interval "▇" "#ffcc66")
          (,tomato-workbar-interval "▆" "#cc9966")
          (,tomato-workbar-interval "▅" "#ff9966")
          (,tomato-workbar-interval "▄" "#cc6666")
          (,tomato-workbar-interval "▃" "#ff6666")
          (,tomato-workbar-interval "▂" "#ff3366")
          (,tomato-workbar-interval "▁" "#ff0066")

          (,tomato-restbar-interval "▁" "#00cc66")
          (,tomato-restbar-interval "▂" "#33cc66")
          (,tomato-restbar-interval "▃" "#66cc66")
          (,tomato-restbar-interval "▄" "#00ff66")
          (,tomato-restbar-interval "▅" "#33ff66")
          (,tomato-restbar-interval "▆" "#66ff66")
          (,tomato-restbar-interval "▇" "#99ff66")
          (,tomato-restbar-interval "█" "#ccff66")
          (nil nil nil))))

  (defun fc--tomato-reset-bar ()
    (fc--tomato-update-bar (car (last tomato-bars))))

  (defun fc--tomato-state-string ()
    (if (fc-tomato-running-p)
        (format "Start at %s, elapsed %s"
                (format-time-string "%p %M:%S" tomato-start-time)
                (format-time-string "%M:%S" (time-subtract (current-time) tomato-start-time)))
      "Not running"))

  (cl-defun fc--tomato-propertize (bar bar-color)
    "Propertize BAR with BAR-COLOR, help echo, and click action."
    (unless bar
      (cl-return-from fc--tomato-propertize ""))
    (fc-text (concat " " bar " ")
             :face `(:foreground ,bar-color)
             :tip '(fc--tomato-state-string)
             :pointer 'hand
             :keys (fc-make-keymap
                    `(
                      ([mode-line down-mouse-1] fc-tomato)))))

  (defun fc--tomato-update-bar (bar-def)
    (setf *fc-tomato-bar* (fc--tomato-propertize (cl-second bar-def)
                                                 (cl-third bar-def))))

  (defun fc-tomato-timer-func (bars)
    (fc--tomato-update-bar (car bars))

    (when (equal (cl-third (car bars)) "#00cc66")
      (run-hooks '*fc-tomato-rest-hook*))

    (let ((time (caar bars)))
      (if time
          (setf tomato-timer
                (run-at-time time
                             nil
                             #'fc-tomato-timer-func
                             (cdr bars)))
        (fc--tomato-reset-bar)
        (setf tomato-timer nil)
        (run-hooks '*fc-tomato-done-hook*)))
    (force-mode-line-update t))

  (defun fc--tomato-start ()
    (run-hooks '*fc-tomato-start-hook*)
    (setf tomato-start-time (current-time))
    (fc-tomato-timer-func tomato-bars))

  (defun fc--tomato-stop ()
    (cancel-timer tomato-timer)
    (setf tomato-timer nil)
    (fc--tomato-reset-bar))

  (defun fc--tomato-running-p ()
    tomato-timer)

  (defun fc-tomato ()
    "start or stop tomato timer."
    (interactive)

    (if (fc--tomato-running-p)
        (fc--tomato-stop)
      (fc--tomato-start)))

  (defun fc-tomato-init ()
    (fc--tomato-reset-bar)))

(defun fc--tomato-modeline ()
  "Returns the tomate status."
  (when (fc--wide-window-p)
    *fc-tomato-bar*))

(add-to-list 'global-mode-string '(t (:eval (fc--tomato-modeline))))

(provide 'fc-tomato)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tomato.el ends here
