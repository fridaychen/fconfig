;;; fc-tomato.el --- tomato -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-tomato-bar* nil)

(defvar *fc-tomato-cycle* (cons (* 60 25) (* 60 5)))

(defvar *fc-tomato-start-hook* nil)
(defvar *fc-tomato-rest-hook* nil)
(defvar *fc-tomato-done-hook* nil)

(defconst *fc--tomato-work-stages*
  '(("█" . "#ffff66")
    ("▇" . "#ffcc66")
    ("▆" . "#cc9966")
    ("▅" . "#ff9966")
    ("▄" . "#cc6666")
    ("▃" . "#ff6666")
    ("▂" . "#ff3366")
    ("▁" . "#ff0066")))

(defconst *fc--tomato-rest-stages*
  '(("▁" . "#00cc66")
    ("▂" . "#33cc66")
    ("▃" . "#66cc66")
    ("▄" . "#00ff66")
    ("▅" . "#33ff66")
    ("▆" . "#66ff66")
    ("▇" . "#99ff66")
    ("█" . "#ccff66")))

(defvar *fc--tomato-phases* nil)
(defvar *fc--tomato-stages* nil)
(defvar *fc--tomato-timer* nil)

(cl-defun fc-tomato ()
  (when *fc--tomato-timer*
    (cancel-timer *fc--tomato-timer*)
    (setf *fc--tomato-timer* nil
          *fc-tomato-bar* nil)
    (cl-return-from fc-tomato))

  (setf *fc--tomato-phases*
        (list (cons (/ (car *fc-tomato-cycle*) (length *fc--tomato-work-stages*))
                    *fc--tomato-work-stages*)
              (cons (/ (cdr *fc-tomato-cycle*) (length *fc--tomato-rest-stages*))
                    *fc--tomato-rest-stages*))
        *fc--tomato-timer* nil
        *fc--tomato-stages* nil)

  (fc--tomato-next-phase))

(cl-defun fc--tomato-next-phase ()
  (run-hooks (pcase (length *fc--tomato-phases*)
               (2 '*fc-tomato-start-hook*)
               (1 '*fc-tomato-rest-hook*)
               (0 '*fc-tomato-done-hook*)))

  (when *fc--tomato-timer*
    (cancel-timer *fc--tomato-timer*)
    (setf *fc--tomato-timer* nil))

  (if *fc--tomato-phases*
      (setq *fc--tomato-stages* (cdar *fc--tomato-phases*)
            *fc--tomato-timer* (run-at-time nil (caar *fc--tomato-phases*) #'fc--tomato-next-stage)
            *fc--tomato-phases* (cdr *fc--tomato-phases*))
    (setq *fc-tomato-bar* nil)))

(cl-defun fc--tomato-next-stage ()
  (unless *fc--tomato-stages*
    (fc--tomato-next-phase))

  (setq *fc-tomato-bar* (fc-text
                         (concat " " (caar *fc--tomato-stages*) " ")
                         :face `(:foreground ,(cdar *fc--tomato-stages*)))
        *fc--tomato-stages* (cdr *fc--tomato-stages*))
  (force-mode-line-update))

(provide 'fc-tomato)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-tomato.el ends here
