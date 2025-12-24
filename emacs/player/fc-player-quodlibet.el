;;; fc-player-quodlibet.el --- Linux Quod Libet player -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-quodlibet*
  (cond (*is-linux* "quodlibet")
        (*is-mac* "/Applications/QuodLibet.app/Contents/MacOS/quodlibet")
        (t nil)))

(defvar *fc--quodlibet-control* nil)

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
  "Run quodlibet commands.
CMDS: list of command."
  (unless *fc--quodlibet-control*
    (setf *fc--quodlibet-control*
          (fc-file-first-exists '("~/.config/quodlibet/control"
                                  "~/.quodlibet/control"))))

  (unless *fc--quodlibet-control*
    (user-error "Not found quodlibet control file!"))

  (f-write-text
   (fc-text cmds :separator "\n")
   'utf-8
   *fc--quodlibet-control*))

(defconst *fc-cmus-map* (fc-make-hash-table
                         '(
                           (a . "artist")
                           (b . "album")
                           (d . "disc")
                           (g . "genre")
                           (n . "track")
                           (p . "composer")
                           (t . "title")
                           )))

(defun fc-quodlibet-eval-expr (x)
  (pcase x
    (`(,c ,v)
     (format "%s=/^%s$/"
             (gethash c *fc-quodlibet-map*)
             v))
    (`(,c has ,v)
     (format "%s=%s"
             (gethash c *fc-quodlibet-map*)
             v))
    ((and `(,c ,op ,v)
          (guard (member op '(> >= < <= !=))))
     (format "%s %s %s"
             (gethash c *fc-quodlibet-map*)
             (symbol-name op)
             v))
    ((and `(,n1 ,op1 n ,op2 ,n2)
          (guard (and (member op1 '(> >= < <=))
                      (member op2 '(> >= < <=)))))
     (format "(%s %s %s & %s %s %s)"
             (gethash 'n *fc-quodlibet-map*)
             (pcase op1
               ('< '>)
               ('> '<)
               ('<= '>=)
               ('>= '<=))
             n1
             (gethash 'n *fc-quodlibet-map*)
             (symbol-name op2)
             n2
             ))))

(defun fc-quodlibet-open (path)
  (let ((def (car (read-from-string path))))
    (fc-quodlibet-play
     (concat "&("
             (s-join ", "
                     (cl-mapcar #'fc-quodlibet-eval-expr def))
             ")"))))

(defconst *fc-player-quodlibet* (fc-player-quodlibet :name "quod Libet [app]"))

(provide 'fc-player-quodlibet)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player-quodlibet.el ends here
