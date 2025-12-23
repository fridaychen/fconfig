;;; fc-player-cmus.el --- Linux Quod Libet player -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defclass fc-player-cmus (fc-player)
  ((play-state :initform 'Paused
               :type symbol)
   (meta :initform nil
         :type list)))

(cl-defmethod initialize-instance :after ((x fc-player-cmus) &rest args)
  )

(cl-defmethod fc-player--play-pause ((x fc-player-cmus))
  (fc-cmus-cmd "--pause")
  (fc-delay
    (oset x play-state
          (intern
           (capitalize
            (cadr (split-string (car (split-string (fc-cmus-cmd "-C" "status") "\n")) " ")))))
    (run-hooks '*fc-player-hook*))
  (cl-call-next-method x))

(cl-defmethod fc-player--next ((x fc-player-cmus))
  (fc-cmus-cmd "--next")
  (cl-call-next-method x))

(cl-defmethod fc-player--previous ((x fc-player-cmus))
  (fc-cmus-cmd "--prev")
  (cl-call-next-method x))

(cl-defmethod fc-player--get-play-status ((x fc-player-cmus))
  (oref x play-state))

(cl-defmethod fc-player--get-volume ((x fc-player-cmus))
  (oref x volume))

(cl-defmethod fc-player--set-volume ((x fc-player-cmus) vol)
  (oset x volume vol)
  (fc-cmus-cmd "-C" "format_print %{lvolume}"))

(cl-defmethod fc-player--get-metadata ((x fc-player-cmus))
  (seq-mapn
   #'cons
   '(artist album title)
   (split-string
    (string-replace
     "\n" " "
     (fc-cmus-cmd "-C" "format_print %{artist} | %{album} | %{title}"))
    " | ")))

(cl-defmethod fc-player--show-metadata ((x fc-player-cmus))
  (let* ((data (fc-player--get-metadata x)))
    (cl-call-next-method x
                         (alist-get 'artist data)
                         (alist-get 'album data)
                         (alist-get 'title data))))

(defun fc-cmus-cmd (&rest cmds)
  "Run cmus commands.
CMDS: list of command."
  (apply #'fc-exec-command-to-string
         (cons "cmus-remote" cmds)))

(defconst *fc-cmus-map* (fc-make-hash-table
                         '(
                           (a . "artist")
                           (b . "album")
                           (d . "discnumber")
                           (g . "genre")
                           (n . "tracknumber")
                           (p . "composer")
                           (t . "title")
                           )))

(defun fc-cmus-play (filter)
  (when *fc-dev-mode*
    (message "cmus filter: %s" filter))

  (fc-cmus-cmd "-C" (concat "filter " filter))
  (sit-for 1)
  (fc-cmus-cmd "--next")
  (fc-cmus-cmd "--play"))

(cl-defun fc-cmus-item-string (o &key has)
  (cond ((stringp o)
         (if has
             (format "\"*%s*\"" o)
           (format "\"%s\"" o)))

        ((symbolp o)
         (if has
             (format "\"*%s*\"" (symbol-name o))
           (format "\"%s\"" (symbol-name o))))

        ((numberp o)
         (number-to-string o))))

(defun fc-cmus-eval-expr (x)
  (pcase x
    (`(,c ,v)
     (format "%s=%s"
             (gethash c *fc-cmus-map*)
             (fc-cmus-item-string v)))
    (`(,c has ,v)
     (format "%s=%s"
             (gethash c *fc-cmus-map*)
             (fc-cmus-item-string v :has t)))
    ((and `(,c ,op ,v)
          (guard (member op '(> >= < <= !=))))
     (format "%s %s %s"
             (gethash c *fc-cmus-map*)
             (symbol-name op)
             (fc-cmus-item-string v)))
    ((and `(,n1 ,op1 n ,op2 ,n2)
          (guard (and (member op1 '(> >= < <=))
                      (member op2 '(> >= < <=)))))
     (format "(%s %s %s & %s %s %s)"
             (gethash 'n *fc-cmus-map*)
             (pcase op1
               ('< '>)
               ('> '<)
               ('<= '>=)
               ('>= '<=))
             n1
             (gethash 'n *fc-cmus-map*)
             (symbol-name op2)
             n2
             ))))

(defun fc-cmus-open (path)
  (let ((def (car (read-from-string path))))
    (fc-cmus-play
     (s-join " & "
             (cl-mapcar #'fc-cmus-eval-expr def)))))

(provide 'fc-player-cmus)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player-cmus.el ends here
