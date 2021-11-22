;;; fc-player-mpris.el --- Linux DBus Mpris player -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defclass fc-player-mpris (fc-player fc-dbus-intf)
  ((play-state :initarg :play-state
               :initform nil
               :type symbol)
   (meta :initarg :meta
         :initform nil
         :type list)))

(cl-defun fc-player-dbus-cb (x prop value)
  (pcase (intern prop)
    ('PlaybackStatus
     (oset x play-state (intern (car value))))

    ('Volume
     (oset *fc-player* volume (round (* (car value) 100))))

    ('Metadata
     (oset *fc-player* meta (car value))))

  (when (eq x *fc-player*)
    (run-hooks '*fc-player-hook*)))

(cl-defmethod initialize-instance :after ((x fc-player-mpris) &rest args)
  (oset x path "/org/mpris/MediaPlayer2")
  (oset x intf "org.mpris.MediaPlayer2.Player")
  (oset x prop-intf "org.freedesktop.DBus.Properties")
  (oset x service (concat "org.mpris.MediaPlayer2." (oref x :name)))
  (fc-dbus--register-signal x
                            "PropertiesChanged"
                            #'(lambda (intf changes _)
                                (--each changes
                                  (apply #'fc-player-dbus-cb x it))))
  (oset x volume
        (round (* (fc-dbus--get x "Volume") 100)))
  (oset x play-state
        (intern (fc-dbus--get x "PlaybackStatus")))
  (oset x meta
        (fc-dbus--get x "Metadata")))

(cl-defmethod fc-player--play-pause ((x fc-player-mpris))
  (fc-dbus--call x "PlayPause")
  (cl-call-next-method x))

(cl-defmethod fc-player--next ((x fc-player-mpris))
  (fc-dbus--call x "Next")
  (cl-call-next-method x))

(cl-defmethod fc-player--previous ((x fc-player-mpris))
  (fc-dbus--call x "Previous")
  (cl-call-next-method x))

(cl-defmethod fc-player--get-volume ((x fc-player-mpris))
  (oref x volume))

(cl-defmethod fc-player--get-play-status ((x fc-player-mpris))
  (oref x play-state))

(cl-defmethod fc-player--set-volume ((x fc-player-mpris) vol)
  (fc-dbus--set x "Volume" (/ vol 100.0)))

(cl-defmethod fc-player--get-metadata ((x fc-player-mpris))
  (let ((data (oref x meta)))
    `((artist ,(cl-second (assoc "xesam:artist" data)))
      (album ,(cl-second (assoc "xesam:album" data)))
      (title ,(cl-second (assoc "xesam:title" data))))))

(cl-defmethod fc-player--show-metadata ((x fc-player-mpris))
  (let ((data (oref x meta)))
    (cl-call-next-method x
                         (cl-first (cl-second (assoc "xesam:artist" data)))
                         (cl-first (cl-second (assoc "xesam:album" data)))
                         (cl-first (cl-second (assoc "xesam:title" data))))))

(cl-defmethod fc-player--app ((x fc-player-mpris))
  (let* ((app (oref x name))
         (apps (list app)))
    (add-to-list 'apps (downcase app))

    (let ((exe (--first (locate-file it exec-path) apps)))
      (fc-exec-command exe))))

(defun fc-player-auto-select ()
  (let ((names (--filter (string-prefix-p "org.mpris.MediaPlayer2" it)
                         (dbus-list-names :session))))
    (--first (when (member (concat "org.mpris.MediaPlayer2." it) names)
               (setf *fc-player* (fc-player-mpris :name it)))
             *fc-prefer-players*)))

(defun fc-player-user-select ()
  (setf *fc-player*
        (fc-player-mpris
         :name
         (let ((names
                (--map (s-chop-prefix "org.mpris.MediaPlayer2." it)
                       (--filter (string-prefix-p "org.mpris.MediaPlayer2." it)
                                 (dbus-list-names :session)))))
           (fc-user-select "Select player" names)))))

(provide 'fc-player-mpris)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player-mpris.el ends here
