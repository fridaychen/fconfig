;;; fc-player.el --- music player -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-enable-player* t)
(defvar *fc-prefer-players* nil)
(defvar *fc-player* nil)
(defvar *fc-player-hook* nil)

(defclass fc-player ()
  ((name :initarg :name
         :initform ""
         :type string)
   (volume :initarg :volume
           :initform 40
           :type integer)
   (vol-big-step :initarg :vol-big-step
                 :initform 10
                 :type integer)
   (vol-small-step :initarg :vol-small-step
                   :initform 5
                   :type integer)))

(cl-defmethod cl-print-object ((x fc-player) stream)
  (princ (oref x name) stream))

(cl-defmethod fc-player--show-metadata ((x fc-player) artist album track)
  "Show metadata to user.
ARTIST: artist name.
ALBUM: album name.
TRACK: current track name."
  (if (or (fc-not-void-p artist)
          (fc-not-void-p album)
          (fc-not-void-p track))
      (message "Current track : %s [%s] %s"
               artist
               album
               track)
    (message "Current track : no information")))

(cl-defmethod fc-player--volume-up ((x fc-player))
  (let* ((old-vol (fc-player--get-volume x))
         (new-vol (if (< 21 old-vol 80)
                      (+ old-vol (oref x vol-big-step))
                    (+ old-vol (oref x vol-small-step)))))
    (if (> new-vol 100)
        (setf new-vol 100))

    (fc-player--set-volume x new-vol)
    (message "Volume %d -> %d" old-vol new-vol)))

(cl-defmethod fc-player--volume-down ((x fc-player))
  (let* ((old-vol (fc-player--get-volume x))
         (new-vol (if (< 20 old-vol 81)
                      (- old-vol (oref x vol-big-step))
                    (- old-vol (oref x vol-small-step)))))
    (if (< new-vol 0)
        (setf new-vol 0))

    (fc-player--set-volume x new-vol)
    (message "Volume %d -> %d" old-vol new-vol)))

(cl-defmethod fc-player--play-pause ((x fc-player))
  (if (fc-player--playing x)
      (fc-player--show-metadata x)
    (message "%s" (fc-player--get-play-status x))))

(cl-defmethod fc-player--playing ((x fc-player))
  (eq 'Playing (fc-player--get-play-status x)))

(cl-defmethod fc-player--next ((x fc-player))
  (fc-player--show-metadata x))

(cl-defmethod fc-player--previous ((x fc-player))
  (fc-player--show-metadata x))

(cl-defmethod fc-player--app ((x fc-player))
  (message "Not implemented yet"))

(add-to-list 'load-path (concat *fc-home* "/emacs/player"))

(require 'fc-player-quodlibet)
(require 'fc-player-cmus)

(when *is-linux*
  (setf *fc-enable-player* *fc-enable-dbus*)

  (when *fc-enable-player*
    (require 'fc-player-mpris))

  (defun fc-player--get-players ()
    (fc-concat (list *fc-player-cmus*
                     *fc-player-quodlibet*)
               (when *fc-enable-player*
                 (fc-player--get-mpris-players)))))

(when *is-cygwin*
  (require 'fc-player-foobar)

  (defun fc-player--get-players ()
    (list
     (fc-player-footbar :name "foobar"))))

(when *is-mac*
  (require 'fc-player-itunes)

  (defun fc-player--get-players ()
    (list
     *fc-player-cmus*
     *fc-player-quodlibet*
     *fc-player-itunes*)))

(defun fc-player-user-select ()
  (setf *fc-player*
        (fc-select "Select player"
            (fc-player--get-players)
          :conv #'fc-string)))

(provide 'fc-player)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player.el ends here
