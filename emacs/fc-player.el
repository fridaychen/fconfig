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
           :initform 0
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

(when *is-linux*
  (setf *fc-enable-player* *fc-enable-dbus*))

(when (and *is-linux* *fc-enable-player*)
  (require 'fc-player-mpris))

(when *is-cygwin*
  (defclass fc-player-foobar (fc-player)
    ((vol :initform 0
          :type integer)

     (playing :initform nil)))

  (defconst *fc-win-music-app* (fc-file-first-exists
                                '("/cygdrive/c/Program Files/foobar2000/foobar2000"
                                  "/cygdrive/c/Program Files (x86)/foobar2000/foobar2000")))

  (cl-defmethod fc-player--play-pause ((x fc-player-foobar))
    (fc-exec-command *fc-win-music-app* "/playpause")
    (oset x playing (if (oref x playing) nil t))
    (cl-call-next-method x))

  (cl-defmethod fc-player--next ((x fc-player-foobar))
    (fc-exec-command *fc-win-music-app* "/next")
    (cl-call-next-method x))

  (cl-defmethod fc-player--previous ((x fc-player-foobar))
    (fc-exec-command *fc-win-music-app* "/prev")
    (cl-call-next-method x))

  (cl-defmethod fc-player--get-volume ((x fc-player-foobar))
    (oref x vol))

  (cl-defmethod fc-player--get-play-status ((x fc-player-foobar))
    (if (oref x playing)
        'Playing
      'Paused))

  (cl-defmethod fc-player--set-volume ((x fc-player-foobar) vol)
    (oset x vol vol)
    (fc-exec-command *fc-assist-app* "--setappvol" (int-to-string vol)))

  (fc-exec-command *fc-win-music-app* "/pause"))

(when *is-mac*
  ;; support iTunes
  (defclass fc-player-itunes (fc-player)
    ())

  (cl-defmethod fc-player--play-pause ((x fc-player-itunes))
    (shell-command "osascript -e 'tell application \"iTunes\"' -e 'playpause' -e 'end tell'")
    (cl-call-next-method x))

  (cl-defmethod fc-player--next ((x fc-player-itunes))
    (shell-command "osascript -e 'tell application \"iTunes\"' -e 'next track' -e 'end tell'")
    (cl-call-next-method x))

  (cl-defmethod fc-player--previous ((x fc-player-itunes))
    (shell-command "osascript -e 'tell application \"iTunes\"' -e 'previous track' -e 'end tell'")
    (cl-call-next-method x))

  (cl-defmethod fc-player--get-volume ((x fc-player-itunes))
    (string-to-number (shell-command-to-string "osascript -e 'tell application \"iTunes\" to sound volume as integer'")))

  (cl-defmethod fc-player--get-play-status ((x fc-player-itunes))
    (s-trim (shell-command-to-string "osascript -e 'tell application \"iTunes\" to player state as string'")))

  (cl-defmethod fc-player--set-volume ((x fc-player-itunes) vol)
    (shell-command (format "osascript -e 'tell application \"iTunes\" to set sound volume to %d'" vol)))

  (cl-defmethod fc-player--show-metadata ((x fc-player-itunes))
    (let* ((meta (s-trim (shell-command-to-string "osascript -e 'tell application \"iTunes\" to get {artist,album,name} of current track'")))
           (data (s-split "," meta t)))
      (cl-call-next-method x
                           (s-trim (cl-first data))
                           (s-trim (cl-second data))
                           (s-trim (cl-third data)))))

  (cl-defmethod fc-player--app ((x fc-player-itunes))
    (shell-command "osascript -e 'tell application \"iTunes\" to activate'")))

(provide 'fc-player)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-music.el ends here
