;;; fc-music.el --- music player -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'dbus)
;;(fc-load 'bongo)

(defvar *fc-vol-big-step* 10)
(defvar *fc-vol-small-step* 5)

(custom-set-variables '(bongo-enabled-backends (quote (vlc))))

(defalias 'fc-music-player (fc-manual (fc-show-hide-buffer "*Bongo-Library*" #'bongo)))

(defun fc-music-play-pause ()
  "Dumb."
  (interactive))

(defun fc-music-volume-up ()
  "Volume up."
  (interactive)

  (let* ((old-vol (fc-music-get-vol))
         (new-vol (if (< 21 old-vol 80)
                      (+ old-vol *fc-vol-big-step*)
                    (+ old-vol *fc-vol-small-step*))))
    (if (> new-vol 100)
        (setf new-vol 100))

    (fc-music-set-vol new-vol)
    (message "Volume %d -> %d" old-vol new-vol)))

(defun fc-music-volume-down ()
  "Volume down."
  (interactive)

  (let* ((old-vol (fc-music-get-vol))
         (new-vol (if (< 20 old-vol 81)
                      (- old-vol *fc-vol-big-step*)
                    (- old-vol *fc-vol-small-step*))))
    (if (< new-vol 0)
        (setf new-vol 0))

    (fc-music-set-vol new-vol)
    (message "Volume %d -> %d" old-vol new-vol)))

(defun fc-music-next ()
  "Dumb."
  (interactive))

(defun fc-music-prev ()
  "Dumb."
  (interactive))

(defun fc-music-show-metadata ()
  "Dumb."
  (interactive))

(defun --music-show-metadata (artist album track)
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

(when *is-linux*
  ;; support rhythmbox
  ;; mdbus2 org.gnome.Rhythmbox3 /org/mpris/MediaPlayer2

  (defvar *fc-mpris-player* "rhythmbox")
  (defun fc-mpris-service ()
    (concat "org.mpris.MediaPlayer2."
            *fc-mpris-player*))

  (defun fc-music-player ()
    (interactive)

    (fc-exec-command *fc-mpris-player*))

  (let ((*path* "/org/mpris/MediaPlayer2")
        (*interface* "org.mpris.MediaPlayer2.Player")
        (*pl-interface* "org.mpris.MediaPlayer2.Playlists"))

    (defun fc-mpris-cmd (command)
      (dbus-call-method :session (fc-mpris-service) *path* *interface* command))

    (defun fc-mpris-get (prop)
      (dbus-get-property :session (fc-mpris-service) *path* *interface* prop))

    (defun fc-mpris-set (prop value)
      (dbus-set-property :session (fc-mpris-service) *path* *interface* prop value))

    (defun fc-mpris-pl-cmd (cmd &rest args)
      (dbus-call-method :session (fc-mpris-service) *path* *pl-interface* cmd)))

  (defun fc-music-play-pause ()
    (interactive)

    (fc-mpris-cmd "PlayPause")

    (if (equal (fc-mpris-get "PlaybackStatus") "Playing")
        (fc-music-show-metadata)
      (message "Paused")))

  (defun fc-music-get-vol ()
    (round (* (fc-mpris-get "Volume") 100)))

  (defun fc-music-set-vol (vol)
    (fc-mpris-set "Volume" (/ vol 100.0)))

  (defun fc-music-next ()
    (interactive)

    (fc-mpris-cmd "Next"))

  (defun fc-music-prev ()
    (interactive)

    (fc-mpris-cmd "Previous"))

  (defun fc-music-show-metadata ()
    (interactive)

    (fc-mpris-show-metadata (fc-mpris-get "Metadata")))

  (defun fc-mpris-show-metadata (data)
    (--music-show-metadata
     (string-join  " & " (cl-first (cl-second (assoc "xesam:artist" data))))
     (cl-first (cl-second (assoc "xesam:album" data)))
     (cl-first (cl-second (assoc "xesam:title" data)))))

  (defun fc-dbus-rhythmbox-signal-handler (interface data conf)
    (when (equal "Metadata" (car (car data)))
      (fc-mpris-show-metadata (car (car (cdr (car data)))))))

  (dbus-register-signal
   :session "org.gnome.Rhythmbox3" "/org/mpris/MediaPlayer2"
   "org.freedesktop.DBus.Properties"
   "PropertiesChanged"
   'fc-dbus-rhythmbox-signal-handler))

(when *is-mac*
  ;; support iTunes
  (defun fc-music-player ()
    (interactive)

    (shell-command "osascript -e 'tell application \"iTunes\" to activate'"))

  (defun fc-music-play-pause ()
    (interactive)

    (fc-exec-command "osascript"
                     "-e" "tell application \"iTunes\""
                     "-e" "playpause"
                     "-e" "end tell")

    (if (equal (fc-itune-get-status) "playing")
        (fc-music-show-metadata)
      (message "Paused")))

  (defun fc-music-get-vol ()
    (string-to-number (shell-command-to-string "osascript -e 'tell application \"iTunes\" to sound volume as integer'")))

  (defun fc-music-set-vol (vol)
    (shell-command (format "osascript -e 'tell application \"iTunes\" to set sound volume to %d'" vol)))

  (defun fc-music-next ()
    (interactive)

    (shell-command "osascript -e 'tell application \"iTunes\"' -e 'next track' -e 'end tell'")
    (fc-music-show-metadata))

  (defun fc-music-prev ()
    (interactive)

    (shell-command "osascript -e 'tell application \"iTunes\"' -e 'previous track' -e 'end tell'")
    (fc-music-show-metadata))

  (defun fc-itune-get-status ()
    (string-trim (shell-command-to-string "osascript -e 'tell application \"iTunes\" to player state as string'")))

  (defun fc-itune-get-metadata ()
    (string-trim (shell-command-to-string "osascript -e 'tell application \"iTunes\" to get {artist,album,name} of current track'")))

  (defun fc-music-show-metadata ()
    (interactive)

    (let* ((meta (fc-itune-get-metadata))
           (data (string-split meta "," t)))
      (--music-show-metadata (string-trim (cl-first data))
                             (string-trim (cl-second data))
                             (string-trim (cl-third data))))))

(when *is-cygwin*
  (defconst *fc-win-music-app* (fc-file-first-exists
                                '("/cygdrive/c/Program Files/foobar2000/foobar2000"
                                  "/cygdrive/c/Program Files (x86)/foobar2000/foobar2000")))

  (defun fc-music-play-pause ()
    (interactive)

    (fc-exec-command *fc-win-music-app* "/playpause"))

  (defun fc-music-next ()
    (interactive)

    (fc-exec-command *fc-win-music-app* "/next"))

  (defun fc-music-prev ()
    (interactive)

    (fc-exec-command *fc-win-music-app* "/prev"))

  (let ((saved-vol 0))
    (defun fc-music-get-vol ()
      saved-vol)

    (defun fc-music-set-vol (vol)
      (setf saved-vol vol)
      (fc-exec-command *fc-assist-app* "--setappvol" (int-to-string vol)))))

(provide 'fc-music)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-music.el ends here
