;;; fc-player-itunes.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defclass fc-player-itunes (fc-player)
  ())

(defun fc-exec-itune-cmd (cmd)
  (fc-exec-command-to-string "osascript"
    "-s" "o"
    "-e" "tell application \"Music\""
    "-e" cmd
    "-e" "end tell"))

(cl-defmethod fc-player--play-pause ((x fc-player-itunes))
  (fc-exec-itune-cmd "playpause")
  (cl-call-next-method x))

(cl-defmethod fc-player--next ((x fc-player-itunes))
  (fc-exec-itune-cmd "next track")
  (cl-call-next-method x))

(cl-defmethod fc-player--previous ((x fc-player-itunes))
  (fc-exec-itune-cmd "previous track")
  (cl-call-next-method x))

(cl-defmethod fc-player--get-volume ((x fc-player-itunes))
  (string-to-number (fc-exec-itune-cmd "sound volume as integer")))

(cl-defmethod fc-player--get-play-status ((x fc-player-itunes))
  (string-trim (fc-exec-itune-cmd "player state as string")))

(cl-defmethod fc-player--set-volume ((x fc-player-itunes) vol)
  (fc-exec-itune-cmd (format "set sound volume to %d" vol)))

(cl-defmethod fc-player--show-metadata ((x fc-player-itunes))
  (let* ((meta (string-trim (fc-exec-itune-cmd "get {artist,album,name} of current track")))
         (data (string-split meta "," t)))
    (cl-call-next-method x
                         (string-trim (cl-first data))
                         (string-trim (cl-second data))
                         (string-trim (cl-third data)))))

(cl-defmethod fc-player--app ((x fc-player-itunes))
  (fc-exec-itune-cmd "activate"))

(provide 'fc-player-itunes)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player-itunes.el ends here
