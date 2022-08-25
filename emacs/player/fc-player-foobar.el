;;; fc-player-foobar.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

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

(fc-exec-command *fc-win-music-app* "/pause")

(defun fc-player--get-players ()
  (list
   (fc-player-foobar :name "foobar")))

(provide 'fc-player-foobar)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-player-foobar.el ends here
