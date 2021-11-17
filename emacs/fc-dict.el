;;; fc-dict.el --- dictionary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-enable-goldendict* (and *is-linux*
                                      (executable-find "goldendict")))
(defconst *fc-enable-online-dict* (not *fc-enable-goldendict*))

(defconst *fc-dict-buffer-name* "*fc-dict-buffer*")

(fc-load 'bing-dict
  :enable *fc-enable-online-dict*
  :autoload t)

(fc-load 'youdao-dictionary
  :enable *fc-enable-online-dict*
  :autoload t
  :after
  (progn
    (cl-defun youdao-dictionary--play-voice (word &optional (volume 25))
      "Play voice of the WORD if there has mplayer or mpg123 program."
      (let ((player (executable-find "mpv")))
        (if player
            (start-process player nil player
                           (format "--volume=%d" volume)
                           (youdao-dictionary--format-voice-url word))
          (user-error "ERROR: mpv or mpg123 is needed to play word voice"))))))

(cl-defun fc--goldendict-lookup (word)
  "Look up word by goldendict.
WORD: target word."
  (save-excursion
    (call-process "goldendict" nil nil nil word)))

(cl-defun fc-dict-lookup (word)
  "Look up in dictionary.
WORD: target word."
  (unless word
    (cl-return-from fc-dict-lookup))

  (with-current-buffer (get-buffer-create *fc-dict-buffer-name*)
    (goto-char (point-max))
    (insert (format "%s\n" word)))

  (cond
   (*fc-enable-goldendict*
    (fc-dict-speak word)
    (fc--goldendict-lookup word))

   (t
    (fc-dict-speak word)
    (bing-dict-brief word))))

(defun fc-dict-speak (words)
  "Speak words out.
WORDS: target words."
  (when *fc-enable-sound*
    (cond
     (*is-mac* (osx-lib-say words))
     (t (google-speak words)))))

(cl-defun fc-translate (string &optional (lang "zh-CN"))
  (fc-exec-command-to-string "trans" "-brief" (concat ":" lang) string))

(provide 'fc-dict)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-dict.el ends here
