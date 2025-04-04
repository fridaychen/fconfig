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

(cl-defun fc--goldendict-lookup (word)
  "Look up word by goldendict.
WORD: target word."
  (save-excursion
    (call-process "goldendict" nil nil nil "-m" word)))

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

(cl-defun fc-translate (string)
  (let ((lang (if (fc-detect-has-wide-char string)
                  "en-US"
                "zh-CN")))
    (ansi-color-apply
     (fc-exec-command-to-string "trans"
       "-brief" (concat ":" lang) string))))

(provide 'fc-dict)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-dict.el ends here
