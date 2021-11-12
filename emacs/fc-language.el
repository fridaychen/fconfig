;;; fc-language.el --- Language -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun fc-zh-to-number (str)
  "Convert chinese number string to number.
STR: chinese number string."
  (let ((al '((?零 . 0)
              (?一 . 1)
              (?二 . 2)
              (?两 . 2)
              (?三 . 3)
              (?四 . 4)
              (?五 . 5)
              (?六 . 6)
              (?七 . 7)
              (?八 . 8)
              (?九 . 9)
              (?十 . 10)
              (?百 . 100)
              (?千 . 1000)
              (?万 . 10000)))
        (n 0)
        (ret 0))
    (--each (append str nil)
      (let ((v (cdr (assoc it al))))
        (if (< v 10)
            (setf n (+ (* n 10) v))
          (when (and (= n 0) (= v 10))
            (setf n 1))
          (cl-incf ret (* n v))
          (setf n 0))))
    (+ ret n)))

;; detech char-script of string
(defun fc-detect-char-script (text)
  (let ((map (make-hash-table)))
    (mapc (lambda (x) (puthash (aref char-script-table x) t map))
          text)
    map))

(defun fc-detect-has-wide-char (text)
  (let ((map (fc-detect-char-script text)))
    (--first (gethash it map)
             '(han kana emoji))))

(cl-defun fc-detect-buf-has-wide-char (&optional (buf (current-buffer)) (max 128))
  (with-current-buffer buf
    (fc-detect-has-wide-char (buffer-substring 1 max))))

(provide 'fc-language)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-language.el ends here
