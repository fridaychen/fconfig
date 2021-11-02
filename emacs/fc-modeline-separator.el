;;; fc-modeline-separator.el --- modeline separator -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc--sep-wave*
  '(("21111111111"
     "00111111111"
     "00011111111"
     "00021111111"
     "00001111111"
     "00002111111"
     "00000111111"
     "00000111111"
     "00000211111")
    ("00000011111")
    ("00000021111"
     "00000001111"
     "00000001111"
     "00000002111"
     "00000000111"
     "00000000211"
     "00000000002")))

(defvar *fc-ml-sep-active-left* "")
(defvar *fc-ml-sep-active-right* "")
(defvar *fc-ml-sep-inactive-left* "")
(defvar *fc-ml-sep-inactive-right* "")

(defun fc--gen-colors (face1 face2)
  `(
    ("0" . ,(fc-get-face-attribute face1 :background))
    ("1" . ,(fc-get-face-attribute face2 :background))
    ("2" . ,(fc-get-face-attribute face1 :background))
    ))

(defun fc-ml-sep-reset ()
  (setf
   *fc-ml-sep-active-left*
   (fc-text " " :display
            (fc-make-xpm-with-pattern
             (frame-char-height)
             (fc--gen-colors 'fc-modeline-highlight-face 'mode-line)
             *fc--sep-wave*))

   *fc-ml-sep-active-right*
   (fc-text " " :display
            (fc-make-xpm-with-pattern
             (frame-char-height)
             (fc--gen-colors 'fc-modeline-highlight-face 'mode-line)
             *fc--sep-wave*
             t))

   *fc-ml-sep-inactive-left*
   (fc-text " " :display
            (fc-make-xpm-with-pattern
             (frame-char-height)
             (fc--gen-colors 'fc-modeline-highlight-inactive-face 'mode-line-inactive)
             *fc--sep-wave*))

   *fc-ml-sep-inactive-right*
   (fc-text " " :display
            (fc-make-xpm-with-pattern
             (frame-char-height)
             (fc--gen-colors 'fc-modeline-highlight-face 'mode-line-inactive)
             *fc--sep-wave*
             t))))

(defun fc-ml-left-sep ()
  (if (fc--active-window-p)
      *fc-ml-sep-active-left*
    *fc-ml-sep-inactive-left*))

(defun fc-ml-right-sep()
  (if (fc--active-window-p)
      *fc-ml-sep-active-right*
    *fc-ml-sep-inactive-right*))

(provide 'fc-modeline-separator)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modeline-separator.el ends here
