;;; fc-modeline-separator.el --- modeline separator -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc--sep-box*
  '(("0000000111"
     "0000000111"
     "0000000111"
     "0000000111"
     "0001111111"
     "0001111111"
     "0001111111"
     "0001111111")))

(defconst *fc--sep-brace*
  '(("11111111111"
     "11111111111"
     "21111111111"
     "02111111111")
    ("00111111111")
    ("00211111111"
     "00011111111"
     "00021111111"
     "00001111111"
     "00002111111"
     "00000211111"
     "00000002111"
     "00000002111"
     "00000211111"
     "00002111111"
     "00001111111"
     "00021111111"
     "00011111111"
     "00211111111")
    ("00111111111")
    ("02111111111"
     "21111111111"
     "11111111111"
     "11111111111")))

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

(defconst *fc--sep-zigzag*
  '(("00111111"
     "00011111"
     "00001111"
     "00000111"
     "00001111"
     "00011111")))

(defconst *fc--sep-patterns* '(*fc--sep-box* *fc--sep-brace* *fc--sep-wave* *fc--sep-zigzag*))

(defvar *fc-ml-sep-active-left* " ")
(defvar *fc-ml-sep-active-right* " ")
(defvar *fc-ml-sep-inactive-left* " ")
(defvar *fc-ml-sep-inactive-right* " ")

(defvar *fc-ml-height* nil)

(defun fc--gen-colors (face1 face2)
  `(
    ("0" . ,(fc-get-face-attribute face1 :background))
    ("1" . ,(fc-get-face-attribute face2 :background))
    ("2" . ,(fc-get-face-attribute face1 :background))
    ))

(defun fc--ml-height()
  (or *fc-ml-height* (frame-char-height)))

(cl-defun fc-ml-sep-reset ()
  (unless *is-gui*
    (cl-return-from fc-ml-sep-reset))

  (let ((pattern (symbol-value (seq-random-elt *fc--sep-patterns*))))
    (setf
     *fc-ml-sep-active-left*
     (fc-text " " :display
              (fc-make-xpm-with-pattern
               (fc--ml-height)
               (fc--gen-colors 'fc-modeline-highlight-face 'mode-line)
               pattern))

     *fc-ml-sep-active-right*
     (fc-text " " :display
              (fc-make-xpm-with-pattern
               (fc--ml-height)
               (fc--gen-colors 'fc-modeline-highlight-face 'mode-line)
               pattern
               t))

     *fc-ml-sep-inactive-left*
     (fc-text " " :display
              (fc-make-xpm-with-pattern
               (fc--ml-height)
               (fc--gen-colors 'fc-modeline-highlight-inactive-face 'mode-line-inactive)
               pattern))

     *fc-ml-sep-inactive-right*
     (fc-text " " :display
              (fc-make-xpm-with-pattern
               (fc--ml-height)
               (fc--gen-colors 'fc-modeline-highlight-face 'mode-line-inactive)
               pattern
               t)))))

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
