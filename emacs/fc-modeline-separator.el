;;; fc-modeline-separator.el --- modeline separator -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc--sep-box*
  '(("00021111111")
    ("00021111111"
     "00022222111"
     "00000002111"
     "00000002111"
     "00000002111"
     "00000002111"
     "00022222111"
     "00021111111")
    ("00021111111")))

(defconst *fc--sep-brace*
  '(("21111111111"
     "21111111111"
     "21111111111"
     "02111111111")
    ("00211111111")
    ("00211111111"
     "00021111111"
     "00021111111"
     "00002111111"
     "00002111111"
     "00000221111"
     "00000002111"
     "00000002111"
     "00000221111"
     "00002111111"
     "00002111111"
     "00021111111"
     "00021111111"
     "00211111111")
    ("00211111111")
    ("02111111111"
     "21111111111"
     "21111111111"
     "21111111111")))

(defconst *fc--sep-wave*
  '(("21111111111"
     "00211111111"
     "00021111111"
     "00021111111"
     "00002111111"
     "00002111111"
     "00000211111"
     "00000211111"
     "00000211111")
    ("00000021111")
    ("00000021111"
     "00000002111"
     "00000002111"
     "00000002111"
     "00000000211"
     "00000000211"
     "00000000002")))

(defconst *fc--sep-zigzag*
  '(("002211111111")
    ("000221111111"
     "000022111111"
     "000002211111"
     "000000221111"
     "000002211111"
     "000022111111"
     "000221111111")
    ("002211111111")))

(defconst *fc--sep-patterns* '(*fc--sep-box* *fc--sep-brace* *fc--sep-gradient* *fc--sep-wave* *fc--sep-zigzag*))

(defvar *fc-ml-sep* nil)
(defvar *fc-ml-sep-active-left* " ")
(defvar *fc-ml-sep-active-right* " ")
(defvar *fc-ml-sep-inactive-left* " ")
(defvar *fc-ml-sep-inactive-right* " ")

(defvar *fc-ml-sep-height* nil)
(defvar *fc-ml-sep-enable* t)

(defun fc--gen-colors (face1 face2)
  (let ((bg1 (fc-get-face-attribute face1 :background))
        (bg2 (fc-get-face-attribute face2 :background)))
    `(
      ("0" . ,bg1)
      ("1" . ,bg2)
      ("2" . ,(colir-blend (color-values bg1) (color-values bg2)))
      )))

(defun fc--ml-height()
  (or *fc-ml-sep-height* (frame-char-height)))

(defun fc--ml-create (face1 face2 pattern &optional reverse)
  (fc-text " " :display
           (fc-make-xpm-with-pattern
            (fc--ml-height)
            (fc--gen-colors face1 face2)
            pattern
            reverse)))

(defun fc--ml-create-gradient (face1 face2 &optional reverse)
  (fc-text " " :display
           (fc-make-xpm-with-gradient
            (frame-char-width)
            (fc--ml-height)
            (fc-get-face-attribute face1 :background)
            (fc-get-face-attribute face2 :background)
            reverse)))

(cl-defun fc-ml-sep-reset ()
  (unless (and *is-gui* *fc-ml-sep-enable*)
    (cl-return-from fc-ml-sep-reset))

  (let* ((sep (or *fc-ml-sep*
                  (seq-random-elt *fc--sep-patterns*)))
         (pattern (unless (eq sep '*fc--sep-gradient*)
                    (symbol-value sep))))
    (if (eq sep '*fc--sep-gradient*)
        (setf
         *fc-ml-sep-active-left* (fc--ml-create-gradient 'fc-modeline-highlight-face 'mode-line)
         *fc-ml-sep-active-right* (fc--ml-create-gradient 'fc-modeline-highlight-face 'mode-line t)
         *fc-ml-sep-inactive-left* (fc--ml-create-gradient 'fc-modeline-highlight-inactive-face 'mode-line-inactive)
         *fc-ml-sep-inactive-right* (fc--ml-create-gradient 'fc-modeline-highlight-face 'mode-line-inactive t))
      (setf
       *fc-ml-sep-active-left* (fc--ml-create 'fc-modeline-highlight-face 'mode-line pattern)
       *fc-ml-sep-active-right* (fc--ml-create 'fc-modeline-highlight-face 'mode-line pattern t)
       *fc-ml-sep-inactive-left* (fc--ml-create 'fc-modeline-highlight-inactive-face 'mode-line-inactive pattern)
       *fc-ml-sep-inactive-right* (fc--ml-create 'fc-modeline-highlight-face 'mode-line-inactive pattern t)))))

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
