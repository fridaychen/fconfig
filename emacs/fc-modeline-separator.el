;;; fc-modeline-separator.el --- modeline separator -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc--sep-box*
  '(("21111")
    ("21111"
     "22222"
     "00002"
     "00002"
     "00002"
     "00002"
     "22222"
     "21111")
    ("21111")))

(defconst *fc--sep-brace*
  '(("21111111"
     "21111111"
     "21111111"
     "02111111")
    ("00211111")
    ("00211111"
     "00021111"
     "00021111"
     "00002111"
     "00002111"
     "00000221"
     "00000002"
     "00000002"
     "00000221"
     "00002111"
     "00002111"
     "00021111"
     "00021111"
     "00211111")
    ("00211111")
    ("02111111"
     "21111111"
     "21111111"
     "21111111")))

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
  '(("221111")
    ("022111"
     "002211"
     "000221"
     "000022"
     "000221"
     "002211"
     "022111")
    ("221111")))

(defconst *fc--sep-patterns* '(*fc--sep-box* *fc--sep-brace* *fc--sep-gradient* *fc--sep-wave* *fc--sep-zigzag*))

(defvar *fc-ml-sep* nil)
(defvar *fc-ml-sep-active-left* " ")
(defvar *fc-ml-sep-active-right* " ")
(defvar *fc-ml-sep-inactive-left* " ")
(defvar *fc-ml-sep-inactive-right* " ")

(defvar *fc-ml-sep-height* nil)
(defvar *fc-ml-sep-enable* t)

(defun fc--pad-pattern (width left-pad-char right-pad-char pattern)
  "Add padding data to left and right side of pattern."
  (let* ((len (length (caar pattern)))
         (left-pad-len (/ (- width len) 2))
         (right-pad-len (- width len left-pad-len))
         (left-pad (make-string left-pad-len left-pad-char))
         (right-pad (make-string right-pad-len right-pad-char)))
    (mapcar (lambda (x)
              (mapcar (lambda (y) (fc-concat left-pad y right-pad)) x))
            pattern)))

(defun fc--gen-colors (face1 face2)
  "Generate XPM color by faces."
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
                    (fc--pad-pattern (frame-char-width) ?0 ?1 (symbol-value sep)))))
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
