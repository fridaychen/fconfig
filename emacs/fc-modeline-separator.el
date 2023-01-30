;;; fc-modeline-separator.el --- modeline separator -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc--sep-round*
  '(("h012311"
     "0h01231"
     "00h0123")
    ("00h0123")
    ("00h0123"
     "0h01231"
     "h012311")))

(defconst *fc--sep-box*
  '(("h0123111")
    ("h0123111"
     "hhhh0123"
     "000h0123"
     "000h0123"
     "000h0123"
     "hhhh0123"
     "h0123111")
    ("h0123111")))

(defconst *fc--sep-wave*
  '(("h012311111111"
     "0h01231111111"
     "00h0123111111"
     "00h0123111111"
     "000h012311111"
     "000h012311111"
     "0000h01231111"
     "0000h01231111"
     "0000h01233111")
    ("00000h0123111")
    ("00000h0123311"
     "000000h012311"
     "000000h012311"
     "000000h012311"
     "0000000h01231"
     "0000000h01231"
     "00000000hh012")))

(defconst *fc--sep-zigzag*
  '(("h0123311")
    ("0h012311"
     "00h01231"
     "000h0123"
     "0000h012"
     "000h0123"
     "00h01231"
     "0h012311")
    ("h0123311")))

(defconst *fc--sep-patterns* '(*fc--sep-box* *fc--sep-gradient* *fc--sep-round* *fc--sep-wave* *fc--sep-zigzag*))

(defvar *fc-ml-sep* nil)
(defvar *fc-ml-sep-string* " ")
(defvar *fc-ml-sep-active-left* " ")
(defvar *fc-ml-sep-active-right* " ")
(defvar *fc-ml-sep-inactive-left* " ")
(defvar *fc-ml-sep-inactive-right* " ")

(defvar *fc-ml-sep-height* nil)
(defvar *fc-ml-sep-enable* t)

(cl-defun fc--pad-pattern (width left-pad-char right-pad-char pattern)
  "Add padding data to left and right side of pattern."
  (unless (> width (length (caar pattern)))
    (cl-return-from fc--pad-pattern pattern))

  (let* ((len (length (caar pattern)))
         (left-pad-len (/ (- width len) 2))
         (right-pad-len (- width len left-pad-len))
         (left-pad (make-string left-pad-len left-pad-char))
         (right-pad (make-string right-pad-len right-pad-char)))
    (mapcar (lambda (x)
              (mapcar (lambda (y) (fc-concat left-pad y right-pad)) x))
            pattern)))

(defun fc--cal-middle-color (color1 color2)
  (apply #'color-rgb-to-hex (car
                             (color-gradient (color-name-to-rgb color1)
                                             (color-name-to-rgb color2)
                                             1))))

(defun fc--gen-colors (face1 face2)
  "Generate XPM color by faces."
  (let* ((bg1 (fc-get-face-attribute face1 :background))
         (bg2 (fc-get-face-attribute face2 :background))
         (c2 (fc--cal-middle-color bg1 bg2))
         (c3 (fc--cal-middle-color c2 bg2))
         (h (fc-get-face-attribute 'fc-modeline-hl-face :background)))
    `(("0" . ,bg1)
      ("1" . ,bg2)
      ("2" . ,c2)
      ("3" . ,c3)
      ("h" . ,h))))

(defun fc--ml-height ()
  (or *fc-ml-sep-height* (frame-char-height)))

(defun fc--ml-create (face1 face2 pattern &optional reverse)
  (fc-text *fc-ml-sep-string* :display
           (fc-make-xpm-with-pattern
            (fc--ml-height)
            (fc--gen-colors face1 face2)
            pattern
            reverse)))

(defun fc--ml-create-gradient (face1 face2 &optional reverse)
  (fc-text *fc-ml-sep-string* :display
           (fc-make-xpm-with-gradient
            (* (length *fc-ml-sep-string*) (frame-char-width))
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
         *fc-ml-sep-active-left* (fc--ml-create-gradient 'fc-modeline-hl-face 'mode-line)
         *fc-ml-sep-active-right* (fc--ml-create-gradient 'fc-modeline-hl-face 'mode-line t)
         *fc-ml-sep-inactive-left* (fc--ml-create-gradient 'fc-modeline-hl-inactive-face 'mode-line-inactive)
         *fc-ml-sep-inactive-right* (fc--ml-create-gradient 'fc-modeline-hl-face 'mode-line-inactive t))
      (setf
       *fc-ml-sep-active-left* (fc--ml-create 'fc-modeline-hl-face 'mode-line pattern)
       *fc-ml-sep-active-right* (fc--ml-create 'fc-modeline-hl-face 'mode-line pattern t)
       *fc-ml-sep-inactive-left* (fc--ml-create 'fc-modeline-hl-inactive-face 'mode-line-inactive pattern)
       *fc-ml-sep-inactive-right* (fc--ml-create 'fc-modeline-hl-face 'mode-line-inactive pattern t)))))

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
