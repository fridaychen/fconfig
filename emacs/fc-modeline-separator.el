;;; fc-modeline-separator.el --- modeline separator -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc--sep-round*
  '(("h2311"
     "0h231"
     "00h23")
    ("00h23")
    ("00h23"
     "0h231"
     "h2311")))

(defconst *fc--sep-box*
  '(("h23111")
    ("h23111"
     "hhhh23"
     "000h23"
     "000h23"
     "000h23"
     "hhhh23"
     "h23111")
    ("h23111")))

(defconst *fc--sep-wave*
  '(("h2311111111"
     "0h231111111"
     "00h23111111"
     "00h23111111"
     "000h2311111"
     "000h2311111"
     "0000h231111"
     "0000h231111"
     "0000h233111")
    ("00000h23111")
    ("00000h23311"
     "000000h2311"
     "000000h2311"
     "000000h2311"
     "0000000h231"
     "0000000h231"
     "00000000hh2")))

(defconst *fc--sep-zigzag*
  '(("h223311")
    ("0h22311"
     "00h2231"
     "000h223"
     "0000h22"
     "000h223"
     "00h2231"
     "0h22311")
    ("h223311")))

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
         (h (fc-get-face-attribute 'fc-modeline-highlight-face :background)))
    `(("0" . ,bg1)
      ("1" . ,bg2)
      ("2" . ,c2)
      ("3" . ,c3)
      ("h" . ,h))))

(defun fc--ml-height()
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
