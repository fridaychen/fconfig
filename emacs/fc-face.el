;;; fc-face.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(cl-defun fc-get-face (face attr &key (default t))
  "Get attribute of specific face.
FACE: target face.
ATTR: attribute.
DEFAULT: use default face."
  (let* ((face-bg (face-attribute face attr nil t)))
    (cond
     ((or (stringp face-bg)
          (numberp face-bg))
      face-bg)
     (t (when default
          (face-attribute 'default attr))))))

(defun fc-set-face (face frame &rest rest)
  "Safely set face attribute.
FACE: target face.
FRAME: target frame.
REST: all arguments."
  (when (facep face)
    (apply 'set-face-attribute face frame (flatten-list rest))))

(cl-defun fc-set-faces (faces &rest rest)
  (--each faces
    (apply #'fc-set-face it nil rest)))

(provide 'fc-face)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-face.el ends here
