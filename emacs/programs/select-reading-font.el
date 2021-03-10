;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let ((font (fc-select-font-family)))
  (when font
    (setf *fc-read-font* font)

    (set-face-attribute 'fc-read-face nil
                        :family *fc-read-font*)))
