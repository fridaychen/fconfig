;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let ((dir (read-directory-name "Input directory : ")))
  (with-current-buffer (get-buffer-create
                        (find-file
                         (concat dir ".dir-locals")))
    (add-dir-local-variable 'dired-mode 'dired-omit-mode 't)
    (add-dir-local-variable 'dired-mode 'dired-omit-files "")))
