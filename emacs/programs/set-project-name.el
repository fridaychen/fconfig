;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let* ((dir (read-directory-name "Input directory : "))
       (name (read-string "Project name : "))
       (path (concat dir "/.dir-locals.el"))
       (is-exists (file-exists-p path)))
  (with-current-buffer (get-buffer-create (find-file path))
    (if is-exists
        (progn
          (add-dir-local-variable nil 'fc-proj-name name))
      (insert "((nil (fc-proj-name . \"ORC\")))"))
    (save-current-buffer)))
