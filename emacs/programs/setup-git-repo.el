;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let ((repo (read-directory-name "Select git repository : ")))
  (when (and (not (null repo))
             (file-exists-p repo)
             (file-exists-p (concat repo "/.git")))
    (let ((cmds (list "core.filemode false"))
          (run-git (lambda (cmd)
                     (shell-command (concat "cd " repo ";git config --local" cmd)))))
      (mapc run-git cmds))))
