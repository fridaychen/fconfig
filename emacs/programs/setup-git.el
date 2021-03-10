;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let ((cmds (list (concat "user.name " (read-string "User name : " user-login-name))
                  (concat "user.email " (read-string "User email : " user-mail-address))
                  "core.filemode false"
                  "core.quotepath off"
                  (concat "core.autocrlf " (if (or *is-windows* *is-cygwin*) "true" "input"))
                  "core.safecrlf true"
                  ))
      (run-git (lambda (cmd)
                 (shell-command (concat "git config --global " cmd)))))
  (mapc run-git cmds))
