;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let ((cmds `("core.filemode false"
              "core.quotepath off"
              ,(concat "core.autocrlf "
                       (if (or *is-windows* *is-cygwin*)
                           "true"
                         "input"))
              "core.safecrlf true"
              ))
      (user-name (read-string "User name : " user-login-name))
      (user-email (read-string "User email : " user-mail-address))
      (run-git (lambda (cmd)
                 (shell-command (concat "git config --global " cmd)))))
  (when user-name
    (add-to-list 'cmds (concat "user.name " user-name)))

  (when user-email
    (add-to-list 'cmds (concat "user.email " user-email)))

  (mapc run-git cmds))
