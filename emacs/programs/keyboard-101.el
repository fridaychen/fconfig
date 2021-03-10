;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(cond
 (*is-linux*
  (shell-command "~/.emacs.d/fconfig/linux/set-keyboard.sh --101")))
