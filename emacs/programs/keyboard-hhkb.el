;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(cond
 (*is-linux*
  (shell-command (fc-home-path "fconfig/linux/set-keyboard.sh --hhkb"))))
