;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(if *fc-location-work*
    (shell-command "cd ~/.emacs.d/fconfig; git pull")
  (shell-command "cd ~/.emacs.d/fconfig; git pull remote master"))
(byte-recompile-directory "~/.emacs.d/fconfig")
