;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(if (file-exists-p "~/.emacs.d/fconfig/fc-init.elc")
    (byte-recompile-directory "~/.emacs.d/fconfig")
  (dolist (x '("fc-book"
               "fc-calendar" "fc-company" "fc-computer"
               "fc-dict" "fc-dired"
               "fc-edit" "fc-ergo"
               "fc-font" "fc-flycheck"
               "fc-vc" "fc-global"
               "fc-init" "fc-light-init"
               "fc-lang" "fc-latex" "fc-lisp"
               "fc-modal" "fc-music"
               "fc-org" "fc-app"
               "fc-package" "fc-program" "fc-project" "fc-python"
               "fc-screen"
               "fc-theme-config"
               "fc-util"
               "fc-yasnippet"
               "computers/common"
               "fc-helm"
               "fc-buffer" "fc-ivy"
               "fc-java" "fc-facility"
               "fc-config"))
    (byte-compile-file (concat "~/.emacs.d/fconfig/" x ".el"))))
