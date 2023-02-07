;;; fc-modeline-mode-name.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc-mode-name* (fc-make-hash-table
                          '(
                            (awk-mode "🌳")
                            (c-mode "🇨")
                            (cc-mode "🇨")
                            (cmake-mode "🇲")

                            (conf-space-mode "⚙️")
                            (conf-unix-mode "⚙️")
                            (conf-window-mode "⚙️")

                            (emacs-lisp-mode "🇪")
                            (fundamental-mode "📃")
                            (go-mode "🇬")
                            (image-mode "🎨")
                            (java-mode "🇯")
                            (js-mode "🇯")
                            (js-json-mode "🇯")
                            (latex-mode "🇱")
                            (mhtml-mode "🕸️")
                            (org-mode "🅾️")
                            (sh-mode "🐚")
                            (text-mode "📝")
                            (plantuml-mode "🇺")
                            (python-mode "🐍")
                            (makefile-gmake-mode "🇲")
                            (markdown-mode "Ⓜ️")
                            (nxml-mode "🕸️")
                            (yaml-mode "🇾"))))

(defun fc-mode-name ()
  (gethash major-mode *fc-mode-name* mode-name))

(provide 'fc-modeline-mode-name)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modeline-mode-name.el ends here
