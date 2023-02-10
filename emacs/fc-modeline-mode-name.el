;;; fc-modeline-mode-name.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc-mode-name* (fc-make-hash-table
                          '(
                            (archive-mode "📦")
                            (awk-mode "🌳")
                            (c-mode "🌊")
                            (cc-mode "🌊")
                            (cmake-mode "🍄")

                            (calc-mode "🧮")
                            (conf-space-mode "⚙️")
                            (conf-unix-mode "⚙️")
                            (conf-window-mode "⚙️")

                            (debugger-mode "🪲")
                            (diff-mode "2️⃣")
                            (dired-mode "📂")
                            (emacs-lisp-mode "♉")
                            (eshell-mode "🤖")
                            (fundamental-mode "📃")
                            (go-mode "🎱")
                            (grep-mode "🔎")
                            (help-mode "ℹ️")
                            (image-mode "🎨")
                            (Info-mode "ℹ️")
                            (java-mode "☕")
                            (js-mode "☕")
                            (js-json-mode "☕️")
                            (latex-mode "🏮")
                            (magit-diff-mode "2️⃣")
                            (magit-log-mode "🪵")
                            (magit-revision-mode "2️⃣")
                            (magit-status-mode "🍱")
                            (Man-mode "ℹ️")
                            (mhtml-mode "🕸️")
                            (org-mode "🅾️")
                            (package-menu-mode "📦")
                            (plantuml-mode "🛸")
                            (python-mode "🐍")
                            (sh-mode "🐚")
                            (special-mode "📊")
                            (text-mode "📝")
                            (makefile-gmake-mode "🍄")
                            (markdown-mode "Ⓜ️")
                            (messages-buffer-mode "💬")
                            (nxml-mode "🕸️")
                            (xref--xref-buffer-mode "📚")
                            (yaml-mode "🖖"))))

(defun fc-mode-name ()
  (gethash major-mode *fc-mode-name* mode-name))

(provide 'fc-modeline-mode-name)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modeline-mode-name.el ends here
