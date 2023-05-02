;;; fc-modeline-mode-name.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc-mode-name* (fc-make-hash-table
                          '(
                            (archive-mode . "📦")
                            (awk-mode . "🌳")
                            (c-mode . "🌊")
                            (cc-mode . "🌊")
                            (cmake-mode . "🍄")

                            (calc-mode . "🧮")
                            (compilation-mode . "🏗️")

                            (conf-space-mode . "⚙️")
                            (conf-unix-mode . "⚙️")
                            (conf-window-mode . "⚙️")

                            (Custom-mode . "⚙️")

                            (debugger-mode . "🪲")
                            (diff-mode . "2️⃣")
                            (dired-mode . "📂")
                            (emacs-lisp-mode . "♉")
                            (eshell-mode . "🤖")
                            (flycheck-mode . "🪲")
                            (flycheck-error-list-mode . "🪲")
                            (fundamental-mode . "📃")
                            (gnuplot-mode . "🧮")
                            (grep-mode . "🔎")
                            (help-mode . "ℹ️")
                            (image-mode . "🎨")
                            (Info-mode . "ℹ️")
                            (java-mode . "☕")
                            (js-mode . "☕")
                            (js-json-mode . "☕️")
                            (latex-mode . "🏮")
                            (log-edit-mode . "🪵")
                            (log-view-mode . "🪵")
                            (matlab-mode . "🧮")
                            (Man-mode . "👨")
                            (mhtml-mode . "🕸️")
                            (octave-mode . "🧮")
                            (org-mode . "🅾️")
                            (org-agenda-mode . "🗓️")
                            (package-menu-mode . "📦")
                            (sh-mode . "🐚")
                            (special-mode . "📊")
                            (text-mode . "📝")
                            (makefile-gmake-mode . "🍄")
                            (messages-buffer-mode . "💬")
                            (nxml-mode . "🕸️")
                            (xref--xref-buffer-mode . "📚")
                            (yaml-mode . "🖖"))))

(fc-load 'nerd-icons
  :enable *is-colorful-term*)

(cl-defun fc-mode-name (&optional (mode major-mode))
  "Get mode name.
MODE: target mode."
  (cond
   (*is-gui*
    (gethash mode *fc-mode-name* mode-name))

   (*is-colorful-term*
    (nerd-icons-icon-for-mode mode))

   (t
    mode-name)))

(defun fc-add-mode-name (mode name)
  "Add mode name.
MODE: target mode.
NAME: mode name."
  (declare (indent 1))
  (cond
   ((symbolp mode)
    (puthash mode name *fc-mode-name*))

   ((sequencep mode)
    (mapc #'(lambda (x) (puthash x name *fc-mode-name*))
          mode))

   (t
    (error "Unknown arg %s" mode))))

(provide 'fc-modeline-mode-name)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modeline-mode-name.el ends here
