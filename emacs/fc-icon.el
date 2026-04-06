;;; Commentary:
;;

;;; Code:

(defconst *fc-mode-name* (fc-make-hash-table
                          '(
                            (archive-mode . "📦")
                            (awk-mode . "🌳")
                            (bash-ts-mode . "🐚")
                            (c-mode . "🌊")
                            (c-ts-mode . "🌊")
                            (cc-mode . "🌊")
                            (cc-ts-mode . "🌊")
                            (cmake-ts-mode . "🍄")

                            (calc-mode . "🧮")
                            (compilation-mode . "🏗️")

                            (conf-space-mode . "⚙️")
                            (conf-toml-mode . "⚙️")
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
                            (html-mode . "🕸️")
                            (image-mode . "🎨")
                            (Info-mode . "ℹ️")
                            (java-mode . "☕")
                            (js-mode . "☕")
                            (js-json-mode . "☕️")
                            (json-ts-mode . "☕️")
                            (latex-mode . "🏮")
                            (log-edit-mode . "🪵")
                            (log-view-mode . "🪵")
                            (lua-mode . "🌛")
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
                            (typescript-ts-mode . "👕")
                            (toml-ts-mode . "⚙️")
                            (fc-ttl-mode . "🖥️")
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

(defun fc-battery-icon (percent)
  (cond
   (*is-gui*
    (cond ((>= percent 30)
           "🔋")
          (t
           "🪫")))

   (*is-colorful-term*
    (cond ((>= percent 90)
           (nerd-icons-faicon "nf-fa-battery_4"))

          ((>= percent 70)
           (nerd-icons-faicon "nf-fa-battery_3"))

          ((>= percent 50)
           (nerd-icons-faicon "nf-fa-battery_2"))

          ((>= percent 25)
           (nerd-icons-faicon "nf-fa-battery_1"))

          (t
           (nerd-icons-faicon "nf-fa-battery_0"))))))

(provide 'fc-icon)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-icon.el ends here
