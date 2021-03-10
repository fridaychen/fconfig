;;; fc-cscope.el --- setup cscope and implement fc-project interface -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'xcscope)

(defun fc-proj-open-tags (dir)
  (setf cscope-initial-directory (expand-file-name fc-proj-dir dir))
  (setf cscope-do-not-update-database nil)

  (global-set-key [f11] 'cscope-find-this-file)
  (global-set-key "\M-." 'cscope-find-global-definition)
  (global-set-key "\M-," 'cscope-next-symbol)
  (global-set-key "\M-*" 'cscope-pop-mark))

(defun fc-proj-update-tag ()
  (interactive)

  (fc-proj-command (concat "rm " fc-proj-dir "/cscope.*"))

  (mapc (lambda (spath)
          (case system-type
                ('gnu/linux
                 (fc-proj-command (concat "find -L " (get 'fc-pinfo 'home) "/" spath
                                          " -type f -iregex '.+\\.\\(h\\|hxx\\|c\\|cpp\\|cxx\\)' >> "
                                          (get 'fc-pinfo 'home) "/" fc-proj-dir "/cscope.files")))
                ('darwin
                 (fc-proj-command (concat "find -E -L " (get 'fc-pinfo 'home) "/" "."
                                          " -type f -iregex '.+\\.(h|hxx|c|cpp|cxx)' >> "
                                          (get 'fc-pinfo 'home) "/" fc-proj-dir "/cscope.files")))
                ('cygwin
                 (fc-proj-command (concat "find -L " (get 'fc-pinfo 'home) "/" spath
                                          " -type f -iregex '.+\\.\\(h\\|hxx\\|c\\|cpp\\|cxx\\)' >> "
                                          (get 'fc-pinfo 'home) "/" fc-proj-dir "/cscope.files")))
                ('berkeley-unix
                 (fc-proj-command (concat "find -E -L " (get 'fc-pinfo 'home) "/" spath
                                          " -type f -iregex '.+\\.(h|hxx|c|cpp|cxx)' >> "
                                          (get 'fc-pinfo 'home) "/" fc-proj-dir "/cscope.files")))
                ))
        (get 'fc-pinfo 'srcs))

  (fc-proj-command (concat "cd " (get 'fc-pinfo 'home) "/" fc-proj-dir ";"
                           cscope-program " -b -k")))

(global-set-key (kbd "M-9") 'cscope-find-functions-calling-this-function)
(global-set-key (kbd "M-8") 'cscope-find-egrep-pattern)

(provide 'fc-cscope)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-cscope.el ends here
