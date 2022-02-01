;;; fc-dired.el --- setup dired-mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'dired
  :local t

  :after (progn
           ;; enable omit submode
           (add-hook 'dired-mode-hook #'dired-omit-mode)

           (setf dired-dwim-target t
                 dired-recursive-copies 'always
                 dired-recursive-deletes 'top
                 global-auto-revert-non-file-buffers t
                 auto-revert-verbose nil
                 dired-omit-verbose nil
                 dired-listing-switches "-aBhl --group-directories-first")

           (when *is-mac*
             (setf insert-directory-program "gls"))

           (defun fc-dired-sys-open ()
             (interactive)

             (apply #'fc-exec-command "fj" "--open" (dired-get-marked-files)))

           (defun fc-dired-preview-file ()
             (interactive)

             (apply #'fc-exec-command "fj" "--preview" (dired-get-marked-files)))

           (defun fc-dired-sort ()
             (interactive)

             (dired-sort-other (pcase (fc-user-select "Sort by"
                                                      '("date" "size" "name" "folder"))
                                 ("name" "-aBhl ")
                                 ("date" "-aBhl -t")
                                 ("size" "-aBhl -S")
                                 ("folder" "-aBhl --group-directories-first")))))

  :bind `((dired-mode-map
           ("4" ,(fc-cond-key :normal 'ivy-switch-buffer))
           ("9" ,(fc-manual (kill-new (dired-get-filename))))
           ("i" previous-line)
           ("k" next-line)
           ("o" fc-dired-sys-open)
           ("r" dired-do-find-regexp-and-replace)
           ("s" fc-dired-sort)
           ("p" fc-dired-preview-file)
           ("w" wdired-change-to-wdired-mode)
           ("N" make-directory)
           (";" fc-fast-switch-window)
           ("C-o" fc-modal-run)
           ("<tab>" dired-toggle-read-only))))

(require 'dired-x)

(fc-load 'wdired
  :autoload t
  :bind '((wdired-mode-map
           ("<tab>" wdired-exit))))

(fc-load 'diredfl
  :idle t
  :after (diredfl-global-mode))

(provide 'fc-dired)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-dired.el ends here
